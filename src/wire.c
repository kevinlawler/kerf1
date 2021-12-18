#include "kerf.h"

//assert: writes are 8-byte aligned (2^{m=3})
//assert: mapping is always at least as big as file, except maybe during expansion

#pragma mark - Tools

I open_file_handle(S path, bool create)
{
  int attr = O_RDWR;
  int perms = 0666;

  S resolved = NULL;
  
  I f = -1;

  if(create)
  {
    attr |= O_CREAT;
  }

  //This strange looking method is significantly faster
  //than simply always using realpath

  errno = 0;
  f = open(path, attr, perms | O_NOFOLLOW);

  if(f > 0) return f;

  errno = 0;
  resolved = realpath(path, NULL);

  S using = path;

  if(resolved)
  {
    using = resolved;
  }
  else
  {
    SW(errno)
    {
      CS(ENOENT, if(!create) goto fail; using = path;)//file does not exist
      CD: goto fail; break;
    }
  }

  f = open(using, attr, perms);

succeed:
  if(resolved) free(resolved);
  return f;

fail:
  if(resolved) free(resolved);
  return -1;
}

int temphandle()
{
  int h;

  if(MAPPED_DISK_FORCES_PATH)
  {
    char template[256] = {0}; 
    snprintf(template, sizeof(template), "%s%s%s", MAPPED_DISK_FORCED_DIR, MAPPED_DISK_FORCED_TEMPLATE, MAPPED_DISK_FORCED_SUFFIX);
    h = mkstemps(template,strlen(MAPPED_DISK_FORCED_SUFFIX));
    unlink(template);
    return h; 
  }

  FILE *f = tmpfile();
  
  if(!f)
  {
    perror("Could not open tmpfile");
    ERROR(ERROR_VMEM);
  }

  h = fileno(f);//does not need unlink

  return h;
}

bool file_exists_valid(S path)
{
  struct stat s;
  int result = stat(path, &s);
  errno = 0;
  bool exists = (result == 0);
  return exists;
}

bool file_path_is_directory(S path)
{
  struct stat c;

  if(0 > stat(path, &c)) //'stat' follows symlinks
  {
    //perror("File handle status failed during directory check.");
    goto failed;
  }

  bool is_dir = S_ISDIR(c.st_mode);

succeed:
  return is_dir;

failed:
  //ERROR(ERROR_FILE);
  return false;
}

bool file_handle_is_directory(I handle)
{
  struct stat c;

  if(0 > fstat(handle,&c))
  {
    perror("File handle status failed during directory check.");
    goto failed;
  }

  bool is_dir = S_ISDIR(c.st_mode);

succeed:
  return is_dir;

failed:
  //ERROR(ERROR_FILE);
  return false;
}

bool file_handles_valid_match(int fd1, int fd2)
{
  //Per the <sys/stat.h> POSIX specification:
  //"The st_ino and st_dev fields taken together uniquely identify the file within the system."

  if(fd1 <= 0 || fd2 <= 0) return false; //invalid

  struct stat stat1, stat2;

  if(fstat(fd1, &stat1) < 0) return false;
  if(fstat(fd2, &stat2) < 0) return false;

  bool st_dev = (stat1.st_dev == stat2.st_dev);
  bool st_ino = (stat1.st_ino == stat2.st_ino);
  bool match = st_dev && st_ino;

  return match;
}

K pointer_for_mapped_file_via_synonymous_handle(int fd)
{
  int last = 0;

  DO(MAPPED_BLOCKS, int h = virtual_memory_blocks[i];
                    if(h <= 0 || last == h) continue;

                    if(file_handles_valid_match(fd, h))
                    {
                      return MAPPED_ORIGIN + (MAPPED_BLOCK_SIZE * i);
                    }
                    last = h;
  )

  return NULL;
}

unsigned int device_major_for_file_handle(int fd)
{
  struct stat s;

  if(fd < 0) goto fail;
  if(fstat(fd, &s) < 0) goto fail;

succeed:
  return major(s.st_dev);

fail:
  return -1;
}

#pragma mark - Disk MMAP Registry 
//we can get rid of this if we want, file handle
//stored in fixed width virtual_memory_blocks[]

K DISK_MMAP_POINTER_REGISTRY = NULL;

void disk_mmap_registry_add_entry(K mapOrigin, I mapSizeBytes, I handle, I fileSize, I isPrivate)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //mmap registry currently linked list
  //our basic sort/binary-tree-list would do better
  //needs deletes (swap to end). high watermark memory storage
  //the intervals never overlap so you get an easy comparator

  K *p = &DISK_MMAP_POINTER_REGISTRY;

  while(*p) p = (V)kP(*p)+REGISTRY_NEXT;

  K y = new_k(-LINK,REGISTRY_SIZE); 
  yP[REGISTRY_NEXT]=NULL;
  yP[REGISTRY_MAP_ORIGIN]=mapOrigin;
  yP[REGISTRY_MAP_SIZE]=(V)mapSizeBytes;
  yP[REGISTRY_FILE_HANDLE]=(V)handle;
  yP[REGISTRY_FILE_SIZE]=(V)fileSize;
  yP[REGISTRY_PRIVATE]=(V)isPrivate;//0 - shared (typical) 1 - private

  *p=y;

  return;
}

void* disk_mmap_registry_find_mapped_origin(V v)
{
  K e = disk_mmap_registry_find_entry_by_pointer_in_mapped_range(v);

  if(e) return kP(e)[REGISTRY_MAP_ORIGIN];

  return NULL;
}

K disk_mmap_registry_find_entry_by_pointer_in_mapped_range(V v)
{
  K *p = &DISK_MMAP_POINTER_REGISTRY;

  while(*p)
  {
    V w = kP(*p)[REGISTRY_MAP_ORIGIN];
    I size = POW2(((K)w)->m);

    if (w <= v && v < (V)(((S)w) + size)) return *p;

    p = (V)kP(*p)+REGISTRY_NEXT;
  }

  return NULL;
}

void disk_mmap_registry_remove_entry_for_mapped_origin_pointer(K x)
{
  bool safe = The_Thread_Safe_Flag;
  if(safe) pthread_mutex_lock(&The_Disk_Mmap_Mutex);

  K *p = &DISK_MMAP_POINTER_REGISTRY;
  K  q = NULL;

  while(*p)
  {
    q = kP(*p)[REGISTRY_NEXT];
    K k = kP(*p)[REGISTRY_MAP_ORIGIN];
    I map_size = (I)kP(*p)[REGISTRY_MAP_SIZE];
    I h = (I)kP(*p)[REGISTRY_FILE_HANDLE];

    if(k==x)
    {
      //Clean up for mapped pointer
      ///////////////////////
      
      //POTENTIAL_OPTIMIZATION_POINT
      //munmap is slow. probably this won't be an issue
      //if it ever becomes an issue, there are a few things you can do
      //one is just never unmap but mark the virtual blocks free and then map over them.  
      //other people use a separate thread to unmap...that won't work as-is for us, but we
      //can have a map/unmap queue and that would work.
      //if it's an msync-related issue, there's probably nothing we can do about it

      munmap(x, map_size); mmap_place_on_hold(x, map_size);
      //This fixed a bad crash: if you unmap wo/ re-holding, the pool can map there. Then you map over the pool incorrectly.
      //It seems like you could just re-hold the addresses without unmapping, that didn't work (vm space limit?)

      I2 freed_sequence = (I2){(((V)x) - MAPPED_ORIGIN)/MAPPED_BLOCK_SIZE, map_size/MAPPED_BLOCK_SIZE};
      mark_block_sequence(freed_sequence, 0); 

      if(h>STDERR_FILENO)close(h);
      ///////////////////////

      rd(*p);
      *p=q;

      goto finished;
    }
    p = (V)kP(*p)+REGISTRY_NEXT;
  }

finished:
  if(safe) pthread_mutex_unlock(&The_Disk_Mmap_Mutex);
  return;
}

void disk_flush_mmap_caches_via_registry()
{
  K *p = &DISK_MMAP_POINTER_REGISTRY;

  while(*p)
  {
    K y = *p;
    K map =          yP[REGISTRY_MAP_ORIGIN];
    I map_size =  (I)yP[REGISTRY_MAP_SIZE];
    I file_size = (I)yP[REGISTRY_FILE_SIZE];
    I handle =    (I)yP[REGISTRY_FILE_HANDLE];

    //I u = munmap(map, map_size);
    //V m = mmap(map, map_size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, handle, 0);

    int madvise_eval = madvise(map, map_size, MADV_DONTNEED);

    p = (V)kP(*p)+REGISTRY_NEXT;
  }

  return;
}

#pragma mark - Disk

//void* disk_alloc_with_min_lane(I bytes_requested, char min_lane, I handle)
//{
//  void *z;
//  char lane = min_lane_for_bytes(bytes_requested, min_lane);
//  z = disk_file_memory(lane, handle);
//
//  *(membuf*)z = (membuf){lane,ATTR_DISK,0,0};
//  disk_mmap_regi____stry_add_entry(z,-1);
//
//  return z;
//}
//
//void* disk_alloc_struct(I bytes_requested, I handle)
//{
//  return disk_alloc_with_min_lane(bytes_requested, DISK_LANE_MIN, handle);
//}
//K new_k_disk(C t, I n, I h)
//{
//  I b = list_size_k(t,n);
//  K x = disk_alloc_struct(b,h);  
//  x = kprep(x,t,n);
//  return x;
//}

K disk_map_handle_shared(I f)
{
  return disk_map_handle_shared_maybe_flat(f, NULL, false);
}

K disk_map_handle_shared_maybe_flat(I f, K starting_value, bool flat_file)
{
  //flat_file means, like text, no Kerf-struct header. Don't append to them
  //Don't let users see flat files as objects. but we can use them internally

  //TODO: where are we closing handles on errors?

  K z = NULL;

  bool safe = The_Thread_Safe_Flag;
  if(safe) pthread_mutex_lock(&The_Disk_Mmap_Mutex);

  struct stat c;
  if(0 > fstat(f,&c))
  {
    perror("File status failed");
    goto failed;
  }

  I file_size = c.st_size;

  if(0 == file_size && !flat_file)//New or empty file
  {
    if(!starting_value)
    {
      starting_value = new_table();
    }
    else
    {
      starting_value = strong(starting_value);
    }

    work_push(starting_value);
    write_k_to_handle(starting_value, f, false);
    work_pop_rd(true);
    z = disk_map_handle_shared(f);
    goto succeeded;
  }

  if(0 > flock(f,LOCK_EX|LOCK_NB))
  {
    if(MAPPED_REDIRECT_REOPEN_TO_EXISTING)
    {
      K mapped = pointer_for_mapped_file_via_synonymous_handle(f);
      if(mapped)
      {
        K x = strong(mapped);
        The_Hacky_Reused_A_Mapping_Flag = true;
        z = x;
        goto succeeded;
      }
    }

    perror("Lock file failed");
    goto failed;
  }

  if(-1 == fcntl(f, F_SETFD, FD_CLOEXEC)) //this fixes a problem with reset() under DEBUG in the leak-tracker 
  {
    perror("File could not set close-on-exec");
  }

  if(MAPPED_FILES_DOUBLE_IN_SIZE)
  {
    I r = round_up_nearest_power_of_2(file_size);
    if(file_size < r)
    {
      if(0 > handle_expander(r, f))
      {
        perror("Reinitializing file to power of two size failed");
        goto failed;
      }
    }
  }

  //Map existing file
  ///////////////////////////////////
  I requested_bytes = file_size;

  if(flat_file)
  {
    requested_bytes += PAGE_SIZE_BYTES; //for faux header
  }

  I minimum_acceptable = requested_bytes;
  I room_to_spare = requested_bytes * 4;

  I minimum_blocks = ceil(minimum_acceptable / (F)MAPPED_BLOCK_SIZE);
  I ideal_blocks = ceil(room_to_spare / (F)MAPPED_BLOCK_SIZE);

  I2 sequence = widest_block_sequence();

  I available_sequence_position = sequence.x;
  I available_sequence_blocks = sequence.y;

  if(available_sequence_blocks < minimum_blocks)
  {
    errno = ENOMEM;
    perror("Not enough virtual memory space for mapping. Map failed");
    goto failed;
  }

  if(!MAPPED_INIT_EXPECTS_FUTURE_GROWTH)
  {
    ideal_blocks = minimum_blocks;
  }

  I ask_blocks = MIN(ideal_blocks, available_sequence_blocks); 

  I mapping_bytes = ask_blocks * MAPPED_BLOCK_SIZE;

  I position = 0;

  if(MAPPED_HEURISTIC_HALVES_WIDEST)
  {
    position = available_sequence_position + ceil((available_sequence_blocks - ask_blocks)/2.0);
  }
  else
  {
    position = available_sequence_position;//left flush in widest
  }

  if(available_sequence_blocks == MAPPED_BLOCKS) //all is clear, use leftmost position instead of center
  {
    position = 0;
  }

  V memory_location = MAPPED_ORIGIN + position*MAPPED_BLOCK_SIZE;
  
  V memory_location_file_map = memory_location;
  I mapping_bytes_file_map = mapping_bytes;

  if(flat_file)
  {
    memory_location_file_map += PAGE_SIZE_BYTES;
    mapping_bytes_file_map -= PAGE_SIZE_BYTES;
  }

  K fake = NULL;

  //POTENTIAL_OPTIMIZATION_POINT
  //here and in other uses of mmap(), OSX (but not Linux currently) allows MAP_NOCACHE

  z = mmap(memory_location_file_map, mapping_bytes_file_map, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, f, 0);

  if(MAP_FAILED == z)
  {
    perror("File map failed");
    goto failed;
  }

  if(file_size != 0 && !flat_file) // z->t into empty z gives bus err
    { 
    if(ZIP == zt && -STR32 == z->nk && file_size !=0)
      {
        //POTENTIAL_OPTIMIZATION_POINT
        //1. if the zips I-index (at KEYS) expands, it will
        //   push under these values... (solution is unlock/relock)
        //2. mlocking here doesn't mlock at write time (I don't think)
        V v = kN(z, VALUES);
        v -= ((I)v) % PAGE_SIZE_BYTES;
        mlock(v, 2*PAGE_SIZE_BYTES); //lock the ZIP's STR32 cache thing into RAM
      }
  }

  if(flat_file)
  {
    z = mmap(memory_location, PAGE_SIZE_BYTES, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANON|MAP_FIXED, -1, 0);

    if(MAP_FAILED == z)
    {
      perror("File map flat file header failed");
      goto failed;
    }

    zm = floor_log_2(mapping_bytes);  
    za = ATTR_DISK;
    zh = 0;
    zt = -CHAR;
    zn = mapping_bytes - sizeof(K0); 

    fake = ((V)z) + PAGE_SIZE_BYTES - sizeof(K0);

    fake->m = floor_log_2(mapping_bytes);;
    fake->a = ATTR_DISK;
    fake->h = 0;
    fake->t = -CHAR;
    fake->r = TENANT_REF_SIGNAL;
    fake->n = file_size;
  }

  I2 used_sequence = (I2){position, ask_blocks};
  mark_block_sequence(used_sequence, f); 
  ///////////////////////////////////

  //optionally verify mapped file sanity here (makes map non-instantaneous so we don't do by default)
  //detailed code, tricky, not much value at all, rarely used paths

  //endian detection would go here before you set zr
  zr = 1; //zr = f;
  if(USES_CREF(z))zw=0;//compiled refs

  if(!GET_ATTR(z,ATTR_DISK)) SET_ATTR(z,ATTR_DISK);

  disk_mmap_registry_add_entry(z,mapping_bytes,f,file_size,false);

succeeded:
  if(safe) pthread_mutex_unlock(&The_Disk_Mmap_Mutex);
  return z;

failed:
  if(safe) pthread_mutex_unlock(&The_Disk_Mmap_Mutex);
  ERROR(ERROR_FILE);
  fprintf(stderr,"Warning: object opened as in-memory, not attached to disk.");
  return Kn();
}

#pragma mark - Handle Sizing

I handle_expander(I requested_bytes, I handle)
{
  struct stat c;

  if(0 > fstat(handle, &c))
  {
    perror("File status failed while expanding disk");
    goto failed;
  }

  I existing = c.st_size;

  if (requested_bytes <= existing)
  {
    return 0;
  }

  return handle_truncate(requested_bytes, handle);

failed:
  return -1;
}

I handle_truncate(I requested_bytes, I handle)
{
  int h = handle;

  if(h<0)
  {
    perror("Bad file handle when reserving disk");
    goto failed;
  }

  struct stat c;
  if(0 > fstat(h,&c))
  {
    perror("File status failed while reserving disk");
    goto failed;
  }

  I existing = c.st_size;

  I missing = requested_bytes - existing;

  if(DISK_ALLOCATE_FORCES_ENOSPC && missing > 0)
  {
#if defined(posix_fallocate)
    int e = posix_fallocate(h, existing, missing);
    if(e)
    {
      errno = e;//sic, posix_fallocate does not set errno
      perror("Bad fallocate reserving disk");
      goto failed;
    }
#else
    //slow, pre-write file with zeroes
    if(0 > lseek(h, 0, SEEK_END))
    {
      perror("File seek failed");
      goto failed;
    }

    if(0 > brute_write_empty(missing, h, false, NULL, NULL, true))
    {
      perror("Bad write reserving disk");
      goto failed;
    }
    
    if(fsync(h))
    {
      perror("Bad fsync reserving disk");
      goto failed;
    }
#endif
  }
  else
  {
    int t = ftruncate(h, requested_bytes);
  
    if(t<0)
    {
      perror("Bad truncate reserving disk");
      goto failed;
    }
  }

  return 0;

failed:
  //ERROR(ERROR_DISK);
  return -1;
}

#pragma mark - Handle Read/Write

I sendall(I socket_fd, S buffer, I length)
{
  I progress = 0;
  I remain = length;
  I sent = 0;
  while(progress < length)
  {
    //int flags = MSG_MORE | MSG_DONTWAIT | MSG_NOSIGNAL ;
    int flags = 0;

    sent = send(socket_fd, buffer + progress, remain, flags);
    if(-1 == sent)break;
    progress += sent;
    remain -= sent;
  }

  if(-1 == sent)
  {
    perror("Error in send");
    return -1;
  }

  return 0;
}

I brute_write(V buf, I count, I h, I socket, FILE *file, V *memory)
{

  if(memory)
  {
    memcpy(*memory, buf, count);
    *memory = ((char*) *memory) + count;
    return 0;
  }
  else if(file)
  {
    size_t written = fwrite(buf, count, 1, file);

    if(ferror(file))
    {
      perror("Writing error");
      return -1;
    }

    return 0;
  }
  else if(socket)
  {
    return sendall(h, buf, count);
  }

  I p = 0, w = 0;
  for (p = 0; p < count; p += w)
  {
    errno = 0;

    if (0>=(w = write(h, ((S)buf)+p, MIN(count - p, sizeof(buf)))))
    {
      if (0 == w)
      {
        perror("Wrote 0 bytes");
        continue;
      }

      if (-1 == w && errno == EINTR) continue;
      return -1;
    }
  }
 
  return 0;
}

I brute_write_empty(I count, I h, I socket, FILE *file, V *memory, bool force_write_empty_bytes)
{

  bool can_seek = !force_write_empty_bytes;

  if(can_seek)
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //FALLOC_FL_PUNCH_HOLE on Linux at least
    //maybe for holes beyond a certain size?
    //(you can always do this, just set the ->m higher for whatever trailing space)
    //note: should respect DISK_ALLOCATE_FORCES_ENOSPC

    //POTENTIAL_OPTIMIZATION_POINT
    //this skips ahead (for regular files at least)
    //but not writing zeroes is...possibly...bad for the zipper
    //since garbage may not zip as well.
    //is there an efficient way to punch zeroes here?
    //(note: it isn't brute write as below. that's why we did lseek.)
    //wait. is it even possible for this to write in a fashion that
    //DOESN'T leave zeroes? since we ftruncate the whole new file beforehand usu.?
    //
    //I think I solved this problem by: disk_ensures_zeroed_space_on_fresh_write
    //which truncates to 0 first then the desired size

    if(memory)
    {
      *memory =  ((char*) *memory) + count;
      return 0;
    }
    else if(file)
    {
      off_t t = fseek(file, count, SEEK_CUR);

      if(t == (off_t)-1)
      {
        perror("Bad seek for empty file write");
        return -1;
      }

      return 0;
    }
    else
    {
      off_t t = lseek(h, count, SEEK_CUR);

      if(t == (off_t)-1)
      {
        perror("Bad seek for empty handle write");
        return -1;
      }
    }

    return 0;
  }

  I n = PAGE_SIZE_BYTES;
  char buf[n];
  DO(n, buf[i]=0)

  I remain = count;

  while (remain > 0)
  {
    I p = MIN(remain, n);
    if (0 > brute_write(buf, p, h, socket, file, memory)) return -1;
    remain -= p;
  }

  return 0;
}

I wire_size_K(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //recursive method recomputes n^2 in depth

  I a = 0;
  
  SW(abs(xt))
  {
   CSF(LINK,)
   CSF(CHAR,)
   CSF(INT,)
   CSF(FLOAT,)
   CSF(STAMP,)
    CS(NIL, a = round_up_nearest_power_of_2(any_size_K(x)))
 
   CSF(FUNC,)
   CSF(LIST,)
   CSF(MAP,)
   CSF(HASH,)
   CSF(BTREE,)
   CSF(TABLE,)
   CSF(ATLAS,)
   CSF(PARTABLE,)
   CSF(DATABASE,)
    CD: a = round_up_nearest_power_of_2(any_size_K(x));
        NEST(x, a+=wire_size_K(v))
        a = round_up_nearest_power_of_2(a);
        break;
  }

  return a;
}

I write_k_to_handle(K x, I h, C zip_type)
{
  return write_k_to_handle_options(x, h, zip_type, MESSAGE_EXECUTION_NONE, MESSAGE_RESPONSE_NO_ACK, MESSAGE_DISPLAY_NONE);
}

I write_k_to_handle_options(K object, I h, C zip_type, C execution_type, C response_type, C display_type)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //reducing the number of write() calls could speed things up
  //fwrite may be faster than write() for certain kinds of file writing (!) (lots of NEST items)
  //fclose(fdopen(dup(fd)))
  //we may want a FILE* work_stack for such a thing 
  //(see the new write_k_helper) 

  //POTENTIAL_OPTIMIZATION_POINT
  //it might be faster for regular files to write with mmap
  //(assuming you don't need fsync at the end)
  //as the memory buffer might let you return/terminate early.
  //time this.
  //you could also modify the empty/garbage/whitespace writer
  //for mmap to simply skip over those areas 

  //ZIP: to zip onto disk you want two-pass, over inet you want interleaved
  //Probably 1 signature option that affects disk only (causes two-pass zip)
  //Interleaved inet zip is autodetected (don't do if already zipped, too small, random, etc.)
  //--you can do "interleaved two-pass" to get _only zipped size_ and then still interleave
  //--interleave needs to be set as a flag on the send header
  //
  //INET:
  //
  //how can we stream a huge file send from disk? interleaved
  //
  //regular ZIP type or interleaved zip send or both?
  //
  //if it's already a zip file (xt==ZIP) you probably
  //don't want to interleaved zip it (zips twice)
  //
  //create a buffer: automatic or -CHAR
  //have a method for adding to/filling buffer
  //have a method to flush buffer
  //handle ragged endpiece
  //optionally: send or zip and send
  //you can probably just take over+rewrite brute_write (& create a flush thing)
  //to use a maybe-zipped buffer
  //
  //interleaved method:
  //zip every page
  //send header in front of zipped page
  //repeat: header, zipped_page, header, zipped_page
  //
  //two pass method:
  //look at data and make determination
  //build index via real zip (pass 1)
  //look at index and make determination
  //send index
  //send data streaming while zipping (pass 2)
  //
  //
  //  char buf[PAGE_SIZE_BYTES];
  //  I p = 0, w = 0;
  //  for (p = 0; p < missing; p += w)
  //  {
  //    if (0>=(w = write(h, buf, MIN(missing - p, sizeof(buf)))))
  //    {
  //      perror("Bad write reserving disk");
  //      goto failed;
  //    }
  //  }
  // 

  K x = strong(object);
  I wire_size = wire_size_K(x);

  bool network_order = false;
 
  struct stat c;
  if(0 > fstat(h,&c))
  {
    perror("File handle status failed while writing");
    goto failed;
  }

  bool regular_file  = S_ISREG( c.st_mode);
  bool link_handle   = S_ISLNK( c.st_mode);
  bool socket_handle = S_ISSOCK(c.st_mode);
  bool pipe_fifo     = S_ISFIFO(c.st_mode);
  bool char_term     = S_ISCHR( c.st_mode);

  bool force_write_empty_bytes = true;

  if(regular_file)
  {
    //TODO: Potential optimization point:
    //It's probably faster to mmap shared the handle
    //(it's ok if another exists)
    //and use the new memory writing utility (write_k_helper *memory)
    //or the FILE* utility
    //to write the file that way 
    //the theory is this avoids calls to write()

    force_write_empty_bytes = false;

    bool disk_ensures_zeroed_space_on_fresh_write = true;
    //I think the only reason you'd have to worry about this
    //is if for some (strange?) reason you modify this function to
    //write into the middle of a file
    if(disk_ensures_zeroed_space_on_fresh_write)
    {
      if(0 > handle_truncate(0, h))
      {
        perror("Failed shrinking, zeroing regular file while writing");
        goto failed;
      }
    }

    I needed = wire_size;
    if(0 > handle_truncate(needed, h))
    {
      perror("Failed sizing regular file while writing");
      goto failed;
    }

    if(0 > lseek(h, 0, SEEK_SET))
    {
      perror("Failed seeking in regular file while writing");
      goto failed;
    }

  }
  else if(pipe_fifo || char_term)
  {

  }
  else if(socket_handle)
  {
    SW(zip_type)
    {
      CS(MESSAGE_ZIP_COMPRESSED_PAYLOAD, 
        
        bool zeroes_unused_space = true;
        if(zeroes_unused_space) zero_unused_space(x);

        K compressed = new_zipped_charvec_temp(x);

        I alt_wire_size = wire_size_K(compressed);

        if(alt_wire_size < wire_size )
        {
          rd(x);
          x = compressed;
          wire_size = alt_wire_size;
        }
        else
        {
          rd(compressed);
          zip_type = MESSAGE_ZIP_NONE;
        }
      )

    }


    network_order = false;

    MESSAGE0 header = 
    {
      .endianness         = 0, //TODO: 1 if big-endian?
      .channel_type       = 0,
      .zip_type           = zip_type,
      .packing_type       = 0,
      .execution_type     = execution_type,
      .response_type      = response_type,
      .display_type       = display_type,
      //.display_type       = MESSAGE_DISPLAY_SHOW_MESSAGE,
      .nested_k_wire_size = htonll(wire_size), 
    };

    //POTENTIAL_OPTIMIZATION_POINT: MSG_MORE
    I sent = sendall(h, (S)&header, sizeof(header));
    if(-1 == sent)
    {
      perror("connection: send header error");
    }

  }

  if (write_k_helper(x,h,network_order,0, socket_handle, NULL, NULL, force_write_empty_bytes)) goto failed;

  rd(x);
  return 0;

failed:
  rd(x);
  perror("Bad write");
  return -1;
}

K0 header_to_network_order(K0 r)
{
  r.m = r.m;
  r.a = r.a;
  r.h = r.h;
  r.t = r.t;
  r.r = htonl(r.r);
  
  switch(r.t)
  {
   CSF( FLOAT,)
    CS( CHAR, break;) //fine as-is
 
   CSF( NIL,) 
   CSF( INT,)
   CSF( STAMP,)
   CSF(-STAMP,)
   CSF(-FLOAT,)
   CSF(-INT,)
   CSF(-CHAR,)
    CS( LIST, r.n = htonll(r.n))//uses int64_t for .n

   CSF( FUNC,)
   CSF( MAP,)
   CSF( HASH,)
   CSF( BTREE,)
   CSF( TABLE,)
   CSF( ATLAS,)
   CSF( PARTABLE,)
   CSF( DATABASE,)
   CSF( ZIP, ) 
    CD: r.w = htonl(r.w);//uses int32_t for .w plus 4 individual bytes
        r.w = 0;//right?
  }

  return r;
}

I write_k_helper(K x, I handle, I useNetworkOrder, I depth, I socket, FILE *file, V *memory, bool force_write_empty_bytes)
{
  //just writes

  if (depth > IO_MAX_DEPTH)
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //depth is not handled super well. wire sizes recomputed, etc.
    errno = EBADMSG;
    perror("Depth limit exceeded");
    return -1;
  }

  bool zeroes_unused_space = false; //I think this is covered by brute_write_empty. maybe we can put it on a toggle w/ force_write_empty_bytes

  if(zeroes_unused_space) zero_unused_space(x);//reused obj could be random-filled. zeroes help compression+sparseness

  I wire_size = wire_size_K(x);
  I self_size = any_size_K(x);

  //Write the header with ATTR_DISK set
  bool sets_disk = socket?false:true; 
  //I don't think this is necessary if it isn't a disk file? jumps are enough
  //2015.07.06 oh, it's necessary. breaks test_allocations on disk at least. you
  //have to set it on the sub-children, so that's why basically. maybe we can
  //get away with not setting it on the socket sends? I don't think so:
  //what if you send a file over socket from disk to memory?
  //or from memory to disk over the socket?
  //in the first case you need it off, the second on.
  //I think sets_disk needs to be a function argument and a lock (not a toggler) or something
  //When file-write-via-receive is implemented you'll need to worry about this
  //2016.09.16 you can't set IS_DISK/ATTR_DISK on files not actually on disk (eg socket/inet)
  //           so long as void rd(K x) doesn't have special code to not go looking
  //           in the mmap registry and to instead free it

  K0 r = *x;
  if(sets_disk) SET_ATTR(&r,ATTR_DISK);
  r.r = (0==depth)?1:TENANT_REF_SIGNAL;
  r.m = ceiling_log_2(wire_size); 
  if(IS_NEST(x)) r.h = ceiling_log_2(self_size);
  
  if (useNetworkOrder)
  {
    r = header_to_network_order(r);
  }

  if(brute_write(&r,sizeof(r),handle,socket,file,memory))
  {
    R -1;
  }

  //Remember to convert LINK/pointers/in-memory to HOP/flat/DISK format

  switch(xt)
  {
   CSF( CHAR,)
   CSF( INT,)
   CSF( FLOAT,)
   CSF( STAMP,) //atoms finished sending with the header
    CS( NIL, break;) 
 
   CSF(-FLOAT,) //send everything minus the header 
    CS(-CHAR, if(brute_write(((S)x)+sizeof(r),wire_size-sizeof(r),handle,socket,file,memory)) R -1;)


   CSF(-STAMP,) //same as above but network order
    CS(-INT, DO((wire_size - sizeof(r)) >> log_size_of_type_element[INT],//not ->n b/c of ATTR_FILL
                    I a = kI(x)[i];
                    if(useNetworkOrder) a = htonll(a);
                    if(brute_write(&a,sizeof(a),handle,socket,file,memory)) R -1;
             )
      )   

   CSF( FUNC,)
   CSF( MAP,)
   CSF( HASH,)
   CSF( BTREE,)
   CSF( TABLE,)
   CSF( ATLAS,)
   CSF( PARTABLE,)
   CSF( DATABASE,)
   CSF( ZIP,)
   CSF( LIST,)
    CD:
    { 
        I sum = 0;

        //write indexes (JUMPs)
        NEST(x,
          K0 s = kj0(useNetworkOrder?htonll(sum):sum);
          if(sets_disk) SET_ATTR(&s,ATTR_DISK);
          if(brute_write(&s,sizeof(s),handle,socket,file,memory)) R -1;
          I size = wire_size_K(v);
          sum += size;
        )

        //write whitespace (indexes)
        I missing_index_space = round_up_nearest_power_of_2(self_size) - self_size;
        if(brute_write(self_size+(V)x, missing_index_space, handle, socket, file,memory)) R -1;

        //write payload objects
        NEST(x, write_k_helper(v, handle, useNetworkOrder, depth+1, socket, file, memory, force_write_empty_bytes))

        //write whitespace (payload)
        I missing_payload_space = POW2(r.m) - (POW2(r.h) + sum); 
        brute_write_empty(missing_payload_space, handle, socket, file, memory, force_write_empty_bytes);//use this because in-mem won't have space
        break;
    }
  }

  return 0;
}

I read_disk_k_from_handle_to_membuf_of_known_good_size(V x, I h)
{
  char buf[PAGE_SIZE_BYTES];
  DO(sizeof(buf),buf[i]=0)
  I r = 0, w = 0;
  while(1)
  {
    if (0>=(r = read(h, buf, sizeof(buf))))
    {
      if(0==r) return 0;//done
      if (-1 == r && errno == EINTR) continue;
      perror("Bad read");
      return -1;
    }
    else
    {
      memcpy(x+w,buf,r);//TODO: this memcpy only works for JUMP (~DISK) objects
      w+=r;
    }
  }

  return 0;
}

K read_k_from_handle(K x, I h)
{
  //IS_FILE(K x): should be OK - writing to file
  //              shouldn't have to set DISK attribute to YES
  //              it's disk/flat/HOP format which is OK
  //TODO:  IS_SOCKET(h): may have to interleave unzip, etc 
  //TODO: !IS_FILE(K x): this is memory 
  //                     set DISK attribute to NO,  everywhere
  //                     also convert to in-memory LINK version
  //TODO: read NETWORK ORDER
  //
  //TODO: the way to proceed is to read in headers and go from there,
  //      it's the reverse of what we did w/ headers in write_k
  //TODO: note: file read has PEEK option for header

  //how can we stream a huge, maybe zipped file recv to disk?
  //mmap shared the handle and write to it like a memory address

  struct stat c;
  char regular_file;
  char link_handle;
  char socket_handle;
  char pipe_fifo;
  char char_term;

  if(0 > fstat(h, &c))
  {
    perror("File handle status failed while reading");
    goto failed;
  }

  regular_file  = S_ISREG( c.st_mode);
  link_handle   = S_ISLNK( c.st_mode);
  socket_handle = S_ISSOCK(c.st_mode);
  pipe_fifo     = S_ISFIFO(c.st_mode);
  char_term     = S_ISCHR( c.st_mode);

  if(regular_file)
  {

  }
  else if(pipe_fifo || char_term)
  {

  }
  else if(socket_handle)
  {
    //TODO: I don't know how we're supposed to multiplex this wo/ blocking on a
    //      full receive - sounds like a bad idea
  }
  
  //temporary
  if(0 > read_disk_k_from_handle_to_membuf_of_known_good_size(x,h))
  {
    goto failed;
  } 

  return x;

failed:
  perror("Bad read");
  return NULL;
}

#pragma mark - Expand
I lineage_index(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT - use binary search instead
  NEST(x, if(v<=y && ((V)y)<(total_space_K(v)+(V)v)) return i)

  return -1;
}

I populate_growth_tracking(K x, K y, I m_requested, V pointers[], C revised_ems[], I depth)
{
  if (depth > IO_MAX_DEPTH)
  {
    errno = EBADMSG;
    perror("Depth limit exceeded");
    goto failed;
  }

  pointers[depth] = x;

  if (x==y)
  {
    revised_ems[depth] = m_requested;
    return m_requested; 
  }

  I index = lineage_index(x,y);

  K child = kJ(x,index);

  I child_wants_m = populate_growth_tracking(child, y, m_requested, pointers, revised_ems, depth+1);

  I delta = POW2(child_wants_m) - POW2(child->m); 
    
  I revised_space = delta + (nest_payload_end(x) - (V)x);

  I wanted_m = ceiling_log_2(revised_space);

  revised_ems[depth] = wanted_m;

  return wanted_m;

failed:
  return -1;
}

K _expand_disk_nest_special(K x, I indexes_needed, I revised_payload_used, I zero_new)
{
  I revised_h  = ceiling_log_2(sizeof(K0)*(1+indexes_needed));
  I revised_pl = ceiling_log_2(MAX(sizeof(K0),revised_payload_used));//don't take log(0)
  I revised_m  = ceiling_log_2(POW2(revised_h)+POW2(revised_pl)); 

  x = _expand_disk_backed(x, revised_m, zero_new);

  //"_expand_disk_index(x, revised_h, zero_new)"
  if(revised_h > xh)
  {
    V old = kENDH(x);
    I delta_index = POW2(revised_h) - POW2(xh);
    I displaced = MAX(revised_payload_used, nest_payload_used(x));

    //overloading for promote/nestset hack
    if(IS_VECTOR(x)) displaced = round_up_nearest_multiple_of_8(any_size_K(x)) - sizeof(K0);

    //POTENTIAL_OPTIMIZATION_POINT
    //1. there's a memmove here and in _expand_disk_backed
    //   zero or one or both of these can be optimized
    //   since skipping moving the garbage/whitespace bytes around is much faster
    //   (2x or 4x or ???). you'd have to manually move each object
    //   in the nesting stack I think or something
    //2. For these memmove you could also PUNCH_HOLE whitespace
    //3. Linux at least lets you PUNCH_HOLE but also *INSERT* holes in O(1)
    //   this is awesome and prob faster than any other expansion method
    //   based on disk roll / memmove. it depends on a fixed block size
    //   but we always get that above a certain power of 2

    memmove(old + delta_index, old, displaced); 

    if(zero_new)
    {
      bzero(old, delta_index);
    }

    xh = revised_h;
  }

  xm = revised_m;

  return x;
}

K _expand_disk_backed(K x, I m_requested, I zero_new)
{
  if(m_requested <= xm)
  {
    return x;//success, already compliant
  }

  K y = disk_mmap_registry_find_entry_by_pointer_in_mapped_range(x);
  K map = yP[REGISTRY_MAP_ORIGIN];

  V pointers[IO_MAX_DEPTH+1] = {0};
  C revised_ems[IO_MAX_DEPTH+1] = {0};

  if(0 > populate_growth_tracking(map, x, m_requested, pointers, revised_ems, 0))
  {
    ERROR(ERROR_DEPTH);
    goto failed;
  }

  I i;

  K z = NULL;
  V prev_old_end = NULL;

  for(i=0; x!=z; i++)
  {
    z = pointers[i];

    I revised = revised_ems[i];
    I original = zm; 

    if(revised > original)
    {

      if(0==i)//i.e., z==map
      {

        I delta = POW2(revised) - POW2(original);
        V split = kENDM(z);

        if(IS_NEST(z)) prev_old_end = nest_payload_end(z);

        if(0 > _disk_roll(y, delta))
        {
          ERROR(ERROR_VMEM);
          goto failed;
        }

      }
      else
      {

        //POTENTIAL_OPTIMIZATION_POINT
        //wherever it is that you're doubling these vectors, (ed. it's here)
        //on linux & other compatible unices, use FALLOC_FL_KEEP_SIZE/ FALLOC_FL_PUNCH_HOLE
        //(you can always do this, just set the ->m higher for whatever trailing space)
        //to keep the file sparse. so side-by-side vectors v1|v2 expanded to v1 v1morespace | v2 v2morespace
        //you punch holes at both of the morespaces

 
        V old_end = kENDM(z);
        V new_end = POW2(revised) + (S)z;

        //update following JUMPs
        //POTENTIAL_OPTIMIZATION_POINT
        //you could binary search for the starting point
        //you could also possibly work this update in someplace better
        K q = pointers[i-1]; 
        I boxes = box_count_K(q);
        I delta = new_end - old_end;

        V parent_payload_end = nest_payload_end(q);

        DO(boxes, if(kJ(q,i)>z) k0i(q,i)->n += delta)

        I width = prev_old_end - old_end;

        if(!prev_old_end)//eat into parent which didn't expand
        {
          width = parent_payload_end - old_end;
        }

        //slide all following bytes into place
        memmove(new_end, old_end, width);

        if(zero_new)
        {
          bzero(old_end, new_end - old_end);
        }

        if(IS_NEST(z)) prev_old_end = nest_payload_end(z);
        else prev_old_end = NULL;//assert: will never use value, loop terminates
      }

      zm = revised;
    }
    else prev_old_end = 0;

  }

  return x;
failed:
  return NULL;
}

I _disk_roll(K mmap_registry_entry, I delta)
{

  //assert: 0 == (delta % 8) for alignment

  K y = mmap_registry_entry;
  K map = yP[REGISTRY_MAP_ORIGIN];
  I map_size = (I)yP[REGISTRY_MAP_SIZE];
  I file_size = (I)yP[REGISTRY_FILE_SIZE];
  I handle = (I)yP[REGISTRY_FILE_HANDLE];

  I in_use_size = total_space_K(map);//could store on registry

  I revised_size = in_use_size + delta;

  I have_room_already = false;

  //do we have space already?
  if(delta <= 0)
  {
    have_room_already = true;
  }
  else if(MAPPED_FILES_DOUBLE_IN_SIZE)
  {
    I round_old = round_up_nearest_power_of_2(file_size);
    I round_new = round_up_nearest_power_of_2(revised_size);
  
    if(round_old == round_new)
    {
      have_room_already = true;
    }
  }
  else
  {
    if(file_size >= revised_size)
    {
      have_room_already = true;
    }
  }

  //no room, expand map/file
  if (!have_room_already)
  {
    //expand map space if needed
    if(map_size < revised_size)
    {
      I delta_blocks = ceil((revised_size - map_size)/(F)MAPPED_BLOCK_SIZE);

      I2 right_window = block_window_at(map_size + (V)map);
      I right_blocks = right_window.y;

      if(right_blocks < delta_blocks)
      {
        errno = ENOMEM;
        perror("Not enough virtual memory space to expand mapping. Map failed");
        ERROR(ERROR_VMEM);
        goto failed;
      }

      I map_expansion_size = map_size + (delta_blocks * MAPPED_BLOCK_SIZE);

      K z = mmap(map, map_expansion_size, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, handle, 0);

      if(MAP_FAILED == z)
      {
        perror("Expand file map failed");
        ERROR(ERROR_VMEM);
        goto failed;
      }

      //posix_madvise(z, map_expansion_size, POSIX_MADV_SEQUENTIAL);

      I2 mark_window = (I2){right_window.x, delta_blocks};

      mark_block_sequence(mark_window, handle);

      yP[REGISTRY_MAP_SIZE] = (V)map_expansion_size;
    }

    //expand file if needed
    if(file_size < revised_size) 
    {
      I expanded_file_size = revised_size;

      I minimum_file_size = 4096; //without this, we lose data

      expanded_file_size = MAX(revised_size, minimum_file_size);
  
      if(MAPPED_FILES_DOUBLE_IN_SIZE)
      {
        expanded_file_size = round_up_nearest_power_of_2(expanded_file_size);
      }

      if(0 > handle_expander(expanded_file_size, handle))
      {
        ERROR(ERROR_DISK);
        goto failed;
      }

      yP[REGISTRY_FILE_SIZE] = (V)expanded_file_size;
    }

    if(MAPPED_POSIX_STICKLER_REMAP)
    {
      K z = mmap(map, (I)yP[REGISTRY_MAP_SIZE], PROT_READ|PROT_WRITE, MAP_SHARED|MAP_FIXED, handle, 0);
    
      if(MAP_FAILED == z)
      {
        perror("Expand file POSIX remap failed");
        ERROR(ERROR_VMEM);
        goto failed;
      }
    }
  }

  return 0;

failed:
  return -1;
}

K expand_disk_double_offspring_over_half_capacity(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //TODO: doubling for table columns over half capacity
  //TODO: when growing nests, expand nest_index if over half capacity
  //      --possibly want vice versa too: nest_payload <-> nest_index
  //see also note at _expand_disk_backed: hole punch sparse vectors when doubling
  //Note: splay tables will not (should not) reach here b/c table is not IS_DISK

 return NULL;
}

#pragma mark - Copy

void* copy_thread(void* arg)
{
  I2 a = *(I2*)arg;
  
  write_k_to_handle((K)a.x,a.y,false);
  close(a.y);

  pthread_exit((void*)0);
}

char copy_from_k_to_good_disk_membuf_temp(V x, K y)
{
  bool network_order = false;
  bool force_write_empty_bytes = false;

  V memory = x;

  //POTENTIAL_OPTIMIZATION_POINT
  //we can also try FILE* here with fmemopen, which has a buffer (see also setvbuf/setbuf)
  //(instead use the FILE* arg in front of the &memory arg we added)
  //it may be faster though I'm doubtful

  I w = write_k_helper(y, 0, false, 0, false, NULL, &memory, force_write_empty_bytes);

  return w;
  
  //POTENTIAL_OPTIMIZATION_POINT
  //if this janky copy is too slow
  //you can split out the code in copy() 
  //into a method that copies to a known-good-sized slab
  //and use the same method here
  //the new method will need to see if IS_DISK(slab) (or uh if it's JUMPs or something??? -ed.)
  //since nests are different for each
  //
  //for the copies in mapcores this was a human-noticeable 50x slowdown
  //vectors that took this .3s could be copied in .006s (same -O0 -Os)
  //that was in-memory though
  //
  //***************
  //To test that a revision is working, you can keep the old method
  //around to compare with the revision byte-by-byte
  //***************

  //This handles top-level vectors, but in an optimization you'll want to handle all of them
  if(IS_VECTOR(y)) 
  { 
    K z = x;
    C m = zm;
    memcpy(z, y, any_size_K(y));  
    zm = m;
    zr = 1;
    SET_ATTR(z, ATTR_DISK);
    OFF_ATTR(z, ATTR_STRIPED);
    return 0;
  }

  int f[2];

  if(0 > pipe(f))
  {
    perror("Bad pipe during copy");
    return -1;
  }

  pthread_t thread;
  I2 args = (I2){(I)y,f[1]};

  pthread_create(&thread, NULL, copy_thread, &args);

  char eval = read_disk_k_from_handle_to_membuf_of_known_good_size(x,f[0]);
  close(f[0]);

  if(0 > eval)
  {
    goto failed;
  }

  return 0;
failed:
  return -1;
}

//char copy_from_k_unfinished(K x, K y)
//{
//  //POTENTIAL_OPTIMIZATION_POINT
//  //see also copy_from_k_to_good_disk_membuf_temp
//  //
//  //for the copies in mapcores this was a human-noticeable 50x slowdown
//  //vectors that took this .3s could be copied in .006s (same -O0 -Os)
//
//  int f[2];
//
//  if(0 > pipe(f))
//  {
//    perror("Bad pipe during copy");
//    return -1;
//  }
//
//  pthread_t thread;
//  I2 args = (I2){(I)y,f[1]};
//  pthread_create(&thread, NULL, copy_thread, &args);
//
//  read_k_from_handle(x,f[0]);
//
//  close(f[0]);
//
//  return 0;
//}

#pragma mark - Endpoints

K disk_map_anonymous_shared()
{
  return disk_map_handle_shared(temphandle());
}

K disk_map_file_shared(S path)    //this is MAPPED_BINARY_READ("file")
{ 
  return disk_map_file_shared_maybe_flat(path, NULL, false);
}

K disk_map_file_shared_maybe_flat(S path, K starting_value, bool flat_file)
{
  //1. Writeable disk object if file does not exist (create)
  //2. Writeable existing disk object if file already exists (assumes well-formed by default)

  bool create = true;

  if(flat_file)
  {
    create = false;

  }

  if(!flat_file) //attempting to map kerf binary object, so run some checks
  {
    S extensions[] = {".csv", ".tsv", ".psv", ".kerf", ".txt", ".sh", ".c", ".h", ".md", ".markdown"};
    I en = sizeof(extensions)/sizeof(extensions[0]);
    if(!strcmp(path,"kerf")) ERROR(ERROR_FILE);//don't overwrite the program either
    DO(en, S ext = extensions[i]; 
           S t = NULL;
           if((t = strrchr(path, '.')) && !strcmp(t, ext))
           {
             //Lots of users had trouble with this, and it can alter the file (assumes ->r and such are valid)
             //This isn't perfect but it's a pretty good safeguard
             //We could maybe go further by adding more type-sanity checks. or by uh not writing on everything we get 
             //POTENTIAL_FEATURE_POINT
             //1. a great check is if MAPPED_FILES_DOUBLE_IN_SIZE: files must be size power of 2, maybe also >=PAGE_SIZE?
             //2. could also restrict kerf binary files to certain extensions: kb, kerfb, dat or somesuch
             fprintf(stderr, "Cowardly refusing to open file '%s' as binary because of non-binary extension '%s' \n", path, ext);
             ERROR(ERROR_FILE);
           }
    )
  }

  I f = open_file_handle(path, create);

  if(0 > f)
  {
    perror("Open file failed");
    ERROR(ERROR_FILE);
  }

  return disk_map_handle_shared_maybe_flat(f, starting_value, flat_file);
}

char write_k_to_path(K x, S path) //this is BINARY_WRITE("file", k_object)
{
  bool start_by_deleting = true;
  if(start_by_deleting)
  {
    recursive_delete(path);
  }

  I h = open_file_handle(path, true);
  write_k_to_handle(x,h,false);
  close(h);
  return 0;
}

K read_k_from_path(S path)      //this is BINARY_READ(" [UNMAPPED, "IN-MEMORY"]
{
  //2015.04.07
  //uh
  //currently 'read_k_from_handle' doesn't seem to do (or maybe isn't intended to)
  //what we want, which is make a "nice" in-memory copy (that is not all one slab)
  //so we're doing this (hack?) which i assume works but may not 

  K x = disk_map_file_shared(path);
  K y = copy_m(x,0,false,true);
  rd(x);

  return y;
}

K write_text(K filename, K text)
{
  if(!IS_STRING(filename))ERROR(ERROR_STRING);

  bool start_by_deleting = true;
  if(start_by_deleting)
  {
    recursive_delete_K(filename);
  }

  K x = copy(filename);
  x = cow_ensure_null_terminated_chars(x);
  work_push(x);

  K y = NULL;

  if(IS_STRING(text))
  {
    y = strong(text);
  }
  else //convert type to string using before writing, instead of error
  {
    y = ex("uneval $1", text);
  }

  work_push(y);

  I length = lenI(y);

  I handle = open_file_handle(xC, true);

  handle_truncate(0, handle);

  brute_write(yC, length, handle, false, NULL, NULL);

  close(handle);
  work_pop_n_rd(2, true);

  return Ki(length);
}

#pragma mark - Striped Disk Objects

int scandir_filter(const struct dirent *e)
{
  S s = (S)&(e->d_name);

  if(!strcmp(".",s) || !strcmp("..",s)) return false;

  return true;
}

K dir_list_k(K w, K x)
{
  if(!IS_NIL(w) && !IS_STRING(w)) ERROR(ERROR_TYPE);

  bool path_prepended = false;

  if(x)
  {
    path_prepended = truthy(x);
  }

  K path = NULL;
  
  if(IS_STRING(w)) path = cow_ensure_null_terminated_chars(strong(w));  
  else path = charvec_from_cstring(".");

  K r = dir_list_s(kC(path), path_prepended, false);

  rd(path);

  return r;
}

K dir_list_s(S path, bool path_prepended, bool skip_hidden)
{
  //This currently does not descend deeper than the shallowest level

  //Option to skip hidden?

  if(!path || strlen(path) < 1) path = ".";

  K list = new_k(LIST, 0);

  struct dirent **namelist = NULL;
  I i, n;

  n = scandir(path, &namelist, scandir_filter, alphasort); //we'd prefer versionsort (not on OSX even w/ _GNU_SOURCE)
  if (n < 0) 
  {
    fprintf(stderr,"scandir path: %s\n", path);
    perror("scandir");
  }
  else 
  {
    for (i = 0; i < n; i++)
    {

      S r = namelist[i]->d_name;
      if(r && '.'==*r && skip_hidden) goto done;

      K s = charvec_from_cstring(r);
      //printf("%s\n", namelist[i]->d_name);
      list = cow_add(list, s);
      rd(s);
    done:
      free(namelist[i]);
    }
    free(namelist);
  }

  if(path_prepended && 0!=strcmp(path, "."))
  {
    K r = charvec_from_cstring(path);
    K t = ex(" if(char('/') match last $1){$1} else{$1 join '/'} join mapright $2", r, list);
    rd(r);
    rd(list);
    list = t;
  }

  return list;
}

K open_dir_object(S path)
{
  K z = NULL;

  K path_k = charvec_from_cstring(path);
  work_push(path_k);

  K4 o;
  K main_here = implode(kc('/'), klist2(path_k, kcv("base.dat"), o)); 
  main_here = cow_ensure_null_terminated_chars(main_here);
  work_push(main_here);

  if(file_exists_valid(kC(main_here)))
  {
    The_Hacky_Reused_A_Mapping_Flag = false;
    z = disk_map_file_shared(kC(main_here));
    if(The_Hacky_Reused_A_Mapping_Flag)
    {
      //We were going to map/striped-map this object but found it already was,
      //and so reference-incremented the existing object instead for reuse.
      //If this causes trouble consider disabling MAPPED_REDIRECT_REOPEN_TO_EXISTING to false instead
      goto succeed;
    }

    SET_ATTR(z, ATTR_STRIPED);
  }
  else
  {
    //TODO ensure that this is a directory (".../base.dat" code above implicitly does this for itself)
    //(this actually forks into non-dir items opened as dirs...)

    bool skip_hidden = true;

    K entries_prepend = dir_list_s(path, true, skip_hidden);
    work_push(entries_prepend);

    K entries_nopend = dir_list_s(path, false, skip_hidden);
    work_push(entries_nopend);

    K4 o;
    z = ex("map(['pre','nop','path'], $1)", klist3(entries_prepend, entries_nopend, path_k, o));

    K e = ex("{{date: eval mapright $1[which $1 != 'parcel.meta'] }}", entries_nopend);

    z = update(z, kcv("parcel_names_evaled"), e);
    rd(e);

    work_pop_n_rd(2, true);
    z->t = PARTABLE;

    goto succeed;
  }

  if(!z || IS_NIL(z))
  {
    goto fail;
  }

  assert(za & ATTR_STRIPED);

  //At this point, we fork into two paths  
  //One path is a sane object, and we can exhaust the dir possibilites
  //The other is a bad object, we might want to use the slow method
  //(a bad situation would be a huge LIST with nested weird stuff,
  // whatever, would get written as a directory. This would force
  // use to either (exhaust too many dirs | scan the whole item on load),
  // which is what we want to avoid. -- At least I think it matters.)
  //[this is for PARTABLE support]
  //We could even set an attribute on these guys ahead of time

  //we can either 
  //1. brute force dirs i from 0 to n
  //2. or we can look on the NEST for inspiration
  //let's start by trying Strategy 2

  bool will_descend = IS_NEST(z);

  if(will_descend)
  {
    //WARNING: You can't release z until the nested stripes are
    //         taken care of.

    I n = box_count_K(z);

    DO(n, 

      K w = kJ(z,i);
      bool is_stripe = wa & ATTR_STRIPED;

      if(!is_stripe) continue;
  
      char number_dir[32] = {0};
      snprintf(number_dir, sizeof(number_dir), "%lld", i); 
      
      K4 o;
      K deeper_path = implode(kc('/'), klist2(path_k, kcv(number_dir), o)); 
      deeper_path = cow_ensure_null_terminated_chars(deeper_path);

      work_push(deeper_path);
      K d = open_dir_object(kC(deeper_path));
      work_pop_rd(true);

      K e = kl(d);
      e->a |= ATTR_STRIPED;

      //I think this is a big deal, because we're mixing
      //what are effectively LINKs into the header with the JUMPs
      //that can cause problems (unless we staunch them)
      //So what we did is use kJ to point to an inline LINK & fix everywhere there was JUMP to resolve
      //one deeper. unmapping splay now requires a nested rd check as usual (since not all JUMPs now)
      //
      //If we end up doing some overwrite variation, may need this to be disk_nestset or somesuch
      *kJ(z,i) = *e;

      //These attempts won't work:
      //k0(z)[k] = *e;
      //k0(z)[k] = kl0(d);
      //k0(z)[k].a |= ATTR_STRIPED;
      //update(z, ki(k), e);
    )

  }

succeed:
  work_pop_n_rd(2, true);
  return z;

fail:
  fprintf(stderr,"Failed to open directory object.\n");
  work_pop_n_rd(2, true);
  if(z) rd(z);
  return Kn();
}

K open_dir_object_slow(S path)
{
  //These entries are only the shallowest level & not recursive
  K entries_prepend = dir_list_s(path, true, true);
  work_push(entries_prepend);

  K entries_nopend = dir_list_s(path, false, true);
  work_push(entries_nopend);

  K path_k = charvec_from_cstring(path);
  work_push(path_k);

  K4 o;
  K main_here = implode(kc('/'), klist2(path_k, kcv("base.dat"), o)); 
  main_here = cow_ensure_null_terminated_chars(main_here);
  work_push(main_here);

  bool skip_hidden = true;

  K z = NULL;

  if(file_exists_valid(kC(main_here)))
  {
    The_Hacky_Reused_A_Mapping_Flag = false;
    z = disk_map_file_shared(kC(main_here));
    if(The_Hacky_Reused_A_Mapping_Flag)
    {
      //We were going to map/striped-map this object but found it already was,
      //and so reference-incremented the existing object instead for reuse.
      //If this causes trouble consider disabling MAPPED_REDIRECT_REOPEN_TO_EXISTING to false instead
      goto succeeded;
    }

    SET_ATTR(z, ATTR_STRIPED);
  }
  else
  {
    //TODO ensure that this is a directory (".../base.dat" above does this implicitly)
    K4 o;
    z = ex("map(['pre','nop','path'],$1)", klist3(entries_prepend, entries_nopend, path_k, o));
    //z->t = PARTABLE;
goto succeeded;
    goto failed;
  }

  LIST2(entries_nopend, entries_prepend,
    if(0 == vn) continue;
    if(skip_hidden && vn > 0 && vC[0]=='.') continue;
    K x = cow_ensure_null_terminated_chars(strong(v));
    work_push(x);

    K y = cow_ensure_null_terminated_chars(strong(u));
    work_push(y);

    S e = xC;

    bool is_dir = file_path_is_directory(e);

    if(is_dir)
    {
      S end = NULL;

      I k = strtoll(yC, &end, 10);

      bool was_int = (end == yC+yn);
      bool nest = IS_NEST(z);
      bool in_range = was_int && nest && (k >= 0 && k < box_count_K(z));
      bool is_stripe = in_range && (kJ(z,k)->a & ATTR_STRIPED);

      bool valid = was_int && nest && in_range && is_stripe;

      if(valid)
      {
        K d = open_dir_object(xC);
        K e = kl(d);
        e->a |= ATTR_STRIPED;

        //I think this is a big deal, because we're mixing
        //what are effectively LINKs into the header with the JUMPs
        //that can cause problems (unless we staunch them)
        //So what we did is use kJ to point to an inline LINK & fix everywhere there was JUMP to resolve
        //one deeper. unmapping splay now requires a nested rd check as usual (since not all JUMPs now)
        //
        //If we end up doing some overwrite variation, may need this to be disk_nestset or somesuch
        *kJ(z,k) = *e;

        //These attempts won't work:
        //k0(z)[k] = *e;
        //k0(z)[k] = kl0(d);
        //k0(z)[k].a |= ATTR_STRIPED;
        //update(z, ki(k), e);
      }
    }
    else
    {
      //noop
    }

    work_pop_n_rd(2, true);
  )

  if(!z) goto failed;

succeeded:
  work_pop_n_rd(4,true);
  return z;
failed:
  fprintf(stderr,"Failed to open directory object.\n");
  work_pop_n_rd(4,true);
  return Kn();
}

bool kerf_mkdir(S dirpath, bool fail_on_exist)
{
  bool success = false;

  errno = 0;
  int r = mkdir(dirpath, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

  if(0 == errno && 0 == r)
  {
    success = true;
  }
  else if(EEXIST == errno)
  {
    if(!fail_on_exist)
    {
      success = true;
    }
  }

  if(!success) perror("Failed to make directory");

  return success;
}

int recursive_delete_K(K path)
{
  K x = copy(path);
  x = cow_ensure_null_terminated_chars(x);
  int e = recursive_delete(xC);
  rd(x);
  return e;
}

int recursive_delete(S path)
{
  //see
  //http://stackoverflow.com/a/1149837/365478
  //http://stackoverflow.com/a/27808574/365478

  int ret = 0;
  FTS *ftsp = NULL;
  FTSENT *curr;

  // Cast needed (in C) because fts_open() takes a "char * const *", instead
  // of a "const char * const *", which is only allowed in C++. fts_open()
  // does not modify the argument.
  char *files[] = { (char *) path, NULL };

  //if(!path || !strlen(path)) goto finish;
  //fprintf(stderr, "we're getting |%s| path\n", path);
  //dd(strlen(path))
  //dd(getpid())

  // FTS_NOCHDIR  - Avoid changing cwd, which could cause unexpected behavior
  //                in multithreaded programs
  // FTS_PHYSICAL - Don't follow symlinks. Prevents deletion of files outside
  //                of the specified directory
  // FTS_XDEV     - Don't cross filesystem boundaries
  ftsp = fts_open(files, FTS_NOCHDIR | FTS_PHYSICAL | FTS_XDEV, NULL);
  if (!ftsp) {
      fprintf(stderr, "%s: fts_open failed: %s\n", path, strerror(errno));
      ret = -1;
      goto finish;
  }

  while ((curr = fts_read(ftsp))) {
      switch (curr->fts_info) {
      case FTS_NS:
      case FTS_DNR:
      case FTS_ERR:
          //undesirable: this will complain if we try to delete a path that doesn't exist
          //fprintf(stderr, "%s: fts_read error: %s\n", curr->fts_accpath, strerror(curr->fts_errno));
          break;

      case FTS_DC:
      case FTS_DOT:
      case FTS_NSOK:
          // Not reached unless FTS_LOGICAL, FTS_SEEDOT, or FTS_NOSTAT were
          // passed to fts_open()
          break;

      case FTS_D:
          // Do nothing. Need depth-first search, so directories are deleted
          // in FTS_DP
          break;

      case FTS_DP:
      case FTS_F:
      case FTS_SL:
      case FTS_SLNONE:
      case FTS_DEFAULT:
          if (remove(curr->fts_accpath) < 0) {
              fprintf(stderr, "%s: Failed to remove: %s\n",
                      curr->fts_path, strerror(errno));
              ret = -1;
          }
          break;
      }
  }

finish:
  if (ftsp) {
      fts_close(ftsp);
  }

  return ret;
}

K write_dir_object(K x, S path)
{
  bool start_by_deleting = true;
  if(start_by_deleting)
  {
    if(recursive_delete(path)) goto fail;
  }

  //we can fork this operation to avoid saving chdir info
  if(kerf_fork())
  {
    int status = 0;
    wait(&status);

    if(status) goto fail;
  }
  else
  {
    _write_dir_object(x, path);
    _exit(EXIT_SUCCESS);
  }

succeeed:
  return open_dir_object(path);
fail:
  ERROR(ERROR_FILE);
  return Kn();
}

void _write_dir_object(K x, S path)
{
  //POTENTIAL_FEATURE_POINT
  //we could allow later column additions to striped tables
  //by maintaining the directory handle or path in 
  //the attributes map at the end of the object
  //see also fchdir - you can use the file handle instead (maybe +fork)
  //The mmap registry is still active, so we could also use that

  S main_file = "base.dat";

  bool r;

  bool fail_on_exist = true;
  
  r = kerf_mkdir(path, fail_on_exist);

  if(!r)
  {
    if(!errno) errno = ECANCELED;
    perror("Directory write failed");
    goto failed;
  }

  r = chdir(path);

  if(r)
  {
    er(chdir)
    goto failed;
  }

  bool no_dirs = false;

  if(!IS_NEST(x))
  {
    write_k_to_path(x, main_file);
  }
  else
  {
    write_k_to_path(kk, main_file);
    K z = disk_map_file_shared(main_file);

    NEST(x, 

      char deeper[PATH_MAX] = {0};
      snprintf(deeper, sizeof(deeper), "%lld", i);

      bool satoms_n_such    = !IS_ZIP(x) && IS_SATOM(v) && !IS_MIXED_KEYED(v);
      bool map_n_table_junk = !IS_ZIP(x) && IS_MIXED_KEYED(x) && (i != VALUES); 
      bool zip_junk         =  IS_ZIP(x) && (i >= KEYS); 

      bool inline_item = satoms_n_such || map_n_table_junk || zip_junk;

      if(inline_item)
      {
        //recursive_delete(deeper);//for overwrite
        z = cow_add(z,v);
      }
      else
      {
        _write_dir_object(v, deeper);

        K k = kn;
        k->a |= ATTR_STRIPED;
        z = cow_add(z,k);
      }
    )

    zm = zm;
    zh = zh;
    za = xa;
    zt = xt;
    zn = xn;
    zr = 1;
    SET_ATTR(z, ATTR_DISK);
    SET_ATTR(z, ATTR_STRIPED);
    rd(z);
  }

  chdir("..");

success:
  return;
failed:
  _exit(EXIT_FAILURE);
  return;
}

K read_parceled_from_path(K x)
{
  return read_striped_from_path(x);
}

K read_striped_from_path(K x)
{
  if(!IS_STRING(x)) ERROR(ERROR_TYPE);

  K z = copy(x);
  z = cow_ensure_null_terminated_chars(z);
  work_push(z);
  K a = open_dir_object(zC);

  work_pop_rd(true);

  return a;
}

K write_striped_to_path(K x, K y)
{
  if(!IS_STRING(x)) ERROR(ERROR_TYPE);
  K z = copy(x);
  z = cow_ensure_null_terminated_chars(z);
  work_push(z);
  K a = write_dir_object(y, zC);
  work_pop_rd(true);
  return a;
}

#pragma mark -

int drive_go()
{
  //Striped Tests
  //  0. generate a bunch of different objects (eg INTERN, HASH, TABLE, atoms), write them, & reopen them testing equivalent
  //  0. insert into a few appropriate objects
  //  0. try to alter objects in interesting ways (eg add new column to table)

  //K t = ex("1 join 'c'");
  //K t = ex("range 10");
  //K t = ex("[1 2 3, 4 5 6]");
  //K t = ex("[1 2 3, 4 5 6, [7 8 9, 10 11 12]]");
  //K t = ex("[1 2 3, 4 5 6, [7 8 9, [10 11 12, 13 14 15]]]");
  //K t = ex("[[[1 2 3, 4 5 6], 7 8 9], 10 11 12]");
  K u = NULL;
  K t = NULL;
  K r = NULL;
  K s = NULL;

  //t = ex("{{a:1 2 3, b:4 5 6}}");
  //u = ex("{{a:1 2 3, b:4.0 5.0 6.0}}");
  ////K t = ex("{{a:[[1 2 3, 4 5 6]], b:[[4 5 6, 7 8 9]]}}");
  ////K t = ex("{{a:[{{c:1 2 3,d:3 4 5}}], b:[{{e:1 2, f:3 4}}]}}");

  ////system("rm -r d-obj");
  //r = write_dir_object(t, "d-obj");
  //s = write_dir_object(u, "d-obj");
  ////r = open_dir_object("d-obj");

  //if(r)
  //{
  //  show(r);
  //  rd(r);
  //}

  //if(t)rd(t);
  //if(s)rd(s);
  //if(u)rd(u);

  return 0;
  S path = "~hallo.dat";
  
  I n = 20;
  K x = til(Ki(n));
  K y = til(Ki(n));
  K z = new_map_from_K_K(x,y,false,false);
  //z = til(ki(55));
  //z = take(ki(200),kc('d'));

  show(z);
  write_k_to_path(z,path);

  K a;
  a = disk_map_file_shared(path);

  a = cow_promote(a);
  show(a);
  a = cow_demote(a);
  show(a);

  rd(a);

  return 0;
}


