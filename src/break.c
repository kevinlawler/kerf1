#include "kerf.h"

//Virtual Memory
int virtual_memory_blocks[MAPPED_BLOCKS];//0-open, otherwise in use (file handles)
I MAPPED_BLOCK_SIZE = -1;
V MAPPED_ORIGIN = NULL;
I PAGE_SIZE_BYTES = -1;

//Pool
char POOL_LANE_STRUCT_MIN = -1;
char POOL_LANE_MAX        = -1;//allocations over max lane are freed on repool
char DISK_LANE_MIN        = -1;
__kerfthread void* POOL[POOL_LANE_BUF];

#pragma mark - Reserve Virtual Memory Address Space

I The_Holding_Temphandle = 0;

V mmap_place_on_hold(void* start, I bytes)//0==start: let OS place anywhere
{

  if(!The_Holding_Temphandle)
  {
    //This solved a bug on OSX at least
    //even though we were properly closing handles
    //I think OSX has the bug
    The_Holding_Temphandle = temphandle();
  }

  V location = NULL;

  location = mmap(start, bytes, PROT_NONE, MAP_SHARED|MAP_NORESERVE, The_Holding_Temphandle, 0);

  //Special fix for Docker
  //Docker needs both start !=0 and bytes <= (POW2(46)+POW2(45))
  //This idea isn't so crazy, we could extend it to try in general 2^44, 2^43, ...
  if(MAP_FAILED == location && NULL == start)
  {
    errno = 0;

    start = start + PAGE_SIZE_BYTES;
    The_Mapped_Virtual_Reserved_Bytes = POW2(46);
    bytes = The_Mapped_Virtual_Reserved_Bytes;
    location = mmap(start, bytes, PROT_NONE, MAP_SHARED|MAP_NORESERVE, The_Holding_Temphandle, 0);
  }

  //if(h>STDERR_FILENO)
  //{
  //  close(h);
  //}

  if(MAP_FAILED == location)
  { 
    perror("Memory reserve failed");
    goto failure;
  }

success:
  return location;
failure:
  return (V)SENTINEL;
}

I memory_init()//reserve memory
{
  PAGE_SIZE_BYTES = sysconf(_SC_PAGE_SIZE);

  The_Mapped_Virtual_Reserved_Bytes = The_Mapped_Virtual_Reserved_Bytes - (The_Mapped_Virtual_Reserved_Bytes % PAGE_SIZE_BYTES);

  MAPPED_ORIGIN = mmap_place_on_hold(0, The_Mapped_Virtual_Reserved_Bytes);

  bool initial_reserve_failed = (((V)SENTINEL) == MAPPED_ORIGIN);

  if(initial_reserve_failed)
  {
    perror("Mapped objects will not work");
    //abort();//Don't abort for now - iOS limited memory
  }

  MAPPED_BLOCK_SIZE = The_Mapped_Virtual_Reserved_Bytes >> MAPPED_LOG_2_BLOCKS;

  return 0;
}

I2 widest_block_sequence()
{
  I i = 0, p = 0, w = 0;
  I best_position = 0, best_width = 0;

  DO(MAPPED_BLOCKS, if(virtual_memory_blocks[i]) {p=-1; w=0;}else {if(!w)p=i; w++;}
                    if(w>best_width){best_width=w; best_position=p;}
  )

  return (I2){best_position, best_width};
}

I2 block_window_at(V p)
{
  I position = floor((p - MAPPED_ORIGIN)/(F)MAPPED_BLOCK_SIZE);
  I width = 0;

  I i = position;

  while(i < MAPPED_BLOCKS)
  {
    if (virtual_memory_blocks[i]) break;
    width++;
    i++;
  }

  return (I2){position, width};
}

void mark_block_sequence(I2 sequence, int mark)
{
  DO(sequence.y, virtual_memory_blocks[i+sequence.x] = mark)
}

I count_occupied_virtual_memory_blocks()
{
  I c = 0;
  DO(MAPPED_BLOCKS, if(virtual_memory_blocks[i])c++)
  return c;
}


void show_virtual_memory_blocks()
{
  DO(MAPPED_BLOCKS, int h = virtual_memory_blocks[i];  O("%d ",h)) O("\n");
}

#pragma mark - Cores

I total_cores() //or "processors"
{
  //return sysconf(_SC_NPROCESSORS_ONLN);//May be less than below
  return sysconf(_SC_NPROCESSORS_CONF);
}

#pragma mark - In-Memory Pool/Slab

I total_physical_system_memory_bytes()
{
#ifdef __MACH__ //Apple OSX
  I memory = 0;
  size_t width = sizeof(I); 
 	if (-1 == sysctlbyname("hw.memsize", &memory, &width, NULL, 0)) memory = 0; 
  return memory;
#else
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  return pages * page_size;
#endif 
}

char ceiling_log_2(unsigned long long v)
{
#if __has_builtin(__builtin_clzll)
  return ((sizeof(unsigned long long) * 8 - 1) - __builtin_clzll(v)) + (!!(v & (v - 1)));
#else
  return floor_log_2(v) + (!!(v & (v - 1)));
#endif
}

char floor_log_2(unsigned long long v)
{
#if __has_builtin(__builtin_clzll)
  return ((sizeof(unsigned long long) * 8 - 1) - __builtin_clzll(v));
#else

  //POTENTIAL_OPTIMIZATION_POINT
  //try this method? needs easy conversion to DOUBLE not FLOAT
  //int RoundedUpLogTwo(uint64_t input)
  //{
  //    assert(input > 0);
  //    union Float_t num;
  //    num.f = (float)input;
  //    // Increment the representation enough that for any non power-
  //    // of-two (FLT_MIN or greater) we will move to the next exponent.
  //    num.i += 0x7FFFFF;
  //    // Return the exponent after subtracting the bias.
  //    return num.parts.exponent - 127;
  //}
  //
  //
  //
  ///* See
  //https://randomascii.wordpress.com/2012/01/11/tricks-with-the-floating-point-format/
  //for the potential portability problems with the union and bit-fields below.
  //*/
  //union Float_t
  //{
  //    Float_t(float num = 0.0f) : f(num) {}
  //    // Portable extraction of components.
  //    bool Negative() const { return (i >> 31) != 0; }
  //    int32_t RawMantissa() const { return i & ((1 << 23) - 1); }
  //    int32_t RawExponent() const { return (i >> 23) & 0xFF; }
  // 
  //    int32_t i;
  //    float f;
  //#ifdef _DEBUG
  //    struct
  //    {   // Bitfields for exploration. Do not use in production code.
  //        uint32_t mantissa : 23;
  //        uint32_t exponent : 8;
  //        uint32_t sign : 1;
  //    } parts;
  //#endif
  //};


  //POTENTIAL_OPTIMIZATION_POINT
  //do any of these beat dgobbi? http://www.hackersdelight.org/hdcodetxt/nlz.c.txt
  
  //fall back to dgobbi method
  static const unsigned long long t[6] = 
  {
    0xFFFFFFFF00000000ull,
    0x00000000FFFF0000ull,
    0x000000000000FF00ull,
    0x00000000000000F0ull,
    0x000000000000000Cull,
    0x0000000000000002ull
  };

  char y = 0;
  char j = 32;
  char i;

  for (i = 0; i < 6; i++)
  {
    char k = (((v & t[i]) == 0) ? 0 : j);
    y += k;
    v >>= k;
    j >>= 1;
  }

  return y;
#endif
}

char is_power_of_2(I v){return !(v & (v - 1));}

I __attribute__ ((hot))  round_up_nearest_power_of_2(I v){R POW2(ceiling_log_2(v));}
I __attribute__ ((pure)) round_up_nearest_multiple_of_8(I v){I m = 8-1; return (v&m)?8+(v&~m):v;}

void inspect_pool()
{

  I total_bytes = 0;

  DO(POOL_LANE_BUF,

    V *p = POOL[i];

    I slabs = 0;

    while(p)
    {
      slabs++;
      p = *p;
    }

    if(0==slabs) continue;
    I bytes_per_slab = POW2(i);

    I bytes_in_lane = slabs * bytes_per_slab;

    total_bytes += bytes_in_lane;

    fprintf(stderr, "Pool Lane %2lld; Slabs: %5lld; Bytes: %9lld; \n", i, slabs, bytes_in_lane );
  ) 

  fprintf(stderr, "Total Bytes in Pool: %lld\n", total_bytes); 
  dd(The_Outstanding_Memory_Counter)
}

void* pool_anonymous_system_memory(I requested_bytes, bool shared) //share with forked children?
{
  if(TEST_TRY_POOL_SIDESTEP_VIA_MALLOC)
  {
    if(!shared) return malloc(requested_bytes);
  }

  void* z = NULL;

  int flags = 0; 

  if(!shared)
  {
    flags = MAP_ANON | MAP_PRIVATE;
  }
  else
  {
    flags = MAP_ANON | MAP_SHARED;
  }

  if(POOL_CHECKS_SANE_ALLOC_BOUNDS)
  {
    if(requested_bytes < 0 || requested_bytes > POOL_SANE_MEMORY_BOUND)
    {
      ERROR(ERROR_VMEM);
    }
  }

  if(The_Past_The_Test_Suite_Flag && 0 <= The_Real_RAM_Memory_Bound && The_Real_RAM_Memory_Bound < The_Outstanding_Memory_Counter + requested_bytes)
  {
    fprintf(stderr, "User-set memory bound prevented further memory allocation.\n");
    ERROR(ERROR_VMEM);
  }

  z = mmap(0, requested_bytes, PROT_READ|PROT_WRITE, flags, -1, 0);

  if(POOL_CHECKS_NONZERO_ALLOC_RETURN)
  { 
    if(MAP_FAILED == z)
    { 
      perror("Anonymous system memory allocation failure");
      //memory_allocation_failed = 1; //panic
      ERROR(ERROR_VMEM);
    }
  }

  The_Outstanding_Memory_Counter += requested_bytes;

  return z;
}

void* pool_depool(char lane)
{
  if(TEST_TRY_POOL_SIDESTEP_VIA_MALLOC)
  {
    return malloc(POW2(lane));
  }

  bool safe = The_Thread_Safe_Flag;
  if(safe) pthread_mutex_lock(&The_Pool_Mutex);

  void** z;

  void** L = ((void**)POOL)+ lane;

  if(!*L)//lane empty
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //if the app is making repeated requests for atoms, say,
    //we could detect this and increase the size of the alloc_width.
    //There are many ways to do this, including successively doubling the size
    //based on a glance at the most recent requests.
    //This may be a substantial savings, or it may be negligible
    //if for instance the overhead is nothing and the OS is only
    //paging in the memory when the pages are touched later anyway.

    I requested_bytes = POW2(lane);

    I alloc_width = MAX(POOL_MIN_ALLOC_WIDTH, requested_bytes); 

    bool shared = false;
    z = pool_anonymous_system_memory(alloc_width, shared);

    if(POOL_SUBDIVIDES_PAGES)
    {
      if(requested_bytes < POOL_MIN_ALLOC_WIDTH)
      { 
        void* y = z;
        while(y < (void*)z + POOL_MIN_ALLOC_WIDTH + -requested_bytes)//Low lanes subdivide pages
        {
          *(void**)y = y + requested_bytes; //set pointer link to next segment
          y += requested_bytes;         //hop to next segment
        }
      }
    }
    *L=z;
  }

  z = *L;
  *L = *z;
  //*z=0;//clear pointer
  //*(char *)z = lane;//whatever calls depool should set this

  if(TEST_TRY_POOL_PREZEROES_ALLOC)
  {
    bzero(z, POW2(lane));
  }

  if(TEST_TRACK_ALLOCATIONS && !The_Process_is_Child_Flag)
  {
    if(The_Allocations) insert_replace(The_Allocations, ki((I)z), ki(1), true, false);
    if(The_Allocations) if(di(The_Allocations,0)) kerf_exit(-1);
  }

  if(safe) pthread_mutex_unlock(&The_Pool_Mutex);

  return z;
}

void pool_repool(void* v, char lane)
{
  if(TEST_TRY_POOL_SIDESTEP_VIA_MALLOC)
  {
    free(v);
    return;
  }

  bool safe = The_Thread_Safe_Flag;

  if(safe) pthread_mutex_lock(&The_Pool_Mutex);

  if (lane > POOL_LANE_MAX)
  {
    I freeing_bytes = POW2(lane);
    munmap(v, freeing_bytes);//anonymous memory, non-disk
    The_Outstanding_Memory_Counter -= freeing_bytes;
  }
  else 
  {
    *(void**)v = POOL[lane];
    POOL[lane] = v;
  }

  if(TEST_TRACK_ALLOCATIONS && !The_Process_is_Child_Flag)
  {
    if(The_Allocations) insert_replace(The_Allocations, ki((I)v), ki(0), true, false);
    if(The_Allocations) if(di(The_Allocations,0)) kerf_exit(-1);
  }

  if(safe) pthread_mutex_unlock(&The_Pool_Mutex);
}

char min_lane_for_bytes(I bytes_requested, char min_lane)
{
  char lane = ceiling_log_2(bytes_requested);
  lane = MAX(lane, min_lane);
  return lane;
}

void* pool_alloc_with_min_lane(I bytes_requested, char min_lane)
{
  void *z;
  char lane = min_lane_for_bytes(bytes_requested, min_lane);
  z = pool_depool(lane);

  *(membuf*)z = (membuf){lane,ATTR_NONE,0,0};

  return z;
}

void* pool_alloc_struct(I bytes_requested)
{
  return pool_alloc_with_min_lane(bytes_requested, POOL_LANE_STRUCT_MIN);
}

void pool_dealloc(void* z)
{
  char lane = *(char *)z;
  pool_repool(z, lane);
}

#pragma mark - Boxing / Generators

K  Kb(C c){K x = new_k(CHAR ,0); xc=c; xa |= ATTR_BYTES; R x;}
K  Kc(C c){K x = new_k(CHAR ,0); xc=c; R x;}
K  Ki(I i){K x = new_k(INT  ,0); xi=i; R x;}
K  Kf(F f){K x = new_k(FLOAT,0); xf=f; R x;}
K  Ks(I i){K x = new_k(STAMP,0); xi=i; R x;} 
K  Kn(   ){K x = new_k(NIL  ,0);       R x;}
K  Kk(   ){K x = new_k(LIST ,0);       R x;}

K charvec_from_cnstring(S s, I n)
{
  K z = new_k(-CHAR, n+1);
  zC[n]=0;
  zn = n;
  strncpy(zC,s,n);
  return z;
}

K charvec_from_cstring(S s)
{
  return charvec_from_cnstring(s, strlen(s));
}

K new_subarray(K x, I start, I end)
{
  if(!IS_ARRAY(x)) return strong(x);

  I n = MIN(end - start,  xn - start);  

  n = MAX(0, n);
  
  K z = take(ki(0), x);//type preservation

  K0 o;
  DO(n, z = cow_add(z, AT2(x,ki(start+i),o))) 

  return z; 
}

K new_map()
{
  K x = new_k(LIST,0);
  K z = new_map_from_K_K(x,x,false,false);
  rd(x);
  return z;
}

K new_hashset()
{
  K z = new_map_from_K_K(kk,kk,true,false);
  return z;
}

K new_intern()
{
  K z = new_k(LIST, HASH_SIZE); 
  z->n = 0;

  K x = new_hashset();
  K y = new_k(-INT,0);

  z = cow_add(z, x);
  z = cow_add(z, y);

  zt = HASH;
  zn = 0;
  z->nn = HASH_SIZE;
  SET_ATTR(z, ATTR_SORTED);

  rd(x);
  rd(y);

  return z;
}

K new_btree()
{
  K x = new_k(LIST,0);
  K z = new_btree_from_K(x);
  rd(x);
  return z;
}

K new_table()
{
  return new_table_x();
}

K new_database()
{
  K x = new_k(LIST,0);
  K z = new_map_from_K_K(x,x,false,true);
  rd(x);
  zt = DATABASE;
  return z;
}

K new_zip()
{
  K z = new_k(LIST, 2); 
  z->n = 0;

  //Putting INTVEC second is better for the
  //case where you want to stream:
  //probably easier to keep index in memory: 0.4% size
  //you can also rebuild the index on transfer actually.
  //it's not that hard with the compressed CHARVEC stuff already.
  K x = new_k(CHARVEC, PAGE_SIZE_BYTES);
  zero_list_payload(x);//do this so we can read page0 without worrying about faults later (not necessary?)
  xn = 0;
  K y = new_k(INTVEC, 0);

  K a = new_k(CHARVEC, 256);
  //SET_ATTR(a,ATTR_FILLED);//so copies don't reduce size when a->n changes. but prints weird

  z = cow_add(z, x);
  z = cow_add(z, y);
  z = cow_add(z, a);

  zt = ZIP;

  OFF_ATTR(z, ATTR_SORTED);//TODO: we can turn this back on... with LOOK_(x,i) checking. PLUS KC(INT4,FLOAT4) and so on... (cheat)

  z->n = 0;

  z->nn = 3;
  z->na = zip_attr_for_subtype(INTVEC);
  z->nk = NIL;//overwritten by INT, STAMP, FLOAT, ...
  z->nf = ZIP_TYPE_LZ4;//can support multiple algos

  //z->mn  
  //z->ma
  //z->mk //log-2-block-size
  SW(z->nf)
  {
    CS(ZIP_TYPE_IDENTITY,   z->mk = 12)
    CS(ZIP_TYPE_WKDM,       z->mk = 12) //must stay this, unless you update wkdm wrapper method
    CS(ZIP_TYPE_GZIP_16K,   z->mk = 14)
    CD:
    CS(ZIP_TYPE_LZ4,        z->mk = 16);//These are actually fixed currently...
  }
  //assert: the smallest log-2-block-size must be >= the largest vetorizable atom log-2 size
  //        so if you make a 256-byte string (8==log_2(256)), then block size must be >= 8
  //        Probably 12 (4096==generic page size) is a good choice for a lower bound
  assert(z->mk >= 12);
  z->mf = 1;  //zip level (lz4 acceleration, gzip level, ...)


  rd(x);
  rd(y);
  rd(a);

  return z;
}

C zip_attr_for_subtype(C t)
{
  SW(t)
  {
   CSF(STAMP,)
    CR(STAMPVEC, ZIP_ATTR_DELTA_DELTA_I | ZIP_ATTR_BYTE_TRANSFORM) //Special transform for timestamps
  }

  return ZIP_ATTR_BYTE_TRANSFORM;
}

K new_zip_for_subtype(C t)
{
  K z = new_zip();

  z->nk = t; 
  z->na = zip_attr_for_subtype(t);

  return z;
}

#pragma mark -

//deque would speed up moving max or moving min, I think was the original idea
//see https://www.geeksforgeeks.org/sliding-window-maximum-maximum-of-all-subarrays-of-size-k/
//K new_deque()
//{
//    I think we can create a suitable deque (with a high-water mark)
//    with O(1) operations (but not fastest in terms of practical speed)
//    by reusing our hash table and using negative indices
//    whenever the queue is empty reset to [0,0)
//
//  K z = new_k(LIST, DEQUE_SIZE);
//  z->n = 0;
//
//  K x = new_k(LIST, 0);
//
//  z = cow_add(z, ki(0));
//  z = cow_add(z, ki(0));
//  z = cow_add(z, x);
//
//  zt = DEQUE;
//  zn = 0;
//  z->nn = DEQUE_SIZE;
//
//  rd(x);
//
//  return z;
//}
//
//I deque_count(K x)
//{
//  return kN(x,1)->i;
//}
//
//bool deque_would_grow(K x)
//{
//  return deque_count(x) >= max_possible_count_K(xValues);
//}
//
//K deque_grow(K x)
//{
//  x = cow(x);
//
//  I first = xIndex->i;
//  I count = xKeys->i;
//  K y = xValues;
//  K z = new_k(yt, count+1);
//  zn = 0;
//
//  z = zero_unused_space(z);//necessary because we skip around in a ring queue
//
//  //POTENTIAL_OPTIMIZATION_POINT: set, not cow_add(). but increment n
//  DO(count, I k = first + i; if(k>=count) k -= count;  K v = LOOK_(y, k);  z = cow_add(z, v))
//  nestset_ri_rd(x, VALUES, z, false, true);
//
//  xIndex->i = 0;
//  xKeys->i += 1; 
//   
//  return x;
//}
//
//K push_back(K x, K y)
//{
//  if(deque_would_grow(x)) x = deque_grow(x);
//  I first = xIndex->i;
//  I count = xKeys->i; 
//
//  K z = xValues;
//
//  I place = first + count; 
//  if(place >= zn) place -= zn;
//
//
//  nestneu(x,VALUES,cow(xValues));
//  nestneu(x,VALUES,z=update_ri_rd(z, ki(place), y, true, true))
//
//  zn++;
//  xKeys->i++; 
//
//  return x;
//}
//
//K push_front(K x, K y)
//{ 
//  return x;
//}
//
//K pop_back(K x, K y)
//{
//  //if 0==size, reset to front
//
//  return x;
//}
//
//K pop_front(K x, K y)
//{ 
//  return x;
//}
//
//K peek_back(K x, K y)
//{ 
//  return x;
//}
//
//K peek_front(K x, K y)
//{ 
//  return x;
//}

#pragma mark - Tools

K new_k(C t, I n)
{
  I b = list_size_k(t,n);
  K x = pool_alloc_struct(b);  
  x = kprep(x,t,n);
  return x;
}

K kprep(K x, C t, I n)
{
  xa = 0;//or you will inherit random data from depool
  SW(n) { CSF(0,) CS(1, xa |= ATTR_SORTED) }
  xh = 0;
  xt = t;
  xr = 1;
  xn = n;
  return x;
}

I list_size_k(C t, I n)//atoms CHAR,INT,FLOAT,NIL pass n==0
{
  if(POOL_CHECKS_SANE_LIST_SIZE)
  {
    if(n > POOL_SANE_MEMORY_BOUND) //approximation when compared to list count
    {
      ERROR(ERROR_SIZE);
    }
  }

  C u = abs(t);
  C e = log_size_of_type_element[u];
  return sizeof(K0) + (n << e);
}

C list_size_m(C t, I n)
{
  return ceiling_log_2(list_size_k(t,n));
}

I box_count_K(K x)
{
  I c = 0;

  SW(xt)
  {
   CSF(DEQUE,)
   CSF(ZIP,)
   CSF(FUNC,)
   CSF(HASH,)
   CSF(BTREE,)
   CSF(TABLE,)
   CSF(ATLAS,)
   CSF(DATABASE,)
   CSF(PARTABLE,)
    CS(MAP, c=x->nn)

   CSF(LIST,)
    CD: c = xn; //we're overloading this for vectors too...
  }

  return c;
}

I any_size_K(K x)
{
  I s = 0;

  if(IS_FILLED(x))R total_space_K(x);

  SW(xt)
  {
    CSF( LINK,  )
    CSF( INT,   )
    CSF( FLOAT, )
    CSF( STAMP, )
     CS( NIL,  s = sizeof(K0))
     CS( CHAR, s = 1+offsetof(K0,c))

    CD: s = list_size_k(xt, box_count_K(x));
  }

  return s;
}

ALWAYS_INLINE I total_space_K(K x)
{
  return POW2(xm);
}

ALWAYS_INLINE I max_possible_count_K(K x)
{ 
  return (total_space_K(x) - sizeof(K0)) >> log_size_of_type_element[abs(xt)];
}

I derived_height(K x)
{
  return floor_log_2(max_possible_count_K(x));
}

I derived_height_I(K x)
{
  //quickly patch 10% loss and move on...
  //if you really want to be explicit, at the end you can
  //have the hashtable and the tree reclaim ->u for their own use
  //provided nothing else superceded it in the meantime
  return floor_log_2((POW2(xm) - sizeof(K0))>>3);
}

#pragma mark -

V list_payload_start(K x)
{
  return kC(x);//assert: same as ((V)x)+sizeof(K0)
}

I list_payload_length(K x)
{
  return total_space_K(x) - sizeof(K0);
}

K fill_list_payload_I(K x, I a)
{
  I n = max_possible_count_K(x);
  DO(n, xI[i]=a)
  return x;
}

//see also: zero_unused_space
K zero_list_payload(K x)
{
  bzero(list_payload_start(x), list_payload_length(x));
  return x;
}

#pragma mark - Nest

K zero_unused_space(K x)
{
  if(IS_DISK(x) && IS_NEST(x))
  {
    V u = nest_index_used_end(x);
    bzero(u, nest_index_space_end(x) - u);
    V v = nest_payload_end(x);
    bzero(v, kENDM(x) - v);//payload whitespace 
  }
  else
  {
    I a = any_size_K(x);
    I b = total_space_K(x);
    bzero(((S)x)+a, b-a);
  }

  return x;
}

I last_jump_index(K x)
{
  if (!IS_DISK(x))return -1;
  I n = box_count_K(x);
//#if MAPPED_ALLOWS_INLINE_ATOMS
//  DO(n, I j = n-i-1; K y = k0i(x,j); if(yt == JUMP){return j;})
//  return -1;
//#else
//  return n-1;
//#endif
  return n-1;
}

K last_jump_start(K x)
{
  I j = last_jump_index(x);

  if (0 > j) return NULL;
  
  return kJ(x,j);
}

V nest_index_used_end(K x)
{
  I n = box_count_K(x);
  return sizeof(K0)*(1+n) + (V)x;
}

V nest_index_space_end(K x)
{
  return xh?kENDH(x):kENDM(x);
}

V nest_payload_start(K x)
{
  return nest_index_space_end(x);
}

V nest_payload_end(K x)
{
  K w = last_jump_start(x);

  if(w) return kENDM(w);

  return nest_index_space_end(x);
}

I nest_payload_total_space(K x)
{
  return total_space_K(x) - POW2(xh?xh:3); 
}

I nest_payload_used(K x)
{
  return nest_payload_end(x) - nest_index_space_end(x); 
}

I nest_payload_remaining(K x)
{
  //assert: == return kENDM(x) - nest_payload_end(x);
  return nest_payload_total_space(x) - nest_payload_used(x);
}

#pragma mark -

I non_empty_count(K x)
{
  return COUNT(x) - empty_count(x);
}

I empty_count(K x)
{
  I c = 0;
  NEST(x,if(IS_EMPTY_ATOM(v))c++)
  return c;
}

#pragma mark -

void pool_assertions()
{
  assert(log2(sizeof(K0)) == LOG2_SIZEOF_K0);
  assert(MAPPED_BLOCK_SIZE > PAGE_SIZE_BYTES);
  assert(0==(MAPPED_BLOCK_SIZE % PAGE_SIZE_BYTES));
  assert(sizeof(void*) <= (POW2(POOL_LANE_STRUCT_MIN)));//pooled memory holds pointers 
  assert(POOL_LANE_STRUCT_MIN <= POOL_LANE_MAX);             
  assert(POOL_LANE_MAX < POOL_LANE_BUF);              
  assert(!(PAGE_SIZE_BYTES & (PAGE_SIZE_BYTES - 1))); //page size must be a power of 2
  assert(PAGE_SIZE_BYTES <= POW2(POOL_LANE_MAX));  //would munmap sibling segments
  char list_lane = min_lane_for_bytes(list_size_k(-1,1), POOL_LANE_STRUCT_MIN);
  char atom_lane = min_lane_for_bytes(sizeof(K0), POOL_LANE_STRUCT_MIN);
  assert(list_lane <= atom_lane);//for cow_coerce_array atoms in place wo/ separate alloc
  assert(sizeof(F)==sizeof(I));//used everywhere -2 in switch defaults to -1 or I
  assert(POOL_MIN_ALLOC_WIDTH >= PAGE_SIZE_BYTES);//mmap gives minimum 1 page anyway
  assert(POOL_MIN_ALLOC_WIDTH <= POW2(POOL_LANE_MAX));//because of unmap on repool >=lane max
  assert(((I)(C)-1) != ((I)(UC)-1));//during dev we assumed C was signed
}

void pool_init()
{
  size_of_type_element[FUNC]     = sizeof(K0);
  size_of_type_element[CHAR]     = sizeof( C);
  size_of_type_element[INT]      = sizeof( I);
  size_of_type_element[FLOAT]    = sizeof( F);
  size_of_type_element[STAMP]    = sizeof( I);
  size_of_type_element[NIL]      = sizeof( I);
  size_of_type_element[LIST]     = sizeof(K0);
  size_of_type_element[MAP]      = sizeof(K0);
  size_of_type_element[HASH]     = sizeof(K0);
  size_of_type_element[BTREE]    = sizeof(K0);
  size_of_type_element[TABLE]    = sizeof(K0);
  size_of_type_element[ATLAS]    = sizeof(K0);
  size_of_type_element[ZIP]      = sizeof(K0);
  size_of_type_element[INT4]     = sizeof(int32_t);
  size_of_type_element[INT2]     = sizeof(int16_t);
  size_of_type_element[INT1]     = sizeof(int8_t);
  size_of_type_element[FLOAT4]   = sizeof(float);
  size_of_type_element[DEC4INT8] = sizeof(int64_t);
  size_of_type_element[STR32]    = 32*sizeof(C);
  //////////////////////////////////////////
  size_of_type_element[LINK]     = sizeof( V); //variable size: 32/64-bit systems
  size_of_type_element[JUMP]     = sizeof( I);
  size_of_type_element[DEQUE]    = sizeof(K0);

  DO(TYPE_SIZE, log_size_of_type_element[i] = ceiling_log_2(size_of_type_element[i]))
  
  DISK_LANE_MIN = ceiling_log_2(PAGE_SIZE_BYTES); 

#if POOL_BIG_ATOMS_PAGE_SIZE
  POOL_LANE_STRUCT_MIN = ceiling_log_2(PAGE_SIZE_BYTES);//nicer: everything at least 1 page (for zip, etc.)
#elif POOL_BIG_ATOMS_CACHE_LINE
  POOL_LANE_STRUCT_MIN = 6;                             //faster?: log_2(cache line size); faster with many small objects
#else
  POOL_LANE_STRUCT_MIN = 4;                             //smaller: log_2(minimum data structure size)
#endif

  POOL_LANE_MAX = POOL_MAX_LANE_RETAINED_IN_POOL;

  if(POOL_DYNAMIC_MAX_LANE_OVERRIDE)
  {
    //If system has 2^{y} bytes of memory, use y-x as max lane
    I logscale_difference_from_physical_memory = 5; 

    I system_memory = total_physical_system_memory_bytes();
    I log_memory = ceiling_log_2(system_memory);
   
    I max_lane = log_memory - logscale_difference_from_physical_memory;

    POOL_LANE_MAX = max_lane;
  }

  if(POOL_WARMS_CACHES_ON_INIT)
  {
    //warm goes here;
  }

}

int pool_go()
{
  return 0;
}

