#include "kerf.h"

//2016.05.04 kevin - variety of ways to do this (signature) and I don't have time to check what's best
//compress   static char*   compress_chunk_X(char *source, I *wrote) 
//decompress static char* decompress_blob_X (char *source, I width, bool unchanged) //wkdm is constant, but can we guarantee?

#pragma mark - 

//Let's call an unzipped data page (not necessarily PAGE_SIZE_BYTES) a "chunk"
//Let's call a chunk after compression a "blob"

V zip_algos_compress[ZIP_TYPES_MAX]   = {[ZIP_TYPE_IDENTITY] = zip_algo_compress_chunk_identity,
                                         [ZIP_TYPE_WKDM]     = zip_algo_compress_chunk_wkdm,
                                         [ZIP_TYPE_GZIP_16K] = zip_algo_compress_chunk_gzip_16K,
                                         [ZIP_TYPE_LZ4]      = zip_algo_compress_chunk_lz4,
                                        }; 

V zip_algos_decompress[ZIP_TYPES_MAX] = {[ZIP_TYPE_IDENTITY] = zip_algo_decompress_blob_identity,
                                         [ZIP_TYPE_WKDM]     = zip_algo_decompress_blob_wkdm,
                                         [ZIP_TYPE_GZIP_16K] = zip_algo_decompress_blob_gzip_16K,
                                         [ZIP_TYPE_LZ4]      = zip_algo_decompress_blob_lz4,
                                        }; 

//POTENTIAL_OPTIMIZATION_POINT
// 0. I didn't profile this (& didn't pre-optimize much), so we could speed it up that way
// 1. slicing chunk of float by {sign,mantissa,exp},
//    maybe subdivide mantissa,  likely good for compression
//    maybe even just grouping bytes
//    see stackoverflow.com/questions/8630609/compressing-floating-point-data 
// 2. slicing bytes may even be good for INTs
// 3. Could be "better" to put decimalization (times float by 10000 and store in INT)
//    wrapped inside of an algo, for some applications

void delta_delta_I_transform_chunk(char *source, I sourcelen, bool inverting)
{
  I width = sizeof(I);
  C log_width = floor_log_2(width);

  assert(sourcelen/width >= 3);
  assert(sourcelen % width == 0);

  I *s = (I*)source;
  I n = sourcelen >> log_width; 

  if(!inverting)
  {
    //regular        i0      i1                  i2                  i3  
    //deltas         i0 (i1-i0)             (i2-i1)             (i3-i2)
    //delta-deltas   i0 (i1-i0)   (i2- i1 - (i1-i0)) (i3-i2 - (i2-i1)))
    //so from i[3] on,   i[k] := i[k] - 2*i[k-1] + i[k-2]
    //POTENTIAL_OPTMIZATION_POINT
    //we do one reverse pass. is two forward passes (minuses) better?
    //does it matter?
    DO(n-2, I k = n-i-1; s[k] = s[k] - 2*s[k-1] + s[k-2]) //delta-delta
    s[1]=s[1]-s[0];                                       //delta
    s[0]=s[0];                                            //identity/base case
  }
  else
  {
    I base  = s[0]; 
    I diff  = s[1];

    s[1] = s[0] + diff;

    I delta = diff;

    DO(n-2, I k = i+2; delta += s[k]; s[k] = s[k-1] + delta)
  }

  return;
}

void byte_transform_chunk(char *source, I sourcelen, I itemwidth, bool inverting)
{
  //Group all the bytes from the items of itemwidth in source
  //So first int64_t replaced by first 0th bytes from first 8 items and so on
  K y = new_k(CHARVEC,sourcelen);
  work_push(y);

  I items = sourcelen / itemwidth;

  //POTENTIAL_OPTIMIZATION_POINT
  //is this the fastest way (order) to do this transform?
  if(!inverting)
  {
    DO(itemwidth, 
      DO2(items,
        yC[(i*items) + j] = source[(j*itemwidth) + i]; 
      )
    )
  }
  else
  {
    DO(items, 
      DO2(itemwidth,
        yC[(i*itemwidth) + j] = source[(j*items) + i]; 
      )
    )
  }

  memcpy(source, yC, sourcelen);

  work_pop_rd(true);

  return;
}

char *zip_algo_compress_chunk_identity(char * source, I chunk_size, I *wrote)
{
  //We could reduce these statically allocated memories by
  //statically saving charvec objects instead...
  static C blob_buf[4096] = {0};

  assert(sizeof(blob_buf) >= chunk_size);

  memcpy(blob_buf, source, chunk_size);

  *wrote=chunk_size;

  return blob_buf;
}

char *zip_algo_compress_chunk_wkdm(char * source, I chunk_size, I *wrote)
{
  static C blob_buf[2*4096] = {0}; //can be larger than chunk size when ratio >1.0

  assert(sizeof(blob_buf) >= 2*chunk_size);

  //If this crashes then maybe wkdm output isn't word-aligned
  *wrote = (I)WKdm_compress((V)source, (V)blob_buf, 1024);

  return blob_buf;
}

char *zip_algo_compress_chunk_gzip_16K(char * source, I chunk_size, I *wrote)
{
  static C blob_buf[2*16384] = {0}; //zlib says can be 0.1% larger + 12 bytes (?)

  assert(sizeof(blob_buf) >= 2*chunk_size);//or 1.10 or something

  uLongf destLen = sizeof(blob_buf);

  int gzip_level = 1;

  int err = compress2((V)blob_buf, &destLen, (V)source, chunk_size, gzip_level);

  if(err)
  {
    fprintf(stderr, "Gzip compression error: %d\n", err);
  }

  *wrote = destLen;

  return blob_buf;
}

char *zip_algo_compress_chunk_lz4(char * source, I chunk_size, I *wrote)
{
  static C blob_buf[2*65536] = {0};

  assert(sizeof(blob_buf) >= 2*chunk_size);//or 1.10 or something

  int acceleration = 1;

  *wrote = LZ4_compress_fast((V)source, (V)blob_buf, chunk_size, sizeof(blob_buf), acceleration);

  if(*wrote <= 0)
  {
    fprintf(stderr, "LZ4 compression error: %lld\n", *wrote);
  }

  return blob_buf;
}

#pragma mark -

char *zip_algo_decompress_blob_identity(char *source, I width, bool just_copy_unchanged)
{
  static C chunk_buf[4096] = {0};

  if(just_copy_unchanged || !just_copy_unchanged)
  {
    memcpy(chunk_buf, source, width);
  }

  return chunk_buf;
}

char *zip_algo_decompress_blob_wkdm(char *source, I width, bool just_copy_unchanged)
{
  static C chunk_buf[4096] = {0};

  if(!just_copy_unchanged)
  {
    WKdm_decompress((V)source, (V)chunk_buf, 0);
  }
  else
  {
    memcpy(chunk_buf, source, width);
  }

  return chunk_buf;
}

char *zip_algo_decompress_blob_gzip_16K(char *source, I width, bool just_copy_unchanged)
{
  static C chunk_buf[16384] = {0};

  if(!just_copy_unchanged)
  {
    uLongf destLen = sizeof(chunk_buf);
    uLong  sourceLen = width;
    int err = uncompress((V)chunk_buf, &destLen, (V)source, sourceLen);

    if(err)
    {
      fprintf(stderr, "Gzip decompression error: %d\n", err);
    }

  }
  else
  {
    memcpy(chunk_buf, source, width);
  }

  return chunk_buf;
}

char *zip_algo_decompress_blob_lz4(char *source, I width, bool just_copy_unchanged)
{
  static C chunk_buf[65536] = {0};

  if(!just_copy_unchanged)
  {
    int maxDecompressedSize = sizeof(chunk_buf);
    int compressed_size = width;

    int out = LZ4_decompress_safe((V)source, (V)chunk_buf, compressed_size, maxDecompressedSize);

    if(out <=0)
    {
      fprintf(stderr, "LZ4 decompression error: %d\n", out);
    }

  }
  else
  {
    memcpy(chunk_buf, source, width);
  }

  return chunk_buf;
}

#pragma mark - Compression (Lossless)

K compressed(K x, K type)
{
  if(!VECTOR_ATOM(axt)) 
  {
    fprintf(stderr, "Refusing to zip non-vectorizable type\n");
    ERROR(ERROR_TYPE);
  }

  C t = 0;
  if (type)
  {
    if (!IS_CHAR(type))
    {
      fprintf(stderr, "Compressed vector type must be a character.\n");
      ERROR(ERROR_TYPE);
    }
    t = type->c;
  }
  else
  {
    // reasonable defaults?
    // (for some value of reasonable):
    SW(xt)
    {
      CS(-STAMP, t = '9')
      CS(-FLOAT, t = 'V')
      CS(-INT  , t = 'W')
    }
  }

  // this is actually a bit more general than calling this routine 'compressed'
  // might imply, but it's the easiest way to keep all our field typecodes consistent:
  K z = t ? new_empty_list_for_delimited_field(t) : new_zip();

  z = cow_join(z,x);

  return z;
}

I compression_bytes_if_decompressed(K x)
{
  I chunk_size = POW2(x->mk);

  I in_compressed = ((xKeys)->n * chunk_size);
  I uncompressed = compression_bytes_count_currently_uncompressed(x);

  I total = in_compressed + uncompressed;

  return total;
}

I compression_bytes_zipped_up_to(K x) //exclusive / half-open
{
  I index_count = xKeys->n;
  I zipped_up_to = 0;

  if(index_count > 0)
  {
    zipped_up_to = kI(xKeys)[index_count - 1];
  }

  return zipped_up_to;
}

I compression_bytes_count_currently_uncompressed(K x)
{
  I byte_count = xIndex->n; 
  I zipped_up_to = compression_bytes_zipped_up_to(x);

  return byte_count - zipped_up_to; 
}

K cow_zip_maybe_compress(K x)
{
  I original_byte_count = xIndex->n; 

  //bytes (before): |compressed bytes|uncompressed complete chunks|uncompressed in-progress, incomplete chunk|
  //bytes (after):  |compressed bytes.................|uncompressed in-progress, incomplete chunk|savings....|
  //ints:           |boundary-of-compressed-blob-1|boundary-of-compressed-blob-2|...|

  I zipped_up_to = compression_bytes_zipped_up_to(x);

  I uncompressed = original_byte_count - zipped_up_to; //same as zip_bytes_count_currently_uncompressed()

  I chunk_size = POW2(x->mk);
  if(uncompressed < chunk_size)
  {
    return x;
  }

  I chunks_to_compress = uncompressed / chunk_size;

  if(chunks_to_compress <= 0)
  {
    return x;
  }

  bool delta_delta    = GET_ALT_ATTR(x, ZIP_ATTR_DELTA_DELTA_I);
  bool byte_transform = GET_ALT_ATTR(x, ZIP_ATTR_BYTE_TRANSFORM);

  //Because the algo can expand, we need to compress all complete chunks
  //into a single object, then join incomplete, then rewrite over the original charvec segment
  //source is x, dest is y, then dest is x

  S source = kC(xIndex)+zipped_up_to;
  I full_chunks = chunks_to_compress;


  char* (*algo)(char *source, I chunk_size, I *wrote) = (V)zip_algos_compress[x->nf];

/////////////////////////////////////////////////////////

  //POTENTIAL_OPTIMIZATION_POINT
  //we can eliminate copying out of the static buffers to the charvec (y)
  //here by doing what we had originally and causing each 
  //subalgorithm to return a y CHARVEC. otoh, they are now also 
  //responsible for either returning INTVEC of widths or
  //updating boundaries on x
  //ALTERNATIVELY, we can pass the dest buffer as an argument
  //though we need the chunk (compressed blob) sizes to behave
  //over algorithms (or max over them)

  //better?: expand each time to have 2-3 surplus chunks
  K y = new_k(CHARVEC, (1+2*chunks_to_compress)*chunk_size);
  yn = 0;
  work_push(y);
  S dest = yC;

  I total_wrote = 0;

  I boundary = zipped_up_to;

  DO(chunks_to_compress,  
                         I wrote = 0;
                         char *bytes_compressed = NULL;

                         bool intermediate = byte_transform | delta_delta;

                         if(intermediate)
                         {
                           //POTENTIAL_OPTIMIZATION_POINT is there a way to
                           //skip this extra copy or does it even matter
                           C item_width = size_of_type_element[ABS(x->nk)];
                           memcpy(dest, source, chunk_size);

                           if(delta_delta) delta_delta_I_transform_chunk(dest, chunk_size, false);
                           if(byte_transform) byte_transform_chunk(dest, chunk_size, item_width, false);

                           bytes_compressed = algo(dest, chunk_size, &wrote);
                         }
                         else
                         {
                           bytes_compressed = algo(source, chunk_size, &wrote);
                         }

                         memcpy(dest, bytes_compressed, wrote);

                         source += chunk_size;
                         dest += wrote;
                         yn += wrote;
                         total_wrote += wrote;
                         boundary += wrote;
                         nestneu(x, KEYS, cow_add(xKeys, ki(boundary)));
  )

/////////////////////////////////////////////////////////

  assert(yn == total_wrote);

  //move uncompressed leftovers into place
  I size_of_leftovers = uncompressed - (chunks_to_compress * chunk_size);

  DO(size_of_leftovers, y = cow_add(y, kc(source[i]))) 

  //Expand bytes to hold y (+ nestneu)
  I revised_count = original_byte_count - uncompressed + total_wrote;

  I possible = max_possible_count_K(xIndex);

  I m = xIndex->m;
  if(revised_count > possible)
  {
    m = m + 1;
  }
  nestneu(x, INDEX, cow_expand(xIndex, m));

  memcpy(kC(xIndex)+zipped_up_to, yC, yn);

  xIndex->n = revised_count;

  work_pop_rd(true);


  return x;
}

K cow_zip_add_bytes(K x, char* bytes, I length) //plus maybe compress
{
  x=cow(x);
  nestneu(x,INDEX,cow(xIndex));

  K y = xIndex;

  if(max_possible_count_K(y) < yn + length)
  {
    y = cow_expand(y,ym+1);
  }
   
  memcpy(yC + yn, bytes, length);
  yn += length;

  nestneu(x,INDEX,y);

  x = cow_zip_maybe_compress(x);

  return x;
}

K cow_zip_add(K x, K y)
{
  x=cow(x);
  nestneu(x,INDEX,cow(xIndex));
  nestneu(x,KEYS, cow(xKeys));

  K0 k;

  SW(yt)
  {
    CS(FLOAT, if(NIL == x->nk)
              {
                x->nk = -ABS(yt);
                x->na = zip_attr_for_subtype(x->nk);
              }
              C zip_subtype = ABS(x->nk);
              SW(zip_subtype)
              {
                CS(DEC4INT8, k.i =yf*10000.0; return cow_zip_add_bytes(x, (S)&k.i, size_of_type_element[zip_subtype]))
                CS(FLOAT4,   k.f4=yf;         return cow_zip_add_bytes(x, (S)&k.i, size_of_type_element[zip_subtype]))
                CS(FLOAT,    k.f =yf;         return cow_zip_add_bytes(x, (S)&k.i, size_of_type_element[zip_subtype]))
                CD: return x;
              }
    )
    CS(STAMP, if(NIL == x->nk)
              {
                x->nk = -ABS(yt);
                x->na = zip_attr_for_subtype(x->nk);
              }
              C zip_subtype = ABS(x->nk);
              SW(zip_subtype)
              {
                CS(STAMP, k.i =yi; return cow_zip_add_bytes(x, (S)&k.i, size_of_type_element[zip_subtype]))
                CD: return x;
              }
    )
    CS(INT,   //POTENTIAL_OPTIMIZATION_POINT
              //you get fast zip joins by
              //filling in join(ZIP x INTVEC) and using cow_zip_add_bytes (sames for STAMPVEC,FLOATVEC)
              if(NIL == x->nk)
              {
                x->nk = -ABS(yt);
                x->na = zip_attr_for_subtype(x->nk);
              }
              C zip_subtype = ABS(x->nk);
              SW(zip_subtype)
              {
                CS(INT1,  k.i1=yi; return cow_zip_add_bytes(x, (S)&k.n, size_of_type_element[zip_subtype]))
                CS(INT2,  k.i2=yi; return cow_zip_add_bytes(x, (S)&k.n, size_of_type_element[zip_subtype]))
                CS(INT4,  k.i4=yi; return cow_zip_add_bytes(x, (S)&k.n, size_of_type_element[zip_subtype]))
               CSF(DEC4INT8,)
                CS(INT,   k.i =yi; return cow_zip_add_bytes(x, (S)&k.n, size_of_type_element[zip_subtype]))
                CD: return x;
              }
    )

   CSF(INTVEC,)
   CSF(FLOATVEC,)
   CSF(STAMPVEC,)
   CSF(STR32,)
   CSF(DEC4INT8)
   CSF(FLOAT4,)
   CSF(INT4,)
   CSF(INT2,)
   CSF(INT1,)
    CS(CHAR,
    
      if(NIL == x->nk)
      {
        x->nk = -ABS(yt);
        x->na = zip_attr_for_subtype(x->nk);
      }
      else if(x->nk != -ABS(yt))
      {
        return x;
      }

      I bytes_to_copy = lenI(y) * size_of_type_element[ABS(x->nk)];
      S bytes_start = (S)PAYLOAD_START(y);

      x = cow_zip_add_bytes(x, bytes_start, bytes_to_copy); 

      return x;
    )
    CS(CHARVEC,
      SW(ABS(x->nk))
      {
        CS(STR32, 
                  I width = size_of_type_element[STR32];
                  C bytes_start[width];

                  bzero(bytes_start,width);  
                  memcpy(bytes_start, yC, MIN(yn,width));

                  x = cow_zip_add_bytes(x, bytes_start, width); 

                  return x;       
        )
      }
    )
    CD: return x;
  }

  //only storing payload, for I 
  //DO compress first chunk, when full
  //last chunk in progress uncompressed
  //Any bytes past the last index are uncompressed

  //notes
  //zip[INT]    look up 1 item
  //caching the chunk is friendly
  //zip[INTVEC] look up n items (simple: look for runs where they'd be in the same chunk. or just cache)

  return x;
}

I2 compression_range_for_compressed_chunk_index(K x, I p)
{
  assert(p < xKeys->n);
  I2 i = {0, 0}; 

  i.x = 0;
  i.y = 0;

  if(p > 0) //if(p > 0 && p < xKeys->n)
  {
    i.x = kI(xKeys)[p-1];
  }

  //if(p < xKeys->n)
  i.y = kI(xKeys)[p];

  return i;
}

C* compression_decompressed_chunk_by_index(K x, I p, bool skip_cache)
{
  I chunk_size = POW2(x->mk);

  static K prev_zip = NULL;
  static I prev_chunk_index = 0; 
  static I prev_byte_length = 0; //wo/ this, then caching fails for uncompressed stragglers
  static C prev_zip_type = 0; //just in case?

  static S chunk_ptr = NULL;

  if(!skip_cache && chunk_ptr && prev_zip == x && prev_chunk_index == p && prev_byte_length == xIndex->n && prev_zip_type == x->nf)
  {
    return chunk_ptr;
  }

  bool is_compressed_chunk = (p < xKeys->n);

  char *source = NULL;
  I width = 0;
  bool just_copy_unchanged = !is_compressed_chunk;

  if(is_compressed_chunk)
  {
    I2 i = compression_range_for_compressed_chunk_index(x, p);

    source = kC(xIndex) + i.x;
    width = i.y - i.x;
    just_copy_unchanged = false;
  }
  else //it's uncompressed stragglers
  {
    //TODO this just gives the last uncompressed maybe-in-progress chunk
    //     but we could be smarter & handle the case where 
    //     we were lazy about compressing multiple added uncompressed chunks
    //     with just a little basic math
    //     to give the actual chunk pointed to by p
    I uncompressed = compression_bytes_count_currently_uncompressed(x);

    width = MIN(uncompressed, chunk_size);
    source = kC(xIndex) + (xIndex->n - width);
    just_copy_unchanged = true;

    //assert width < chunk_size
  }

  char* (*algo)(char *source, I width, bool just_copy_changed) = (V)zip_algos_decompress[x->nf];

  chunk_ptr = algo(source, width, just_copy_unchanged);

  bool was_compressed_chunk = is_compressed_chunk;

  //De-transform
  bool byte_transform = GET_ALT_ATTR(x, ZIP_ATTR_BYTE_TRANSFORM);
  bool delta_delta    = GET_ALT_ATTR(x, ZIP_ATTR_DELTA_DELTA_I);
  if(was_compressed_chunk && (byte_transform || delta_delta))
  {
    C item_width = size_of_type_element[ABS(x->nk)];
    if(byte_transform)byte_transform_chunk(chunk_ptr, chunk_size, item_width, true);
    if(delta_delta) delta_delta_I_transform_chunk(chunk_ptr, chunk_size, true);
  }

  prev_zip = x;
  prev_chunk_index = p;
  prev_byte_length = xIndex->n;
  prev_zip_type = x->nf;

  return chunk_ptr;
}

#pragma mark -

//DO NOT do this apparently, since it will cause writes?
//and so on to fail, b/c the memory wasn't populated
//
//catching accesses to unallocated memory via mprotect
//long *long_view =
//   mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
//double *double_view =
//   mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
//
//static void on_segv(int signum, siginfo_t *info, void *data) {
//    void *addr = info->si_addr;
//    if ((uintptr_t)addr - (uintptr_t)long_view < 4096) {
//        mprotect(long_view, 4096, PROT_READ|PROT_WRITE);
//        /* translate from double_view to long_view */
//        mprotect(double_view, 4096, PROT_NONE);
//    } else if ((uintptr_t)addr - (uintptr_t)double_view < 4096) {
//        mprotect(double_view, 4096, PROT_READ|PROT_WRITE);
//        /* translate from long_view to long_view */
//        mprotect(double_view, 4096, PROT_NONE);
//    } else {
//        abort();
//    }
//}
//
//struct sigaction segv_action = {
//    .sa_sigaction = on_segv,
//    .sa_flags = SA_RESTART | SA_SIGINFO,
//};
//sigaction(SIGSEGV, &segv_action, NULL);
//
//long_view[0] = 42;
///* hopefully, this will trigger the code to fixup double_view and resume */
//printf("%g\n", double_view[0]);

K zip_with_options(K x, K *char_vector, K *index_vector)
{

  //if char_vector not null, populate with compressed pages
  //if index vector not null, populate with sizes
  //this gives us several useful tools in one
  //
  //we'll also want build index from zipped chars
  //and probably the reverse operations of all these

  return NULL;
}

K new_unzipped_charvec_temp(K x)
{
  S start = xC;
  S end   = start + xn;

  //POTENTIAL_OPTIMIZATION_POINT
  //1. first page should indicate accurate m
  //   so then we can preallocate and also
  //   write bytes directly instead of using cow_add

  K y = new_k(CHARVEC,0);

  C page[4096];

  while(start < end)
  {
    uint32_t *a = (V)start;
    uint32_t line = a[3];
    WKdm_decompress((V)start, (V)page, 0);

    DO(4096, y = cow_add(y, kc(page[i]))) 

    start += 4*line;
  }

  C m = ym;
  I n = yn;

  memmove(y, yC, n);

  OFF_ATTR(y, ATTR_DISK);
  ym = m;
  yr = 1;

  return y;
}

K new_zipped_charvec_temp(K x)
{
  I wire = wire_size_K(x);

  assert(wire >= 4096);

  //can alloc wire_size * 2
  //use what you need: win
  K y = new_k(CHARVEC, wire);
  yn = wire;

  V memory = yC;
  bool force_write_empty_bytes = true;

  write_k_helper(x, 0, false, 0, false, NULL, &memory, force_write_empty_bytes);

  K z = new_k(CHARVEC, wire*2);
  zn = 0;
  
  I pages = wire/4096;

  S source = yC;
  S dest = zC;
    
  DO(pages,  I c = WKdm_compress((V)source, (V)dest, 1024);
             source += 4096;
             dest += c;
             zn += c;
  )

  rd(y);;

  //dd(wire)
  //dd(zn);

  return z;
}


I pages = 1000;
K zipg;
K zip(K x)
{
  C store[2*4096]={0};   

  K z = new_k(-CHAR,0);

  zipg = new_k(-INT,pages);
  ENUM(zipg, kI(zipg)[i]=0)

  I run = 0;

  TIME(
  DO2(pages,
    I c = WKdm_compress((((V)xI)+(j*4096)), (V)store, 1024);
    DO(c, z = cow_add(z, kc(store[i]));)
    kI(zipg)[j+1]=c;
  )
  )

  return z;
}

int zip_go()
{
return 0;
 I n = 512*pages;
 K x;
 
 x = til(ki(n));// 0.294 ratio  9us 7us  zip/unzip per page
 //x = take(ki(n),ki(44)); //0.129 ratio 3u 4u
 //DO(n,xI[i]=rI()) //1.066 ratio  5u 5u
 
 K y = til(ki(n));
 //y = take(ki(n),ki(44)); //0.129 ratio 3u 4u
 
 K z = new_k(-CHAR,n*sizeof(I)+100);
 K a = new_k(-INT,n);
 
 I d;
 
 volatile I sum = 0;
 
 TIME(d = WKdm_compress((V)xI, (V)zC, 2*4096/(sizeof(I)));)
 TIME(WKdm_decompress((V)zI,(V)yI,0);)
 
 TIME(LIST2(x,y,kI(a)[i]=ui+vi))
 TIME(LIST2(x,y,sum+=(xI[i]+yI[i])))
 er(^uncompressed int time)
 
 //sum 1000 pages of ints: 0.001
 //sum 1000 compressed pages of ints: 0.004
 
 dd(sum)
 sum=0;
 
 K zipx = zip(x);
 K zipy = zip(y);
 
   I readx[1*512]={0};   
   I ready[1*512]={0};   
   I run = 0;
 
 I e = 0;
 
 TIME(DO(zipg->n, 
   I c = kI(zipg)[i];
   run += c;
   WKdm_decompress((V)(((S)kI(zipx))+run),(V)readx,0);
   WKdm_decompress((V)(((S)kI(zipy))+run),(V)ready,0);
   DO2(512, sum +=(readx[j]+ready[j]))
 ))
 er(^compressed int time)
 
 dd(sum);
 
 fprintf(stderr,"WKdm ratio: %f\n", d/4096.0);
 return 0;
}

#pragma mark - WKdm
//WKdm Compression
//From [xnu sources](http://www.opensource.apple.com/source/xnu/xnu-1456.1.26/iokit/Kernel/WKdm.h).
//Apparent [license terms](http://www.opensource.apple.com/source/xnu/xnu-1456.1.26/APPLE_LICENSE).
//
//
/* direct-mapped partial matching compressor with simple 22/10 split
 *
 *  Compresses buffers using a dictionary based match and partial match
 *  (high bits only or full match) scheme.
 *
 *  Paul Wilson -- wilson@cs.utexas.edu
 *  Scott F. Kaplan -- sfkaplan@cs.utexas.edu
 *  September 1997
 */

/* compressed output format, in memory order
 *  1. a four-word HEADER containing four one-word values:
 *     i.   a one-word code saying what algorithm compressed the data
 *     ii.  an integer WORD offset into the page saying
 *          where the queue position area starts
 *     iii. an integer WORD offset into the page saying where
 *          the low-bits area starts
 *     iv.  an integer WORD offset into the page saying where the
 *          low-bits area ends
 *
 *  2. a 64-word TAGS AREA holding one two-bit tag for each word in 
 *     the original (1024-word) page, packed 16 per word
 *
 *  3. a variable-sized FULL WORDS AREA (always word aligned and an
 *     integral number of words) holding full-word patterns that
 *     were not in the dictionary when encoded (i.e., dictionary misses)
 *
 *  4. a variable-sized QUEUE POSITIONS AREA (always word aligned and
 *     an integral number of words) holding four-bit queue positions,
 *     packed eight per word.
 *
 *  5. a variable-sized LOW BITS AREA (always word aligned and an
 *     integral number of words) holding ten-bit low-bit patterns
 *     (from partial matches), packed three per word. 
 */

/* ============================================================ */


/* at the moment we have dependencies on the page size.  That should
 * be changed to work for any power-of-two size that's at least 16
 * words, or something like that
 */

#define PAGE_SIZE_IN_WORDS 1024
#define PAGE_SIZE_IN_BYTES 4096

#define DICTIONARY_SIZE 16

/*
 * macros defining the basic layout of stuff in a page
 */
#define HEADER_SIZE_IN_WORDS 4
#define TAGS_AREA_OFFSET 4
#define TAGS_AREA_SIZE 64

/* the next few are used during compression to write the header */
#define SET_QPOS_AREA_START(compr_dest_buf,qpos_start_addr)  \
        (compr_dest_buf[1] = qpos_start_addr - compr_dest_buf)
#define SET_LOW_BITS_AREA_START(compr_dest_buf,lb_start_addr) \
        (compr_dest_buf[2] = lb_start_addr - compr_dest_buf)
#define SET_LOW_BITS_AREA_END(compr_dest_buf,lb_end_addr) \
        (compr_dest_buf[3] = lb_end_addr - compr_dest_buf)

/* the next few are only use during decompression to read the header */
#define TAGS_AREA_START(decomp_src_buf)       \
        (decomp_src_buf + TAGS_AREA_OFFSET)
#define TAGS_AREA_END(decomp_src_buf)         \
        (TAGS_AREA_START(decomp_src_buf) + TAGS_AREA_SIZE)
#define FULL_WORD_AREA_START(the_buf) TAGS_AREA_END(the_buf)
#define QPOS_AREA_START(decomp_src_buf)       \
        (decomp_src_buf + decomp_src_buf[1])   
#define LOW_BITS_AREA_START(decomp_src_buf)   \
        (decomp_src_buf + (decomp_src_buf[2]))
#define QPOS_AREA_END(the_buf) LOW_BITS_AREA_START(the_buf)
#define LOW_BITS_AREA_END(decomp_src_buf)     \
        (decomp_src_buf + (decomp_src_buf[3]))


/* ============================================================ */
/* Misc constants */

#define BITS_PER_WORD 32
#define BYTES_PER_WORD 4
#define NUM_LOW_BITS 10
#define LOW_BITS_MASK 0x3FF
#define ALL_ONES_MASK 0xFFFFFFFF

#define TWO_BITS_PACKING_MASK 0x03030303
#define FOUR_BITS_PACKING_MASK 0x0F0F0F0F
#define TEN_LOW_BITS_MASK 0x000003FF
#define TWENTY_TWO_HIGH_BITS_MASK 0xFFFFFC00

/* Tag values.  NOTE THAT CODE MAY DEPEND ON THE NUMBERS USED.
 * Check for conditionals doing arithmetic on these things
 * before changing them
 */
#define ZERO_TAG 0x0
#define PARTIAL_TAG 0x1
#define MISS_TAG 0x2
#define EXACT_TAG 0x3

#define BITS_PER_BYTE 8

/* ============================================================ */
/* Global macros */

/* Shift out the low bits of a pattern to give the high bits pattern.
   The stripped patterns are used for initial tests of partial
   matches. */
#define HIGH_BITS(word_pattern) (word_pattern >> NUM_LOW_BITS)

/* String the high bits of a pattern so the low order bits can
   be included in an encoding of a partial match. */
#define LOW_BITS(word_pattern) (word_pattern & LOW_BITS_MASK)

#if defined DEBUG_WK
#define DEBUG_PRINT_1(string) printf (string)
#define DEBUG_PRINT_2(string,value) printf(string, value)
#else
#define DEBUG_PRINT_1(string)
#define DEBUG_PRINT_2(string, value)
#endif

/* Set up the dictionary before performing compression or
   decompression.  Each element is loaded with some value, the
   high-bits version of that value, and a next pointer. */
#define PRELOAD_DICTIONARY { \
  dictionary[0] = 1; \
  dictionary[1] = 1; \
  dictionary[2] = 1; \
  dictionary[3] = 1; \
  dictionary[4] = 1; \
  dictionary[5] = 1; \
  dictionary[6] = 1; \
  dictionary[7] = 1; \
  dictionary[8] = 1; \
  dictionary[9] = 1; \
  dictionary[10] = 1; \
  dictionary[11] = 1; \
  dictionary[12] = 1; \
  dictionary[13] = 1; \
  dictionary[14] = 1; \
  dictionary[15] = 1; \
}

/* these are the constants for the hash function lookup table.
 * Only zero maps to zero.  The rest of the tabale is the result
 * of appending 17 randomizations of the multiples of 4 from
 * 4 to 56.  Generated by a Scheme script in hash.scm. 
 */
#define HASH_LOOKUP_TABLE_CONTENTS { \
   0, 52,  8, 56, 16, 12, 28, 20,  4, 36, 48, 24, 44, 40, 32, 60, \
   8, 12, 28, 20,  4, 60, 16, 36, 24, 48, 44, 32, 52, 56, 40, 12, \
   8, 48, 16, 52, 60, 28, 56, 32, 20, 24, 36, 40, 44,  4,  8, 40, \
  60, 32, 20, 44,  4, 36, 52, 24, 16, 56, 48, 12, 28, 16,  8, 40, \
  36, 28, 32, 12,  4, 44, 52, 20, 24, 48, 60, 56, 40, 48,  8, 32, \
  28, 36,  4, 44, 20, 56, 60, 24, 52, 16, 12, 12,  4, 48, 20,  8, \
  52, 16, 60, 24, 36, 44, 28, 56, 40, 32, 36, 20, 24, 60, 40, 44, \
  52, 16, 32,  4, 48,  8, 28, 56, 12, 28, 32, 40, 52, 36, 16, 20, \
  48,  8,  4, 60, 24, 56, 44, 12,  8, 36, 24, 28, 16, 60, 20, 56, \
  32, 40, 48, 12,  4, 44, 52, 44, 40, 12, 56,  8, 36, 24, 60, 28, \
  48,  4, 32, 20, 16, 52, 60, 12, 24, 36,  8,  4, 16, 56, 48, 44, \
  40, 52, 32, 20, 28, 32, 12, 36, 28, 24, 56, 40, 16, 52, 44,  4, \
  20, 60,  8, 48, 48, 52, 12, 20, 32, 44, 36, 28,  4, 40, 24,  8, \
  56, 60, 16, 36, 32,  8, 40,  4, 52, 24, 44, 20, 12, 28, 48, 56, \
  16, 60,  4, 52, 60, 48, 20, 16, 56, 44, 24,  8, 40, 12, 32, 28, \
  36, 24, 32, 12,  4, 20, 16, 60, 36, 28,  8, 52, 40, 48, 44, 56  \
}

#define HASH_TO_DICT_BYTE_OFFSET(pattern) \
        (hashLookupTable[((pattern) >> 10) & 0xFF])

extern const char hashLookupTable[];

/* EMIT... macros emit bytes or words into the intermediate arrays
 */

#define EMIT_BYTE(fill_ptr, byte_value) {*fill_ptr = byte_value; fill_ptr++;}
#define EMIT_WORD(fill_ptr,word_value) {*fill_ptr = word_value; fill_ptr++;}

/* RECORD... macros record the results of modeling in the intermediate
 * arrays
 */

#define RECORD_ZERO { EMIT_BYTE(next_tag,ZERO_TAG); }

#define RECORD_EXACT(queue_posn)  EMIT_BYTE(next_tag,EXACT_TAG);  \
                                  EMIT_BYTE(next_qp,(queue_posn)); 

#define RECORD_PARTIAL(queue_posn,low_bits_pattern) { \
   EMIT_BYTE(next_tag,PARTIAL_TAG);                   \
   EMIT_BYTE(next_qp,(queue_posn));                   \
   EMIT_WORD(next_low_bits,(low_bits_pattern))  }

#define RECORD_MISS(word_pattern) EMIT_BYTE(next_tag,MISS_TAG); \
                                  EMIT_WORD(next_full_patt,(word_pattern)); 
				  
void
WKdm_decompress (uint32_t* src_buf,
		 uint32_t* dest_buf,
		 uint32_t words);
uint32_t
WKdm_compress (uint32_t* src_buf,
               uint32_t* dest_buf,
	       uint32_t num_input_words);


/***********************************************************************
 *                   THE PACKING ROUTINES
 */

/* WK_pack_2bits()
 * Pack some multiple of four words holding two-bit tags (in the low
 * two bits of each byte) into an integral number of words, i.e.,
 * one fourth as many.  
 * NOTE: Pad the input out with zeroes to a multiple of four words!
 */
static uint32_t*
WK_pack_2bits(uint32_t* source_buf,
              uint32_t* source_end,
	      uint32_t* dest_buf) {

   register uint32_t* src_next = source_buf;
   uint32_t* dest_next = dest_buf;
  
   while (src_next < source_end) {
      register uint32_t temp = src_next[0];
      temp |= (src_next[1] << 2);
      temp |= (src_next[2] << 4);
      temp |= (src_next[3] << 6);
    
      dest_next[0] = temp;
      dest_next++;     
      src_next += 4;
   }
  
   return dest_next;

}

/* WK_pack_4bits()
 * Pack an even number of words holding 4-bit patterns in the low bits
 * of each byte into half as many words.
 * note: pad out the input with zeroes to an even number of words!
 */

static uint32_t*
WK_pack_4bits(uint32_t* source_buf,
	      uint32_t* source_end,
	      uint32_t* dest_buf) {
   register uint32_t* src_next = source_buf;
   uint32_t* dest_next = dest_buf;
  
   /* this loop should probably be unrolled */
   while (src_next < source_end) {
     register uint32_t temp = src_next[0];
     temp |= (src_next[1] << 4);
    
     dest_next[0] = temp;
     dest_next++;     
     src_next += 2;
   }

   return dest_next;

}

/* pack_3_tenbits()
 * Pack a sequence of three ten bit items into one word.
 * note: pad out the input with zeroes to an even number of words!
 */
static uint32_t*
WK_pack_3_tenbits(uint32_t* source_buf,
		  uint32_t* source_end,
		  uint32_t* dest_buf) {

   register uint32_t* src_next = source_buf;
   uint32_t* dest_next = dest_buf;
  
   /* this loop should probably be unrolled */
   while (src_next < source_end) {
      register uint32_t temp = src_next[0];
      temp |= (src_next[1] << 10);
      temp |= (src_next[2] << 20);
    
      dest_next[0] = temp;
      dest_next++;     
      src_next += 3;
   }

   return dest_next;

}

/***************************************************************************
 *  WKdm_compress()---THE COMPRESSOR
 */

uint32_t
WKdm_compress (uint32_t* src_buf,
               uint32_t* dest_buf,
	       uint32_t num_input_words)
{
  uint32_t dictionary[DICTIONARY_SIZE];

  /* arrays that hold output data in intermediate form during modeling */
  /* and whose contents are packed into the actual output after modeling */

  /* sizes of these arrays should be increased if you want to compress
   * pages larger than 4KB
   */
  uint32_t tempTagsArray[300];         /* tags for everything          */
  uint32_t tempQPosArray[300];         /* queue positions for matches  */
  uint32_t tempLowBitsArray[1200];     /* low bits for partial matches */

  /* boundary_tmp will be used for keeping track of what's where in
   * the compressed page during packing
   */
  uint32_t* boundary_tmp;

  /* Fill pointers for filling intermediate arrays (of queue positions
   * and low bits) during encoding.
   * Full words go straight to the destination buffer area reserved
   * for them.  (Right after where the tags go.)
   */
  uint32_t* next_full_patt;
  char* next_tag = (char *) tempTagsArray;
  char* next_qp = (char *) tempQPosArray;
  uint32_t* next_low_bits = tempLowBitsArray;

  uint32_t* next_input_word = src_buf;
  uint32_t* end_of_input = src_buf + num_input_words;

  PRELOAD_DICTIONARY;

  next_full_patt = dest_buf + TAGS_AREA_OFFSET + (num_input_words / 16);

#ifdef WK_DEBUG
  printf("\nIn WKdm_compress\n");
  printf("About to actually compress, src_buf is %u\n", src_buf);
  printf("dictionary is at %u\n", dictionary);
  printf("dest_buf is %u next_full_patt is %u\n", dest_buf, next_full_patt);
  fflush(stdout);
#endif

  while (next_input_word < end_of_input)
  {
     uint32_t *dict_location;
     uint32_t dict_word;
     uint32_t input_word = *next_input_word;

     /* compute hash value, which is a byte offset into the dictionary,
      * and add it to the base address of the dictionary. Cast back and
      * forth to/from char * so no shifts are needed
      */
     dict_location =
       (uint32_t *)
       (((char*) dictionary) + HASH_TO_DICT_BYTE_OFFSET(input_word));

     dict_word = *dict_location;

     if (input_word == dict_word)
     {
        RECORD_EXACT(dict_location - dictionary); 
     }
     else if (input_word == 0) {
        RECORD_ZERO;
     }
     else
     {
        uint32_t input_high_bits = HIGH_BITS(input_word);
        if (input_high_bits == HIGH_BITS(dict_word)) {
	  RECORD_PARTIAL(dict_location - dictionary, LOW_BITS(input_word));
          *dict_location = input_word;
        }
        else {
	  RECORD_MISS(input_word);
            *dict_location = input_word;
        }
     }
     next_input_word++;
  }

#ifdef WK_DEBUG
  printf("AFTER MODELING in WKdm_compress()\n");  fflush(stdout);
  printf("tempTagsArray holds %u bytes\n",
         next_tag - (char *) tempTagsArray);
  printf("tempQPosArray holds %u bytes\n",
         next_qp - (char *) tempQPosArray);
  printf("tempLowBitsArray holds %u bytes\n",
         (char *) next_low_bits - (char *) tempLowBitsArray);

  printf("next_full_patt is %p\n",
          next_full_patt);

  printf(" i.e., there are %u full patterns\n",
     next_full_patt - (dest_buf + TAGS_AREA_OFFSET + (num_input_words / 16)));
  fflush(stdout);

  { int i;
    uint32_t *arr =(dest_buf + TAGS_AREA_OFFSET + (num_input_words / 16));

    printf("  first 20 full patterns are: \n");
    for (i = 0; i < 20; i++) {
      printf(" %d", arr[i]);
    }
    printf("\n");
  }
#endif

  /* Record (into the header) where we stopped writing full words,
   * which is where we will pack the queue positions.  (Recall
   * that we wrote the full words directly into the dest buffer
   * during modeling.
   */

  SET_QPOS_AREA_START(dest_buf,next_full_patt);

  /* Pack the tags into the tags area, between the page header
   * and the full words area.  We don't pad for the packer
   * because we assume that the page size is a multiple of 16.
   */     

#ifdef WK_DEBUG
  printf("about to pack %u bytes holding tags\n", 
         next_tag - (char *) tempTagsArray);

  { int i;
    char* arr = (char *) tempTagsArray;

    printf("  first 200 tags are: \n");
    for (i = 0; i < 200; i++) {
      printf(" %d", arr[i]);
    }
    printf("\n");
  }
#endif

  boundary_tmp = WK_pack_2bits(tempTagsArray,
		               (uint32_t *) next_tag,
			       dest_buf + HEADER_SIZE_IN_WORDS);

#ifdef WK_DEBUG  
    printf("packing tags stopped at %u\n", boundary_tmp);
#endif
  
  /* Pack the queue positions into the area just after
   * the full words.  We have to round up the source
   * region to a multiple of two words.
   */

  {
    uint32_t num_bytes_to_pack = next_qp - (char *) tempQPosArray;
    uint32_t num_packed_words = (num_bytes_to_pack + 7) >> 3; // ceil((double) num_bytes_to_pack / 8);
    uint32_t num_source_words = num_packed_words * 2;
    uint32_t* endQPosArray = tempQPosArray + num_source_words;

    /* Pad out the array with zeros to avoid corrupting real packed
       values. */
    for (; /* next_qp is already set as desired */
	 next_qp < (char*)endQPosArray;
	 next_qp++) {
      *next_qp = 0;
    }

#ifdef WK_DEBUG    
    printf("about to pack %u (bytes holding) queue posns.\n",
           num_bytes_to_pack);
    printf("packing them from %u words into %u words\n",
           num_source_words, num_packed_words);
    printf("dest is range %u to %u\n",
           next_full_patt, next_full_patt + num_packed_words);
    { int i;
      char *arr = (char *) tempQPosArray;
      printf("  first 200 queue positions are: \n");
      for (i = 0; i < 200; i++) {
        printf(" %d", arr[i]);
      }
      printf("\n");
    }
#endif
    
    boundary_tmp = WK_pack_4bits(tempQPosArray,
			         endQPosArray,
				 next_full_patt);
#ifdef WK_DEBUG
     printf("Packing of queue positions stopped at %u\n", boundary_tmp);
#endif // WK_DEBUG

    /* Record (into the header) where we stopped packing queue positions,
     * which is where we will start packing low bits.
     */
    SET_LOW_BITS_AREA_START(dest_buf,boundary_tmp);

  }

  /* Pack the low bit patterns into the area just after
   * the queue positions.  We have to round up the source
   * region to a multiple of three words.
   */

  {
    uint32_t num_tenbits_to_pack =
      next_low_bits - tempLowBitsArray;
    uint32_t num_packed_words = (num_tenbits_to_pack + 2) / 3; //ceil((double) num_tenbits_to_pack / 3);
    uint32_t num_source_words = num_packed_words * 3;
    uint32_t* endLowBitsArray = tempLowBitsArray + num_source_words;

    /* Pad out the array with zeros to avoid corrupting real packed
       values. */

    for (; /* next_low_bits is already set as desired */
	 next_low_bits < endLowBitsArray;
	 next_low_bits++) {
      *next_low_bits = 0;
    }

#ifdef WK_DEBUG
	  printf("about to pack low bits\n");
          printf("num_tenbits_to_pack is %u\n", num_tenbits_to_pack);
          printf("endLowBitsArray is %u\n", endLowBitsArray);
#endif
    
    boundary_tmp = WK_pack_3_tenbits (tempLowBitsArray,
		                      endLowBitsArray,
				      boundary_tmp);

    SET_LOW_BITS_AREA_END(dest_buf,boundary_tmp);

  }

  return ((char *) boundary_tmp - (char *) dest_buf);
} 




/***************************************************************************
 *          THE UNPACKING ROUTINES should GO HERE
 */

const char hashLookupTable [] = HASH_LOOKUP_TABLE_CONTENTS;

#if 0
#define GET_NEXT_TAG tags[tagsIndex++]
#define GET_NEXT_FULL_PATTERN fullPatterns[fullPatternsIndex++]
#define GET_NEXT_LOW_BITS lowBits[lowBitsIndex++]
#define GET_NEXT_DICTIONARY_INDEX dictionaryIndices[dictionaryIndicesIndex++]
#endif

/*  WK_unpack_2bits takes any number of words containing 16 two-bit values
 *  and unpacks them into four times as many words containg those
 *  two bit values as bytes (with the low two bits of each byte holding
 *  the actual value.
 */
static uint32_t*
WK_unpack_2bits(uint32_t *input_buf,
                uint32_t *input_end,
                uint32_t *output_buf) {

  register uint32_t *input_next = input_buf;
  register uint32_t *output_next = output_buf;
  register uint32_t packing_mask = TWO_BITS_PACKING_MASK;

  /* loop to repeatedly grab one input word and unpack it into
   * 4 output words.  This loop could be unrolled a little---it's
   * designed to be easy to do that.
   */   
  while (input_next < input_end) {
    register uint32_t temp = input_next[0];
    DEBUG_PRINT_2("Unpacked tags word: %.8x\n", temp);
    output_next[0] = temp & packing_mask;
    output_next[1] = (temp >> 2) & packing_mask;
    output_next[2] = (temp >> 4) & packing_mask;
    output_next[3] = (temp >> 6) & packing_mask;
    
    output_next += 4;
    input_next++;
  }

  return output_next;

}

/* unpack four bits consumes any number of words (between input_buf
 * and input_end) holding 8 4-bit values per word, and unpacks them
 * into twice as many words, with each value in a separate byte.
 * (The four-bit values occupy the low halves of the bytes in the
 * result).
 */
static uint32_t*
WK_unpack_4bits(uint32_t *input_buf,
                uint32_t *input_end,
                uint32_t *output_buf) {

  register uint32_t *input_next = input_buf;
  register uint32_t *output_next = output_buf;
  register uint32_t packing_mask = FOUR_BITS_PACKING_MASK;
  
  
  /* loop to repeatedly grab one input word and unpack it into
   * 4 output words.  This loop should probably be unrolled
   * a little---it's designed to be easy to do that.
   */   
  while (input_next < input_end) {
    register uint32_t temp = input_next[0];
    DEBUG_PRINT_2("Unpacked dictionary indices word: %.8x\n", temp);
    output_next[0] = temp & packing_mask;
    output_next[1] = (temp >> 4) & packing_mask;
    
    output_next += 2;
    input_next++;
  }
  
  return output_next;

}

/* unpack_3_tenbits unpacks three 10-bit items from (the low 30 bits of)
 * a 32-bit word
 */
static uint32_t*
WK_unpack_3_tenbits(uint32_t *input_buf,
                    uint32_t *input_end,
                    uint32_t *output_buf) {

  register uint32_t *input_next = input_buf;
  register uint32_t *output_next = output_buf;
  register uint32_t packing_mask = LOW_BITS_MASK;
  
  /* loop to fetch words of input, splitting each into three
   * words of output with 10 meaningful low bits.  This loop
   * probably ought to be unrolled and maybe coiled
   */
  while (input_next < input_end) {
    register uint32_t temp = input_next[0];
    
    output_next[0] = temp & packing_mask;
    output_next[1] = (temp >> 10) & packing_mask;
    output_next[2] = temp >> 20;
    
    input_next++;
    output_next += 3;
  }
  
  return output_next;

}

/*********************************************************************
 * WKdm_decompress --- THE DECOMPRESSOR                                 
 * Expects WORD pointers to the source and destination buffers
 * and a page size in words.  The page size had better be 1024 unless     
 * somebody finds the places that are dependent on the page size and 
 * fixes them
 */

void
WKdm_decompress (uint32_t* src_buf,
		 uint32_t* dest_buf,
		 uint32_t words)
{

  uint32_t dictionary[DICTIONARY_SIZE];

  /* arrays that hold output data in intermediate form during modeling */
  /* and whose contents are packed into the actual output after modeling */

  /* sizes of these arrays should be increased if you want to compress
   * pages larger than 4KB
   */
  uint32_t tempTagsArray[300];        /* tags for everything          */
  uint32_t tempQPosArray[300];        /* queue positions for matches  */
  uint32_t tempLowBitsArray[1200];    /* low bits for partial matches */

  PRELOAD_DICTIONARY;

#ifdef WK_DEBUG
  printf("\nIn DECOMPRESSOR\n");
  printf("tempTagsArray is at %p\n", tempTagsArray);
  printf("tempQPosArray is at %p\n", tempQPosArray);
  printf("tempLowBitsArray is at %p\n", tempLowBitsArray);

  printf(" first four words of source buffer are:\n");
  printf("   %u\n   %u\n   %u\n   %u\n",
         src_buf[0], src_buf[1], src_buf[2], src_buf[3]);
  
  { int i;
    uint32_t *arr =(src_buf + TAGS_AREA_OFFSET + (PAGE_SIZE_IN_WORDS / 16));

    printf("  first 20 full patterns are: \n");
    for (i = 0; i < 20; i++) {
      printf(" %d", arr[i]);
    }
    printf("\n");
  }
#endif

  WK_unpack_2bits(TAGS_AREA_START(src_buf),
                  TAGS_AREA_END(src_buf),
                  tempTagsArray);

#ifdef WK_DEBUG
  { int i;
    char* arr = (char *) tempTagsArray;

    printf("  first 200 tags are: \n");
    for (i = 0; i < 200; i++) {
      printf(" %d", arr[i]);
    }
    printf("\n");
  }
#endif

  WK_unpack_4bits(QPOS_AREA_START(src_buf),
                  QPOS_AREA_END(src_buf),
                  tempQPosArray);

#ifdef WK_DEBUG
  { int i;
    char* arr = (char *) tempQPosArray;

    printf("  first 200 queue positions are: \n");
    for (i = 0; i < 200; i++) {
      printf(" %d", arr[i]);
    }
    printf("\n");
  }
#endif

  WK_unpack_3_tenbits(LOW_BITS_AREA_START(src_buf),
                      LOW_BITS_AREA_END(src_buf),
                      tempLowBitsArray);

#ifdef WK_DEBUG
  printf("AFTER UNPACKING, about to enter main block \n");
#endif

  {
    register char *next_tag = (char *) tempTagsArray;
    char *tags_area_end =
       ((char *) tempTagsArray) + PAGE_SIZE_IN_WORDS;
    char *next_q_pos = (char *) tempQPosArray;
    uint32_t *next_low_bits = tempLowBitsArray;
    uint32_t *next_full_word = FULL_WORD_AREA_START(src_buf);

    uint32_t *next_output = dest_buf;

#ifdef WK_DEBUG
    printf("next_output is %u\n", next_output);

    printf("next_tag is %u \n", next_tag);
    printf("tags_area_end is %u\n", tags_area_end);
    printf("next_q_pos is %u\n", next_q_pos);
    printf("next_low_bits is %u\n", next_low_bits);
    printf("next_full_word is %u\n", next_full_word);
#endif 

    /* this loop should probably be unrolled. Maybe we should unpack
     * as 4 bit values, giving two consecutive tags, and switch on
     * that 16 ways to decompress 2 words at a whack
     */
    while (next_tag < tags_area_end) {

       char tag = next_tag[0];

       switch(tag) {

         case ZERO_TAG: {
            *next_output = 0;
            break;
         }
         case EXACT_TAG: {
            uint32_t *dict_location = dictionary + *(next_q_pos++);
            /* no need to replace dict. entry if matched exactly */
            *next_output = *dict_location;
            break;
         }
         case PARTIAL_TAG: {
            uint32_t *dict_location = dictionary + *(next_q_pos++);
            {
               uint32_t temp = *dict_location;

               /* strip out low bits */
               temp = ((temp >> NUM_LOW_BITS) << NUM_LOW_BITS);

               /* add in stored low bits from temp array */
               temp = temp | *(next_low_bits++);

               *dict_location = temp;      /* replace old value in dict. */
               *next_output = temp;    /* and echo it to output */
            }
            break;
         }
         case MISS_TAG: {
            uint32_t missed_word = *(next_full_word++);
            uint32_t *dict_location = 
              (uint32_t *)
              (((char *) dictionary) + HASH_TO_DICT_BYTE_OFFSET(missed_word));
            *dict_location = missed_word;
            *next_output = missed_word;
            break;
         }
       }
       next_tag++;
       next_output++;
    }

#ifdef WK_DEBUG        
    printf("AFTER DECOMPRESSING\n");
    printf("next_output is %p\n", next_output);
    printf("next_tag is %p\n", next_tag);
    printf("next_full_word is %p\n", next_full_word);
    printf("next_q_pos is %p\n", next_q_pos);
#endif
  }
}


