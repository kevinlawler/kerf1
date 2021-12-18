#include "kerf.h"

//dictionary or hashtable
//for a hashset: map keys to (C)1
//for a hashbag: map keys to (I)1 or (F)1, fast increment via lookupI
//for a sparse array: use keys from (I); cast (F) or (C) key lookups to (I)
//for an array: use an array
//for a sorted set: use a set

//IKV map/dictionary: 5-type with MAP_SIZE K0 objects
//Keys: A vector with appends at the end (actually any K object)
//Values: Same as Keys. Should have same count or be conformable (atom/1-list)
//Index: A -INT vector with a reserved value fill (0N)
//       index->n counts populated keys, space used is really next power of 2
//       densehash as numeric index w/ robin hooding
//       open addressing, 0.5 load factor
//       quadratic probing w/ triangular numbers 
//       implicit distance via hensel lifting

#pragma mark - Hash Function

// Fast-hash Copyright 2012 Zilong Tan (eric.zltan@gmail.com) - MIT License
// Compression function for Merkle-Damgard construction.
// This function is generated using the framework provided.
#define mix(h) ({          \
      (h) ^= (h) >> 23;    \
      (h) *= 0x2127599bf4325c37ULL;  \
      (h) ^= (h) >> 47; })

uint64_t fasthash64(const void *buf, size_t len, uint64_t seed)
{
  const uint64_t    m = 0x880355f21e6d1965ULL;
  const uint64_t *pos = (const uint64_t *)buf;
  const uint64_t *end = pos + (len / 8);
  const unsigned char *pos2;
  uint64_t h = seed ^ (len * m);
  uint64_t v;

  while (pos != end) {
    v  = *pos++;
    h ^= mix(v);
    h *= m;
  }

  pos2 = (const unsigned char*)pos;
  v = 0;

  switch (len & 7) {
  case 7: v ^= (uint64_t)pos2[6] << 48;
  case 6: v ^= (uint64_t)pos2[5] << 40;
  case 5: v ^= (uint64_t)pos2[4] << 32;
  case 4: v ^= (uint64_t)pos2[3] << 24;
  case 3: v ^= (uint64_t)pos2[2] << 16;
  case 2: v ^= (uint64_t)pos2[1] << 8;
  case 1: v ^= (uint64_t)pos2[0];
    h ^= mix(v);
    h *= m;
  }

  return mix(h);
} 

#undef mix

#pragma mark -

uint64_t HASH_KEY;

void hash_init()
{
  I key_bytes = sizeof(HASH_KEY);

  if(HASH_USES_RANDOMIZED_SEED_ON_INIT)
  {
    I *k = (void *)&HASH_KEY;
    DO(key_bytes/sizeof(I), k[i] = urandomBits())
  }
  else
  {
    UC *k = (void *)&HASH_KEY;
    DO(key_bytes, k[i] = (UC)((i<<4)|i))   //0x0011223344556677...
  }

  //O("Hash key value: ");UC *k=(V)&HASH_KEY; DO(key_bytes, printf("%02x",k[i])) O("\n");
}

uint64_t hash_bytes(void *in, uint64_t length)
{
  uint64_t out;
 
  out = fasthash64(in, length, HASH_KEY);
  
  return out;
}

uint64_t hash_byte_stream(void *in, uint64_t length, uint64_t seed)
{
  uint64_t out;
 
  out = fasthash64(in, length, seed);
  
  return out;
}

#pragma mark -

uint64_t hash_K_stream(K x, uint64_t base)
{
  if(IS_DEFINED(DEBUG))
  {
    if(!x) er(Warning: hashing null object.)
  }


  bool avoidO1Collisions = false;
  if(avoidO1Collisions)//slower, avoid O(1) collisions on type by hashing data type first
  {
    base = hash_byte_stream(&xt,sizeof(xt),base);
  }

  uint64_t h = base;

  SW(xt)
  {
     CS( NIL,  S s="_n"; h=hash_byte_stream(s,strlen(s),h))
    CSF(BTREE,)
    CSF(HASH,)
     CS( LIST, ENUM(x, h=hash_K_stream(v,h)))
     CS( MAP, if(HASH_MODS_OUT_DICTIONARY_ORDER) ENUM(x, h += hash_K_stream(v,hash_K_stream(u,base))) 
              else h=hash_K_stream(xValues, hash_K_stream(xKeys, h));)
    CSF(TABLE,)
    CSF(ATLAS,)
    CSF(PARTABLE,)
    CSF(DATABASE,)
     CS( FUNC,  //probably OK? (not too many collisions? ok to let KC() handle if we really need to distinguish?)
                //too slow for sql funcs (300ms): NEST(x, h=hash_K_stream(v,h))
                h = hash_K_stream(kN(x,FUNC_BYTECODE), h)) 
     CD:        h=hash_byte_stream(&xc,any_size_K(x)-offsetof(K0,c),h);
  }

  return h;
}

uint64_t hash(K x)
{
  return hash_K_stream(x, HASH_KEY);
}

#pragma mark - Hashtable

#pragma mark - Lookup (Read)

I lookupI(K map, K key)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //increase this value
  I small = 100;
  bool linearSearchInSmallCases = true;

  if(linearSearchInSmallCases)
  {
    K keys = kN(map,KEYS);
  
    if(lenI(keys) <= small)
    {
      ENUM(keys, if(matchC(v,key,false)) return i)
      return HASH_NULL;
    }
  }

  I distance = 0;
  
  uint64_t key_hash = hash(key);

  I h = find_index_index(map, key_hash, key, &distance);

  K index = kN(map,INDEX);

  I p = kI(index)[h]; 

  if(IS_HASH_NULL(p))
  {

  }

  return p;
}

K map_lookup(K map, K list)
{
  K x = map;

  K0 o;
  ENUM(list, if(!x) return NULL; x = LOOKUP_(x, v, o);)

  if(x) return strong(x);

  return NULL;
}

#pragma mark - Insert (Write)

I find_index_index(K map, uint64_t key_hash, K key, I *probe_distance)
{
  K x = map;

  K index = xIndex;
  K keys  = xKeys;

  I u = derived_height_I(index);
  I buckets = POW2(u);
  I b = buckets - 1;

  uint64_t h = key_hash, t = 0;
  I p = HASH_NULL, k = 0;

  *probe_distance = 0;

  do
  {
    k = *probe_distance;
    t = (k*(k+1))>>1;
    h = ((key_hash&b) + (t&b))&b;

    p = kI(index)[h]; 
  
    if(IS_HASH_NULL(p))
    {
      return h;
    }

    K0 o;
    if(matchC(key,LOOK_(keys,p,o),false))
    {
      return h;
    }

    (*probe_distance)++;
  }
  while((*probe_distance)<II);

  return h;
}

I distance(K map, uint64_t key_hash, I eventual)
{
  bool TRY_EXHAUST_METHOD = true; 
  I first_exhaust_count = 10; 
  bool TRY_SQROOT_METHOD = true;//primarily informative

  K x = kN(map,INDEX);
  I h = derived_height_I(x);
  I buckets = (1ULL<<h);
  I b = buckets - 1;

  I initial = (key_hash & b);

  I d = eventual - initial;
  if(d<0) {d = (d+b+1)&b;}

  //The next step would be caching, but I think
  //already it doesn't make a difference...
  //You can either cache the small Hensel lifts,
  //or cache the small exhausts
  //
  //Hensel lifting is more effective than brute force starting around 2^12 buckets
  //With robin hooding, a table with 2^p buckets generally has max distance <<p
  if(TRY_EXHAUST_METHOD)
  {
    DO(first_exhaust_count, I k = ((i*(i+1))>>1); if(d==(k&b))R i;)
  }

  //Optionally check square roots
  if(TRY_SQROOT_METHOD)
  {
    I k = sqrt(2.0*d);
    if(d==((k*(k+1))>>1)) R k;
  }

  //Finally compute Hensel liftings
  return hensel(d,h);
}

void robin_hood_insert(K map, uint64_t key_hash, I key_i, I original_distance)
{
  K index = kN(map,INDEX);
  K keys  = kN(map,KEYS);

  I u = derived_height_I(index);
  I buckets = POW2(u);
  I b = buckets - 1;

  uint64_t h = key_hash, t = 0;
  I p = HASH_NULL;
  I k = 0;

  do
  {
    t = (k*(k+1))>>1;
    h = ((key_hash&b) + (t&b))&b;
    p = kI(index)[h]; 
  
    if(IS_HASH_NULL(p))//empty, so we're at the end - done
    {
      kI(index)[h] = key_i;
      index->n++;

      //O("put %lld\n", key_i); 
      return;
    }

    I stored_distance = 0;

    K0 o;
    K v = LOOK_(keys, p,o); 

    uint64_t stored_hash = hash(v); 
  
    I stored_distance_computational = 0;
    I blah = 0;

    I stored_distance_analytical = distance(map, stored_hash, h);
    stored_distance = stored_distance_analytical;

//    I unused = find_index_index(map, stored_hash, x, &stored_distance_computational);
//    stored_distance=stored_distance_computational;

//    if(stored_distance_computational != stored_distance_analytical)
//    {
//      dd(kI(keys)[p])
//      er(rh distance mismatch)
//      exit(1);
//    }

    if(original_distance >= stored_distance)
    {
      //swap
      I stored_i = kI(index)[h];
      kI(index)[h] = key_i;

      key_hash         =stored_hash;
      key_i            =stored_i; 
      original_distance=stored_distance;
      k                =stored_distance;
    }
    //keep looking 
    
 
    k++;
  }
  while(k<II);

  return ;
}

char map_indexer(K map, I k)//basically, insert without growing or replacing or c.o.w.
{
  bool INDEXES_FIRST = true;
  bool INDEXES_LAST = !INDEXES_FIRST;

  K index = kN(map,INDEX);
  K keys  = kN(map,KEYS);

  K0 o;
  K z = LOOK_(keys, k, o);
  uint64_t zhash = hash(z);

  I distance = 0;

  I h = find_index_index(map, zhash, z, &distance);

  I p = kI(index)[h];

  if(IS_HASH_NULL(p))
  {
    if(HASH_ROBIN_HOODS)
    {
      robin_hood_insert(map, zhash, k, distance);
      return 1;//new element
    }
    else
    {
      kI(index)[h] = k;
      index->n++;
      return 1;//new element
    }
  }

  if (INDEXES_LAST)
  {
    if(HASH_ROBIN_HOODS)
    {
      robin_hood_insert(map, zhash, k, distance);
      return 0;//collision
    }
    else
    {
      kI(index)[h] = k;
      index->n++;
      return 0;//collision
    }
  }

  return 0;//collision
}

void hash_grow_index(K map)
{
  K x = kN(map, INDEX); 

  I h = derived_height_I(x);
  I xbuckets = POW2(h);
  I ybuckets = (xbuckets << 1);

  if(IS_DISK(map))
  {
    I m = ceiling_log_2(list_size_k(-INT, ybuckets));
    cow_expand(x, m);
  }
  else
  {
    K y = new_k(-INT, ybuckets);
    nestset_ri_rd(map, INDEX, y, false, true);
  }

  SET_ATTR(kN(map, INDEX), ATTR_FILLED);
  OFF_ATTR(kN(map, INDEX), ATTR_SORTED);
  fill_list_payload_I(kN(map, INDEX), HASH_NULL);

  kN(map, INDEX)->n = 0;
  ENUM(kN(map, KEYS), map_indexer(map, i)) 

  return;
}

I insert_replace(K z, K key, K value, I replaces, I hashset)//write
{
  return insert_replace_ri_rd(z, key, value, replaces, hashset, true, true);
}

I insert_replace_ri_rd(K z, K key, K value, I replaces, I hashset, bool increment, bool decrement)
{
  I distance = 0;

  uint64_t key_hash = hash(key);

  I h = find_index_index(z, key_hash, key, &distance);

  I p = kI(zIndex)[h]; 

  //key exists
  if(!IS_HASH_NULL(p))
  {
    if(replaces && !hashset) //same key: replaces/overwrites/updates/alters
    {
      K0 o; 
      K v = LOOK_(zValues,p,o);

      if(IS_CREFED(v) && IS_CREFED(z)) //added z check b/c 'group by' was triggering???
      {
        //error check: overwriting compile-ref'd dictionary
        //catch anywhere upsert or insert_replace is used
        //or longjmp

        ERROR(ERROR_REFERENCE);
        return HASH_NULL;
      }

      assert(CAN_WRITE(z));
      //nestneu(z, VALUES, cow(zValues));//not necessary if update handles cow
                                        //2015.07.14 maybe so, but UPDATE isn't handling it & this caused an assert bug, so ...
                                        //           actually, we're trying this in update now....
      nestneu(z, VALUES, update_ri_rd(zValues, ki(p), value, increment, decrement));

      //demote(values);
      //you can avoid the O(n) demoting here if you are careful elsewhere about demoting
      //e.g. in K values(K x);
      //on the other hand, this will cause the wrong
      //dictionary null "missing index value" to be returned
      //0-type null instead of whatever vector
      //unless you demote values on missing key ... 
      //it also stymies trying to use _n as missing value
    }

    return p;
  }

  //key absent
  nestneu(z,KEYS,cow_add(zKeys, key));

  if(!hashset) nestneu(z,VALUES,cow_add(zValues, value));//TODO: bug? does not respect increment rule? see "group()"
  //if(!hashset)
  //{
  //  nestneu(z,VALUES,cow_add(zValues, value));
  //  if(!increment) rd(value);
  //}


  p = -1 + zKeys->n;


  if(HASH_ROBIN_HOODS)
  {
    nestneu(z,INDEX,cow(zIndex));
    robin_hood_insert(z, key_hash, p, distance);
  }
  else
  {
    nestneu(z,INDEX,cow(zIndex));
    K x = zIndex;
    xI[h] = p;
    xn++;
  }

  K x = zIndex;
  I u = derived_height_I(x);
  I buckets = POW2(u);
  I dense = (xn >= buckets*HASH_LOAD_FACTOR);//>= lets you use load factor 1

  if(dense)
  {
    hash_grow_index(z);
  }

  return p;
}

K cow_reindex(K x)
{
  x = cow(x);
  nestneu(x,INDEX,cow(xIndex));
  reindex(x);
  return x;
}

void reindex(K x)
{
  fill_list_payload_I(xIndex, HASH_NULL);
  xIndex->n = 0;
  DO(xKeys->n, map_indexer(x,i))
}

//lame: Θ(n) in general, but O(1) for columns
//adding DELETE() to hashtable would give O(1) index update,
//O(1) rename whenever key array alter is O(1)
char renameKey(K x, K keyOld, K keyNew)//for rename column on disk
{
  I k = lookupI(x, keyOld); 
  if(IS_HASH_NULL(k)) return 1;//old key DNE

  I j = lookupI(x, keyNew);
  if(!IS_HASH_NULL(j)) return 1;//new key E

  nestneu(x, KEYS, update(xKeys, ki(k), keyNew));

  assert(CAN_WRITE(x));
  assert(CAN_WRITE(kN(x,INDEX)));

  reindex(x);
  return 0;
}

#pragma mark -

//"Algorithm 3 Explicit Quadratic Modular inverse modulo 2^m"
//in Dumas - "On Newton-Raphson iteration for multiplicative inverses modulo prime powers"
I fast_invert2(I x, I m, I n)//n=-1+2^m; x^-1 = x^{phi(n)-1} mod 2^m
{
  //not currently tweaked to handle 1==x
  I y = x-1;
  I z = 2-x;

#if __has_builtin(__builtin_ctzll)
  I s = __builtin_ctzll(y);
#else
  x=x-1;
  I s = 0;
  for(;!(x&1); x>>=1, s++);
#endif

  I i;

  for (i = 1; i < m/s; i<<=1)
  {
    z=(z*(1+(y=((y*y)&n))))&n;
  }
  return z;
}

I fast_invert1(I x, I m, I n)//n=-1+2^m; x^-1 = x^{phi(2^m)-1} mod 2^m
{
  uint64_t a = x;
  //char l = MAX(3, floor_log_2(m));
  //char l = 1;
  char l = 3;
  DO(m - l, a = ((((a*a)&n)*x)&n))
  R a;
}

I lift(I c, I r, I k, I m)
{
  if (m>k)
  {
    er(bad m or k)
    R -1;
  }

  I t = 0;
  I s = 0;

  I pk = POW2(k); 
  I pm = POW2(m);
  I pmk = POW2(m+k);

  I fr = (r*r + r) - 2*c;

  I fpr = (2*r + 1)&(pm-1);

  I fpri = 1;
  
  if(1!=fpr) 
  {
    if (m<=9) fpri = fast_invert1(fpr, m, pm-1); 
    else fpri = fast_invert2(fpr, m, pm-1); 
  }
  
  //I inv = (fpri * fpr)&(pm-1);
  //if (inv!=1){ er(bad inverse)dd(m)dd(fpr)dd(fpri)   exit(0);}

  t = -(fr/pk);
  if (t < 0) {t+=pm;}
  t =  t * fpri;
  t = t&(pm-1);

  s = (r + pk * t)&(pmk-1);

  return s;
}

I multilift(I c, I endk, I r)
{
  I k = 1;

  I s = r;

  while (k < endk)
  {
    I m = MIN(k, endk-k);
    s = lift(c,s,k,m);
    k+=m;
  }

  return s;
}

I hensel(I c, I p)
{
  I b = POW2(p)-1;
  
  I s0 = multilift(c,p,0);
  I k0 = (s0*(s0+1)/2)&b;
 
  if (c==k0)
  {
    return s0;
  }

  I s1 = multilift(c,p,1);
  I k1 = (s1*(s1+1)/2)&b;

  if(c==k1){
    return s1;
  }

O("bogus: c:%lld p:%lld s0:%lld s1:%lld\n", c, p, s0, s1);
dd(s0)
dd(k0)
dd(s1)
dd(k1)
exit(0);

  return -1;
}

#pragma mark - Map/Hashset
K new_map_from_K_K(K x, K y, I hashset, I traits)
{
  bool ENFORCES_DISTINCT = true;
  
  if(!hashset && cx != cy) R NULL;

  I n = cx;

  I size = traits?TABLE_SIZE:MAP_SIZE;

  K z = new_k(LIST,size);
  z->n = 0;

  I capacity =1+(1.0/HASH_LOAD_FACTOR)*n;//initial capacity

  I buckets = round_up_nearest_power_of_2(capacity);

  K index = new_k(-INT, buckets);
  SET_ATTR(index, ATTR_FILLED);
  OFF_ATTR(index, ATTR_SORTED);
  fill_list_payload_I(index, HASH_NULL);
  index->n = 0;//no slots in use yet

  K keys   = x;
  K values = hashset?ki(IN):y;

  //could also do 1 2 3!0 is  (1:0 2:0 3:0)

  z = cow_add(z, index);
  z = cow_add(z, keys);
  z = cow_add(z, values);

  rd(index); 

  //in this context what we want coerce array to do is FORCE a VECTOR or LIST
  //but not anything else

  //nestneu doesn't work correctly if either of these begins life as an atom (non-LINK)
  nestset_ri_rd(z, KEYS,   cow_coerce_vlist(zKeys), false, false);
  nestset_ri_rd(z, VALUES, cow_coerce_vlist(zValues), false, false);

  if(traits)
  {
    //MAP is a better choice since LIST indices
    //are not forward compatible
    K k = new_map();
    z = cow_add(z, k);
    rd(k);
  }

  z->t = MAP;
  z->n = 0;
  z->nn = size;
  zw = 0; //dictionary compiled-reference-count
  OFF_ATTR(z, ATTR_SORTED);
  if(hashset) SET_ALT_ATTR(z, MAP_ATTR_HASHSET);

  bool distinct = 1;
  DO(n, distinct &= map_indexer(z,i))

  if(!distinct && ENFORCES_DISTINCT)
  {
    //create unique keys & unique values
    //(preferably maintaining key order)
    //then
    //recurse to create new index. rd(z)
    //xn == distinct key count
    K ukeys   = Kk();
    K uvalues = Kk();
    I d = 0;

    //maintains key order
    ENUM(keys, 
      uint64_t vhash = hash(v);
      I h = find_index_index(z, vhash, v, &d);
      I p = kI(index)[h];
      if(IS_HASH_NULL(p)) continue;
      //POTENTIAL_OPTIMIZATION_POINT - slow adds?
      //xn == distinct key count
      //but you'd have to scan to get type of resultant lists 
      //it's not that bad -- see optimized demote
      K0 o1,o2;
                   ukeys = cow_add(ukeys,LOOK_(keys,p,o1));//same as add(&ukeys, x);
      if(!hashset) uvalues = cow_add(uvalues,LOOK_(values,p,o2));
      kI(index)[h]=HASH_NULL;//ruins existing index
    )

    K unique = new_map_from_K_K(ukeys, uvalues, hashset, traits);//reindex
    rd(ukeys);
    rd(uvalues);
    rd(z);
    return unique;
  }

  return z; 
}


#pragma mark - 

K cow_intern_add(K x, K y)
{
  x=cow(x);
  nestneu(x,INDEX,cow(xIndex));
  I p = insert_replace(xIndex, y, 0, false, true);
  nestneu(x,KEYS,cow_add(xKeys,ki(p))); 

  return x;
}

K _alter_intern_single(K x, I k, K y)
{
  nestneu(x,INDEX,cow(xIndex));
  I p = insert_replace(xIndex, y, 0, false, true);
  //nestneu(x,KEYS,alter_array_single(xKeys,k,ki(p))); 
  nestneu(x,KEYS, update(xKeys,ki(k),ki(p))); 
  return x;
}

int hashtable_go()
{
  I n = POW2(6);
  K x = til(Ki(n));
  K y = til(Ki(n));
  K z;

  DO(xn, I r = rI(); xI[i] = ABS(r) % 3);

  z = new_map_from_K_K(x,y,0,false);
  //TIME(z = new_map_from_K_K(x,ki(IN),true,false))


  //z = new_intern();
  //TIME(ENUM(x, z=cow_add(z, v)))
  //show(zIndex);
  //show(zKeys);



return 0;
//volatile I hx = 0;
//volatile I dx = 0;
//volatile I sx = 0;
//
//I p = 10;
//I check = 4 & ((1ULL<<p)-1);
//
//TIME(hx = hensel(check,p))
//TIME(I k = sqrt(2.0*check); sx = (check == ((k*(k+1))>>1)))
//return 0;

//volatile I a;
//volatile I b;
//I c = 100;
//I m = 10;
//I nn = (1ULL<<m)-1;
//
//TIME(DO(c, a = fast_invert1(i*2+3,m,nn)))
//TIME(DO(c, a = fast_invert2(i*2+3,m,nn)))
//
//  I mp = 20;
//
//  F bad = 0;
//  F total = 0;
//
//  TIME(
//  DO(mp, I p = i+1;
//    I b = (1LL<<p)-1;
//    DO2(b+1,
//
//     total++;
//
//     I e = j;
//     I f = (j*(j+1)/2)&b;
//     volatile I h = hensel(f,p);
//
//     if (e!=h)
//     {
//       bad++;
//       O("bad: %lld check != %lld hensel @ (%lld, %lld)\n\n", e, h, j, p);
//     }
//    
//    )
//  )
//  )
//
//  O("%s: %.0f/%.0f = %.1f%% \n", bad>0?"Failed":"Pass",total-bad,total,100*((total-bad)/total));
//  
//return 0;



//x0. KVI format - keys values index

//x0. Don't "unique-ify" incoming key or value lists/vectors (mmap)
//x0. alloc
//x0. initial capacity
//x0. load factor 0.5 
//x0. all keys
//x0. all values
//x0. lookup
//x0. linear lookup
//x0. insert
//x0. insert-replace (free old value)
//x0. grow/rehash
//x0. hash set (return value at value_index*is_hash_set ?)
//x0. all unique keys
//x0. all uniquely keyed values
//x0. generalized append
//x0. robin hood
//x0. crossover for linear lookups versus hash lookups?
//x0. timings. timings versus tree ( 2^20 random elements )
//    CO-BFJ-VEB tree: 20s inserting sorted 
//    tree: 2.00s inserting random from scratch
//    tree: 0.39s using presort (merely creating BTREE)
//    hash: 0.19s using hashset

//  K x = Kc('x');
//  K y = Ki(0xffffffffffffffffULL);
//  yt = 3;
//  yc = 'x';
//  *((&yc)+2) = 'b';

//  I n = 1LL<<20;
//  K x = til(Ki(n));
//  K y = til(Ki(n));
//  I keymax = 1ULL<<10;
//  K x2 = new_k(-1,n); DO(n, kI(x2)[i]=3333);
//  K x3 = new_k(-1,n); DO(n, kI(x3)[i]=rI());
//  K y3 = new_k(-1,n); DO(n, kI(y3)[i]=rI());
//  K z;
//
//  TIME(
//  z = new_map_from_K_K(x,y);
//  )
//
//  //z = new_map(); 
//
//  K key = Ki(0);
//  K val = Ki(1);
//
//  TIME(DO(n, if(0&&0==(i&((1LL<<16)-1)))dd(i)key->i=kI(x3)[i]; val->i=kI(y3)[i]; insert_replace(z, key,val,0); ))
//
//  er(insertions done)
//
//  K index = kK(z)[INDEX];
//  K keys =  kK(z)[KEYS];
//   
//  F avg=0;
//  
//  I d = 0;
//  I maxp = 0;
//  ENUM(keys, find_index_index(z,hash(v),v,&d); avg+=d;  if(d>maxp)maxp=d;)
//  avg/=keys->n;
//
//  F stdev = 0;
//
//  ENUM(keys,  find_index_index(z,hash(v),v,&d); stdev += pow(avg-d, 2);  )
//
//  F var = stdev/keys->n;
//  stdev = sqrt(var);
//
//  printf("avg  : %.3f\n", avg);
//  printf("stdev: %.3f\n", stdev);
//  printf("var  : %.3f\n", var);
//  printf("max  : %lld\n", maxp);
//  printf("robin: %s\n",   HASH_ROBIN_HOODS?"true":"false");

  return 0;
}


