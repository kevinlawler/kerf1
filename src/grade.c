#include "kerf.h"

// x 0. test height bounds, reachable
// x 1. insert node
// x 2. veb+avl insertion - skip
// x 3  reduce to 2*sizeof(I) sized indices
// x 4. find node-index (lowest, highest) / range
// x 5. find plus stack
// x 6. delete node (depends on find). check sane balance 
// x    --balance verifier, node-gone-verifier, n-1-reachable-verifier. bfactor for inserts too
// x 7. alter
// x    --sort test is not strong enough (needs "in-order" checking, for left's right sibling left child  5 !< 4.9 < 6 )
// x 8. landis_can_add 
// x 9. landis_has_element
// x10. optimized min/max
// x11. pre-whered
// x12. pre-sorting (we can build temp table and grade table) at most 2-3x gain now? 
//-----------------
// x0. atlas - handle nested via something like marshal/unmarshal flat b.c, b.d in {a:1, b:{c:2, d:3}}
// x0. atlas pre-whered (=symbol intersect symbol?val)  row compare uses MIN count values?
//      --intersect node-index-range <-- note: this has nice properties we can exploit
//-----------------
//   0. merge avl (joining lists)

#pragma mark - General Grade Methods

ALWAYS_INLINE char FC(F a, F b)
{
  F E=0.00000000000000000001; //also suggested: small multiple of DBL_EPSILON

  if(FLOAT_NANS_COMPARE_AS_SMALLEST)
  {
    if(isnan(a))
    {
      if(isnan(b)) return EQUAL;
      return ASCENDING;
    }
    else if(isnan(b))
    {
      return DESCENDING;
    }
  }
  
  if(isinf(a))
  {
    if(isinf(b))
    {
      R ((a<0 && b<0) || (a>0 && b>0))?0:(a<0 && b>0)?ASCENDING:DESCENDING;
    }
    R a<0?ASCENDING:DESCENDING;
  }
  else if(isinf(b))
  {
    R b>0?ASCENDING:DESCENDING;
  }

  if(ABS(a-b) <= E*MAX(ABS(a),ABS(b)))R EQUAL;
  R a<b?ASCENDING:DESCENDING;
}

char FC_ulps(F a, F b) //Floating-Point Compare
{
  //POTENTIAL_OPTIMIZATION_POINT
  //if this method can get rid of the NAN if-branch
  //it'll be a lot faster than our regular method.
  //if not, it's the seem speed and nonstandard so junk it

  //See: Bruce Dawson
  //http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
  //ftp://ftp.cygnus-software.com/pub/comparecode.zip

  //See wellOrderSignedFloat for a method we may prefer to use
 
  //ALWAYS_INLINE bool IsInfinite(F A)
  //{
  //    const kInfAsInt = 0x7F80000000000000;
  //
  //    // An infinity has an exponent of 255 (shift left 23 positions) and
  //    // a zero mantissa. There are two infinities - positive and negative.
  //    if ((*(I*)&A & 0x7FFFFFFFFFFFFFFF) == kInfAsInt)
  //        return true;
  //    return false;
  //}
  //
  
  //
  //ALWAYS_INLINE int Sign(F A)
  //{
  //    // The sign bit of a number is the high bit.
  //    return (*(I*)&A) & 0x80000000;
  //}

  //ALWAYS_INLINE bool IsNan(F A)
  //{
  //  // A NAN has an exponent of 255 and a non-zero mantissa.
  //  I exp = *(I*)&A & 0x7F80000000000000;
  //  I mantissa = *(I*)&A & 0x007FFFFFFFFFFFFF;
  //  if (exp == 0x7F80000000000000 && mantissa != 0) return true;
  //  return false;
  //}

  int maxUlps = 100;
  I c,d;

  assert(sizeof(F)==sizeof(I));
  assert(8==sizeof(F));

  //1. there is probably a fast nan int mask thing here
  //2. or just ADD to whatever int nan is to roll it around
  //3. there may be a bug in checking max ulps
  //4. ***now that the inf check uses isnan we can munge the nan ints here**

  if(FLOAT_NANS_COMPARE_AS_SMALLEST)
  {
   //4. this if is slow vvvvvv
    if(isnan(a))
    {
      if(isnan(b)) return EQUAL;
      return ASCENDING;
    }
    else if(isnan(b))
    {
      return DESCENDING;
    }
  }
  
  if(FLOAT_INF_IS_FAR_FROM_FLT_MAX) //we can get rid of this if we want
  {
    if(isinf(a))
    {
      if(isinf(b))
      {
        return ((a<0 && b<0) || (a>0 && b>0))?EQUAL:(a<0 && b>0)?ASCENDING:DESCENDING;
      }
      return (isnan(b)||a>0)?DESCENDING:ASCENDING;
    }
    else if(isinf(b))
    {
      return (isnan(a)||b>0)?ASCENDING:DESCENDING;
    }
  }

  //optional
  //  if(FLOAT_NEGATIVE_IS_FAR_FROM_POSITIVE)
  //  {
  //    if(Sign(A)!=Sign(B)return A==B;
  //
  //  }

  // Make each of a,b lexicographically ordered as a twos-complement int
  c= *(I*)&a; if (c<0) c = 0x8000000000000000 - c;
  d= *(I*)&b; if (d<0) d = 0x8000000000000000 - d;

  I diff = llabs(c - d);
  if (diff <= maxUlps) return EQUAL;
  return a<b?ASCENDING:DESCENDING;
}

ALWAYS_INLINE char IC(I i, I j)
{
  if(i<j)return ASCENDING;
  if(i>j)return DESCENDING;
  return 0;
}

char SC(S a, S b)//strcmp unfortunately does not draw from {-1,0,1}
{
  int x=strcmp(a,b);
  R x<0?ASCENDING:x>0?DESCENDING:EQUAL;
}

char lexicographic(K x, K y)
{
  I n = MIN(xn, yn); 

  //POTENTIAL_OPTIMIZATION_POINT
  //*maybe compressing this into one pass is faster?
  //*maybe find some workaround to avoid toupper function calls
  DO(n, C a = toupper(xC[i]), b = toupper(yC[i]); if(a!=b) return IC((UC)a,(UC)b);)

  I on_length = IC(xn, yn);
  if(on_length) return on_length;

  DO(n, C a = xC[i], b = yC[i]; if(a!=b) return IC((UC)a,(UC)b);)

  return EQUAL;
}

char LC(K x, K y) //Landis's Compare
{
  return KC_NUM(x,y);
}

ALWAYS_INLINE C AC(K x, K y)
{
  SW(xt)
  {
   CSF(STAMP, )
    CS(INT,   return IC(xi,yi))
    CS(FLOAT, return FC(xf,yf))
    CS(CHAR,  return IC(xc,yc))
  }

  return EQUAL;
}

char KC_NUM(K x, K y) //Numeric INT<->FLOAT compatibility
{
  if(INT==xt && FLOAT==yt)
  {
    return FC(xi,yf);
  }
  else if(FLOAT==xt && INT==yt)
  {
    return FC(xf,yi);
  }
  
  return KC(x, y);
}

char KC(K x, K y)
{
  bool moduloDictionaryKeyOrder = HASH_MODS_OUT_DICTIONARY_ORDER;

  if(xt<yt) return ASCENDING;
  if(xt>yt) return DESCENDING;

  if(SORT_ORDER_STRINGS_LEXICOGRAPHIC)
  {
    if(WANTS_LEXICOGRAPHIC(x) && WANTS_LEXICOGRAPHIC(y)) return lexicographic(x, y);
  }

  //POTENTIAL_FEATURE_POINT
  //Lexicographic Sorting for Arbitrary Lists
  //one argument for changing the LIST KC comparator to go element-by-element
  //and not return early on differing counts is that it speeds up the
  //natural-order version of sorted integer part().
  //(items may appear at multiple indices, if you have a randomized key order,
  // you can retrieve the original order by sorting keys by the first element in each index list
  // but because the indices-lists are of varying size you can't do that unless lists of
  // differing size are compared element by element)  eg {-100:[6,7], 10:[2], 20:[0,1], 30:[3,4,5]}
  // that list is not in the natural order it appeared in

  if(ECOUNT(x)<ECOUNT(y)) return ASCENDING;
  if(ECOUNT(x)>ECOUNT(y)) return DESCENDING;

  I a=0;

  if(IS_MAP(x) || IS_TABLE(x))
  {
    if(moduloDictionaryKeyOrder)
    {
      //POTENTIAL_OPTIMIZATION_POINT
      //this repeats work that will be done if this match check fails?
      if(ECOUNT(x)!=ECOUNT(y))R EQUAL;
      I ok = 0;
      ENUM(x, K0 o; I p=lookupI(y,u); if(IS_HASH_NULL(p))break; if(!matchC(v,LOOK_(yValues,p,o),true))break; ok++)
      if(ok==ECOUNT(x))R EQUAL;
    }

    if((a=KC(xKeys,yKeys)))R a;
    if((a=KC(xValues,yValues)))R a;
    return EQUAL;
  }

  if(IS_ATLAS(x))
  {
    if((a=KC(xIndex,yIndex)))R a;
    if((a=KC(xKeys,yKeys)))R a;
    return EQUAL;
  }

  LIST2(x,y,
    SW(abs(xt))
    {
      CS(FUNC, a=IC((I)u,(I)v))//compare pointers? hope this works
      CS(NIL,a=EQUAL)
      CS(TABLE,assert(TABLE!=xt))//handled above
      CS(MAP,  assert(MAP  !=xt))//handled above
      CS(CHAR, a=IC((UC)uc,(UC)vc))
      CS(FLOAT,a=FC(uf,vf))
     CSF(STAMP,)
      CS(INT,  a=IC(ui,vi))
     CD:
     CSF(ZIP, )
     CSF(HASH,)
     CSF(BTREE,)
      CS(LIST, a=KC(u ,v ))
    }
    if(a)R a;
  );

  return EQUAL;
}

C matchC(K x, K y, I tolerant)//hashtable doesn't want float tolerance - hashes differ
{
  //It's tempting to want to factor this with KC, but don't.
  //memcmp won't factor & is 10x faster - hashing likes this
  if(xt!=yt || cx!=cy) R 0;

  if(x==y) R 1;

  bool moduloDictionaryKeyOrder = HASH_MODS_OUT_DICTIONARY_ORDER;

  SW(xt)
  {
      CS(ATLAS, DO(cx, K u = atlas_map_at_index(x,i);
                       K v = atlas_map_at_index(y,i);
                       //NOTE: this will rely on MAP's
                       //setting for moduloDictionaryKeyOrder
                       I e = matchC(u,v,tolerant);
                       rd(u);
                       rd(v);
                       if(!e)R 0;
                  )
        )
     CSF(ZIP,)
     CSF(BTREE,)
     CSF(HASH,)
      CS(LIST, LIST2(x,y, if(!matchC(u,v,tolerant))R 0))
     CSF(TABLE,)
      CS(MAP, 
            if(moduloDictionaryKeyOrder)
            {
              if(ECOUNT(x)!=ECOUNT(y))R 0;
              ENUM(x, I p=lookupI(y,u); 
                if(IS_HASH_NULL(p))R 0; 
                K0 o;
                if(!matchC(v,LOOK_(yValues,p,o),tolerant))R 0)
            }
            else
            {
              if(!(matchC(xKeys,yKeys,tolerant) && matchC(xValues,yValues,tolerant))) R 0;
            }
      )
      CS(NIL, R 1)
      CS(FUNC, R (x==y))//Compare pointers? hope this works...
     CSF(FLOAT,)   //FALLTHROUGH
     CSF(FLOATVEC, //FALLTHROUGH
        F *fx = (F*)PAYLOAD_START(x);
        F *fy = (F*)PAYLOAD_START(y);
        bool all_nans_match = true;
        if(tolerant)
        {
          DO(cx, if(FC(fx[i],fy[i])) return 0)
          break;
        }
        else if(all_nans_match)
        {
          DO(cx, if(isnan(fx[i]) && isnan(fy[i])) continue; if(fx[i]!=fy[i]) return 0;)
          break;
        }
        else
        {
          //FALLTHROUGH
        }
      )
      CD:if(memcmp(&xc,&yc,any_size_K(x)-offsetof(K0,c)))R 0;//8==sizeof(m,a,t,u)+sizeof(r)
  }
  R 1;
}

//Note: doesn't factor with "lexicographic insensitive order -> {-1, 0, 1}"
bool charvec_case_matchC(K x, K y, bool case_insensitive)
{
  bool case_sensitive = !case_insensitive;

  if(case_sensitive) return matchC(x, y, false);

  //case insensitive match
  if(!IS_CHARVEC(x) || !IS_CHARVEC(y))return false;
  if(cx != cy) return false;
  LIST2(x, y, if(tolower(rc) != tolower(sc)) return false)

  return true;
}

#pragma mark - HASH/INTERN-type Array Pre-where

K hash_type_compare_pre_whered(K intern, K y, I *original_table)
{
  if(SORTED(intern))
  {
    return attr_sort_compare_pre_whered(intern, y, original_table);
  }

  //Note: Must treat strings as atom (SATOM)
  K x = intern;
  K z = NULL;

  I *table = original_table + 1;//so -1 is the first element, 0 second, 1 third

  K hashset = xIndex;
  K hashset_keys = kN(hashset, KEYS);

  K intern_intvec = xKeys;

  K pass_unwhered = new_k(INTVEC, lenI(hashset_keys));
  work_push(pass_unwhered);

  //POTENTIAL_OPTIMIZATION_POINT 
  //We could move this (or similar) technique into xin_pre_whered
  //it assumes INTVECs look like HASH's key INTVEC
  //(non-zero and small)
  //xin_p_w may also want an O(n*k) version among other things???
  //K pass_whered = new_k(INTVEC, lenI(hashset_keys));
  //pass_whered->n = 0;
  //work_push(pass_whered);
  
  ENUM(hashset_keys, bool passed = table[KC(v,y)];
                     kI(pass_unwhered)[i] = (passed ? true : false);
                     //if(passed) kI(pass_whered)[pass_whered->n++] = i;
  )

  z = new_k(INTVEC, 0);

  DO(intern_intvec->n, I p = kI(intern_intvec)[i];
                       bool b = kI(pass_unwhered)[p];
                       if(b)
                       {
                         z = cow_add(z, ki(i));
                       }
  )

  //work_pop_n_rd(1, true);//pass_whered
  work_pop_n_rd(1, true);//pass_unwhered

  return z;

  //////////////////////
  //Unoptimized version

  z = new_k(INTVEC, 0);

  ENUM(intern, if(table[KC(v,y)]) z = cow_add(z, ki(i)))

  return z;
}

K hash_type_compare_unwhered_bytes(K x, K y, I *original_table)
{
  //Note: Must treat strings as atom (SATOM)

  //POTENTIAL_OPTIMIZATION_POINT
  //rewrite to not leverage pre_whered version
  K indices = hash_type_compare_pre_whered(x, y, original_table);

  work_push(indices);
  K boolified = boolean_char_vector_from_true_indices(indices, lenI(x));
  work_pop_rd(true);

  return boolified;
}

#pragma mark - Verb Functions Compare

K boolean_char_vector_from_true_indices(K indices, I length)
{
  assert(length >= 0);
  assert(indices->t == INTVEC);

  K y = indices;

  K z = new_k(CHARVEC, length);
  SET_ATTR(z, ATTR_BYTES);
  zero_list_payload(z);

  DO(yn, assert(yI[i] >= 0 ); assert(yI[i] < length))

  DO(yn, zC[yI[i]] = 1;)

  return z;
}

K promote_char_types_to_int_types(K x)
{
  assert(CHAR==xt || CHARVEC==xt);

  K z = NULL;

  SW(xt)
  {
    CS(CHAR, z = Ki(xc))
    CS(CHARVEC, z = new_k(INTVEC, xn); DO(zn, zI[i]=xC[i]))
  }

  return z;
}

K tri_compare(K x, K y, I *original_table) //Note: forked with pre-whered version
{
  K w = tri_compare_chars(x, y, original_table);

  K z = promote_char_types_to_int_types(w);

  rd(w);
  return z;
}

K tri_compare_chars(K x, K y, I *original_table)
{
  //POTENTIAL_OPTIMIZATION_POINT - see other versions as well
  //hash v. hash
  //hash v. plain old list
  //att_sorted array v. attr_sorted array

  bool safx = is_special_atlas_form(x);
  bool safy = is_special_atlas_form(y);
  if(safx || safy)
  {
    K special = x;
    if(safy) special = y;
    K atlas = kN(special,1);

    K indices = tri_compare_pre_whered(x, y, original_table);

    work_push(indices);
    K boolified = boolean_char_vector_from_true_indices(indices, lenI(atlas));
    work_pop_rd(true);

    return boolified;
  }

  C small_table[3] = {original_table[0], original_table[1], original_table[2]};
  C *table = small_table + 1;//so -1 is the first element, 0 second, 1 third

  bool is_string_x = IS_STRING(x);
  bool is_string_y = IS_STRING(y);

  bool unoptimized_compare_string_atomicity = true;
  //Exceptions for string comparison
  //Note: we could sensibly atomize this unoptimized version in the verb table.
  if(unoptimized_compare_string_atomicity)
  {
    if(is_string_x || is_string_y)
    {
      if(is_string_x && is_string_y)
      {
        //return enlist(ki(table[KC(x,y)]));
        char c = table[KC(x,y)];
        return Kb(c);
      }

      K non = x;
      K string = y;

      if(!is_string_y)
      {
        non = y;
        string = x;

        I temp = table[-1]; //swap ends
        table[-1] = table[1];
        table[1] = temp;
      }

      if(!IS_MIXED_ARRAY(non) && !IS_CHAR(non))
      {
        I n = COUNT(non);
        K z = take(ki(n),kc(0)); 
        SET_ATTR(z, ATTR_BYTES);
        return z; 
      }

      K z = new_k(CHARVEC, 0);
      SET_ATTR(z, ATTR_BYTES);

      SW(non->t)
      {
        CS(HASH, rd(z); z = hash_type_compare_unwhered_bytes(x,y,original_table))
       CSF(ZIP,)
       CSF(BTREE,)
        CS(LIST,
            ENUM(non, z = cow_add(z, kc(table[KC(v,string)])))
        )
        CS(CHAR, ENUM(string, z = cow_add(z,kc(table[KC(non,v)])))) //POTENTIAL_OPTIMIZATION_POINT
      }

      return z;
    }
  }

  if(IS_TABLE(x) || IS_TABLE(y))
  {
    //this solves a crash bug but we don't have to do it like this. bool per column, or per row
    //possibly the right way to do this is to fix/make a new CONFORM_TRY below to work with tables
    //Note that to use columns & not rows is prob. more useful but arguably
    //breaks a certain parallelism?
    //and LIST2 (column hits?) that it doesn't work now makes me worried about other CONFORM_TRYs with tables
    //this has copy-pasta in tri_compare_pre_whered
    return Kb(table[KC_NUM(x,y)]);
  }

  if(IS_ATLAS(x) || IS_ATLAS(y))
  {
    //this solves a crash bug but we don't have to do it like this. see above
    return Kb(table[KC_NUM(x,y)]);
  }

  C t = CHAR;
  if(IS_ARRAY(x) || IS_ARRAY(y)) t = CHARVEC;

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);
  SET_ATTR(z, ATTR_BYTES);

  //POTENTIAL_OPTIMIZATION_POINT
  //the pre-whered version has a case that will split out
  //hashes sorts and so on. do so here? to use the following?? 
  //hash_type_compare_unwhered_bytes(x,y,original_table))

  //We can combine atom and vector cases by using pointers
  C *iz = (C*) PAYLOAD_START(z);

  bool local = false;//for datetimes

  SW(xt)
  {
   CSF(STAMPVEC,)
    CS(INTVEC, 
      SW(yt)
      {
       CSF(STAMP,)
        CS(INT,   DO(xn, I k = yi; zC[i] = table[IC(xI[i], k)]; ) R z)
        CS(FLOAT, DO(xn, F f = yf; zC[i] = table[FC(xI[i], f)]; ) R z)
      }
    )
    CS(FLOATVEC,
      SW(yt)
      {
       CSF(STAMP,)
        CS(INT,   F f = yi; DO(xn, zC[i] = table[FC(xF[i], f)]; ) R z)
        CS(FLOAT, F f = yf; DO(xn, zC[i] = table[FC(xF[i], f)]; ) R z)
      }
    )
   CSF(STAMP,)
    CS(INT, 
      SW(yt)
      {
       CSF(STAMPVEC,)
        CS(INTVEC,   DO(yn, I k = xi; zC[i] = table[IC(k, yI[i])]; ) R z)
        CS(FLOATVEC, DO(yn, F f = xi; zC[i] = table[FC(f, yF[i])]; ) R z)
      }
    )
    CS(FLOAT,
      SW(yt)
      {
       CSF(STAMPVEC,)
        CS(INTVEC,   F f = xf; DO(yn, zC[i] = table[FC(f, yI[i])]; ) R z)
        CS(FLOATVEC, F f = xf; DO(yn, zC[i] = table[FC(f, yF[i])]; ) R z)
      }
    )
  }

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  //see above for some Vector_x_Atom versions
  SW(axt)
  {
    CS(MAP,
      SW(ayt)
      { 
        CS(STAMP, if(!IS_DTBUCKET(x))break; 
                  
                  TM_NANOS rel_tm = TM_NANOS_from_dtbucket_and_initializer(x, TM_NANOS_NANS);
                  
                  ENUM(y, TM_NANOS abs_tm = TM_NANOS_from_stampI(si, local);
                          //POTENTIAL_OPTIMIZATION_POINT
                          //here (and copy-paste below) we allocate intvecs for the two sides and recurse
                          //this is not direct at all
                          K c = *work_push(array_thing_bucket_nanos(original_table, x, abs_tm, false));//note: not swapping
                          C truthy = 1; 
                          DO(COUNT(c), truthy &= kI(c)[i])
                          iz[i] = truthy;
                          work_pop_rd(true);
                  )
                  

                  R z;
          )
      }
    )
    CSF(STAMP, //fallthrough to int
      SW(ayt)
      {
        CS(MAP,   if(!IS_DTBUCKET(y))break; 
                  TM_NANOS rel_tm = TM_NANOS_from_dtbucket_and_initializer(y, TM_NANOS_NANS);
                  
                  ENUM(x, TM_NANOS abs_tm = TM_NANOS_from_stampI(si, local);
                          K c = *work_push(array_thing_bucket_nanos(original_table, y, abs_tm, true));//note: swapping
                          C truthy = 1; 
                          DO(COUNT(c), truthy &= kI(c)[i])
                          iz[i] = truthy;
                          work_pop_rd(true);
                  )
                  R z;
        )
      }
    )
    CS(INT, 
      SW(ayt)
      { 
       CSF(STAMP,)
        CS(INT,   LIST2(x, y, iz[i] = table[IC(ri,si)]) R z;)
        CS(FLOAT, LIST2(x, y, iz[i] = table[FC(ri,sf)]) R z;)
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
       CSF(STAMP,)
        CS(INT,   LIST2(x, y, iz[i] = table[FC(rf,si)]) R z;)
        CS(FLOAT, LIST2(x, y, iz[i] = table[FC(rf,sf)]) R z;)
      }
    )
  }

  LIST2(x, y, iz[i] = table[KC_NUM(u,v)]);

  R z;
}

K tri_compare_optimized(K x, K y, I *original_table)
{
  //Decide whether you want to return:
  //CHARVEC of 0,1 of same length as input
  //INTVEC of indices corresponding to matches

  bool use_intvec = false;

  I count_nsarrays = 0;
  K an_nsarray = NULL; //non-string array
  if(!IS_SATOM(y)){ count_nsarrays++; an_nsarray = y; }
  if(!IS_SATOM(x)){ count_nsarrays++; an_nsarray = x; }

  //Note: within INTVEC (pre_whered) methods, we'll still
  //want to fall back to boolean if the number of indices is too high

  //These cases are/should be decided on a heuristic basis (whatever goes fast)
  SW(count_nsarrays)
  {
    CS(0, use_intvec = true;
          break;
    )
    CS(1, if( SORTED(an_nsarray))    { use_intvec = true; break; }
          if(IS_BTREE(an_nsarray))    { use_intvec = true; break; }
          if(IS_HASH(an_nsarray))    { use_intvec = true; break; }
          use_intvec = false;
          break;
    )
    CS(2, if(SORTED(x) && SORTED(y)) { use_intvec = true; break; }
          use_intvec = false; 
          break;
    )
  }

  bool use_charvec = !use_intvec;

decided:
  if(use_intvec)
  {
    return tri_compare_pre_whered(x, y, original_table);
  }
  else
  {
    return tri_compare_chars(x, y, original_table);
  }
}

K tri_compare_pre_whered(K x, K y, I *original_table) //Note: forked from not-whered version
{
  //POTENTIAL_OPTIMIZATION_POINT
  //the two tri_compare methods, this one and the un-whered original (this is a
  //fork sorta), can be extended to have a faster equality comparison for HASH
  //objects. create an array that's a map from one ENUM to the other (INT ->
  //INT) (you may need two of these?) or whatever. then you get fast equality
  //and not-equality. the previous way would have you compare the objects each
  //time, which is suboptimal in the cases of say string enumerations, a common
  //occurrence.

  //POTENTIAL_OPTIMIZATION_POINT - see "non pre_whered" version as well
  //hash v. hash
  //hash v. plain old list
  //att_sorted array v. attr_sorted array: +fall back to bytes if the intersection is too high

  I *table = original_table + 1;//so -1 is the first element, 0 second, 1 third

  bool safx = is_special_atlas_form(x);
  bool safy = is_special_atlas_form(y);
  if(safx || safy)
  {
    if(safy)//swap sides for `select from my_atlas where 3<b` (instead of `b>3`)
    {
      K tempk = x;
      x = y;
      y = tempk;

      I temp = table[-1]; //swap ends
      table[-1] = table[1];
      table[1] = temp;
    }

    K string = xIndex;
    K other  = y;
    K atlas  = xKeys;
    K sort   = kN(atlas, KEYS);

    K4 o;
    K indices = sort_type_compare_pre_whered(sort, klist2(string, other, o), original_table, true, false);

    //not sorting indices returns atlases in sorted order
    bool atlas_select_preserves_natural_order = true; 
    if(atlas_select_preserves_natural_order)
    {
      indices = cow_sort(indices); 
    }

    work_push(indices);

    K deindexed = atlas_indices_from_key_indices(atlas, indices);

    work_pop_rd(true);
    return deindexed;
  }

  //Exceptions for string comparison
  //Note: you can't [sensibly] atomize this optimized version in the verb table.
  if(IS_STRING(x) || IS_STRING(y))
  {
    if(IS_STRING(x) && IS_STRING(y))
    {
      K z = new_k(INTVEC, 0);
      if(table[KC(x,y)]) z = cow_add(z, ki(0));
      return z;
    }

    K non = x;
    K string = y;

    if(!IS_STRING(y))
    {
      non = y;
      string = x;

      I temp = table[-1]; //swap ends
      table[-1] = table[1];
      table[1] = temp;
    }

    K z = new_k(INTVEC, 0);

    if(!IS_MIXED_ARRAY(non) && !IS_CHAR(non)) return z;

    SW(non->t)
    {
      CS(HASH, rd(z); return hash_type_compare_pre_whered(non, string, original_table))
      CS(BTREE, rd(z); return sort_type_compare_pre_whered(non, string, original_table, false, false))
     CSF(ZIP,)
      CS(LIST,
          if(SORTED(non))
          {
            rd(z);
            return attr_sort_compare_pre_whered(non, string, original_table);
          }
          ENUM(non, if(table[KC(v,string)]) z = cow_add(z, ki(i)))
      )

      CS(CHAR, ENUM(string, if(table[KC(non,v)]) z = cow_add(z,ki(i)))) //POTENTIAL_OPTIMIZATION_POINT
    }

    return z;
  }

  if(IS_TABLE(x) || IS_TABLE(y))
  {
    //this solves a crash bug but we don't have to do it like this. bool per column, or per row
    //see original at tri_compare not pre whered
    if(table[KC_NUM(x,y)]) return enlist(ki(0));
    return new_k(INTVEC,0);
  }

  //TODO: technically, we don't need this to conform for pre_whered:
  //      we can compare the smaller guy against everything in the bigger list
  //      so eg we can compare "[5,6] <= [[4,5],[5,6],[6,7],...]"
  //
  volatile I n = CONFORM_TRY(x,y);

  if(n > 1 && cx != cy)
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //x,y both in {ATTR_SORTED, BTREE-TREE} gives a fast
    //method for finding empty-set and/or reducing
    //test surface: we compute the overlap range and work
    //there only
    K big = y;
    if(cx > cy) big = x;

    if(IS_ARRAY(big) && !IS_CHARVEC(big) && GET_ATTR(big, ATTR_SORTED))
    {
      return attr_sort_compare_pre_whered(x, y, original_table);
    }

    if(IS_BTREE(big))
    {
      return sort_type_compare_pre_whered(x, y, original_table, false, false);
    }

    if(IS_HASH(big))
    {
      return hash_type_compare_pre_whered(x, y, original_table);
    }
  }

  K z = new_k(INTVEC, 0);

  //We can combine atom and vector cases by using pointers
  bool local = false;//for datetimes

  SW(axt)
  {
    CS(MAP,
      SW(ayt)
      { 
        CS(STAMP, if(!IS_DTBUCKET(x))break; 
                  
                  TM_NANOS rel_tm = TM_NANOS_from_dtbucket_and_initializer(x, TM_NANOS_NANS);
                  
                  ENUM(y, TM_NANOS abs_tm = TM_NANOS_from_stampI(si, local);
                          //POTENTIAL_OPTIMIZATION_POINT
                          //here (and copy-paste below) we allocate intvecs for the two sides and recurse
                          //this is not direct at all
                          K c = *work_push(array_thing_bucket_nanos(original_table, x, abs_tm, false));//note: not swapping
                          C truthy = 1; 
                          DO(COUNT(c), truthy &= kI(c)[i])
                          if(truthy) z = cow_add(z, ki(i));
                          work_pop_rd(true);
                  )
                  R z;
          )
      }
    )
    CSF(STAMP, //fallthrough to int
      SW(ayt)
      {
        CS(MAP,   if(!IS_DTBUCKET(y))break; 
                  TM_NANOS rel_tm = TM_NANOS_from_dtbucket_and_initializer(y, TM_NANOS_NANS);
                  
                  ENUM(x, TM_NANOS abs_tm = TM_NANOS_from_stampI(si, local);
                          K c = *work_push(array_thing_bucket_nanos(original_table, y, abs_tm, true));//note: swapping
                          C truthy = 1; 
                          DO(COUNT(c), truthy &= kI(c)[i])
                          if(truthy) z = cow_add(z, ki(i));
                          work_pop_rd(true);
                  )
                  R z;
        )
      }
    )
    CS(INT, 
      SW(ayt)
      { 
       CSF(STAMP,)
        CS(INT,   LIST2(x, y, if(table[IC(ri,si)]) z = cow_add(z, ki(i))); R z;)
        CS(FLOAT, LIST2(x, y, if(table[FC(ri,sf)]) z = cow_add(z, ki(i))); R z;)
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
       CSF(STAMP,)
        CS(INT,   LIST2(x, y, if(table[FC(rf,si)]) z = cow_add(z, ki(i))); R z;)
        CS(FLOAT, LIST2(x, y, if(table[FC(rf,sf)]) z = cow_add(z, ki(i))); R z;)
      }
    )
  }

  LIST2(x, y,                 if(table[KC_NUM( u, v)]) z = cow_add(z, ki(i)));

  R z;
}

I finder(K x, K y, K column_decoder)//exact index of presence or IN  x:needle y:haystack
{
  I fail = FINDER_FAIL;

  if(SORTED(y) && (IS_ARRAY(y) || IS_TABLE(y)))
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //if array and fuzzy
    //I low_equal_mark  = search_equal_mark_recursive(x, y, 0, n - 1, 0);
    //fail if out of range || low_equal_mark;

    I2 range = ranger(x, y, column_decoder);

    I width = range.y - range.x;

    I place = range.x;

    if(width <= 0) place = fail;

    return place;
  }

////POTENTIAL_OPTIMIZATION_POINT (also useful for binner/closest)
//  if(IS_BTREE(y))
//  {
//    K z = yIndex; 
//    I link = landis_find_index_child(y, 1, klist1(x), TREE_EQUAL_LEFT, 0, 0, 0); 
//    link = ABS(link);
//
//    I node;
//
//    bool was_exact = (0 != zI[link]);
//
//    if(was_exact)
//    {
//      node = zI[link];
//    }
//    else
//    {
//      if(fuzzy) node = link / 2; 
//      else node = fail
//    }
//
//    node = ABS(node);
//
//    assert(n!=0);//otherwise we need to handle the node-1==0-1 case since we return "fail" and not -1 
//
//    I place = node - 1;
//
//    return place;
//  }

  if(IS_ARRAY(y)) //Linear Search O(n)
  {
    //POTENTIAL_OPTIMIZATION_POINT: break down by types - much faster
    ENUM(y, if(matchC(x,v,true))return i)
  }

  return fail;
}

I2 ranger(K x, K y, K column_decoder) //fast find, closest on no-match. x:needle y:haystack.  returns [NAN,0,...,n], note: n is not n-1
{
  I2 fail = (I2){IN, IN};

  if(IS_MAP(y)) y = yValues;//enables key lookup

  I n = lenI(y);//Not COUNT b/c we want table rows

  if(!(IS_ARRAY(y) || IS_TABLE(y))) return fail;//or lenI(x), or what have you

  if(0==n) return fail;

  if(SORTED(y))
  {
    if(IS_ARRAY(y))
    {
      return search_equal_range(x, y);
    }
    else if(IS_TABLE(y))
    {
      if(!IS_MAP(x)) ERROR(ERROR_MAP);

      column_decoder = stronger_decoder(x, y, column_decoder);
      work_push(column_decoder);

      K z = yValues;

      //Search each column, non-increasing interval
      I2 range = {0, n};
      ENUM(x, I p = kI(column_decoder)[i];
              if(IN==p)continue;
              K0 o;
              K col = LOOK_(z, p,o);
              range = search_equal_range_within_range(v, col, range);
              if(range.y - range.x <= 0) break;
      )

      work_pop_rd(true);

      return range;
    }
    else
    {
      bool unreachable = 0;
      assert(1==unreachable);
    }
  }
  
  return fail;
}

#pragma mark - Pre-whered Methods for ATTR_SORTED Arrays

I search_equal_mark_recursive(K x, K y, I low, I high, C style) //needle, haystack, {0: least-closed, 1: greatest-open, 2: first-closed}
{
  //POTENTIAL_OPTIMIZATION_POINT
  //*interpolation search not binary
  //*linear search if interval is small
  //*type specific comparisons (but probably no difference)

  I i = low + ((high - low)/2);

  K0 o;
  I r = LC(x, LOOK_(y, i,o)); //i will never escape valid bounds

  if(EQUAL == r)
  {
    SW(style)
    {
      CS(0, r = ASCENDING)
      CS(1, r = DESCENDING)
      CS(2, return i)
    }
  }

  if(low >= high)
  {
    SW(r)
    {
      CS(ASCENDING,  return i)
      CS(DESCENDING, return i + 1)
    }
  }

  SW(r)
  {
    CS( ASCENDING, return search_equal_mark_recursive(x, y,   low, i - 1, style))
    CS(DESCENDING, return search_equal_mark_recursive(x, y, i + 1,  high, style))
  }

  return 0;
}

I2 search_equal_range(K x, K y)
{
  I n = cy;

  if(0==n) return (I2){0,0};

  I2 full_range = {0, n};

  return search_equal_range_within_range(x, y, full_range);
}

I2 search_equal_range_within_range(K x, K y, I2 range) //needle, haystack. half-open, e.g., [0,0) or [3,3) or [4,7) or [n,n)
{
  I low_equal_mark  = search_equal_mark_recursive(x, y, range.x, range.y - 1, 0);
  I high_equal_mark = search_equal_mark_recursive(x, y, range.x, range.y - 1, 1);

  return (I2){low_equal_mark, high_equal_mark};
}

K attr_sort_compare_pre_whered(K x, K y, I *original_table)//optimized SQL WHERE for ATTR_SORTED
{
  I table[3] = {0};

  DO(3, table[i] = original_table[i]);

  K needle = NULL;
  K haystack = NULL;

  //I might've got this flipped around... ?
  //I think I did. at least if you trust grade.c
  //boolean tables are probably right given the forward iteration works
  //(to produce ascending sorted indices lists)
  //so the needle/haystack pairing should jump to that tune
  //see also the companion "compare_pre_whered" for sort objects
  //if(cx < cy)
  if(!(IS_ARRAY(x) && SORTED(x)))
  {
    needle = x;
    haystack = y;

    //DO(3, table[i] = 1 - table[i]) //flip all
    //table[0] = 1 - table[0]; table[2] = 1 - table[2]; //flip ends

    I temp = table[0]; //swap ends
    table[0] = table[2];
    table[2] = temp;
  }
  else
  {
    needle = y;
    haystack = x;
  }

  I n = COUNT(haystack);

  bool check_first_and_last = true;
  if(check_first_and_last)
  {
    if(0==n) return new_k(INTVEC, 0);

    K0 o1,o2;
    C left  = LC(needle, LOOK_(haystack, 0, o1));
    if(ASCENDING == left && !table[2])
    {
      return new_k(INTVEC, 0);
    }

    C right = LC(LOOK_(haystack, n-1, o2), needle);
    if(ASCENDING == right && !table[0])
    {
      return new_k(INTVEC, 0);
    }

    //A silly case we probably don't need
    if(!table[1] && (EQUAL==left && EQUAL==right))
    {
      return new_k(INTVEC, 0);
    }
  }



  I2 equal = search_equal_range(needle, haystack);

  return indices_from_n_equal_range_and_comparison_truthtable(n, equal, (I*)table);
}

K indices_from_n_equal_range_and_comparison_truthtable(I n, I2 equal, I *table)
{
  I2 less  = {0, equal.x};
  I2 greater = {equal.y, n};

  I2 ranges[3] = {less, equal, greater};

  K z = new_k(INTVEC, 0);

  //POTENTIAL_OPTIMIZATION_POINT
  //we know how to prealloc z
  DO(3, 
    if(table[i])
    {
      I2 range = ranges[i];
      I i = 0;
      for(i = range.x; i < range.y; i++)
      {
        z = cow_add(z, ki(i));
      }
    }
  )

  return z;
}

#pragma mark -

//TODO: low priority: all these grade allocations should go onto the temporary workspace...eg work_push()

K charGrade(K x, I r)
{//Variation on Knuth Algorithm 5.2D Distribution counting
  I n=xn;
  I c[1+UCHAR_MAX] = {0};
  I b = sizeof(c)/sizeof(I);
  K s=new_k(-INT,n);  
  DO(n,c[(UC)xC[i]]++)
  if(!r) DO(b-1,c[i+1]+=c[i])        //0==r: grade up
  else   DO(b-1,c[_i-i-1]+=c[_i-i-0])//1==r: grade down
  DO(n, kI(s)[-1+c[(UC)xC[n-i-1]]--]=n-i-1)
  R s;
}

K distributionGrade(K x, I r, I u, I v)//u,v: precomputed min,max
{//Variation on Knuth Algorithm 5.2D Distribution counting
  I n=xn, b=v-u+1, *c;
  K d=new_k(-INT,b);
  c=kI(d); 
  DO(b,c[i]=0)
  K y=new_k(-INT,n);  
  DO(n,c[xI[i]-u]++)
  if(!r) DO(b-1,c[i+1]+=c[i])        //0==r: grade up
  else   DO(b-1,c[_i-i-1]+=c[_i-i-0])//1==r: grade down
  DO(n, yI[-1+c[xI[n-i-1]-u]--]=n-i-1)
  rd(d);
  R y;
}

UI makeMask(C start, C width)
{
  UI m = 0; C i;
  for(i = 0; i < width; i++)
  {
    m |= (1ULL << (start + i));
  }
  return m;
}

ALWAYS_INLINE UI wellOrderSignedInt(I i)//transform/convert signed to unsigned but preserve order
{
  UI u = (UI)i;
  u = u + 1 + (UINT64_MAX/2);
  return u;
}

ALWAYS_INLINE UI wellOrderSignedFloat(F f)//transform/convert to UI but preserve order
{
  //see http://stereopsis.com/radix.html
  UI b = *(UI*)&f;

  if(FLOAT_NANS_COMPARE_AS_SMALLEST)
  {
    //todo: there is probably a collision with some FP value here
    //that is also zero. probably the right way to do this is to
    //do an ADDITION if nans are at the top of the range
    //plus a mask to make NANs (with payloads/quiet bits) uniform
    //in practice none of this will matter, however.
    //See FC() for related discussion.
    b ^= isnan(f)?b:((~((b >> 63) - 1)) | (1ULL << 63));
  }
  else
  {
    b ^= ((~((b >> 63) - 1)) | (1ULL << 63));
  }

  return b;
}

K digitalGradePass(K x, I updown, K y, K buckets, C kind, C bits, C pass)
{
  I *b = kI(buckets);

  C shift = pass * bits;
  UI mask = makeMask(shift, bits); //fprintf(stderr,"Mask: %llu\n",mask);

  //Bucket Integration pass
  if(!updown) DO(buckets->n - 1, b[i+1]   += b[i])   //0: grade up
  else        DO(buckets->n - 1, b[_i-i-1]+= b[_i-i])//1: grade down

  //POTENTIAL_OPTIMIZATION_POINT
  //Is it faster, here and in distribution grade, to go fwd not backwd?

  //Placement pass
  DO(yn,  
          I place    = _i-i-1;
          I grade    = place;
          I rawInt   = xI[grade];
          F rawFloat = xF[grade];

          UI good;

          SW(kind)
          {
           CD:
           CSF(STAMPVEC,)
            CS(INTVEC,   good = wellOrderSignedInt(rawInt))
            CS(FLOATVEC, good = wellOrderSignedFloat(rawFloat))
          }
         
          UI word = ((good & mask) >> shift);

          assert(0 <= word);
          assert(word < buckets->n - 1);
          I walk = -1 + (b[word]--);
          assert(walk >= 0);
          assert(walk < yn);
          yI[walk] = place;
  //fprintf(stderr, "pass: %d, raw: %20lld, good %20llu, word %5llu, walk %3lld, place %3lld\n", pass, rawInt, good, word, walk, place);
  )

  return y;
}

K digitalGrade(K x, I r) //bucket sort/radix sort counting words, least to most sig word (stable)
{
  //Float and int each need conversion function. So just build like that

  I n = COUNT(x);

  char kind = xt; 
  const char bits = 16;
  const char rounds = ceil(sizeof(I)*8/(F)bits);
  assert(rounds >= 1);

  K pails = new_k(LIST, rounds);
  pails->n = 0;

  DO(rounds, K buckets = take(ki(1+POW2(bits)), ki(0)); 
             pails = cow_add(pails, buckets);
             rd(buckets);
  )
  work_push(pails);

  K y = new_k(INTVEC, n);
  work_push(y);

  UI mask = makeMask(0, bits); //fprintf(stderr,"Mask: %llu\n",mask);
  //Counting pass
  DO(n,   I rawInt   = xI[i];
          F rawFloat = xF[i];
          UI good;
          
          SW(kind)
          {
           CD:
           CSF(STAMPVEC,)
            CS(INTVEC,   good = wellOrderSignedInt(rawInt))
            CS(FLOATVEC, good = wellOrderSignedFloat(rawFloat))
          }

          DO2(rounds, 
            K0 o;
            K buckets = LOOK_(pails, j, o);
            UI word = (good >> (j * bits)) & mask;
            kI(buckets)[word]++;
          )
  )

  K q = strong(x);
  K z = NULL;
  
  DO(rounds, 
             K0 o;
             K buckets = LOOK_(pails,i,o);
             //bool all_same = false; //pointless: so cache-friendly already
             //DO(buckets->n - 1, if(kI(buckets)[i]==n) all_same = true;)

             K was = digitalGradePass(q,r,y,buckets,kind,bits,i);

             if(!z)
             {
               z = work_pop();
               assert(z==y);
               //because we modify y in place we can't merely assign z=strong(y)
               y = new_k(INTVEC, n);
               work_push(y);
             }
             else
             {
               K t = at(z,y);
               rd(z);
               z = t;
             }

             if(i < rounds - 1)
             {
              K t = at(x,z);
              rd(q);
              q=t;
             }
  )

  rd(q);

  work_pop_n_rd(2, false);
  rd(pails);
  rd(y);

  return z;
}

ALWAYS_INLINE C table_row_compare(K x, I a, I b)
{
  //POTENTIAL_OPTIMIZATION_POINT (if this code becomes reachable again)
  //This is still 10x too slow.
  //You can exploit that column-types will be the same internally
  //to bypass/special-case KC and the LOOK.
  //possibly using a static K-LIST will help, works pretty good for 1 < AVL-strips

  C c;
  I cols = COUNT(xKeys);

  DO(cols, K0 o1,o2; K y = kN(xValues, i);  if((c = KC(LOOK_(y,a,o1),LOOK_(y,b,o2)))) return c)

  return EQUAL;
}

bool table_is_sorted(K x)
{
  I r = table_rows(x);

  DO(r - 1, if(table_row_compare(x,i, i+1) == DESCENDING)return false;)

  return true;
}

static inline I mergerComparer(K x, I r, I i, I j)
{
  I c = 0;

  SW(xt)
  {
    CS(-FLOAT, c = FC(xF[i],xF[j]))
   CSF(-STAMP,)
    CS(-INT,   c = IC(xI[i],xI[j]))
    CS(-CHAR,  c = IC((UC)xC[i],(UC)xC[j])) //unreachable. -CHAR has its own sort, won't be merged...
    CD:
   CSF( ZIP,)
   CSF( HASH,)
   CSF( BTREE,) //assert: unreachable: returns early in optimized method
    CS( LIST,  K0 o1,o2; c = KC(LOOK_(x,i,o1),LOOK_(x,j,o2)))
    CS( TABLE, 
               //assert: unreachable - we use a right-to-left columnar stable sort for now
               c = table_row_compare(x, i, j);
               break;
               //Bad/slow: you don't need to alloc and dealloc rows every time
               //K y = at(x,ki(i));
               //K z = at(x,ki(j)); 
               //c = KC(y,z);
               //rd(y);rd(z);
      )
  }

  SW(r)
  {
    CS(0, if(c < +1) R 1)
    CS(1, if(c > -1) R 1)
  }

  R 0;
}

void merger(K x, I r, K u, K v, I s, I t, I m)
{
  I i,j,k;
  I *c=kI(u),*d=kI(v);
  for(i=s;i<=t;i++)d[i]=c[i];
  i=s;j=m+1;k=s;
  while(i<=m && j<=t)
   if(mergerComparer(x,r,d[i],d[j]))c[k++]=d[i++];
   else c[k++]=d[j++];
  while(i<=m)c[k++]=d[i++]; 
}

void doMergeGrade(K x, I r, K u, K v, I s, I t)
{
  if(s >= t) R; //Faster: another sort when small |t-s| 
  I m=s+(t-s)/2; //sic
  doMergeGrade(x,r,u,v,s,m);
  doMergeGrade(x,r,u,v,m+1,t);
  merger(x,r,u,v,s,t,m);
}

K mergeGrade(K x, I r)
{
  I n = 0;
  
  SW(xt)
  {
    CS(TABLE, n = table_rows(x)) 
    CD: n = COUNT(x); 
        break;
  }

  K u = new_k(-INT,n);//Indices
  K v = new_k(-INT,n);//Temporary storage
  DO(n, uI[i]=i)
  doMergeGrade(x,r,u,v,0,n-1);
  rd(v);
  R u;
}

K grade_updown_table(K x, I r)
{
  //We pursue the following strategy:
  //A. grade the rightmost untreated column, at(grades) the left, grade, repeat
  //   if the sorts are stable this works.
  //POTENTIAL_OPTIMIZATION_POINT
  //B. Instead of at()ing the column, modify functions to use the list of grades only
  //   possibly only something like mergesort will be amenable
  //   this is still O(n)
  //C. It has different applications (no grades), but in-place stable sort on the
  //   table is possibly O(1) space and does not need memory for an entire column
  //   (our mergesort seems to be O(n) so that's not good)
  //Note 1: If there are a *lot* of columns, it may make more sense
  //        to use the table_row_compare version? since it can break early
  //Note 2: There is slick kerf code that will do this, but you need two versions (asc and desc)

  I columns = table_columns(x);


  K grades = NULL;

  DO(columns, K0 o; K current = LOOK_(xValues, columns - (1+i), o); 
              if(!grades)
              {
                grades = grade_updown(current, r); 
              }
              else
              {
                K temp = at(current, grades);
                K better = grade_updown(temp, r);
                K best = at(grades, better);
                rd(temp);
                rd(grades);
                rd(better);
                grades=best;
              }
  )

  if(!grades) grades = new_k(INTVEC,0);

  return grades;
}

K grade_updown_hash(K x, I r)
{
  if(r) return ex(">(>>$1)[$2]",kN(xIndex,KEYS), xKeys); 
  else  return ex("<(<<$1)[$2]",kN(xIndex,KEYS), xKeys); 
}

K grade_updown(K x, I r)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //most INTVECs are going to be fine except for NANs.
  //but if you can substitute the NANS you can use the better
  //distributionGrade instead of digitalGrade.
  //(NAN hoses this because it's very small and ruins interval size)
  //So on the initial pass, detect that (non-NAN) min
  //is OK (not -inf) as so on. replace NANs by x:(min - 1),
  //distributionGrade that, replace x by NAN. can probably
  //do this in code as follows:
  //y: non-NAN-min v; v[which v = NAN]:y; <v  
  //actually, you can just junk the substituted vector
  //once you get the grades
  //this can even be expanded to work with infinities

  //POTENTIAL_OPTIMIZATION_POINT
  //If TABLE columns are marked sorted already, big win here

  //POTENTIAL_OPTIMIZATION_POINT
  //INTERNs/HASHes with few enough keys can use
  //distribution/bucket/char-ish sort
  //hacky way to do this fast:
  //maybe distribution-grade the INTVEC on the intern
  //whatever-grade the uniques on the INTERN
  //spit out the distribution grade buckets in the uniques' order
  //
  //here's another hacky way. do it this way:
  //it will be really quick
  //g: grade(grade(hashset->INDEX->KEYS))
  //return grade(g[hashset->KEYS])

  //POTENTIAL_OPTIMIZATION_POINT
  //If an arbitary LIST/VECTOR has enough repeats, hashing it into an INTERN first
  //and then specialized-intern-distribution-sorting may be better!

  //POTENTIAL_OPTIMIZATION_POINT
  //String sort http://algs4.cs.princeton.edu/lectures/51StringSorts.pdf
  //for LIST of CHARVECS

  //POTENTIAL_OPTIMIZATION_POINT
  //For single pass grades (ascend, "<" etc) could use unstable sort?
  //Unstable (eg MSB->LSB radix) is likely much faster
  //Depends whether user needs
  //Randall Farmer points out: you can replace stable sorts with
  //unstable sorts if you have a guarantee the vector items are unique

  //POTENTIAL_OPTIMIZATION_POINT
  //Better bucket sorting

  //POTENTIAL_OPTIMIZATION_POINT: penny sort
  //Test if every floating point number is eg at worst two decimal places
  //then time by 100 sort as ints and divide by 100.
  //(and no NAN and no INF - or handle them somehow)
  //this is actually really smart:
  //provided floats are smallish (you won't overflow exponent with 1e100 or whatever)
  //create a temp vector: multiply by 1000, or whatever, truncate, return that grade
  //this lets you use regular counting sort on most price vectors 
  //this seems to always be xx.yy  not xx.yzabcdef range(0,100,0.01)
  //maybe read floating-point book and look directly at mantissa

  //POTENTIAL_OPTIMIZATION_POINT:
  //it currently takes us 420ms to sort 10^6 FLOATs. But this could be down
  //to at least 100ms, for sure, and maybe less.
  //idea:
  //get the min and the max. 
  //(during this step we can also see about doing decimal sort)
  //subdivide. count the items. distribute. recurse
  //*this will give you trouble because the min can be apart
  //we could try to fix it by using bucket which 
  //create *averages* during a single linear pass. for c buckets,
  //do n log(c) comparisons, find the average, bucket based on the average
  //add the new integer to the average
  //(take first unused - reorder based on ...)
  //take first unused - or pre-pop so never unused
  //reorder: buckets bubbling left or right Once each value is probably sufficient
  //         why reorder them at all?
  //cap-off/rotate out buckets past n/b items
  // calculate min/max/avg for each bucket

  //POTENTIAL_OPTIMIZATION_POINT
  //This solves the problem of STAMP being too far apart for counting sort
  //You can sort 64-bit INT kinds (and potentially FLOAT via INT conversion)
  //using 4x 2-byte passes of counting sort. (kind of a bucket sort)
  //**this works if you do them as backwards passes, start at LSB**
  //(Randall Farmer suggests via evidence that 8x 1-byte may be faster
  // due to cache friendliness)
  //to handle negatives, potentially convert using conversion to
  //unsigned INT then adding 1/2 INT_MAX or something
  //this method could also be good as a pre-method for regular merge sort

  //POTENTIAL_OPTIMIZATION_POINT
  //personal communication from Bakul Shah:
  //  > For something like grade up/down, it may be better to do a
  //  > sort-merge, where sort is done over a piece that can fit in L1
  //  > cache, followed by a merge. Not clear if there is a superior
  //  > algorithm that works across all sizes.

  //POTENTIAL_OPTIMIZATION_POINT
  //here's a linear strategy that should/may work with some modifications:
  //the idea is array is nearly sorted, but avoid insertion-sort style fix
  //identify the elements that are out of place
  //(wrinkle: is first element out of place or second? both in same pass?)
  //if less < c (== million? magic_number?)  then
  //create sorted list of out of place (de-merge)
  //shift all items sans out-of-places right in the array 
  //merge right-shifted array and out of place starting at
  //leftmost position in the array

  I magic_number = 87654321;

  if(!IS_ARRAY(x) && !(IS_TABLE(x)||IS_MAP(x))) return Ki(0);

  //grade-down cannot leverage ATTR_SORTED by reversing: loses at-equal-index ascending order
  if(SORTED(x) && !r) return til(ki(lenI(x)));

  SW(xt)
  {
    CR(TABLE,   grade_updown_table(x,r))
    CR(BTREE,    grade_updown_from_landis(x, r))
    CR(HASH,    grade_updown_hash(x,r))
    CR(CHARVEC, charGrade(x,r))

    CS(MAP, K g = grade_updown(xValues, r);
            work_push(g);
            K z = at(xKeys, g); 
            work_pop_n_rd(1, true);
            return z;
    )

  }

  if(-INT==xt) //-STAMP usually won't be helped in a plain counting sort since 1s >= 10^9 ns
  {
    I t,u=II,v=-II;//MIN,MAX
    DO(xn, t=xI[i]; if(t<u)u=t; if(t>v)v=t;)  
    //POTENTIAL_OPTIMIZATION_POINT
    //dynamically set this based on system memory, similar to POOL_LANE_MAX
    I allowed = magic_number;
    bool ok = false;
      
#ifdef __int128
    __int128 diff = ((__int128)v) - (__int128)u;//or check diff>=0 for really big int64 differences
    ok = diff < allowed;
#else
    I diff = v-u;
    ok = diff < allowed && diff >= 0;
#endif
      
    if(ok)
    {
      return distributionGrade(x,r,u,v);
    }
  }

  if(-INT==xt || -STAMP==xt)
  {
    return digitalGrade(x, r);
  }

  if(-FLOAT==xt)
  {
    return digitalGrade(x, r);
    //Does not appear to help since the INT sorts can't leverage the transform integers
    //K y = new_k(INTVEC, xn);
    //work_push(y);
    //DO(yn, yI[i] = wellOrderSignedFloat(xF[i]) + 1 + (UINT64_MAX/2))
    //K z = grade_updown(y, r);
    //work_pop_rd(true);
    //return z;
  }

  return mergeGrade(x,r);
}
K grade_up(K x){K z = grade_updown(x,0); za |= (ATTR_SORTED & xa); R z;}
K grade_down(K x){return grade_updown(x,1);}

#pragma mark -

bool is_sorted_vlist(K x)
{
  DO(xn-1, if(!mergerComparer(x,0,i,i+1))return false;)
  SET_ATTR(x, ATTR_SORTED);
  return true;
}

bool is_sorted_array(K x)
{
  //Technically, testing whether a floating-point vector is sorted probably requires
  //checking not only all neighbors, but that the first and last are sorted as well
  ENUM(x, K0 o; if(i==0)continue; if(DESCENDING == KC(LOOK_(x,i-1,o),v)) return false;)
  SET_ATTR(x, ATTR_SORTED);
  return true;
}

int grade_go()
{
  return 0;
  I n = 99;
  K a = til(ki(n)); 
  K b = til(ki(n)); 
  dd(KC(a,b))
  return 0;
}

#pragma mark - BTREE-type Indexed Array Pre-whered

void sort_type_indices_via_comparison(K z, K reference_row, I index, I *table, K *places,
                                        I parent_to_right_was_equal, 
                                        I parent_to_left_was_equal,
                                        bool atlas_key_required,
                                        bool between_override)
{
  if(!index) return; 

  index = ABS(index);

  K x = zIndex;

  LANDIS_NODE *self = xA + index;

  I left  = ABS(self->left);
  I right = ABS(self->right);

  I place = index - 1;

  K obviator = NULL;

  if(atlas_key_required)  //disallow some checks-skipping for atlases
  {
    obviator = reference_row;
  }
  else
  {
    obviator = NULL;
  }


  if(!reference_row) //use to capture whole subtree
  {
    sort_type_indices_via_comparison(z, NULL, left, table, places, 0, 0, atlas_key_required, between_override);
    *places = cow_add(*places, ki(place));
    sort_type_indices_via_comparison(z, NULL, right, table, places, 0, 0, atlas_key_required, between_override);
    return;
  }

  //POTENTIAL_OPTIMIZATION_POINT - Atlas Search
  //After the first few comparisons, the traversal will enter
  //into a point where the keys is [probably going to be] the
  //same. So you can leverage that Atlas keys are stored in an
  //ENUM/INTERN to merely check for equality to a cached number,
  //when checking the first item in the atlas tuple, eg ['b',3.4]
  //This is if the key comparisons are what's slow.

  K place_row = landis_static_row_for_place(z, place, 1);

  C compared = landis_row_compare_override(place_row, reference_row, between_override);

  bool atlas_stuff_ok = true; //Start on (for the main non-atlas cases)
  if(atlas_key_required)
  {
    //POTENTIAL_FEATURE_POINT
    //I'm not positive, but I think hacking on this function to split out partial key matches
    //would give you `select from my_atlas where b` matching on b.c.d and so forth.
    //Maybe this is helpful maybe not.

    bool atlas_key_checked_equal = false;

    if(EQUAL == compared)
    {
      atlas_key_checked_equal = true;
    }
    else
    { 
      //POTENTIAL_OPTIMIZATION_POINT
      //hack landis_row_compare to cache the 0-th item comparison result with *pointer
      //so that we can use it here (from the initial compare above)
      
      K4 o;
      K fake = klist1(kN(reference_row,0),o);
      
      C result = landis_row_compare(place_row, fake);

      atlas_key_checked_equal = (EQUAL == result);
    }
    
    if(!atlas_key_checked_equal)
    {
      atlas_stuff_ok = false;
    }
  }

  SW(compared)
  {
    CS(ASCENDING,
                  if(table[0] && atlas_stuff_ok)
                  {
                    sort_type_indices_via_comparison(z, obviator, left, table, places, 0, 0, atlas_key_required, between_override);

                    *places = cow_add(*places, ki(place));
                  }
                  sort_type_indices_via_comparison(z, reference_row, right, table, places, 0, 0, atlas_key_required, between_override);
    )
    CS(EQUAL,
                  if(parent_to_left_was_equal)
                  {
                    if(table[1]) //take everything to the left, by NULLing out reference_row 
                    {
                      sort_type_indices_via_comparison(z, NULL, left, table, places, 1, 0, atlas_key_required, between_override); 
                    }
                    else ; //drop everything
                  }
                  else if( (table[0] || table[1])   )
                  {
                    sort_type_indices_via_comparison(z, reference_row, left, table, places, 1, 0, atlas_key_required, between_override); 
                  }

                  if(table[1])
                  {
                    *places = cow_add(*places, ki(place));
                  }

                  if(parent_to_right_was_equal)
                  {
                    if(table[1]) //take everything to the right, by NULLing out reference row
                    {
                      sort_type_indices_via_comparison(z, NULL, right, table, places, 0, 1, atlas_key_required, between_override);
                    }
                    else ; //drop everything
                  }
                  else if(table[1] || table[2])
                  {
                    sort_type_indices_via_comparison(z, reference_row, right, table, places, 0, 1, atlas_key_required, between_override);
                  }
    )
    CS(DESCENDING,
                  sort_type_indices_via_comparison(z, reference_row, left, table, places, 0, 0, atlas_key_required, between_override);
                  if(table[2] && atlas_stuff_ok)
                  {
                    *places = cow_add(*places, ki(place));
                    sort_type_indices_via_comparison(z, obviator, right, table, places, 0, 0, atlas_key_required, between_override);
                  }
    )
  }

  return;
}

K sort_type_compare_pre_whered(K x, K y, I *original_table, bool atlas_key_required, bool between_override) //optimized SQL WHERE for type BTREE/indexes
{
  //An atlas_key_required required value of true means that the atlas key in question must be 
  //present in the SORT2 row for the found index to be added. This means that
  //in `select from my_atlas where b > 2` that the index for row ['b',3] would be added,
  //but the index for row ['c',3] would not, as is desired, despite `['c',3] > ['b',2]`.

  I table[3] = {0};

  DO(3, table[i] = original_table[i]);

  K needle = NULL;
  K haystack = NULL;

  if(!IS_BTREE(x))
  {
    needle = x;
    haystack = y;

    //DO(3, table[i] = 1 - table[i]) //flip all
    //table[0] = 1 - table[0]; table[2] = 1 - table[2]; //flip ends

    I temp = table[0]; //swap ends
    table[0] = table[2];
    table[2] = temp;
  }
  else
  {
    needle = y;
    haystack = x;
  }


  K4 o;
  if(IS_STRING(needle)) needle = klist1(needle,o);

  K places = new_k(INTVEC, 0);

  I n = COUNT(haystack);

  if(0 == n) return places;

  LANDIS_NODE *ground = kA(kN(haystack,INDEX)) + 0;

  I root_index = ABS(ground->right);

  //I2 equal = landis_find_equals_index_child_range(haystack, needle);
  sort_type_indices_via_comparison(haystack, needle, root_index, table, &places, 0, 0, atlas_key_required, between_override);

  return places;
}

#pragma mark - BTREE-type Indexed Array Find Methods

I sort_find_min_max_value_place(K z, I option) //0 - min, 1 - max
{
  if(COUNT(z) <= 0)
  {
    //or whatever you want...
    if(option) return II;
    return -II;
  }

  K x = zIndex;

  LANDIS_NODE *ground = xA + 0;

  I root_index = ABS(ground->right);

  I i = root_index;

  I j = i;

  while(i)
  {
    j = i;

    LANDIS_NODE *node = xA + i;

    I left = ABS(node->left);
    I right = ABS(node->right);

    SW(option)
    {
      CS(0, i = left)
      CS(1, i = right)
    }
  }

  return j - 1;
}

I2 landis_find_equals_index_child_range(K z, K needle_row)
{
  K x = zIndex; 

  I top   = landis_find_index_child(z,   1, needle_row, TREE_EQUAL_TOP,   0, 0, 0);

  I left  = landis_find_index_child(z, top, needle_row, TREE_EQUAL_LEFT,  0, 0, 0);
  I right = landis_find_index_child(z, top, needle_row, TREE_EQUAL_RIGHT, 0, 0, 0);

  return (I2){left, right};
}

I landis_find_index_child(K z, I start_child, K needle_row, I equal_behavior, I provided_index, I *stack, I *stack_pointer)
{
  //From this format, to get:
  //CLOSEST index/value if not found: derive parent from child index (easy: int divide ABS)
  //SENTINEL if not found: child's integer will be 0

  //Needle needs to be a row (a LIST).

  K x = zIndex; 

  assert(start_child < COUNT(x));
  assert(start_child > 0);

  I i = start_child; //1 - ground's right child (start looking at root, if populated)

  I node_index = 0;

  while((node_index = ABS(xI[i]))) //while the child link is not null
  {
    I save = i;

    I node_width = sizeof(LANDIS_NODE)/sizeof(I);

    //LANDIS_NODE *r = xA + node_index;
    I left  = node_width * node_index + 0;
    I right = node_width * node_index + 1;

    I cmp = EQUAL;
    I place = node_index - 1;

    //Type of comparison branches
    if(TREE_EQUAL_INDEX == equal_behavior)
    {
      cmp = landis_compare_rows_place(z, provided_index, place);
    }
    else
    {
      I strips_in_question = MIN(z->nn - 1, COUNT(needle_row));
      if(1 == strips_in_question)
      {
        K0 o;
        cmp = KC(kN(needle_row, 0), LOOK_(zKeys, place,o));
      }
      else
      {
        K place_row = landis_static_row_for_place(z, place, 1);
        cmp = landis_row_compare(needle_row, place_row);
      }
    }

    SW(cmp)
    {
      CS(ASCENDING,   i = left )  //less than, go left
      CS(DESCENDING,  i = right)  //greater than, go right
      CD: CS(EQUAL,               //equal, breaks into cases
                SW(equal_behavior)
                {
                  CS(TREE_EQUAL_TOP,   return i) 

                  CS(TREE_EQUAL_INDEX, if     (provided_index < place)
                                       {
                                         i = left;//probably redundant given comparison branch above
                                       }
                                       else if(provided_index > place)
                                       {
                                         i = right; 
                                       }
                                       else
                                       {
                                         return i;
                                       }
                                         
                  )

                  CS(TREE_EQUAL_LEFT,  I ahead = landis_find_index_child(z, left,  needle_row, TREE_EQUAL_LEFT,  provided_index, 0, 0); 
                                       if(xI[ahead]) return ahead;
                                       return i;
                  ) 

                  CS(TREE_EQUAL_RIGHT, I ahead = landis_find_index_child(z, right, needle_row, TREE_EQUAL_RIGHT, provided_index, 0, 0); 
                                       if(xI[ahead]) return ahead;
                                       return i;
                  ) 
                }
      )
    }

    if(stack && stack_pointer)
    {
      stack[*stack_pointer] = i;
      *stack_pointer += 1;
    }

  }
  return i;
}

#pragma mark - Comparing Rows

K landis_row_for_place(K z, I place)
{
  place=ABS(place);

  K x = new_k(LIST, z->nn - 1);
  xn = 0;
  NEST(z, if(0==i) continue; K0 o; K entry = LOOK_(v, place,o); x = cow_add(x, entry))

  return x;
}

static char landis_row_static1[sizeof(K0)*257];
static char landis_row_static2[sizeof(K0)*257];
K landis_static_row_for_place(K z, I place, I which)
{
  place=ABS(place);

  K x = (V)&landis_row_static1;
  if(which) x = (V)&landis_row_static2;

  x->m = 11;   
  x->t = LIST;
  x->n = z->nn - 1; 

  NEST(z, if(0==i) continue;
          K0 o;
          K entry = LOOK_(v, place,o);
          assert(BTREE != entry->t);//nested won't work with static
          nestset_ri_rd(x, i-1, entry, false, false)
          )

  return x;
}

char landis_row_compare(K p_row, K q_row)
{
  return landis_row_compare_override(p_row, q_row, false);
}


char landis_row_compare_override(K p_row, K q_row, bool between_override)
{
  if(between_override)
  {
    I p_count = COUNT(p_row);
    I q_count = COUNT(q_row); 

    assert(1==p_count);
    assert(1<=q_count);

    K0 o1,o2,o3;
    K place = LOOK_(p_row, 0,o1);

    I right_index = q_count > 1 ? 1 : 0; //use same thing twice if busted

    K left  = LOOK_(q_row, 0, o2);
    K right = LOOK_(q_row, right_index,o3);

    if(ASCENDING  == LC(place, left))  return ASCENDING;
    if(DESCENDING == LC(place, right)) return DESCENDING;

    return EQUAL;
  }

  I n = MIN(COUNT(p_row), COUNT(q_row));

  DO(n, K0 o1,o2; C c = KC_NUM(LOOK_(p_row, i, o1), LOOK_(q_row, i, o2)); if(EQUAL != c) return c;)

  return EQUAL;
}

char landis_compare_rows_place(K z, I p, I q)
{
  p=ABS(p);
  q=ABS(q);

  C result = EQUAL;

  if(p == q) return EQUAL;
    
  if(1 == z->nn - 1)
  {
      K0 o1,o2;
      result = KC(LOOK_(zKeys,p,o1),LOOK_(zKeys, q, o2));
  }
  else
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //KC on each instead of static row from strip building

    K p_row = landis_static_row_for_place(z, p, 0);
    K q_row = landis_static_row_for_place(z, q, 1);
    //work_push(p_row);
    //work_push(q_row);
    
    result = landis_row_compare(p_row, q_row);
    
    //work_pop_n_rd(2, true);
  }
    

  if(EQUAL == result)
  {
    result = IC(p, q);//equal rows? distinguish by place
  }

  return result;
}


//#define      LINK(node,x)     (node)->c[((x)+1)>>1]  // indices: -1 => 0 , 1 => 1
#define SAFE_LINK(node,x) ABS((node)->c[((x)+1)>>1]) // indices: -1 => 0 , 1 => 1
#define BALANCE(node) ( ((node)->left < 0) ? -1 : ((node)->right < 0) ? 1 : 0)
#define PLACE(x,y) (-1 + y - kA(x))

ALWAYS_INLINE void SET_LINK(LANDIS_NODE *node, char child, I place)
{
  assert(child);

//  char balance = BALANCE(node); //preserve balance

  child = (child + 1) >> 1; // indices: -1 => 0 , 1 => 1

  node->c[child] = ABS(place);

//  SET_BALANCE(node, balance); //restore balance
}

ALWAYS_INLINE void SET_BALANCE(LANDIS_NODE *n, char balance)
{
  I left  = 1;  
  I right = 1;  

  SW(balance)
  {
    CS(-1,  left = -1;)//  assert(n->left  != 0)) 
    CS( 1, right = -1;)//  assert(n->right != 0))
  }

  n->left  = ABS(n->left)  * left;
  n->right = ABS(n->right) * right;
}

I landis_index_add_node_for_position(K z, I place)
{
  //assumes it's passed a position referring to a valid object for entry 
  //(exists, passes any unique tests, etc.)

  //pessimistic POTENTIAL_OPTIMIZATION_POINT
  //I am no longer optimistic about this but I think
  //applying some form of VEB order to the inserts can help
  //(in the triples case). but the branch rebalancing will screw
  //this up until it stabilizes. I doubt this method will work THAT well.
  //though it might be nice to apply on the pre-sort.
  //It is a bit like robin-hooding for trees.
  //however, you need triples instead of doubles (to store the place explicitly)
  //and you still don't get to VEB search the ordered time-series payload
  //so it's probably a pointless, space-wasting exercise

  I next_index = COUNT(zIndex)/(sizeof(LANDIS_NODE)/sizeof(I));
  I adding_index = 1 + place;

  assert(adding_index <= next_index);

  if(adding_index == next_index)//adding a new node instead of revising an old [deleted] one
  {
    K4 o;
    nestneu(z, INDEX, cow_join(zIndex, kdoublet(o)));
  }

  K x = zIndex;

  I ground_index  = 0;

  LANDIS_NODE *ground = xA + ground_index;
  LANDIS_NODE *adding  = xA + adding_index;

  I t_index       = ground_index;
  I s_index       = ground->right;
  I p_index       = ground->right;
  I q_index       = ground->right;
  I r_index;

  LANDIS_NODE *t      = xA + t_index;
  LANDIS_NODE *s      = xA + s_index;
  LANDIS_NODE *p      = xA + p_index;
  LANDIS_NODE *q      = xA + q_index;
  LANDIS_NODE *r;

  *adding = (LANDIS_NODE){0, 0};

  //AVL - See Knuth Algorithm 6.2.2T

  if(IS_LANDIS_NULL(ground->right)) //root node not set
  {
    ground->right = adding_index;
    return adding_index;// 1
  }


  while(!IS_LANDIS_NULL(q_index))
  { 
    char a = landis_compare_rows_place(z, place, PLACE(x,p));

    q_index = SAFE_LINK(p, a);
    q = xA + q_index;

    if(IS_LANDIS_NULL(q_index))
    {
      q_index = adding_index;
      q = xA + q_index;

      SET_LINK(p, a, q_index);

      break;
    }
    else if(BALANCE(q))
    {
      t_index = p_index; t=p;
      s_index = q_index; s=q;
    }

    p_index = q_index; p = q;
  }
  char sbalance = BALANCE(s);

  char a = landis_compare_rows_place(z, place, PLACE(x,s));
  assert(EQUAL != a);
  if(EQUAL == a) a = 1;//skew right. but technically EQUAL cannot result

  p_index = SAFE_LINK(s, a); p = xA + p_index;

  r_index = p_index; r = p;

  while(p_index != q_index)
  {
    char b = landis_compare_rows_place(z, place, PLACE(x,p));
    SET_BALANCE(p, b);
    assert(b == BALANCE(p));
    p_index = SAFE_LINK(p, b); p = xA + p_index;
  }

  assert(p_index);

  if(!sbalance)
  {
    SET_BALANCE(s, a);
    return p_index;
  }
  else if(sbalance == -a)
  {
    SET_BALANCE(s, 0);
    return p_index;
  }

  char rbalance = BALANCE(r);
  if(rbalance == a)
  {
    p_index = r_index; p = xA + p_index;

    SET_LINK(s,  a, SAFE_LINK(r, -a));
    SET_LINK(r, -a, s_index);
    SET_BALANCE(s, 0);
    SET_BALANCE(r, 0);
  }
  else if(rbalance == -a)
  {
    p_index = SAFE_LINK(r, -a); p = xA + p_index;
    char pbalance = BALANCE(p);

    SET_LINK(r, -a, SAFE_LINK(p,  a));
    SET_LINK(p,  a, r_index);
    SET_LINK(s,  a, SAFE_LINK(p, -a));
    SET_LINK(p, -a, s_index);

    if(pbalance == a)
    {
      SET_BALANCE(s, -a);
      SET_BALANCE(r,  0);
    }
    else if(pbalance == 0)
    {
      SET_BALANCE(s, 0);
      SET_BALANCE(r, 0);
    }
    else if(pbalance == -a)
    {
      SET_BALANCE(s, 0);
      SET_BALANCE(r, a);
    }

    SET_BALANCE(p, 0);
  }

  char tbalance = BALANCE(t);

  if(s_index==SAFE_LINK(t, 1))
  {
    SET_LINK(t, 1, p_index);
  }
  else
  {
    SET_LINK(t, -1, p_index);
  }

  SET_BALANCE(t, tbalance);

  return q_index;
}

ALWAYS_INLINE void SAFE_STACK_CHILD_SET(K x, I *stack, I k, I value)
{
    assert(k >= 0);

    I parent_index = ABS(stack[k] / 2);
    LANDIS_NODE *parent = xA + parent_index;
    C parent_balance = BALANCE(parent);

    xI[stack[k]] = value;

    SET_BALANCE(parent, parent_balance);
}

void landis_index_delete_node_for_position(K z, I place) //could return deleted value
{
  I deleting_index = 1 + place;

  assert(place >= 0);
  assert(deleting_index < COUNT(zIndex)/(sizeof(LANDIS_NODE)/sizeof(I)));

  //Assumes you're only deleting an index within range
  //It's OK for any numbers of deletions, including it, to have already occured, in any order

  K x = zIndex;
  LANDIS_NODE *ground = xA;

#define LANDIS_HEIGHT_BOUND (128) //inexhaustible height for sizeof(I)==8. <= ~96 probably works
  I stack[LANDIS_HEIGHT_BOUND]; 
  C balance_stack[LANDIS_HEIGHT_BOUND];
  I k = 0;


  bool push_ground = true;
  if(push_ground)
  {
    stack[0]    = 1;
    k++;
  }

  int result = EQUAL;

  I child_index = landis_find_index_child(z, 1, NULL, TREE_EQUAL_INDEX, place, (I*)&stack, &k);

  DO(k, balance_stack[i] = BALANCE(xA + ABS(xI[stack[i]])))  

  I node_index = ABS(xI[child_index]);

  I node_width = sizeof(LANDIS_NODE)/sizeof(I);

  assert(node_index < COUNT(x)/node_width);

  assert(node_index);
  //if(!node_index) //not found, inexplicably
  //{
  //  return; 
  //}

  //LANDIS_NODE *ancestors[LANDIS_HEIGHT_BOUND];
  //DO(k, ancestors[i] = xA + ABS(xI[stack[i]]))

  I found_index = node_index;
  LANDIS_NODE *found = xA + node_index;
  char fbalance = BALANCE(found);

  //Does found have a right subtree?
  if(!found->right)
  {
    SAFE_STACK_CHILD_SET(x, stack, k - 1, found->left);
  }
  else
  {
    I r_index = ABS(found->right);
    LANDIS_NODE *r = xA + r_index;

    //Does found's right subtree have a left subtree?
    if(!r->left)
    {
      r->left = found->left;

      SAFE_STACK_CHILD_SET(x, stack, k - 1, r_index);

      SET_BALANCE(r, fbalance); 

      stack[k] = node_width * r_index + 1;
      balance_stack[k] = fbalance;
      k++;
    }
    else
    {
      I s_index;
      LANDIS_NODE *s;

      I j = k;
      k++;

      I pre_index = ABS(found_index);
      LANDIS_NODE *pre = xA + pre_index; 

      //stack[j] = -1;//unnecessary...merely ensure stack is not being used
      //balance_stack[j] = 99;

      while(1)
      {
        stack[k] = node_width * r_index + 0;
        balance_stack[k] = BALANCE(xA + ABS(xI[stack[k]]));

        k++;

        s_index = ABS(r->left);
        s = xA + s_index;

        if(!s->left) break;

        r_index = s_index;
        r = xA + r_index;
      }

      I par_index = ABS(stack[j - 1] / 2);
      LANDIS_NODE *par = xA + par_index;
      C parbalance = BALANCE(par);

      char rbalance = BALANCE(r);

      s->left  = found->left;
      r->left  = s->right;  //XXX balance ok here?
      s->right = found->right;

      xI[stack[j - 1]] = s_index; 

      SET_BALANCE(s, fbalance);
      SET_BALANCE(r, rbalance);
      SET_BALANCE(par, parbalance);

      stack[j] = node_width * s_index + 1;
      balance_stack[j] = BALANCE(xA + ABS(xI[stack[j]]));
    }
  }

  found->left  = 0;
  found->right = 0;
  //found->place = 0;

  assert (k > 0);

  while (--k > 0)
  {
    I y_index = ABS(xI[stack[k-1]]); //I y_index = ABS(stack[k] / 2);
    LANDIS_NODE *y = xA + y_index;
    char ybalance = balance_stack[k-1];

    bool went_left = (0 == (stack[k] % 2));

    if (went_left)
    {
      ybalance++;

      if (1 == ybalance)
      {
        SET_BALANCE(y, ybalance);
        if(BALANCE(y)) break;//if check is necessary so our balance-bit-storage method works with case 3
      }
      else if(2 == ybalance)
      {
        I z_index = ABS(y->right); 
        LANDIS_NODE *z = xA + z_index;

        char zbalance = BALANCE(z);

        if (-1 == zbalance)
        {
          I w_index = ABS(z->left); 
          LANDIS_NODE *w = xA + w_index;

          char wbalance = BALANCE(w);

          z->left  = w->right;
          w->right = z_index;
          y->right = w->left;
          w->left  = y_index;

          SAFE_STACK_CHILD_SET(x, stack, k - 1, w_index);

          SW(wbalance)
          {
            CS(+1,  SET_BALANCE(z, 0); SET_BALANCE(y,-1))
            CS( 0,  SET_BALANCE(z, 0); SET_BALANCE(y, 0))
            CD:
            CS(-1,  SET_BALANCE(z,+1); SET_BALANCE(y, 0))
          }

          SET_BALANCE(w, 0);
        }
        else
        {
          char zbalance = BALANCE(z);

          y->right = z->left;
          z->left  = y_index;

          SAFE_STACK_CHILD_SET(x, stack, k - 1, z_index);

          if(0 == zbalance)
          {
            SET_BALANCE(z, -1);
            SET_BALANCE(y,  1);
            break;
          }
          else
          {
            SET_BALANCE(z, 0);
            SET_BALANCE(y, 0);
          }
        }
      }
      else
      {
        SET_BALANCE(y, ybalance);
      }
    }
    else
    {
      ybalance--;

      if (-1 == ybalance)
      {
        SET_BALANCE(y, ybalance);
        break;
      }
      else if (-2 == ybalance)
      {
        I z_index = ABS(y->left); 
        LANDIS_NODE *z = xA + z_index;

        char zbalance = BALANCE(z);

        if (1 == zbalance)
        {
          I w_index = ABS(z->right); 
          LANDIS_NODE *w = xA + w_index;

          char wbalance = BALANCE(w);

          z->right = w->left;
          w->left  = z_index;
          y->left  = w->right;
          w->right = y_index;

          SAFE_STACK_CHILD_SET(x, stack, k - 1, w_index);

          SW(wbalance)
          {
            CS(-1, SET_BALANCE(z, 0); SET_BALANCE(y, +1))
            CS( 0, SET_BALANCE(z, 0); SET_BALANCE(y,  0))
            CD:
            CS( 1, SET_BALANCE(z,-1); SET_BALANCE(y,  0))
          }

          SET_BALANCE(w, 0);
        }
        else
        {
          y->left  = z->right;
          z->right = y_index;

          SAFE_STACK_CHILD_SET(x, stack, k - 1, z_index);

          if (0 == zbalance)
          {
            SET_BALANCE(z, 1);
            SET_BALANCE(y,-1);
            break;
          }
          else
          {
            SET_BALANCE(z, 0);
            SET_BALANCE(y, 0);
          }
        }
      }
      else
      {
        SET_BALANCE(y, ybalance);    
      }
    }
  }

  return;
}

K _alter_tree_single(K x, I k, K y)
{
  //this could/should be:
  //return _alter_tree_general(x, k, klist1(y));

  landis_index_delete_node_for_position(x, k);
  //nestneu(x,KEYS,alter_array_single(xKeys,k,y)); 
  nestneu(x,KEYS,update(xKeys, ki(k), y)); 
  landis_index_add_node_for_position(x, k);
  return x;
}

K _alter_tree_general(K x, I k, K row)
{
  landis_index_delete_node_for_position(x, k);
  //nestneu(x,KEYS,alter_array_single(xKeys,k,y)); 
  ENUM(row, nestneu(x, i+1, update(kN(x,i+1), ki(k), v)))
  landis_index_add_node_for_position(x, k);
  return x;
}

K cow_sort_add_single(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //special case this. remember any can_add checks.
  K4 o;
  return cow_sort_add_general(x, klist1(y,o));
}

K cow_sort_add_general(K x, K row)
{
  if(!landis_can_add_row(x,row)) goto failed;

  x=cow(x);
  nestneu(x, INDEX, cow(xIndex));//could potentially remove this check (is it possible to ref-increment index?)
  ENUM(row, nestneu(x, i+1, cow_add(kN(x,i+1),v)))
  landis_index_add_node_for_position(x, COUNT(xKeys) - 1);

  return x;

failed:
  return x;
}

bool landis_has_row(K z, K needle_row)
{
  I top = landis_find_index_child(z, 1, needle_row, TREE_EQUAL_TOP, 0, 0, 0);

  K x = zIndex;

  bool presence = (0 != xI[top]);

  return presence;
}

bool landis_can_add_row(K x, K y)
{
  I unique = GET_ALT_ATTR(x,SORT_ATTR_UNIQUE);

  I allowedNulls = II;//0,1,II

  if(GET_ALT_ATTR(x,SORT_ATTR_NONNULL))
  {
    allowedNulls = 0;
  }

  if(unique || allowedNulls < II)
  {
    if(0==allowedNulls && isnull(y))
    {
      goto failed;
    }

    I a = landis_has_row(x, y);

    if (a)
    {
      goto failed;
    }
  }

  return true;
failed:
  return false;
}

K grade_updown_landis_recursive(K z, I descending, I index, K *grades) 
{
  index=ABS(index);

  if(0==index) return *grades;

  K x = zIndex;

  LANDIS_NODE *self = xA + index; 

  I left  = ABS(self->left);
  I right = ABS(self->right);

  //in-order traversal
  if(!descending)
  {
    if(left) grade_updown_landis_recursive(z, descending, left, grades);
    *grades = cow_add(*grades, ki(index - 1));
    if(right) grade_updown_landis_recursive(z, descending, right, grades);
  }
  else
  {
    if(right) grade_updown_landis_recursive(z, descending, right, grades);
    *grades = cow_add(*grades, ki(index - 1));
    if(left) grade_updown_landis_recursive(z, descending, left, grades);
  }

  return *grades;
}

K grade_updown_from_landis(K z, I descending)
{
  I count = COUNT(z);

  K grades = new_k(INTVEC, count); //prealloc

  if(0 == count) return grades; 

  grades->n = 0;

  K x = zIndex;
  LANDIS_NODE *ground = xA; 

  I root = ABS(ground->right); 

  return grade_updown_landis_recursive(z, descending, root, &grades); 
}

#pragma mark - Testing

bool landis_is_sorted(K z)
{
  K grades = grade_updown_from_landis(z, 0);
  work_push(grades);

  //not yet checked: rows/strips in AT
  K reveal = at(zKeys, grades);
  work_push(reveal);

  bool sorted = is_sorted_array(reveal); 

  if(!sorted)
  {
    fprintf(stderr, "Bad sort\n");
    show(z);
    show(grades);
  }

  work_pop_n_rd(2, true);

  return sorted;
}

bool landis_unreachable_place(K z, I place)
{
  if(COUNT(z) < 1) return true;

  K x = zIndex;
  LANDIS_NODE *ground = xA; 

  return landis_unreachable_node(z, ground->right, place + 1);
}

bool landis_unreachable_node(K z, I haystack, I needle)
{
  if(!haystack) return true;
 
  haystack = ABS(haystack);
  needle   = ABS(needle);

  if(haystack == needle) return false;

  K x = zIndex;
  LANDIS_NODE *self = xA + haystack; 

  return landis_unreachable_node(z, self->left,  needle) &&
         landis_unreachable_node(z, self->right, needle);
}

bool landis_nodes_ok(K z, I a, I b)
{
  a=ABS(a);
  b=ABS(b);
  if(!a || !b) return true;

  K x = zIndex;

  LANDIS_NODE *A = xA + a;
  LANDIS_NODE *B = xA + b;

  char c = landis_compare_rows_place(z, PLACE(x,A), PLACE(x,B));

  if(DESCENDING == c) return false;

  return true;
}

bool landis_node_balance_ok(K z, I index)
{
  index=ABS(index);

  if(0==index) return true;

  K x = zIndex;

  LANDIS_NODE *self = xA + index; 

  I left  = landis_node_height(z, self->left);
  I right = landis_node_height(z, self->right);

  I diff = right - left;

  if(ABS(diff) > 1)
  {
    fprintf(stderr, "Bad height diff: %lld at node %lld (place %lld) \n", diff, index, index - 1);
    return false;
  }

  char balance = BALANCE(self);

  if(balance != diff)
  {
    fprintf(stderr, "Balance does not match height diff: balance: %d diff: %lld at node %lld (place %lld) \n", balance, diff, index, index - 1);
    return false;
  }

  return landis_node_balance_ok(z, self->left) && landis_node_balance_ok(z, self->right);
}

bool landis_balance_ok(K z)
{
  if(COUNT(z) < 1) return true;

  K x = zIndex;
  LANDIS_NODE *ground = xA; 

  return landis_node_balance_ok(z, ground->right);
}

I landis_node_reachable(K z, I index)
{
  index=ABS(index);

  if(0 == index) return 0;

  K x = zIndex;
  LANDIS_NODE *me = xA + index;

  return 1 + landis_node_reachable(z, me->left) + landis_node_reachable(z, me->right);
}

I landis_reachable_count(K z)
{
  if(COUNT(z) < 1) return 0;

  K x = zIndex;
  LANDIS_NODE *ground = xA; 

  return landis_node_reachable(z, ground->right);

}

char landis_node_height(K z, I index)
{
  index=ABS(index);

  if(0 == index) return 0;
  
  K x = zIndex;
  LANDIS_NODE *me = xA + index;

  return 1 + MAX(landis_node_height(z, me->left),landis_node_height(z, me->right));
}

char landis_height(K z)
{
  if(COUNT(z) < 1) return 0;

  K x = zIndex;
  LANDIS_NODE *ground = xA; 

  return landis_node_height(z, ground->right);
}

char landis_is_sane(K z)
{
  I n = COUNT(zKeys);

  I height = landis_height(z);
  bool height_sane = (height < (1.6 * ceiling_log_2(n) + 2));
  if(!height_sane)
  {
    dd(height)
    dd(n)
    return false;
  }
  I reachable = landis_reachable_count(z);
  bool reachable_sane = (reachable == n);
  if(!reachable_sane)
  {
    dd(reachable)
    dd(n)
    return false;
  }

  bool balanced = landis_balance_ok(z);

  if(!balanced)
  {
    er(not balanced)
    return false;
  }

  return true;
}

bool test_landis()
{
  //To really test inserts turn off the preindexing flag...
  if(!test_landis_preindexing()) return false;
  if(!test_landis_deletes())     return false;
  if(!test_landis_alter())       return false;

  return true;
}

bool test_landis_alter()
{
  I square = 256; 
  K a = new_btree();
  TIME(DO(pow(2,20), a = cow_sort_add_single(a, ki(rF()*100))))
  rd(a);

  a = new_btree();
  TIME(DO(square, a = cow_sort_add_single(a, ki(rF()*100))))
  if(!landis_is_sorted(a)) return false;
  if(!landis_is_sane(a)) return false;

  DO(square,
    _alter_tree_single(a, i, ki(rF()*100));
    if(!landis_is_sorted(a)) return false;
    if(!landis_is_sane(a)) return false;
  )

  DO(square,
    _alter_tree_single(a, rF()*COUNT(a), ki(rF()*100));
    if(!landis_is_sorted(a)) return false;
    if(!landis_is_sane(a)) return false;
  )
  return true;
}

bool test_landis_deletes()
{
  I max    = 1000000;
  I square = 100;

  I height = 0;
  I reachable = 0;

  DO(square,  if(!(i%20)) dd(i);
              K x = til(ki(i)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;

              I q = i;

              ENUM(x, I p = vi; 
                landis_index_delete_node_for_position(a, p);
                assert(landis_unreachable_place(a, p));
                assert(landis_reachable_count(a) == q - (i + 1));
                assert(landis_balance_ok(a));
              )

              rd(x);
              rd(a);
  )

  DO(square,  if(!(i%20)) dd(i);
              K x = verb_rand(ki(i), kf(10000.0)); 
              K y = til(ki(i)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              I q = i;
              ENUM(y, I p = vi; 
                landis_index_delete_node_for_position(a, p);
                assert(landis_unreachable_place(a, p));
                assert(landis_reachable_count(a) == q - (i + 1));
                assert(landis_balance_ok(a));
              )

              rd(x);
              rd(y);
              rd(a);
  )

  DO(square,  if(!(i%20)) dd(i);
              K x = verb_rand(ki(i), kf(10000.0)); 
              K y = verb_deal(ki(i), ki(i)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              I q = i;
              ENUM(y, I p = vi; 
                landis_index_delete_node_for_position(a, p);
                assert(landis_unreachable_place(a, p));
                assert(landis_reachable_count(a) == q - (i + 1));
                assert(landis_balance_ok(a));
              )

              rd(x);
              rd(y);
              rd(a);
  )

  DO(32,  I q = i; if(0==(i%5))dd(i)
  DO2(1000,   
              K x = verb_rand(ki(q), kf(10000.0)); 
              K y = verb_deal(ki(q), ki(q)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              ENUM(y, I p = vi; 
                landis_index_delete_node_for_position(a, p);
                assert(landis_unreachable_place(a, p));
                assert(landis_reachable_count(a) == q - (i + 1));
                assert(landis_balance_ok(a));
              )

              rd(x);
              rd(y);
              rd(a);
  ))

  DO(128,  I q = i; if(!(i%20)) dd(i)
  DO2(1,   
              K x = verb_rand(ki(q), kf(10000.0)); 
              K y = verb_deal(ki(q), ki(q)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              ENUM(y, I p = vi; 
//O("Deleting place: %lld\n", p);
//show(kN(a,INDEX));
//O("\n");
                landis_index_delete_node_for_position(a, p);
                assert(landis_unreachable_place(a, p));
                assert(landis_reachable_count(a) == q - (i + 1));
                assert(landis_balance_ok(a));
              )

              rd(x);
              rd(y);
              rd(a);
  ))

  return true;
}

bool test_landis_preindexing()
{
  I max    = 1000000;
  I square = 1000;

  I height = 0;
  I reachable = 0;

  DO(square,  if(!(i%200))dd(i);
              K x = til(ki(i)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              rd(x);
              rd(a);
  )

  DO(square,  if(!(i%200))dd(i);
              K x = verb_rand(ki(i), kf(10000.0)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              rd(x);
              rd(a);
  )

  DO(square,  if(!(i%200))dd(i);
              K x = verb_rand(ki(i), ki(10)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              rd(x);
              rd(a);
  )

  DO(square,  if(!(i%200))dd(i);
              K x = verb_rand(ki(10), kf(10000.0)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              rd(x);
              rd(a);
  )

  DO(square,  if(!(i%200))dd(i);
              K x = verb_rand(ki(20), kf(10000.0)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              rd(x);
              rd(a);
  )

  DO(square,  if(!(i%200))dd(i);
              K x = verb_rand(ki(100), kf(10000.0)); 
              K a = new_btree_from_K(x);
              if(!landis_is_sorted(a)) return false;
              if(!landis_is_sane(a)) return false;
              rd(x);
              rd(a);
  )


  return true;

  K x = til(ki(max));
  K z = reverse(x);
  K a = NULL;
  TIME( a = new_btree_from_K(x);)
  if(!landis_is_sorted(a)) return false;
  if(!landis_is_sane(a)) return false;

  rd(x);
  rd(a);

  TIME( a = new_btree_from_K(z);)
  if(!landis_is_sorted(a)) return false;
  if(!landis_is_sane(a)) return false;
  rd(z);
  rd(a);

  K y = verb_rand(ki(max), kf(1000.0));
  TIME( a = new_btree_from_K(y);)
  if(!landis_is_sorted(a)) return false;
  if(!landis_is_sane(a)) return false;
  rd(y);
  rd(a);

  return true;
}
#pragma mark - 

K new_btree_from_K(K x)
{
  if(IS_BTREE(x)) return strong(x);
  K4 o;
  return new_btree_from_strips(klist1(x,o));
}

K new_btree_from_strips(K list)
{
  C strips = COUNT(list);
  assert(strips > 0);
  //later: 255? simply interpret as UC? if z[0] was INT before z[1] INDEX: infty strips
  //(will break NEST() enumerator, rd() freeing, static rows at least: easy to repair)
  //actually, a few places you could store it: 
  //INDEX node 0/ground node's place child (for triples) or left child (always unused)
  //special case out the rd free condition if up
  assert(strips + 1 <= 127); 

  //TODO: assert all strips equal count (we'll use x's count for now)

  K x = kN(list, 0);

  I n = cx;
  K z = new_k(LIST, strips + 1);
  z->n = 0;

  I nodes = 1 + n; //"holder" node at 0, tree-root at 1
  K index = new_k(INTVEC, 2*nodes);//pre-alloc
  OFF_ATTR(index, ATTR_SORTED);
  kA(index)[0] = NULL_NODE;
  index->n = 2;

  z = cow_add(z, index);
  rd(index);

  ENUM(list, z = cow_add(z, v))
  //in this context what we want coerce to do is FORCE a VECTOR or LIST
  //but not anything else
  DO(strips, nestneu(z, 1+i, cow_coerce_array(kN(z,1+i))))

  zt = BTREE;
  zn = 0;
  zw = 0; //dictionary compiled-reference-count
  z->nn = strips + 1;
  OFF_ATTR(z, ATTR_SORTED);

  z = cow_landis_reindex(z);

  return z;
}

K cow_landis_reindex(K z)
{
  z = cow(z);
  nestneu(z, INDEX, cow(zIndex));

  I n = COUNT(zKeys);

  OFF_ATTR(zIndex, ATTR_SORTED);
  kA(zIndex)[0] = NULL_NODE;
  zIndex->n = 2;

  I pre_sort_list = true;
  if(!pre_sort_list)
  {
    DO(n, landis_index_add_node_for_position(z,i);)
  }
  else
  {
    K grades = NULL;
     
    bool checksForSorted = false;//it's not really faster to check...

    //Make a faux table and grade it
    K x = new_table();
    NEST(z, if(0==i)continue; x = cow_table_add_column(x, ki(i), v))
    work_push(x);
    grades = grade_up(x);
    work_pop_rd(true);
   
    landis_index_from_grades(z, grades); 
    rd(grades);
  }

  return z;
}

I landis_index_from_grades_recursive(K z, I link, I low, I high, K grades)
{
  K x = zIndex;

  if(high < low)
  {
    xI[link] = 0;
    return 0;
  }

  I mid = low + ((high - low) / 2);

  I place = kI(grades)[mid];

  I node_index = 1 + place;

  I node_width = (sizeof(LANDIS_NODE)/sizeof(I));

  xI[link] = node_index;
  xn += node_width;

  I left = node_index * node_width;
  I right = left + 1;

  I left_height  = landis_index_from_grades_recursive(z, left,  low,     mid - 1, grades);
  I right_height = landis_index_from_grades_recursive(z, right, mid + 1, high,    grades);

  I height = 1 + MAX(left_height, right_height);

  I balance = right_height - left_height;

  assert(ABS(balance) < 2);

  //POTENTIAL_OPTIMIZATION_POINT
  //the sort is the time-dominating factor in pre-indexing, but potentially
  //maybe after that's fixed it's possible jumping back to set the balance
  //is slowing things down, in which case if you can compute the balance
  //analytically you can do this at the node instead of later in the recursion.
  //it might be something like 1 + 1 + each child a 2^n - 1 size or it might
  //be something much trickier.
  //i doubt now that the current code slows things down appreciably
  //in total, the pre-indexing process takes 30ms at 2**20 nodes on top
  //of whatever the sorting process takes

  if(0 != balance)
  {
    LANDIS_NODE *node = xA + node_index;
    SET_BALANCE(node, balance);
  }

  return height;
}

I landis_index_from_grades(K z, K grades)
{
  I n = COUNT(z);

  K x = zIndex;

  if(n <= 0)
  {
    xA[0] = NULL_NODE;
    return 0;
  }

  LANDIS_NODE *ground = xA + 0;

  I low  = 0;//0-indexed
  I high = n - 1;

  I ground_right_link = 1;

  I height = landis_index_from_grades_recursive(z, ground_right_link, low, high, grades);

  return height;
}


int run_landis()
{
  dd(test_landis())

  return 0;
}

