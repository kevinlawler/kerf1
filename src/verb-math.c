#include "kerf.h"

#pragma mark - Comparison Verbs

K equals   (K x, K y){ return tri_compare(x, y, (I*)(I[3]){0,1,0}); }
K less     (K x, K y){ return tri_compare(x, y, (I*)(I[3]){1,0,0}); } 
K greater  (K x, K y){ return tri_compare(x, y, (I*)(I[3]){0,0,1}); }
K lesseq   (K x, K y){ return tri_compare(x, y, (I*)(I[3]){1,1,0}); }
K greatereq(K x, K y){ return tri_compare(x, y, (I*)(I[3]){0,1,1}); }
K noteq    (K x, K y){ return tri_compare(x, y, (I*)(I[3]){1,0,1}); }

K    equals_pre(K x, K y){ return tri_compare_optimized(x, y, (I*)(I[3]){0,1,0}); }
K      less_pre(K x, K y){ return tri_compare_optimized(x, y, (I*)(I[3]){1,0,0}); } 
K   greater_pre(K x, K y){ return tri_compare_optimized(x, y, (I*)(I[3]){0,0,1}); }
K    lesseq_pre(K x, K y){ return tri_compare_optimized(x, y, (I*)(I[3]){1,1,0}); }
K greatereq_pre(K x, K y){ return tri_compare_optimized(x, y, (I*)(I[3]){0,1,1}); }
K     noteq_pre(K x, K y){ return tri_compare_optimized(x, y, (I*)(I[3]){1,0,1}); }


K between_pre(K x, K y) //pre_whered
{
  //[inclusive, inclusive]

  K4 o;
  if(IS_STRING(y) || !IS_ARRAY(y)) y = klist2(y, y,o);

  if(!IS_ARRAY(y) || 2 != lenI(y)) ERROR(ERROR_ARRAY);

  //POTENTIAL_OPTIMIZATION_POINT
  //see tri_compare_pre_whered for an example of how to proceed
  //may need to update whatever magic landis stuff BTREE is using
  if(IS_ATLAS(x)) return ex("which ($1 >= $2[0] ) & ($1 <= $2[1])", x, y); 

  K0 o1,o2;
  K left  = LOOK_(y,0, o1); 
  K right = LOOK_(y,1, o2); 

  bool pre_check_args = true;
  if(pre_check_args)
  {
    //if arguments are disorded, empty list must result
    if(DESCENDING == LC(left, right))
    {
      return new_k(INTVEC, 0);
    }
  }

  //cheating a little bit =X
  //to handle int v. float comparisons ("numeric")
  F left_float   =  left->t == FLOAT ?  left->f :  left->i;
  F right_float  = right->t == FLOAT ? right->f : right->i;

  if(SORTED(x))
  {

    I n = cx;

    bool check_first_and_last = true;
    if(check_first_and_last)
    {
      K0 o1,o2;
      if(0==n || ASCENDING==LC(right, LOOK_(x,0, o1)) || ASCENDING==LC(LOOK_(x,n-1, o2),left))
      {
        return new_k(INTVEC, 0);
      }
    }

    I2 range0 = search_equal_range(left, x);

    //POTENTIAL_OPTIMIZATION_POINT
    //we reduce the range to "half" of the interval (reduced) and do another search.
    //(dependent on pre_check_args)
    //if you were really mincing about it, you could get both ranges
    //in one pass by changing search_equal_range family to accept
    //"between" style arguments (2 needles instead of one)
    
    I mid_bound = 0;

    if(pre_check_args)
    {
      mid_bound =  range0.y;
    }

    I2 reduced = {mid_bound, n};

    I2 range1 = search_equal_range_within_range(right, x, reduced); 

    range0.y = range1.y;

    I *table = (I*)(I[3]){0,1,0}; //equals

    //POTENTIAL_OPTIMIZATION_POINT
    //we can change this item below to return a boolean char vector of 0,1
    //instead of a 64-bit INTVEC of indices, when the range is a 
    //sufficiently high proportion of the length of the list

    return indices_from_n_equal_range_and_comparison_truthtable(lenI(x), range0, table);
  }

  K z = new_k(INTVEC, 0);

  //POTENTIAL_OPTIMIZATION_POINT
  //special case other types

  SW(xt)
  {
    CS(BTREE,     rd(z);
                 I *table = (I*)(I[3]){0,1,0}; //equals

                 bool preserves_natural_order = false;//seems to add 30ms on 10^7 row one-column table

                 //POTENTIAL_OPTIMIZATION_POINT
                 //return bool char vector here instead of 64-bit INTVEC under certain conditions

                 z = sort_type_compare_pre_whered(x, y, table, false, true);

                 if(preserves_natural_order)
                 {
                   z = cow_sort(z);
                 }

                 return z;
                 //unoptimized: return between_pre(xKeys, y);
    )
    CS(STAMPVEC, if(!IS_STAMP(left) || !IS_STAMP(right)) ERROR(ERROR_TYPE);
                 DO(xn, if(left->i <= xI[i] && xI[i] <= right->i) z = cow_add(z, ki(i)))
    )
    CS(FLOATVEC, DO(xn, if(left_float <= xF[i] && xF[i] <= right_float) z = cow_add(z, ki(i))))
    CS(INTVEC,   DO(xn, if(left_float <= xI[i] && xI[i] <= right_float) z = cow_add(z, ki(i))))

   CSF(HASH, ) //POTENTIAL_OPTIMIZATION_POINT (as in xin and/or hash_type_compare_pre_whered)
    CD: ENUM(x, if(DESCENDING != KC(left,v) && KC(v,right) != DESCENDING) z = cow_add(z,ki(i))) 
  }

  return z;
}

K between(K x, K y)
{
  return ex("($1 >= $2[0] ) & ($1 <= $2[1])", x, y); 

  //POTENTIAL_OPTIMIZATION_POINT
  //rewrite to not leverage pre_whered version
  K indices = between(x, y);

  work_push(indices);
  K pre = boolean_char_vector_from_true_indices(indices, lenI(x));
  K boolified = promote_char_types_to_int_types(pre);
  rd(pre);
  work_pop_rd(true);

  return boolified;
}

#pragma mark -

K xin_pre_whered(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //uh, expand this function to other types...

  //POTENTIAL_OPTIMIZATION_POINT
  //attr_sorted: bsearch, but also
  //use min (first) and max (last) to skip! see between_pre SORTED

  //POTENTIAL_OPTIMIZATION_POINT
  //intvec x intvec can use lookup table

  //POTENTIAL_OPTIMIZATION_POINT
  //HASH, BTREE
  
  K z = new_k(INTVEC, 0);

  SW(xt)
  {

    CS(INTVEC, 
      SW(yt)
      {
        //this assumes a small number
        //if k >= 0 and <= 12345
        CS(INTVEC,     )
      }
    )
  }

  return z;
}

K xin(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //There are many ways to do this
  //A. Sort x (keeping grade), sort y, determine matches?
  //B. Special version optimized for BTREE, HASH?
  //C. is_sorted attribute: many other optimizations

  if(TABLE==xt || TABLE==yt)
  {
    //you could do this for one column tables.....buuuut it doesn't really make sense in general
    //there is a way that does make sense:
    //make a dummy ones (or 1+i) column in the RHS
    //left join to the LHS on all the LHS keys
    //null fill the nans with 0 or something
    //everything that's non-zero is a keeper row
    //btw, unique(table) can use the same mechanism as group_by

    //if(TABLE==xt && TABLE==yt) { return ex("and fold xvals($1) in mapdown $2[xkeys $1]", x, y); }
    
    ERROR(ERROR_TABLE);//we could make this work but it doesn't currently
  }

  if(MAP == xt)
  {
    //Previously, this caused bug when `select from t where syms in sym`
    //if syms undefined, hence {}, then `{} in ['AA','BB']` gave nonsense
    //if(MAP !=xt || MAP !=yt)
    ERROR(ERROR_MAP);
    //See also comments on TABLEs above, perhaps.

    //We have a compelling use case for hashsets [HASHxHASH joins] so that's what's here
    //for all maps. (We ended up going a different route)
    //If you need to split behavior along hashsets and non-hashsets, 
    //we added the MAP_ATTR_HASHSET flag.
  }

  K hashset = NULL;
  
  if(IS_HASH(y))
  {
    //hashset = strong(yIndex);
    K v = distinct(yKeys);
    work_push(v);
    K w = at(kN(yIndex,KEYS), v);
    work_push(w);
    //POTENTIAL_OPTIMIZATION_POINT: assert already getting a map here
    hashset = new_map_from_K_K(w, kk, true, false);
    work_pop_n_rd(2, true);
  }
  else if(IS_MAP(y))
  {
    hashset = strong(y);
  }
  else
  {
    hashset = new_map_from_K_K(y, kk, true, false);
  }

  work_push(hashset);

  K z = new_k(INTVEC, lenI(x));
  work_push(z);
  
  ENUM(x, zI[i] = (IS_HASH_NULL(lookupI(hashset, v))?0:1))

  work_pop_n_rd(2, false);
  rd(hashset);

  return z;
}

K xexcept(K x, K y)//setminus
{
  return ex("$1[?!$1 in $2]", x, y);
}

K xunion(K x, K y)
{
  return ex("%$1#$2",x,y);
}

K xintersect(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //if w and z are both sorted INTVECs, you get a
  //nice algorithm

  return ex("{[x,y] x:distinct(x); x[? x in y]}($1,$2)",x,y);
}

K verb_powerset(K x)
{
  if(IS_MAP(x))
  {
    return verb_powerset(xKeys);
  }

  if(IS_ARRAY(x))
  {
    return ex("$1[which mapright range repeat(count $1, 2)]", x);
  }

  return strong(x);
}

#pragma mark - Permutations & Combinations

K cow_next_perm(K x)
{
  x = cow(x);
  assert(INTVEC==xt);

  I n = xn - 1;

  int j = n;

  while(j > 0 && xI[j-1] >= xI[j] ) j--;

  if(0 >= j) 
  {
    DO(xn/2, SWAPI(xI[i], xI[n-i]))//swap to reverse
    SET_ATTR(x, ATTR_SORTED);
    return x;
  }

  OFF_ATTR(x, ATTR_SORTED);

  I i = n;

  while(i > 1 && xI[j-1] >= xI[i]) i--;

  SWAPI(xI[j-1],xI[i]);

  i = n;

  while(j < i)
  {
    SWAPI(xI[j],xI[i]);
    j++;
    i--;
  }

  return x; 
}

K verb_perms(K x, K w) //permutations
{
  if(IS_MAP(x)) return verb_perms(xKeys, w);
  if(!IS_ARRAY(x) || 0==lenI(x)) return enlist(x);

  bool preserves_duplicates = (w && truthy(w));

  K z = new_k(LIST, 0);
  K y = NULL;
  
  if(!preserves_duplicates) y = ex("flatten {[x] repeat(count x,first x) } mapright xvals part $1", x);
  else                      y = xobj_keys(x);

  y = cow(y);

  work_push(y);
  work_push(z);

  do
  {
    K a = at(x,y);
    work_pop();
    z = cow_add(z,a);
    work_push(z);
    rd(a);
    y = cow_next_perm(y);

  } while(!SORTED(y));

  work_pop_rd(false);
  work_pop_rd(true);

  return z;
}

K verb_combs(K x, K y, K z) //combinations
{
  bool preserves_duplicates = (z && truthy(z));

  if(IS_MAP(x)) return verb_combs(xKeys, y, z);
  if(!IS_ARRAY(x)) return strong(x);

  if(!preserves_duplicates) return ex("combinations(distinct $1, $2, 1)", x, y);

  return ex("if(or($2>count($1),$2<0)){return []}; $1[which mapright permutations(xkeys($1) < $2)]", x, y);
}

#pragma mark - Standard Math Library

K xsqrt (K x){return monadic_math(x, sqrt,  true );}//return ex("$1**0.5", x); 
K xln   (K x){return monadic_math(x, log,   true );}  
K xlog10(K x){return monadic_math(x, log10, true );}  
K xlog2 (K x){return monadic_math(x, log2,  true );}  
K xsin  (K x){return monadic_math(x, sin,   false);}  
K xcos  (K x){return monadic_math(x, cos,   false);}  
K xtan  (K x){return monadic_math(x, tan,   false);} 
K xasin (K x){return monadic_math(x, asin,  false);}
K xacos (K x){return monadic_math(x, acos,  false);}
K xatan (K x){return monadic_math(x, atan,  false);}
K xsinh (K x){return monadic_math(x, sinh,  false);}
K xcosh (K x){return monadic_math(x, cosh,  false);}
K xtanh (K x){return monadic_math(x, tanh,  false);}
K xerf  (K x){return monadic_math(x, erf,   true );}
K xerfc (K x){return monadic_math(x, erfc,  false);}

K monadic_math(K x, F(*passed_function)(F), bool monotonic_increasing)
{
  C t = 0;

  SW(xt)
  {
    CS(INT,      t=FLOAT)
    CS(FLOAT,    t=FLOAT)
    CS(INTVEC,   t=FLOATVEC)
    CS(FLOATVEC, t=FLOATVEC)
  }

  I n = cx;

  K z = new_k(t, n);

  //We can combine atom and vector cases by using pointers
  F *fz = (F*)PAYLOAD_START(z);

  SW(axt)
  {
    CS(INT,   ENUM(x, fz[i] = passed_function(si)))
    CS(FLOAT, ENUM(x, fz[i] = passed_function(sf)))
  }


  if(monotonic_increasing)
  {
    za |= (ATTR_SORTED & xa);//is order-preserving, most ops aren't
  }

  return z;
}

#pragma mark -

K xfloor(K x)
{
  if(INT  == axt || (CHAR == axt && BYTEY(x))) return strong(x); 

  C t = 0;

  SW(xt)
  {
    CS(FLOAT,    t=INT)
    CS(FLOATVEC, t=INTVEC)
    CS(CHAR,     t=CHAR)
    CS(CHARVEC,  t=CHARVEC)
  }

  I n = cx;

  K z = new_k(t, n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  C *bz = (V)iz;//assert(&zi == &zf && zI == zF)

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(CHAR,  ENUM(x, bz[i] = tolower(sc)))
    CS(FLOAT, ENUM(x, iz[i] = floor(sf)) za |= (ATTR_SORTED & xa))
  }

  R z;
}

K xceil(K x)
{
  if(INT  == axt || (CHAR == axt && BYTEY(x))) return strong(x); 

  C t = 0;

  SW(xt)
  {
    CS(FLOAT,    t=INT)
    CS(FLOATVEC, t=INTVEC)
    CS(CHAR,     t=CHAR)
    CS(CHARVEC,  t=CHARVEC)
  }

  I n = cx;

  K z = new_k(t, n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  C *bz = (V)iz;//assert(&zi == &zf && zI == zF)

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(CHAR,  ENUM(x, bz[i] = toupper(sc)))
    CS(FLOAT, ENUM(x, iz[i] = ceil(sf)) za |= (ATTR_SORTED & xa))
  }

  R z;
}

K xabs(K x)
{
  C t = 0;

  SW(xt)
  {
    CS(INT,      t=INT)
    CS(INTVEC,   t=INTVEC)
    CS(FLOAT,    t=FLOAT)
    CS(FLOATVEC, t=FLOATVEC)
  }

  I n = cx;

  K z = new_k(t, n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (F*)iz;

  SW(axt)
  {
    CS(INT,   ENUM(x, iz[i] = ABS(si)))
    CS(FLOAT, ENUM(x, fz[i] = ABS(sf)))
  }

  return z;
}

K xlog(K w, K x)
{
  if(w && x) return ex("ln($2)/ln($1)", w, x);

  return xlog10(w);
}

K xlg(K x)
{
  return xlog2(x);
}

K msum(K x, K y)
{
  //Note: this thing doesn't handle +/- INF/inf, but handles NAN/nan because rsum does.
  //      so...maybe tweak rsum if you want INF support?
  //also, a multiply-by-zero version would handle arb. list shape
  return ex("rsum($2) - rsum(shift($1,$2,$2[0]-$2[0]))", x, y);
}

K mcount(K x, K y) { return ex("msum($1,not isnull $2)", x, y); }

//POTENTIAL_OPTIMIZATION_POINT
//mavg is maybe ~6x slower than it could be

K mavg(K x, K y)   {if(!IS_ARRAY(y)) return strong(y); return ex("msum($1,$2)/mcount($1,$2)", x, y); }
K mmax(K x, K y)   {if(!IS_ARRAY(y)) return strong(y); return ex("($1-1) or  mapback converge $2", x, y); }
K mmin(K x, K y)   {if(!IS_ARRAY(y)) return strong(y); return ex("($1-1) and mapback converge $2", x, y); }

K plus(K x, K y)
{
  C t = MAX(axt, ayt);
  SW(t)
  {
    CS(MAP, return plus_rel_time(x, y))
  }
  if(IS_VECTOR(x) || IS_VECTOR(y)) t = -abs(t);

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;//assert(&zi == &zf && zI == zF)

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(INT, 
      SW(ayt)
      { 
       CSF(STAMP,)
        CS(INT,   LIST2(x, y, iz[i] = ri + si))
        CS(FLOAT, LIST2(x, y, fz[i] = ri + sf))
        CD: ERROR(ERROR_TYPE);
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
        CS(INT,   LIST2(x, y, fz[i] = rf + si))
        CS(FLOAT, LIST2(x, y, fz[i] = rf + sf))
        CD: ERROR(ERROR_TYPE);
      }
    )

    CS(STAMP,
      SW(ayt)
      {
       CSF(STAMP, if(!has_time_only(x) && !has_time_only(y))ERROR(ERROR_TIME))//sic: fallthrough
        CS(INT,   LIST2(x, y, iz[i] = ri + si)) //add as nanos
        CD: ERROR(ERROR_TYPE);
      }
    )

    CD: ERROR(ERROR_TYPE);
  }

  za |= (ATTR_SORTED & xa & ya);//is order-preserving, most ops aren't

  R z;
}

K plus_rel_time(K x, K y)
{
  if(MAP != xt)
  {
    SWAP(x,y);
  }

  TM_NANOS rel_tm = TM_NANOS_from_dtbucket_and_initializer(x, TM_NANOS_ZEROES);
  TM_NANOS abs_tm = {0};
  TM_NANOS sum_tm = {0};

  //time_t seconds = xi/BILLION;
  //time_t nanos   = xi - (seconds*BILLION)

  I n = IS_VECTOR(y) ? cy : 0;
  K z = new_k(yt, n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;

  //POTENTIAL_OPTIMIZATION_POINT
  //Maybe this is faster if we pass pointers instead of structs
  SW(ayt)
  {
    bool local = false;
    CS(STAMP, ENUM(y, abs_tm = TM_NANOS_from_stamp(v, local);
                      sum_tm = TM_NANOS_sum(rel_tm, abs_tm);
                      iz[i] = stampI_from_TM_NANOS(sum_tm, local)))
  }

  return z;
}

K minus(K x, K y)
{
  if(CHAR==axt || CHAR==ayt)
  {
    if(IS_STRING(x)) return xexcept(x,y); 
    ERROR(ERROR_TYPE);//for now, but byte/char ops below would be interesting
  }

  C t = MAX(axt, ayt);
  SW(t)
  {
    CS(STAMP, ERROR(ERROR_TYPE))//TODO, but do it below for all 8? add'l cases
    CS(MAP, return minus_rel_time(x, y))
  }
  if(IS_VECTOR(x) || IS_VECTOR(y)) t = -abs(t);

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;//assert(&zi == &zf && zI == zF)

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(INT, 
      SW(ayt)
      { 
        CS(INT,   LIST2(x, y, iz[i] = ri - si))
        CS(FLOAT, LIST2(x, y, fz[i] = ri - sf))
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
        CS(INT,   LIST2(x, y, fz[i] = rf - si))
        CS(FLOAT, LIST2(x, y, fz[i] = rf - sf))
      }
    )
  }

  R z;
}

K minus_rel_time(volatile K x, volatile K y)
{
  if(MAP != xt)
  {
    SWAP(x,y);
  }

  //Negate the map and re-use the method from plus
  //Alternatively we could do a Kerf snippet bootstrap
  K negated = ply(x,0,0,0,1,VERB_NEGATE);
  K z = plus_rel_time(negated, y);

  rd(negated);

  return z;
}

K xsum(K x, K y)
{
  if(x && y)
  {
    //We could potentially refactor all aggregation functions via the table to take N args, do like self[x, y] or self[x, self y,...]
    //See also x__min, x__max, join-over's flatten, etc, the verbs that are optimized folds in the table.
    //this format has to do with adverbial fold accepting left arguments as "initializers"
    return ex("sum [$1, sum $2]", x, y); 
  }

  I sumI=0;
  F sumF=0;

  K z = NULL;
  K r = NULL;




  SW(xt)
  {
    CS(BTREE, return xsum(xKeys,0))
    CS(INT,   z=Ki(xi))
    CS(FLOAT, z=Kf(xf))


    CS(INTVEC,     if(gby_hacktivated(x))
                   {
                     K z = new_k(INTVEC, gby_hack_count);
                     zero_list_payload(z);
                     K y = gby_hack;
                     DO(xn, if(IN==xI[i])continue; zI[yI[i]] += xI[i])
                     return z;
                   }

                 DO(xn, if(    IN==xI[i])continue; sumI += xI[i]) z=Ki(sumI))
    CS(FLOATVEC, 
    
                   if(gby_hacktivated(x))
                   {
                     K z = new_k(FLOATVEC, gby_hack_count);
                     zero_list_payload(z);
                     K y = gby_hack;
                     DO(xn, if( isnan(xF[i]))continue; zF[yI[i]] += xF[i])
                     return z;
                   }

                 DO(xn, if( isnan(xF[i]))continue; sumF += xF[i]) z=Kf(sumF))
    CD:
        if(!IS_ARRAY(x)) return strong(x);
        SW(cx)
        {
          CR(0, Ki(0))       //Empty array
          CD: 
          
              if(gby_hacktivated(x))
              {
                K y = gby_hack;
                K z = new_k(LIST, gby_hack_count);
                zero_list_payload(z);

                //K base = at(x, ki(0));
                K base = ki(0);
                DO(zn, nestset_ri_rd(z, i, base, true, false))
                rd(base);

                work_push(z);
                DO(cx, K0 o1,o2; I k = yI[i]; K r=LOOK_(z,k,o1); r=ply(r,LOOK_(x,i,o2),0,0,2,VERB_PLUS); nestset_ri_rd(z,k,r,true,true);  rd(r)) 

                work_pop();

                return z;
              }

              z=at(x,ki(0)); //1+ item array
              //use ply() for atomicity
              work_push(z);
              DO(cx-1, K0 o; r=z; z=ply(z,LOOK_(x,i+1,o),0,0,2,VERB_PLUS); work_pop(); rd(r); work_push(z)) 
              work_pop();
              break;
        }
  }

  R z;
}

K rsum(K x, K y)
{
  if(x && y)
  {
    return ex("$1 join sum[$1, rsum $2]", x, y); //didn't spend a lot of time thinking about this
  }

  if(!IS_ARRAY(x)) return strong(x);

  I sumI=0;
  F sumF=0;

  K z = NULL;
  K r = NULL;

  SW(xt)
  {
    CS(BTREE,     return rsum(xKeys,0))
    CS(INTVEC,   z = new_k(INTVEC,xn);   ENUM(x, if(    IN!=xI[i]){sumI += xI[i];} zI[i]=sumI))
    CS(FLOATVEC, z = new_k(FLOATVEC,xn); ENUM(x, if(!isnan(xF[i])){sumF += xF[i];} zF[i]=sumF))
    CD:
        SW(cx)
        {
          CR(0, take(ki(0),x)) //Empty array
          CD: 
              //POTENTIAL_OPTIMIZATION_POINT
              //but nobody is going to use this
              z = strong(x);
              DO(cx-1, K0 o1,o2; K s = ply(LOOK_(z,i,o1),LOOK_(z,i+1,o2),0,0,2,VERB_PLUS); z = update_ri_rd(z, ki(i+1), s, false, true)) 
              break;
        }
  }

  //POTENTIAL_FEATURE_POINT
  //more arrays than merely sorted arrays result in sorted output (all non-negative for instance)
  za |= (ATTR_SORTED & xa);//is order-preserving, most ops aren't

  return z;
}

K xmin(K x, K y)
{
  if(x && y)
  {
    return ex("min [$1, min $2]", x, y); 
  }

  I minI = II;
  F minF = FI;

  K z = NULL;
  K r = NULL;

  I n = COUNT(x);

  if(IS_ARRAY(x) && SORTED(x) && n > 0 && !gby_hacktivated(x))
  {
    return at(x, ki(0));
  }

  SW(xt)
  {
    CS(STAMP,    z=Ks(xi))
    CS(INT,      z=Ki(xi))
    CS(FLOAT,    z=Kf(xf))
    CS(STAMPVEC, 
                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), ks(minI));
                   K y = gby_hack;
                   DO(xn, if(IN==xI[i])continue; zI[yI[i]] = MIN(xI[i],zI[yI[i]]))
                   return z;
                 }
 
                 ENUM(x, if(   IN==xI[i])continue; minI = MIN(minI, xI[i])) z=Ks(minI))
    CS(INTVEC,   
                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), ki(minI));
                   K y = gby_hack;
                   DO(xn, if(IN==xI[i])continue; zI[yI[i]] = MIN(xI[i],zI[yI[i]]))
                   return z;
                 }
                 ENUM(x, if(   IN==xI[i])continue; minI = MIN(minI, xI[i])) z=Ki(minI))
    CS(FLOATVEC, 

                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), kf(minF));
                   K y = gby_hack;
                   DO(xn, if(isnan(xF[i]))continue; zF[yI[i]] = MIN(xF[i],zF[yI[i]]))
                   return z;
                 }

                 ENUM(x, if(isnan(xF[i]))continue; minF = MIN(minF, xF[i])) z=Kf(minF))
    CS(BTREE,     if(0==n || gby_hacktivated(x))return xmin(xKeys, NULL);
                 I i = sort_find_min_max_value_place(x, 0);
                 return at(x,ki(i)))
    CS(HASH,     if(0==n)return xmin(kN(xIndex,KEYS),NULL);

                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), ki(minI));//orders
                   K u = take(ki(gby_hack_count), ki(0));   //k's
                   K y = gby_hack;

                   K orders = order(kN(xIndex,KEYS));
                   K w = xKeys;
                   DO(wn, I k = wI[i]; I o = kI(orders)[k];  if(o < zI[yI[i]]){zI[yI[i]] = o; uI[yI[i]] = k;}  )
                   rd(orders);

                   K r = at(kN(xIndex,KEYS),u);
                   rd(z);
                   rd(u);

                   return r;
                 }

                 K orders = order(kN(xIndex,KEYS));
                 y = xKeys;
                 I mino = II;
                 I mink = 0;
                 DO(yn, I k = yI[i]; I o = kI(orders)[k]; if(o < mino){mino = o; mink = k;}  )
                 rd(orders);
                 K0 o;
                 z = strong(LOOK_(x, mink,o));
                 return z;
    )
    CD:          if(!IS_ARRAY(x)) return strong(x);
                 if(0==n)return Kf(minF);

                 if(gby_hacktivated(x))
                 {
                   ERROR(ERROR_MISSING);
                 }

                 z=at(x,ki(0)); //1+ item array
                 DO(n-1, K0 o; r=z; K e= LOOK_(x,i+1,o); z=(KC(z,e)<=0)?strong(z):strong(e); rd(r))

                 break;
  }

  R z;
}

K xmax(K x, K y)
{
  if(x && y)
  {
    return ex("max [$1, max $2]", x, y); 
  }

  I maxI = -II;
  F maxF = -FI;

  K z = NULL;
  K r = NULL;
  
  I n = COUNT(x);

  if(IS_ARRAY(x) && SORTED(x) && n > 0 && !gby_hacktivated(x))
  {
    return at(x, ki(n-1));
  }

  SW(xt)
  {
    CS(STAMP,    z=Ks(xi))
    CS(INT,      z=Ki(xi))
    CS(FLOAT,    z=Kf(xf))
    CS(STAMPVEC, 
                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), ks(maxI));
                   K y = gby_hack;
                   DO(xn, if(IN==xI[i])continue; zI[yI[i]] = MAX(xI[i],zI[yI[i]]))
                   return z;
                 }
    
                 ENUM(x, if(   IN==xI[i])continue; maxI = MAX(maxI, xI[i])) z=Ks(maxI))
    CS(INTVEC,   
                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), ki(maxI));
                   K y = gby_hack;
                   DO(xn, if(IN==xI[i])continue; zI[yI[i]] = MAX(xI[i],zI[yI[i]]))
                   return z;
                 }
 
                 ENUM(x, if(   IN==xI[i])continue; maxI = MAX(maxI, xI[i])) z=Ki(maxI))
    CS(FLOATVEC, 
                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), kf(maxF));
                   K y = gby_hack;
                   DO(xn, if(isnan(xF[i]))continue; zF[yI[i]] = MAX(xF[i],zF[yI[i]]))
                   return z;
                 }

                 ENUM(x, if(isnan(xF[i]))continue; maxF = MAX(maxF, xF[i])) z=Kf(maxF))

    CS(BTREE,    if(0==n || gby_hacktivated(x))return xmax(xKeys,NULL);
                 I i = sort_find_min_max_value_place(x, 1);
                 return at(x,ki(i)))

    CS(HASH,     if(0==n)return xmax(kN(xIndex,KEYS),NULL);

                 if(gby_hacktivated(x))
                 {
                   K z = take(ki(gby_hack_count), ki(maxI));//orders
                   K u = take(ki(gby_hack_count), ki(0));   //k's
                   K y = gby_hack;

                   K orders = order(kN(xIndex,KEYS));
                   K w = xKeys;
                   DO(wn, I k = wI[i]; I o = kI(orders)[k]; if(o > zI[yI[i]]){ zI[yI[i]] = o; uI[yI[i]] = k;}  
                   
                      dd(i)
                      dd(k)
                      dd(o)
                      er()
                   
                   )
                   rd(orders);

                   K r = at(kN(xIndex,KEYS),u);
                   rd(z);
                   rd(u);

                   return r;
                 }

                 K orders = order(kN(xIndex,KEYS));
                 y = xKeys;
                 I maxo = -II;
                 I maxk = 0;
                 DO(yn, I k = yI[i]; I o = kI(orders)[k]; if(o > maxo){maxo = o; maxk = k;}  )
                 rd(orders);
                 K0 o;
                 z = strong(LOOK_(x, maxk,o));
                 return z;
    )
    CD:          if(!IS_ARRAY(x)) return strong(x);
                 if(0==n) return Kf(maxF);

                 if(gby_hacktivated(x))
                 {
                   ERROR(ERROR_MISSING);
                 }

                 z=at(x,ki(0)); //1+ item array
                 DO(n-1, r=z; K0 o; K e= LOOK_(x,i+1,o); z=(KC(z,e)>=0)?strong(z):strong(e); rd(r))
                 break;
  }

  R z;
}

K maxback(K x, K y)
{
  if(x && y)
  {
    return ex("1 drop | mapback $1 join $2", x, y); 
  }

  K z = NULL;
  I n = COUNT(x);
  K r = NULL;

  SW(xt)
  {
    CS(FLOATVEC, z = new_k(FLOATVEC, xn); DO(xn, if(0==i) zF[i]=xF[i]; else zF[i] = MAX(xF[i],xF[i-1])))
    CS(INTVEC,   z = new_k(INTVEC,   xn); DO(xn, if(0==i) zF[i]=xF[i]; else zF[i] = MAX(xF[i],xF[i-1])))
    CD: if(!IS_ARRAY(x) || 0==n)
        {
          return strong(x);
        }

        z=take(ki(1),x);
        DO(n-1, K0 o1,o2; r=ply(LOOK_(x,i, o1),LOOK_(x,i+1,o2),0,0,2,VERB_OR); z=cow_add_funny(z,r); rd(r))
        break;
  }
 
  return z;
}

K minback(K x, K y)
{
  if(x && y)
  {
    return ex("1 drop & mapback $1 join $2", x, y); 
  }

  K z = NULL;
  I n = COUNT(x);
  K r = NULL;

  SW(xt)
  {
    CS(FLOATVEC, z = new_k(FLOATVEC, xn); DO(xn, if(0==i) zF[i]=xF[i]; else zF[i] = MIN(xF[i],xF[i-1])))
    CS(INTVEC,   z = new_k(INTVEC,   xn); DO(xn, if(0==i) zF[i]=xF[i]; else zF[i] = MIN(xF[i],xF[i-1])))
    CD: if(!IS_ARRAY(x) || 0==n)
        {
          return strong(x);
        }

        z=take(ki(1),x);
        DO(n-1,K0 o1,o2; r=ply(LOOK_(x,i,o1),LOOK_(x,i+1,o2),0,0,2,VERB_AND); z=cow_add_funny(z,r); rd(r))
        break;
  }
 
  return z;
}

K xavg(K x)
{
  return ex("(sum $1)/count_nonnull $1", x);
}

K xstd(K x)
{
  return ex("sqrt var $1", x);
}

K _debug_gby(K x)
{
  if(gby_hacktivated(x) && IS_ARRAY(x))
  {
    K4 o;
    K z = take(ki(gby_hack_count),klist1(kk,o));

    K y = gby_hack;

    ENUM(x, I k = yI[i]; 

            K old = kN(z, k);
            K fresh = cow_add(old, v);
            nestneu(z, k, fresh);
    )

    return z;
  }

  return strong(x);
}

K xvar(K x)
{
  //this is fairly optimized (14ms on 10^6 items. compare 4ms for a sum)
  //we could get it down lower by performing summing, squaring, counting operations in one pass, possibly
  if(gby_hacktivated(x))
  {
    K temp = gby_hack;

    K y = ex("{[x] (y*y:(_debug_gby(x) - avg x))/count_nonnull x} $1", x);
    gby_hack = NULL;

    K z = ex("{[x] sum mapright x }$1", y);

    gby_hack = temp;
    rd(y);
    return z;
  }

  return ex("{[x] (sum y*y:(x - avg x))/count_nonnull x} $1", x);
}

K verb_median(K x)
{
  if(0==lenI(x)) return Kf(FN); //we could eliminate this if sum() did not ignore nan

  return ex("0.5 * sum $1[ascend($1) floor(-0.5 0.0 + count($1)/2.0)]", x);
}

K xtimes(K x, K y)
{
  C t = MAX(axt, ayt);
  if(IS_VECTOR(x) || IS_VECTOR(y)) t = -abs(t);

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;//assert(&zi == &zf && zI == zF)

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(INT, 
      SW(ayt)
      { 
        CS(INT,   LIST2(x, y, iz[i] = ri * si))
        CS(FLOAT, LIST2(x, y, fz[i] = ri * sf))
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
        CS(INT,   LIST2(x, y, fz[i] = rf * si))
        CS(FLOAT, LIST2(x, y, fz[i] = rf * sf))
      }
    )
  }

  R z;
}

K negate(K x)
{
  return xtimes(ki(-1),x);
}

K max_or(K x, K y)
{
  if(IS_STRING(x) && IS_STRING(y))
  {
    return (KC(x,y)>=0)?strong(x):strong(y);
  }

  C t = MAX(axt, ayt);
  if(IS_VECTOR(x) || IS_VECTOR(y)) t = -abs(t);

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);

  za |=  (ATTR_SORTED & xa & ya);//is order-preserving, most ops aren't

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;//assert(&zi == &zf && zI == zF)
  C *zz = (V)iz;

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(CHAR,
      SW(ayt)
      {
        CS(CHAR, LIST2(x, y, zz[i] = MAX(rc,sc)) return z)
      }
    )
    CS(STAMP, 
      SW(ayt)
      {
        CS(STAMP, LIST2(x, y, iz[i] = MAX(ri,si)) return z)
      }
    )
    CS(INT, 
      SW(ayt)
      { 
        CS(INT,   LIST2(x, y, iz[i] = MAX(ri,si)) return z)
        CS(FLOAT, LIST2(x, y, fz[i] = MAX(ri,sf)) return z)
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
        CS(INT,   LIST2(x, y, fz[i] = MAX(rf,si)) return z)
        CS(FLOAT, LIST2(x, y, fz[i] = MAX(rf,sf)) return z)
      }
    )
  }

  ERROR(ERROR_TYPE);

  R z;
}

K min_and(K x, K y)
{
  if(IS_STRING(x) && IS_STRING(y))
  {
    return (KC(x,y)<=0)?strong(x):strong(y);
  }

  C t = MAX(axt, ayt);
  if(IS_VECTOR(x) || IS_VECTOR(y)) t = -abs(t);

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);

  za |=  (ATTR_SORTED & xa & ya);//is order-preserving, most ops aren't

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;//assert(&zi == &zf && zI == zF)
  C *zz = (V)iz;

  if(CHARVEC==xt && CHARVEC==yt)
  {
    LIST2(x, y, zz[i] = MIN(xC[i],yC[i])) 
    if(BYTEY(x) || BYTEY(y)) SET_ATTR(z, ATTR_BYTES);
    return z;
  }

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(CHAR,
      SW(ayt)
      {
        CS(CHAR, LIST2(x, y, zz[i] = MIN(rc,sc)) 
                 if(BYTEY(x) || BYTEY(y)) SET_ATTR(z, ATTR_BYTES);
                 return z
        )
      }
    )
    CS(STAMP, 
      SW(ayt)
      {
        CS(STAMP, LIST2(x, y, iz[i] = MIN(ri,si)) return z)
      }
    )
    CS(INT, 
      SW(ayt)
      { 
        CS(INT,   LIST2(x, y, iz[i] = MIN(ri,si)) return z)
        CS(FLOAT, LIST2(x, y, fz[i] = MIN(ri,sf)) return z)
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
        CS(INT,   LIST2(x, y, fz[i] = MIN(rf,si)) return z)
        CS(FLOAT, LIST2(x, y, fz[i] = MIN(rf,sf)) return z)
      }
    )
  }

  ERROR(ERROR_TYPE);

  R z;
}

K xnot(K x)
{
  I n=0;
  K z;

  SW(xt)
  {
    CS(FLOAT,  z = Ki(!xf))
    CS(INT,    z = Ki(!xi))
    CS(-FLOAT, z = new_k(-INT,xn); DO(xn, zI[i] = !xF[i]))
    CS(-INT,   z = new_k(-INT,xn); DO(xn, zI[i] = !xI[i]))
    CS(NIL,    z = Ki(!TRUTHTABLE_VALUE_FOR_NIL)) 
    CD:        z = Ki(0); break;
  }

  return z;
}

K divide(K x, K y)
{
  C t = MAX(axt, ayt);
  t = FLOAT; //lock to float for division
  if(IS_VECTOR(x) || IS_VECTOR(y)) t = -abs(t);

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;//assert(&zi == &zf && zI == zF)

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(INT, 
      SW(ayt)
      { 
        CS(INT,   LIST2(x, y, fz[i] = ri / (F)  si)) //IxI->F
        CS(FLOAT, LIST2(x, y, fz[i] = ri / sf))
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
        CS(INT,   LIST2(x, y, fz[i] = rf / si))
        CS(FLOAT, LIST2(x, y, fz[i] = rf / sf))
      }
    )
  }

  R z;
}

K xmod(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  return ex("$1 - $2 * floor($1/$2)", x, y);
  //if you stop using the ex method you need to change to
  //the verb table properties of say "divide"
  //for atomicity, type checking, etc.
}

K xexp(K x, K y)
{

  if(!y)
  {
    return ex("$1 exp $2", kf(M_E), x);
  }

  C t = MAX(axt, ayt);
  t = FLOAT; //lock to float for exponentiation
  if(IS_VECTOR(x) || IS_VECTOR(y)) t = -abs(t);

  I n = CONFORM_TRY(x,y);
  K z = new_k(t,n);

  //We can combine atom and vector cases by using pointers
  I *iz = PAYLOAD_START(z);
  F *fz = (V)iz;//assert(&zi == &zf && zI == zF)

  //POTENTIAL_OPTIMIZATION_POINT: 
  //Further split out VECTORs
  // .8 versus .5 :: iz[i] = ri+si versus zI[i] = xI[i]+yI[i]
  SW(axt)
  {
    CS(INT, 
      SW(ayt)
      { 
        CS(INT,   LIST2(x, y, fz[i] = pow(ri, si))) //IxI->F
        CS(FLOAT, LIST2(x, y, fz[i] = pow(ri, sf)))
      }
    )
    CS(FLOAT, 
      SW(ayt)
      {
        CS(INT,   LIST2(x, y, fz[i] = pow(rf, si)))
        CS(FLOAT, LIST2(x, y, fz[i] = pow(rf, sf)))
      }
    )
  }

  //POTENTIAL_IMPROVEMENT_POINT
  //Technically, I think exp maintains sort order for certain args & we should keep it

  R z;
}

K xrank(K x, K y)
{
  return ex("floor (<<$2) * floor($1) / count $2",x,y);
}

K xor(K x, K y)
{
  if(CHARVEC != xt || CHARVEC != yt) ERROR(ERROR_TYPE);

  if(xn < yn)
  {
    K z = x;
    x = y;
    y = z;
  }

  K z = copy(x);

  I j = 0;

  DO(zn, if(j>=yn)j=0; zC[i] ^= yC[j]; j++)

  return z;
}

#pragma mark - Matrix Operations & Algorithms

K transpose(K x)
{
  if(!IS_MIXED_ARRAY(x)) return strong(x); //Identity on atoms, vectors

  I n = COUNT(x);

  if(n <= 0) return strong(x); //Identity on empty arrays

  I m = SENTINEL;

  ENUM(x, if(!IS_SATOM(v)){m = COUNT(v); break;})

  if(SENTINEL == m) return strong(x); //Identity on arrays of atoms

  ENUM(x, if(!IS_SATOM(v)){I c = COUNT(v); if(c != m) ERROR(ERROR_LENGTH);}) 

  //POTENTIAL_OPTIMIZATION_POINT
  //*maybe setting these directly at indices is faster than appending to them?
  //*maybe adding a bunch of string "atoms" (vectors) should create a HASH object

  K z = new_k(LIST, m/2);//optimistically: vectors are half the size of LISTs
  zn = 0;

  DO(m,

    K y = new_k(LIST, n/2);
    yn = 0;

    DO2(n, 
           K0 o1,o2;
           K u = LOOK_(x, j, o1); 
           if(IS_SATOM(u))
           {
             y = cow_add(y, u);
           }
           else
           {
             K v = LOOK_(u, i, o2);
             y = cow_add(y, v);
           }
    )
    
    z = cow_add(z, y);

    rd(y);
  )

  return z;
}

K dotp(K x, K y)
{
  return ex("sum $1*$2", x, y);
}

K mmul(K x, K y)
{
  return ex("$1 dotp mapleft $2", x, y);
}

K minv(K x)
{
  return ex("lsq($1, {[x] x = mapleft x} xkeys($1))",x);
}

static void svdcmp(F **a, I m, I n, F *w, F **v, F *t);

K lsq(K b, K a)
{
  //a must be MIXED_ARRAY *OR* int/float vec
  //b must be a MIXED_ARRAY *CONTAINING* int/float vecs
  //neither can be 0 length
  //the length of b's first element must match all of b's elements' lengths, and either (a->n if float/intvec || all of a's elements)

  I at = a->t;
  I bt = b->t;

  bool is_mixed_a = IS_MIXED_ARRAY(a);

  if(!(is_mixed_a || FLOATVEC == at || INTVEC == at)) ERROR(ERROR_TYPE);

  if(!IS_MIXED_ARRAY(b)) ERROR(ERROR_TYPE);

  I an = lenI(a);
  I bn = lenI(b);

  if(an <= 0 || bn <= 0) ERROR(ERROR_LENGTH);

  F TOL = 1.0e-6, s;

  K x, y, z;

  K0 o;
  K bfirst = LOOK_(b, 0, o);

  I r = lenI(bfirst);
  if(r <= 0) ERROR(ERROR_LENGTH);

  DO(bn,  K0 o; y = LOOK_(b,i,o);
          if(INTVEC != yt && FLOATVEC != yt) ERROR(ERROR_TYPE);
          if(r != yn) ERROR(ERROR_LENGTH);
  )

  if(is_mixed_a)
  {
    DO(an, K0 o; y = LOOK_(a,i,o);
           if(INTVEC != yt && FLOATVEC != yt) ERROR(ERROR_TYPE);
           if(r != yn) ERROR(ERROR_LENGTH);
    )
  }
  else
  {
    if(r != an) ERROR(ERROR_LENGTH);
  }

  I n=bn;
  I m=MAX(r,n);

  K u_k1 = new_k(LINKVEC,    m);
  K u_k2 = new_k(FLOATVEC, n*m);

  F **u = (F**)kP(u_k1);
  u[0]  = kF(u_k2);

  work_push(u_k1);
  work_push(u_k2);

  K v_k1 = new_k(LINKVEC,    n);
  K v_k2 = new_k(FLOATVEC, n*n);


  F **v = (F**)kP(v_k1);
  v[0]  = kF(v_k2);
  work_push(v_k1);
  work_push(v_k2);

  K w_k = new_k(FLOATVEC, n); 
  F *w = kF(w_k);
  work_push(w_k);

  K t_k = new_k(FLOATVEC, n); 
  F *t = kF(t_k);
  work_push(t_k);


  DO(m,u[i]=u[0]+n*i)
  DO(n,v[i]=v[0]+n*i)
  DO(n*m,u[0][i]=0)//zero out any tacked on rows


  DO(r, //matrix is tranposed (list is a column vector)
        DO2(n, K0 o; y=LOOK_(b,j,o); u[i][j]= FLOATVEC==yt?yF[i]:yI[i])
  ) 

	svdcmp(u,m,n,w,v,t);

	F wmax=0.0;
	DO(n, if (w[i] > wmax) wmax=w[i])
	F thresh=TOL*wmax;
	DO(n, if (w[i] < thresh) w[i]=0.0)

  if(is_mixed_a)
  {
    z=new_k(LIST, an);
    DO(an, nestset_ri_rd(z, i, new_k(FLOATVEC, n), false, false))
  }
  else
  {
    z = new_k(FLOATVEC, n); 
  }

  work_push(z);

  DO3((!is_mixed_a)?1:an,  //backsubstitution (see svdbksb from Numerical Recipes)
    K0 o1,o2;
    y=(!is_mixed_a)?a:LOOK_(a,k, o1);
    x=(!is_mixed_a)?z:LOOK_(z,k, o2);
    DO(n,s=0.;if(w[i]){DO2(m,s+=u[j][i]*(FLOATVEC==yt?yF[j]:yI[j]))s/=w[i];}t[i]=s)
    DO(n,s=0.;DO2(n,s+=v[i][j]*t[j];xF[i]=s)) 
  )

  work_pop_n_rd(1, false);
  work_pop_n_rd(6, true);

  return z;
}

//SVD stuff cribbed from TINA who cribbed from Numerical Recipes (this is ok license-wise)
#define Sign(u,v)               ( (v)>=0.0 ? ABS(u) : -ABS(u) )
static F radius(F u, F v) //aka 'pythag' compute (a^2+b^2)^(1/2) without under-/over-flow
{
  F  Au, Av, Aw;
  Au = ABS(u);
  Av = ABS(v);
  if (Au > Av) { Aw = Av / Au; R Au * sqrt(1. + Aw * Aw); }
  if (Av != 0.0) { Aw = Au / Av; R Av * sqrt(1. + Aw * Aw); }
  R 0.0;
}

/*************************** SVDcmp *****************************************
* Given matrix A[m][n], m>=n, using svd decomposition A = U W V' to get     *
* U[m][n], W[n][n] and V[n][n], where U occupies the position of A.         *
* NOTE: if m<n, A should be filled up to square with zero rows.             *
*       A[m][n] has been destroyed by U[m][n] after the decomposition.      *
****************************************************************************/
static void svdcmp(F **a, I m, I n, F *w, F **v, F *t)
{
    /* BUG `nm' may be used uninitialized in this function */
    I     flag, i, its, j, jj, k, l, nm, nm1 = n - 1, mm1 = m - 1;
    F  c, f, h, s, x, y, z;
    F  anorm = 0.0, g = 0.0, scale = 0.0;
    F *rv1;

    //if (m < n) O("SVDCMP: You must augment A with extra zero rows");//err, but we won't have m<n
    rv1 = t;

    /* Householder reduction to bidiagonal form */
    for (i = 0; i < n; i++)
    {
        l = i + 1;
        rv1[i] = scale * g;
        g = s = scale = 0.0;
        if (i < m)
        {
            for (k = i; k < m; k++)
                scale += ABS(a[k][i]);
            if (scale)
            {
                for (k = i; k < m; k++)
                {
                    a[k][i] /= scale;
                    s += a[k][i] * a[k][i];
                }
                f = a[i][i];
                g = -Sign(sqrt(s), f);
                h = f * g - s;
                a[i][i] = f - g;
                if (i != nm1)
                {
                    for (j = l; j < n; j++)
                    {
                        for (s = 0.0, k = i; k < m; k++)
                            s += a[k][i] * a[k][j];
                        f = s / h;
                        for (k = i; k < m; k++)
                            a[k][j] += f * a[k][i];
                    }
                }
                for (k = i; k < m; k++)
                    a[k][i] *= scale;
            }
        }
        w[i] = scale * g;
        g = s = scale = 0.0;
        if (i < m && i != nm1)
        {
            for (k = l; k < n; k++)
                scale += ABS(a[i][k]);
            if (scale)
            {
                for (k = l; k < n; k++)
                {
                    a[i][k] /= scale;
                    s += a[i][k] * a[i][k];
                }
                f = a[i][l];
                g = -Sign(sqrt(s), f);
                h = f * g - s;
                a[i][l] = f - g;
                for (k = l; k < n; k++)
                    rv1[k] = a[i][k] / h;
                if (i != mm1)
                {
                    for (j = l; j < m; j++)
                    {
                        for (s = 0.0, k = l; k < n; k++)
                            s += a[j][k] * a[i][k];
                        for (k = l; k < n; k++)
                            a[j][k] += s * rv1[k];
                    }
                }
                for (k = l; k < n; k++)
                    a[i][k] *= scale;
            }
        }
        anorm = MAX(anorm, (ABS(w[i]) + ABS(rv1[i])));
    }

    /* Accumulation of right-hand transformations */
    for (i = n - 1; i >= 0; i--)
    {
        if (i < nm1)
        {
            if (g)
            {
                /* F division to avoid possible underflow */
                for (j = l; j < n; j++)
                    v[j][i] = (a[i][j] / a[i][l]) / g;
                for (j = l; j < n; j++)
                {
                    for (s = 0.0, k = l; k < n; k++)
                        s += a[i][k] * v[k][j];
                    for (k = l; k < n; k++)
                        v[k][j] += s * v[k][i];
                }
            }
            for (j = l; j < n; j++)
                v[i][j] = v[j][i] = 0.0;
        }
        v[i][i] = 1.0;
        g = rv1[i];
        l = i;
    }
    /* Accumulation of left-hand transformations */
    for (i = n - 1; i >= 0; i--)
    {
        l = i + 1;
        g = w[i];
        if (i < nm1)
            for (j = l; j < n; j++)
                a[i][j] = 0.0;
        if (g)
        {
            g = 1.0 / g;
            if (i != nm1)
            {
                for (j = l; j < n; j++)
                {
                    for (s = 0.0, k = l; k < m; k++)
                        s += a[k][i] * a[k][j];
                    f = (s / a[i][i]) * g;
                    for (k = i; k < m; k++)
                        a[k][j] += f * a[k][i];
                }
            }
            for (j = i; j < m; j++)
                a[j][i] *= g;
        } else
            for (j = i; j < m; j++)
                a[j][i] = 0.0;
        ++a[i][i];
    }
    /* diagonalization of the bidiagonal form */
    for (k = n - 1; k >= 0; k--)
    {                           /* loop over singular values */
        for (its = 0; its < 30; its++)
        {                       /* loop over allowed iterations */
            flag = 1;
            for (l = k; l >= 0; l--)
            {                   /* test for splitting */
                nm = l - 1;     /* note that rv1[l] is always zero */
                if (ABS(rv1[l]) + anorm == anorm)
                {
                    flag = 0;
                    break;
                }
                if (ABS(w[nm]) + anorm == anorm)
                    break;
            }
            if (flag)
            {
                c = 0.0;        /* cancellation of rv1[l], if l>1 */
                s = 1.0;
                for (i = l; i <= k; i++)
                {
                    f = s * rv1[i];
                    if (ABS(f) + anorm != anorm)
                    {
                        g = w[i];
                        h = radius(f, g);
                        w[i] = h;
                        h = 1.0 / h;
                        c = g * h;
                        s = (-f * h);
                        for (j = 0; j < m; j++)
                        {
                            y = a[j][nm];
                            z = a[j][i];
                            a[j][nm] = y * c + z * s;
                            a[j][i] = z * c - y * s;
                        }
                    }
                }
            }
            z = w[k];
            if (l == k)
            {                   /* convergence */
                if (z < 0.0)
                {
                    w[k] = -z;
                    for (j = 0; j < n; j++)
                        v[j][k] = (-v[j][k]);
                }
                break;
            }
            if (its == 30){ ERROR(ERROR_DEPTH); return;}  //O("No convergence in 30 SVDCMP iterations");
            x = w[l];           /* shift from bottom 2-by-2 minor */
            nm = k - 1;
            y = w[nm];
            g = rv1[nm];
            h = rv1[k];
            f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y);
            g = radius(f, 1.0);
            /* next QR transformation */
            f = ((x - z) * (x + z) + h * ((y / (f + Sign(g, f))) - h)) / x;
            c = s = 1.0;
            for (j = l; j <= nm; j++)
            {
                i = j + 1;
                g = rv1[i];
                y = w[i];
                h = s * g;
                g = c * g;
                z = radius(f, h);
                rv1[j] = z;
                c = f / z;
                s = h / z;
                f = x * c + g * s;
                g = g * c - x * s;
                h = y * s;
                y = y * c;
                for (jj = 0; jj < n; jj++)
                {
                    x = v[jj][j];
                    z = v[jj][i];
                    v[jj][j] = x * c + z * s;
                    v[jj][i] = z * c - x * s;
                }
                z = radius(f, h);
                w[j] = z;       /* rotation can be arbitrary id z=0 */
                if (z)
                {
                    z = 1.0 / z;
                    c = f * z;
                    s = h * z;
                }
                f = (c * g) + (s * y);
                x = (c * y) - (s * g);
                for (jj = 0; jj < m; jj++)
                {
                    y = a[jj][j];
                    z = a[jj][i];
                    a[jj][j] = y * c + z * s;
                    a[jj][i] = z * c - y * s;
                }
            }
            rv1[l] = 0.0;
            rv1[k] = f;
            w[k] = x;
        }
    }
}

#pragma mark -
