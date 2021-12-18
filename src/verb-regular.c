#include "kerf.h"

I lenI(K x) 
{
  I n = 0;

  //POTENTIAL_OPTIMIZATION_POINT
  // 0. always_inline
  // 0. split out types / customize them

  SW(xt)
  {
   CSF(BTREE,)
    CS(HASH,  n = lenI(xKeys))
    CS(TABLE, n = table_rows(x))
    CS(ATLAS, n = lenI(xIndex))
    CS(ZIP,   n = compression_bytes_if_decompressed(x) >> log_size_of_type_element[ABS(x->nk)] )
    CD: n = cx;
  }

  return n;
}

K len(K x)
{
  if(gby_hacktivated(x))
  {
    K z = new_k(INTVEC, gby_hack_count);
    zero_list_payload(z);
    K y = gby_hack;
    DO(yn, zI[yI[i]]++)
    return z;
  }

  return Ki(lenI(x));
}

K atom(K x){R Ki(ax);}
K match(K x, K y){R Ki(matchC(x,y,true));}

K monad_eval(K x)
{ 
  x = strong(x);
  x = cow_ensure_null_terminated_chars(x);
  work_push(x);
  K z = ex(xC);
  work_pop();
  rd(x);
  R z;
}

K join(K x, K y)
{
  //TODO Join is actually a complicated verb? this is a placeholder?
  K z = NULL;
  
  if(IS_DISK(x))
  {
    z = copy_override(x);
  }
  else
  {
    z = strong(x);
  }

  return cow_join(z, y); 
}

K verb_cross(K x, K y)
{
  I pre = ECOUNT(x) * ECOUNT(y);
  K z = new_k(LIST, pre);
  zn = 0;

  work_push(z);

  ENUM(x, K a = v; ENUM(y, 
    K b = join(a, v);
    z = cow_add(z, b);
    rd(b);
  ))

  work_pop();

  return z;
}

K enumer(K x)
{
  SW(xt)
  {
     CS(MAP, return strong(xKeys))
    CSF(FLOAT,)
     CS(INT, return til(x))
     //POTENTIAL_OPTIMIZATION_POINT
     //may be faster to generate these by hand, as in Kona source
     CS(INTVEC, return ex("cross fold range mapright $1",x)) //odometer
  }
  
  return ERROR(ERROR_TYPE);
}

K hashed(K x)
{
  SW(xt)
  {
   CSF(ATLAS,)
    CS(HASH, return strong(x))
    CD: break;
  }

  K z = new_intern();
  K y = strong(x);
  y = cow_coerce_column(y);

  //probably it's a better idea to avoid
  //many of these problems and simply
  //build a new_intern_from_k() style function
  //to pair with new_intern()
  //(which I guess is supposed to be this one)
  //note: enum of enum should return strong of the original
  //same goes for other types which might rely on nested enum, say ATLAS
  if(IS_VECTOR(y) && yn == 0)
  {
    kN(zIndex,KEYS)->t = yt; //careful, low level demote logic
  }

  //POTENTIAL_OPTIMIZATION_POINT
  //batch
  work_push(y);
  ENUM(y, z = cow_add(z, v));

  work_pop_rd(true);
  return z;
}

K atlased(K x)
{
  return new_atlas_from_K(x);
}

K indexed(K x)
{
  K y = strong(x);
  y = cow_coerce_column(y);
  work_push(y);
  K z = new_btree_from_K(y);
  work_pop_rd(true);
  return z;
}

K distinct(K x)
{
  if(!IS_ARRAY(x)) return strong(x);

  //POTENTIAL_OPTIMIZATION_POINT
  //*improved version for BTREE
  //*improved version for ATTR_SORTED
  //*"PART" verb can be similarly improved as has been done in this function everywhere
  //*INTVEC has a similar tactic to HASH below: use a boolean array (this is like
  // counting sort) to track appearances in a range. then use the same array (flipping off
  // as you add) to add integers as you scan through. the important thing is this
  // preserves natural order of appearance. more complicated grade algos may
  // also work but be slower...

  SW(xt)
  {
    CS(HASH, K w = xKeys;
             K special = kN(xIndex,KEYS);
             I possible = COUNT(special);
             I count = 0;

             bool preserves_order = true;

             if(possible < II) //< 87654321) //magic number
             {
               //POTENTIAL_OPTIMIZATION_POINT
               //could use actual char-sized boolean. is that faster?
               K y = take(ki(possible), ki(0));
               work_push(y);

               K firsts = NULL;

               if(preserves_order)
               {
                 firsts = new_k(INTVEC, possible);
                 firsts->n = 0;
                 work_push(firsts);
               }

               DO(wn, I p = wI[i]; 
                      if(yI[p])continue;
                      yI[p]=1;

                      if(preserves_order)
                      {
                        kI(firsts)[firsts->n++] = p;
                      }

                      count++;
                      if(count >= possible) break;
               )

               K z = NULL;
               
               if(!preserves_order)
               {
                 z = ex("$1[which $2]", special, y);
               }
               else
               {
                  z = at(special, firsts);
                  work_pop();
                  rd(firsts);
               }

               work_pop_rd(true);
               return z;
             }
    )
  }

  K y = new_hashset();

  ENUM(x, insert_replace(y, v, 0, false, true);)

  K r = strong(yKeys);
  rd(y);

  return r;
}

K xsort(K x)
{
  return cow_sort(strong(x));
}

K xshuffle(K x)
{
  if(IS_TABLE(x)) return ex("$1[shuffle(range(len $1))]",x);
  if(IS_ARRAY(x)) return verb_deal(ki(lenI(x)),x);
  return strong(x);
}

K order(K x)
{
  return ex("<<$1",x);
}

K part_hash(K x)
{
  K ipart = part_intvec(xKeys); //to preserve order for hash, preserve order for intvec
  work_push(ipart);
  K z = ex("map($2[xkeys $1], xvals $1)", ipart, kN(xIndex,KEYS));
  work_pop_rd(true);
  return z;
}

K crossover(K x)
{
  assert(SORTED(x));//necessary if using bin/exp search

  K y = new_k(INTVEC, 0);

  //POTENTIAL_OPTIMIZATION_POINT
  //Don't compare everything: if it's sorted you can use bin/exp search

  I fence = 0;//Use this to avoid float comparison associativity problems (look back to beginning of group not back 1)

  ENUM(x,K0 o;  if(0==i || !matchC(LOOK_(x,fence,o), v, true)) {fence = i;  y = cow_add(y, ki(fence));} ) //optionally can start with 0 or not

  return y; 
}

K part_intvec(K x)
{
  bool preserves_natural_order = true;

  K grades = grade_up(x);  
  work_push(grades);

  K sorted = at(x, grades);
  SET_ATTR(sorted, ATTR_SORTED);
  work_push(sorted);

  //POTENTIAL_OPTIMIZATION_POINT
  //it might be faster to skip the at() have this func accept grades+list
  K crossovers = crossover(sorted); 
  work_push(crossovers);

  K list = new_k(LIST, 1 + crossovers->n);
  list->n = 0;

  //POTENTIAL_OPTIMIZATION_POINT
  //1. if we increase crossover's size by 1 we can reuse it as `firsts`
  //   as we step through.
  //2. we don't need `firsts` at all unless preservering natural order

  K firsts = new_k(INTVEC, 1 + crossovers->n);
  firsts->n = 0;

  ENUM(crossovers, I start = vi;
                   I end = grades->n;
                   if(i < crossovers->n - 1) end = kI(crossovers)[i+1];

                   I length = end - start;
                   assert(length > 0);

                   K y = new_k(INTVEC, length);
                   kI(firsts)[i]=kI(grades)[start]; firsts->n++;

                   DO(length, yI[i] = kI(grades)[start+i])

                   SET_ATTR(y, ATTR_SORTED);

                   list = cow_add(list, y);
                   rd(y);
  )


  if(preserves_natural_order)
  {
    //list = cow_sort(list); //this doesn't work unless arbitrary lists sort lexicographically

    //K t = ex("$2[<$1]", firsts, list);
    K g = grade_up(firsts);
    K t = at(list, g);
    rd(g);
    rd(list);
    list = t;
  }

  rd(firsts);

  work_push(list);

  K keys = new_k(LIST, 0); 

  ENUM(list, K0 o; keys = cow_add(keys, LOOK_(x, vI[0],o)))
  work_push(keys);

  K part = new_map_from_K_K(keys, list, false, false);

  work_pop_n_rd(5, true);

  return part;
}

K part(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //possibly we want to switch some or even *all* types over to depend on sort
  //we'll want sub-1. an optimized thing that checks for grouping-changes in the indices
  //                  do this as a function that accepts a sorted list and outputs indices where it changes: O(n) then exp-search strat
  //                  (we could also have it accept grades+list instead)
  //                  hack O(n) is where (NAN join -1 drop original) not_equal original
  //           sub-2. a thingy that puts the keys back in the natural order
  //                  actually, it's not a big deal and you can reconstruct it yourself: sort by values in the part
  //
  //warning: be careful about depending on sort: if you lose sort order of
  //keys, you can mess up things like table operations
  //t:{{a: 0 0 0 1 1 1 2, PRICE:10 20 30 100 200 300 NAN}}; select msum(4,PRICE) from t group by a

  SW(xt)
  {
    CS(HASH, return part_hash(x))

    CS(INTVEC, return part_intvec(x))

    CD: break;
  }

  if(!IS_ARRAY(x)) return strong(x);

  K z = new_map();

  ENUM(x, K key = v;
          K value = NULL;
          
          I position = lookupI(z, key); 
          bool exists = !IS_HASH_NULL(position);
          if(!exists)
          {
            value = enlist(ki(i));
            insert_replace_ri_rd(z, key, value, true, false, false, false);
            rd(value);//bug? ^^ does not respect increment setting
          }
          else
          {
            K0 o;
            K existing = LOOK_(zValues, position,o);
            existing = cow_add(existing, ki(i));
            value = existing;
            insert_replace_ri_rd(z, key, value, true, false, false, false);
          }
  )

  //Note: `part` currently does not sort keys. Sort order of original list is maintained

  if(!VERB_MONADIC_GROUP_RETURNS_MAP)
  {

    K r = strong(zValues);
    rd(z);
    return r;
  }

  return z;
}

K til(K x)
{
  I n=0;

  SW(xt)
  {
    CS(FLOAT, n=xf)
    CS(INT,   n=xi)
  }

  if(n<0) n=0;

  K z = new_k(-INT,n);
  DO(n,zI[i]=i)
  SET_ATTR(z, ATTR_SORTED);
  return z;
}

K which(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //can precalc size of z
  //can work directly with I
  K z = new_k(INTVEC, 0);

  SW(xt)
  {
   CSF(CHAR,)
    CS(CHARVEC,
               C *ix = (C*)PAYLOAD_START(x);
               DO(COUNT(x),  I p = i; C n = ix[p]; if(n>0)  { DO2(n, z = cow_add(z, ki(p))) })
    )

   CSF(INT,)
    CS(INTVEC,
               I *ix = (I*)PAYLOAD_START(x);
               DO(COUNT(x),  I p = i; I n = ix[p]; if(n>0)  { DO2(n, z = cow_add(z, ki(p))) })
    )

   CSF(STAMP,)
    CS(STAMPVEC,
               I *ix = (I*)PAYLOAD_START(x);
               DO(COUNT(x),  I p = i; I n = ix[p]; if(n!=0) { DO2(1, z = cow_add(z, ki(p))) })
    )

   CSF(FLOAT,)
    CS(FLOATVEC,
               F *ix = (F*)PAYLOAD_START(x);
               DO(COUNT(x),  I p = i; I n = ix[p]; if(n>0)  { DO2(n, z = cow_add(z, ki(p))) })
    )
  }

  return z;
}

K py_range(K w, K x, K y)
{
  if(w && x && y)
  { 
    return ex("$1 + $3 * range ceil ($2 - $1) / $3", w, x, y);
  }

  if(w && x)
  {
    if(INT==wt && INT==xt)
    {
      I n = MAX(0, xi - wi);
      K z = new_k(INTVEC, n);
      DO(n, zI[i] = wi + i)
      return z;
    }

    return ex("$1 + range $2 - $1", w, x);
  }

  return enumer(w);
}

K xshift(K x, K y, K z)
{
  if(x && y && z)
  {
    return ex("repeat(min($1, count $2),$3) join drop(-$1,$2) join repeat(min(-$1, count $2),$3)",x,y,z);
  }
  //vvvv works with -x ^^^^
  return ex("$2[(range count $2) - $1]",x,y);
}

K xisnull(K x, K y)
{
  if(x && y)
  {
    if(!IS_ARRAY(x)) return (!is_null_atom(x))?strong(x):strong(y);
    return ex("{[x,y] x[which isnull x]:y}($1,$2)",x,y);
  }

  SW(xt)
  {
    //TODO
    //CR(CHARVEC,  )
    //CR(STAMPVEC, )
    CR(INTVEC,   ex("NAN=$1",x))
    CR(FLOATVEC, ex("nan=$1",x))

    CD: return Ki(is_null_atom(x));
        break;
  }

  ERROR(ERROR_TYPE);
  return Ki(1);
}

K enlist(K x)
{
  C t=VECTOR_ATOM(xt)?-xt:LIST;
  K y=NULL; 
  SW(t)
  {
   CSF(-LINK,)
   CSF(-CHAR,)
   CSF(-STAMP,)
   CSF(-FLOAT,)
    CS(-INT, y=new_k(t,1); *yI=xi)
   CSF( LIST,)
    CD: y=new_k(t,0); y=cow_add(y,x);
  }

  SET_ATTR(y, ATTR_SORTED);
  R y;
}

K take(K x, K y)
{ 
  //POTENTIAL_OPTIMIZATION_POINT
  //This could be maybe 10x faster
  //One way of many is to switch to
  //using ENUM or something similar

  if(MAP == yt)
  {
    return take(x, yValues); //don't error b/c it's very nice elsewhere to have this, eg take(0,map)
  }

  if(ATLAS == yt || TABLE == yt || HASH == yt)
  {
    I c = lenI(y);
    I m = INTIFY(x);
    I k = 0;
    
    if(c > 0)
    {
      k = m%c; //was bug: floating-point exception
    }
    else
    {
      k = 0;

      if(HASH == yt)
      {
        //POTENTIAL_OPTIMIZATION_POINT
        //will anyone even notice?
        //the real way to do this is to build
        //a one-element INDEX->KEYS portion and a INTIFY(x) element 
        //KEYS portion of all zeros and form an intern out of that
        return ex("enum take($1,$2)",x,kN(yIndex,KEYS));
      }
    }
  
    k = (k<0)?(c+k):0;
    K z=new_k(INTVEC, ABS(m));
    DO(zn, if(k>=c)k=0; zI[i] = k; k++) 
    work_push(z);
    K a = at(y,z);
    work_pop_rd(true);
    return a;
  }

  C t = FUNC!=yt && yt<=STAMP?-ayt:LIST;

  I m = INTIFY(x);
  
  if(0==cy)
  {
    SW(yt)
    {
      CR(HASH, new_intern())
      CR(BTREE, new_btree())
    }

    K0 empty = empty_atom(y);
    R take(x, &empty);
  }

  I n = ABS(m);

  I k=m%cy;
  k=k<0?cy+k:0;

  if(IS_ARRAY(y) && n==cy) return strong(y);

  K z=new_k(t,n);

  if(BYTEY(y)) SET_ATTR(z,ATTR_BYTES);

  SW(yt)
  {
    CS(CHAR,  DO(n, zC[i]=yc))
   CSF(STAMP,)
    CS(INT,   DO(n, zI[i]=yi))
    CS(FLOAT, DO(n, zF[i]=yf))
    CD:
        DO(n, if(k>=cy)k=0; K0 o; K v = LOOK_(y,k,o); z=update_ri_rd(z,ki(i),v,true,false); k++)
        break;
  }

  if(1==ECOUNT(y)) SET_ATTR(z, ATTR_SORTED);
  //POTENTIAL_OPTIMIZATION_POINT
  //(the original line probably redundant given that new_k sets)
  //There are a lot more cases where TAKE produces
  //sorted lists
  
  z = cow_demote(z);

  if(SORT_INDEX_YIELDS_SORT_NOT_LIST)
  {
    if(BTREE == yt)
    {
      work_push(z);
      K b = new_btree_from_K(z);
      work_pop_rd(true);
      return b;
    }
  }

  return z;
}

K explode(K x, K y)
{
  C t = LIST;
  
  if(IS_VECTOR(y))
  {
    t = yt;
  }

  K z = new_k(t, 0);
  K w = new_k(LIST, 0); 

  ENUM(y, 
    if(matchC(v,x,true))
    {
      w = cow_add(w,z);
      rd(z);
      z = new_k(t,0);
    }
    else 
    {
      z = cow_add(z, v);
    }
  )

  w = cow_add(w,z);
  rd(z);
  return w;
} 

K implode(K x, K y)
{
  K z = new_k(LIST, 0);
  work_push(z);
  ENUM(y, 
    if(i>0)
    {
      work_pop(); z = cow_join(z,x); work_push(z);
    }
      work_pop(); z = cow_join(z,v); work_push(z);
  )
  return work_pop();
}

K count_null(K x)
{
  I k = 0;

  if(IS_ARRAY(x))
  {

    if(gby_hacktivated(x))
    {
      K z = new_k(INTVEC, gby_hack_count);
      zero_list_payload(z);
      K y = gby_hack;

      SW(xt)
      {
        CS(INTVEC,   DO(xn, if(  IN==xI[i] ) zI[yI[i]]++ ))
        CS(FLOATVEC, DO(xn, if(isnan(xF[i])) zI[yI[i]]++ ))
        CD:  ENUM(x, if(is_null_atom(v))     zI[yI[i]]++  )
      }
 
      return z;
    }
 
    SW(xt)
    {
      CS(INTVEC,   DO(xn, if(  IN==xI[i] )k++))
      CS(FLOATVEC, DO(xn, if(isnan(xF[i]))k++))
      CD:  ENUM(x, if(is_null_atom(v))    k++ )
    }
    return Ki(k);
  }

  return ex("sum isnull $1", x);  
}

K count_nonnull(K x)
{
  I k = 0;

  if(IS_ARRAY(x))
  {

    if(gby_hacktivated(x))
    {
      K z = new_k(INTVEC, gby_hack_count);
      zero_list_payload(z);
      K y = gby_hack;

      SW(xt)
      {
        CS(INTVEC,   DO(xn, if(   IN!=xI[i] )zI[yI[i]]++))
        CS(FLOATVEC, DO(xn, if(!isnan(xF[i]))zI[yI[i]]++))
        CD:  ENUM(x, if(!is_null_atom(v))    zI[yI[i]]++ )
      }

      return z;
    }
    
    SW(xt)
    {
      CS(INTVEC,   DO(xn, if(   IN!=xI[i] )k++))
      CS(FLOATVEC, DO(xn, if(!isnan(xF[i]))k++))
      CD:  ENUM(x, if(!is_null_atom(v))    k++ )
    }
    return Ki(k);
  }

  return ex("sum not isnull $1", x);  
}

K car(K x){ return xcar(x, NULL);}

K xcar(K x, K y)
{
  if(x && y) return take(x,y);

  if(gby_hacktivated(x))
  {
    K z = take(ki(gby_hack_count), ki(-1));
    K y = gby_hack;
    DO(yn, I *k = &zI[yI[i]]; if(-1 == *k) *k = i)
    work_push(z);
    K firsts = at(x, z);
    work_pop_rd(true);
    return firsts;
  }

  //may want to refactor this function and K last(). didn't spend much time on either
  if(IS_TABLE(x) || IS_ATLAS(x)) return at(x,ki(0));

  if(!IS_ARRAY(x)) return strong(x);

  bool throws_errors = false;

  K0 o;
  return strong(_AT(x, ki(0), false, throws_errors, o));
}

K xcdr(K x)
{
  return drop(ki(1),x);
}

K last(K x, K y)
{
  if(x && y) return ex("take(-$1, $2)", x, y);


  if(gby_hacktivated(x))
  {
    K z = new_k(INTVEC, gby_hack_count);
    zero_list_payload(z);
    K y = gby_hack;
    DO(yn, zI[yI[i]] = i)
    work_push(z);
    K lasts = at(x, z);
    work_pop_rd(true);
    return lasts;
  }

  if(IS_TABLE(x) || IS_ATLAS(x)) return at(x,ki(lenI(x)-1));
  if(!IS_ARRAY(x)) return strong(x);
  bool throws_errors = false;
  K0 o;
  return strong(_AT(x, ki(cx-1), false, throws_errors, o));
}

K reverse(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //spent 0 time optimizing this 

  if(IS_ATOM(x)) return strong(x);

  I n = COUNT(x);
  K z = new_k(LIST, n);
  zn = 0;

  K0 o;
  DO(n, z = cow_add(z, AT2(x, ki(n-i-1),o)))

  return z;
}

K flatten(K x, K y)
{
  if(x && y)
  {
    return ex("flatten [$1, flatten $2]", x, y); 
  }

  if(!IS_MIXED_ARRAY(x) || 0==COUNT(x)) return strong(x);

  //POTENTIAL_OPTIMIZATION_POINT
  //doubtless there are a variety of ways to make this faster
  //i. descend into the values part of BTREE
  //ii. pre-alloc z to the correct size
  //iii. etc.

  K z = at(x, ki(0));

  if(IS_DISK(z))
  {
    K tmp = copy_override(z);
    rd(z);
    z = tmp;
  }
  else
  {
    z = cow(z);
  }

  ENUM(x, if(0==i)continue; z = cow_join(z, v))
  
  return z;
}

K drop(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //spent 0 time optimizing this 

  //TODO: preserve HASH, right? (& support TABLE if not already)

  if(IS_ATOM(y)) return strong(y);

  I k = INTIFY(x);

  I start = 0;
  I end = COUNT(y);
  
  if(k >= 0) start += k;
  else end += k;

  start = MIN(start, end);
  end = MAX(start, end);

  I n = end - start;
  I kind = LIST;

  if(IS_VECTOR(y)) kind = yt;

  K z = new_k(LIST, n);
  zn = 0;

  K0 o;
  DO(n, z = cow_add_funny(z, AT2(y, ki(start + i),o)))

  return z;
}

K repeat(K x, K y)
{
  return ex("max($1,0) take enlist $2", x, y);
}

#pragma mark - Cast
I INTIFY(K x) //CAST_TO_INT
{
  SW(xt)
  {
    CR(INT, xi)
    CR(FLOAT, xf)
    CR(CHAR, xc)
    CS(INTVEC, if(0==xn)R 0; R !!xI[0])
    CD: break; 
  }

  return 0LL;
}

F FLOATIFY(K x) //CAST_TO_FLOAT
{
  SW(xt)
  {
    CR(INT, xi)
    CR(FLOAT, xf)
    CR(CHAR, xc)
    CD: break; 
  }

  return 0.0;
}

K parse_int(K x, K y)
{
  I radix = y ? INTIFY(y) : 10;
  if (radix < 2 || radix > 36) { ERROR(ERROR_RADIX); }
  if (!IS_STRING(x)) { ERROR(ERROR_TYPE); }

  // strtoll requires a null terminated C-string, so we're forced to copy:
  K z = new_k(CHARVEC, xn+1);
  snprintf(zC, xn+1, "%s", xC);
  work_push(z);
  K r = Ki(strtoll(zC, NULL, radix));
  work_pop_rd(true);

  return r;
}

K parse_float(K x)
{
  K y = parse_number(x);
  K z = Kf(FLOATIFY(y));
  rd(y);

  return z;
}

K int_cast(K x)
{
  //TODO: maybe for hash/sort we keep index, only the payload part cast to intvec

  SW(xt)
  {
   CSF(INTVEC,)
    CS(INT, return strong(x))

   CSF(FLOATVEC, K z = new_k(INTVEC, xn); ENUM(x, zI[i] = xF[i]); return z; )
    CS(FLOAT, return Ki(xf))

   CSF(CHARVEC, K z = new_k(INTVEC, xn); ENUM(x, zI[i] = xC[i]); return z; )
    CS(CHAR, return Ki(INTIFY(x)))

   CSF(FUNC,)
    CS(NIL, return Ki(0))

    CD: 
        if(IS_ARRAY(x))
        {
          K z = new_k(INTVEC, cx);
          zn=0;
          ENUM(x, K a = int_cast(v);  z = cow_add(z,a); rd(a))
          return z;
        }
        return new_k(INTVEC, 0);
  }

}

K float_cast(K x)
{
  //TODO: maybe for hash/sort we keep index, only the payload part cast to floatvec

  SW(xt)
  {
   CSF(FLOATVEC,)
    CS(FLOAT, return strong(x))

   CSF(INTVEC, K z = new_k(FLOATVEC, xn); ENUM(x, zF[i] = xI[i]); return z; )
    CS(INT,    return Kf(xi))

   CSF(CHARVEC,) //TODO: this is for strings but not bytes
    CS(CHAR,
      K y = parse_number(x);
      K z = Kf(FLOATIFY(y));
      rd(y);
      return z;
    )

   CSF(FUNC,)
    CS(NIL, return Kf(0))

    CD: 
        if(IS_ARRAY(x))
        {
          K z = new_k(FLOATVEC, cx);
          zn = 0;
          ENUM(x, K a = float_cast(v);  z = cow_add(z,a); rd(a))
          return z;
        }
        return new_k(FLOATVEC, 0);
  }

}

K char_cast(K x)
{
  //TODO: maybe for hash/sort we keep index, only the payload part cast to...

  SW(xt)
  {

   CSF(FLOATVEC,)
    CS(INTVEC, K z = new_k(CHARVEC, xn); ENUM(x, zC[i] = (UC)INTIFY(v)); return z)

   CSF(FLOAT,)
    CS(INT, return Kc((UC)INTIFY(x)))

    CS(CHARVEC, C c = ' '; if(xn>0) c = xC[0]; return Kc(c))  //TODO: this is for strings but not bytes
    CS(CHAR, return strong(x))

   CSF(FUNC,)
    CS(NIL, return strong(kcv("")))

    CD: 
        if(IS_ARRAY(x))
        {
          K z = new_k(CHARVEC, cx);
          zn = 0;
          ENUM(x, K a = char_cast(v);  z = cow_add(z,a); rd(a))
          return z;
        }
        return strong(kcv(""));
  }
}

K stamp_cast(K x)
{
  //TODO: just filled this out quickly to squash STAMP[] error

  SW(xt)
  {
    CS(CHARVEC, return monad_eval(x))
    CD: 
       if(IS_ARRAY(x))
        {
          K z = new_k(STAMPVEC, cx);
          zn = 0;
          ENUM(x, K a = stamp_cast(v);  z = cow_add(z,a); rd(a))
          return z;
        }

        return Ks(SN);
  }

}

K string_cast_I(I i)
{ 
  I n = snprintf(0, 0, "%lld", i);
  K z = new_k(CHARVEC, n + 1);
  zn = n;
  snprintf(zC, n + 1, "%lld", i); 
  R z;
}


K string_cast_F(F f, I points, I c)
{ 
  S fmt = NULL; 

  SW(c)
  {
    CD:   fmt = "%.*g";
    CS(1, fmt = "%.*f")
    CS(2, fmt = "%.*e")
  }

  I n = snprintf(0, 0, fmt, points, f);  
  K z = new_k(CHARVEC, n + 1);
  zn = n;
  snprintf(zC, n+1, fmt, points, f);
  R z;
}

K string_cast_D(K x, bool local, int decimal_places)
{ 
  stampbuf buf = datetime_from_stamp(x, local, decimal_places);
  I n = strlen((S)&buf);
  K z = new_k(CHARVEC, n + 1);
  zn = n;
  //POTENTIAL_OPTIMIZATION_POINT
  //could limit it to only strlen...does it even make a difference?
  memcpy(zC, &buf, n+1);
  R z;
}

K string_cast(K x)
{
  bool local = false; 
  int stamp_decimals = 3;

  SW(xt)
  {
    CS(CHAR,    return enlist(x)) //TODO: special cast for BYTE
    CS(INT,     return string_cast_I(xi))
    CS(FLOAT,   return string_cast_F(xf, The_Precision, 0))
    CS(STAMP,   return string_cast_D(x, local, stamp_decimals))

    //POTENTIAL_OPTIMIZATION_POINT: fixed z size, update in place?
    CS(CHARVEC, return strong(x)) //TODO: special cast for BYTEVEC
    CS(INTVEC,   K z = Kk(); ENUM(x, K e = string_cast_I(si);                       z = cow_add(z, e); rd(e)) return z)
    CS(FLOATVEC, K z = Kk(); ENUM(x, K e = string_cast_F(sf, The_Precision, 0);     z = cow_add(z, e); rd(e)) return z)
    CS(STAMPVEC, K z = Kk(); ENUM(x, K e = string_cast_D(v, local, stamp_decimals); z = cow_add(z, e); rd(e)) return z)

    CS(NIL,  return strong(kcv("null")))

    CS(FUNC, )//TODO: model on/factor with puts.c
    CS(MAP, if(IS_DTBUCKET(x)){ }) //TODO model on/factor with puts.c
  }

  return strong(kcv("nyi-string-cast"));
}

#pragma mark -

K trim(K x)
{
  if(!IS_STRING(x)) return strong(x);

  I start = 0;
  I end   = cx;

  while(isspace(xC[start]) && start < end)start++;
  while(isspace(xC[end-1]) && end > start)end--;

  I n = end - start;
  K z = new_k(-CHAR, n);

  DO(n, zC[i] = xC[start + i])

  return z;
}

K timing(K x)
{
  if(truthy(x))
  {
    The_Timing = 1;
  }
  else
  {
    The_Timing = 0;
  }

  return Ki(The_Timing);
}


K emu_debug_mode(K x)
{
  if(truthy(x))
  {
    The_Emu_Debug_Flag = 1;
  }
  else
  {
    The_Emu_Debug_Flag = 0;
  }
  return Ki(The_Emu_Debug_Flag);
}

K reserved_verb(K x)
{
  //In the future, we'll want to return reserved words as a map of lists of strings keyed by type (eg "verb" or whatnot)
  K z = strong(kN(The_Reserved, KEYS));

  ENUM(z, if(COUNT(v)>0 && isdigit(*kC(v)))continue; easy_show2(v,false,0,false); O(" "));

  O("\n\n");

  return z;
}

K xsleep(K x)
{
  I m = INTIFY(x);
  usleep(m * 1000);//millis
  return Ki(m);
}

K xexit(K x)
{
  I code = 0;

  if(INT == xt) code = xi;

  kerf_exit(code);

  assert(1==0);
  
  return Ki(code); //unreachable
}

K verb_reset(K x)
{
  if (!x) { x = ki(0); }

  bool wipe_args = truthy(x);

  if(IS_NIL(x)) wipe_args = false; //explicitly don't wipe argv if no reset arg

  kerf_reset(wipe_args);

  return Kn(); // unreachable
}

K xobj_keys(K x)
{
  if(IS_ATLAS(x))
  {
    K sort = xKeys;
    K intern = kN(sort, KEYS);

    return ex("{[x,y] z: unique x; z[which z != y]}[$1,$2]", intern, kcv(ATLAS_REVERSE_KEY));
  }

  if(IS_TABLE(x) || IS_MAP(x)) return strong(xKeys); 
  if(IS_ARRAY(x)) return til(ki(lenI(x)));

  ERROR(ERROR_TYPE);
  return Kn();
}

K xobj_values(K x)
{
  if(IS_TABLE(x) || IS_MAP(x)) return strong(xValues); 
  if(IS_ARRAY(x)) return til(ki(lenI(x)));

  ERROR(ERROR_TYPE);
  return Kn();
}

K xmap(K w, K x)
{
  if(!x)//??? we could do anything here, for casting to map
  {
    SW(wt)
    {
      CS(MAP, return strong(w))
      CS(CHARVEC, K4 o1,o2; return xmap(klist1(w,o1), klist1(w,o2)))
      //CS(TABLE, return ex("map(keys $1, first mapright values $1)", w))
      CD: x = w; break;
    }
  }

  w = strong(w);
  x = strong(x);

  w = cow_coerce_array(w);
  work_push(w);

  x = cow_coerce_array(x);
  work_push(x);

  K map = new_map_from_K_K(w, x, false, false);

  if(!map) ERROR(ERROR_MAP);

  work_pop_n_rd(2, true);

  return map;
}

K xtable(K x, K y)
{
  if(!y)
  {
    K z = new_table();
    ENUM(x, z = cow_table_add_column(z, u, kk))
    return z;
  }

  if (lenI(x) != lenI(y)) { ERROR(ERROR_LENGTH); }

  if (lenI(y) == 0) { return new_table(); }

  // if we have a nonzero length set of columns, they must have identical length
  K first = at(y, ki(0));
  I first_len = lenI(first);
  rd(first);
  bool ragged = false;
  SATOM(y, ragged |= (lenI(u) != first_len));
  if (ragged) { ERROR(ERROR_RAGGED); }

  K t = new_table();
  SATOM2(x, y,
    if (MAKE_TABLE_USES_LAST_BINDING)
    {
      t = update(t, u, v);
    }
    else
    {
      t = cow_table_add_column(t, u, v);
    }
  );

  return t;
}

K stronger_decoder(K x, K y, K column_decoder)//reference increment if exists, populate if null
{
  if(column_decoder) return strong(column_decoder);
  else return decode_columns(x, y);
}

K decode_columns(K x, K y)//x:needle, y:haystack
{
  if(!IS_MIXED_KEYED(x) || !IS_MIXED_KEYED(y)) ERROR(ERROR_KEYS);
 
  K z = new_k(INTVEC, COUNT(xKeys));
  ENUM(x, I p = lookupI(y, u);
          if(IS_HASH_NULL(p)) p = IN;
          zI[i]=p;
  )

  return z;
}

K xkerf_type(K x)
{
  return Ki(xt);
}

K xhash(K x)
{
  return Ki(hash(x));
}

S kerf_type_name_cstring_from_char(C c)
{
  SW(c)
  {
    CR(STAMPVEC, "stamp vector")
    CR(FLOATVEC, "float vector")
    CR(INTVEC  , "integer vector")
    CR(CHARVEC , "character vector")
    CR(FUNC    , "function")
    CR(CHAR    , "character")
    CR(INT     , "integer")
    CR(FLOAT   , "float")
    CR(STAMP   , "stamp")
    CR(NIL     , "null")
    CR(LIST    , "list")
    CR(MAP     , "map")
    CR(HASH    , "enum")
    CR(BTREE    , "btree sort")
    CR(TABLE   , "table")
    CR(ATLAS   , "atlas")
    CR(DATABASE, "database")
    CR(LINK    , "link")
    CR(JUMP    , "jump")
    CR(ZIP     , "zip")
    CR(EMU     , "emu")
    CR(PARTABLE, "parceled table")
    CD: break;
  }

  return "nyi-type-unknown";
}

K xkerf_type_name(K x)
{
  return charvec_from_cstring(kerf_type_name_cstring_from_char(xt));
}

K xtables(K x)
{
  //todo: if IS_NIL(x) && IS_STRING(x) use x as dir
  K0 o;
  K1 k1;
  K c = denest_create(KVM->KERFDIR, true, o, k1); 

  K z = new_k(LIST, 0);

  ENUM(c, if(IS_TABLE(v)) z = cow_add(z, u))

  return z;
}

K xdelete(K x, K y)
{
  //TODO: table deletes rows if numbers (use cow_delete_indices not _keys)
  y = cow_coerce_array(strong(y));
  work_push(y);

  K z = cow_delete(strong(x),y);

  work_pop_rd(true);

  return z;
}

K xhas_column(K x, K y)
{
  return Ki(table_has_column(x,y));
}

K xhas_key(K x, K y)
{
  SW(xt)
  {
    CR(TABLE, xhas_column(x,y))
    CR(MAP, K0 o; Ki(LOOKUP_(x,y,o)?1:0))
  }

  if(IS_ARRAY(x) && (INT == yt || FLOAT == yt))
  {
    I i = INTIFY(y);
    bool has = (0 <= i && i < COUNT(x));
    return Ki(has?1:0);
  }
  
  ERROR(ERROR_TYPE);
  return Ki(0);//could update arrays to 
}

K xsystem(K x) // 'clear' terminal command currently depends on this
{
  if(!IS_STRING(x))ERROR(ERROR_STRING);

  K y = copy(x);
  y = cow_ensure_null_terminated_chars(y);

  work_push(y);
 
  system(yC);

  work_pop_rd(true);
  return Kn();
}

K xshell(K x)
{
  //useful opts:
  //returns_output_as_{lines, string} and sync/async


  if(!IS_STRING(x)) ERROR(ERROR_STRING);

  K y = copy(x);
  y = cow_ensure_null_terminated_chars(y);
  work_push(y);
 
  FILE *f = popen(yC, "r"); 

  if(!f) goto failed;
  
  K z = new_k(LIST, 0);

  S s = NULL; 
  size_t n = 0;
  I k = 0;
  while(getline(&s, &n, f) >= 0)
  {
    I k = strlen(s);
    if(k>0 && s[k-1]=='\n') k--;
    K l = new_k(CHARVEC, k); 
    strncpy(kC(l), s, k);
    z = cow_add(z, l);
    rd(l);
  }

  free(s);
  pclose(f);

  work_pop_rd(true);

  return z; 
failed:
  work_pop_rd(true);
  return Kn();
}

K sort_debug(K x)
{
  K y = new_map();
  if(IS_FUNC(x)) ERROR(ERROR_STRING);

  bool attr = SORTED(x);
  bool is_array = IS_ARRAY(x);
  bool array_sorted = is_array && is_sorted_array(x); 
  bool is_index = BTREE == xt;
  bool is_enum = HASH == xt;
  bool index_is_working = is_index &&  landis_is_sorted(x);
  bool is_table = TABLE ==xt;
  bool is_table_sorted = is_table && table_is_sorted(x);

  update(y, kcv("attr_sorted"),              ki(attr));
  update(y, kcv("is_array"),                 ki(is_array));
  update(y, kcv("is_enum"),                  ki(is_enum));
  update(y, kcv("is_actually_sorted_array"), ki(array_sorted));
  update(y, kcv("is_index"),                 ki(is_index));
  update(y, kcv("is_index_working"),         ki(index_is_working));
  update(y, kcv("is_table"),                 ki(is_table));
  update(y, kcv("is_table_sorted"),          ki(is_table_sorted));

  //if we're going to bother to check for sortedness, may as well set ATTR_SORTED while we're at it
  //  setting at end so we can see starting value
  if (is_table && is_table_sorted && !attr)
    {
        SET_ATTR(x, ATTR_SORTED);
    }

  return y;
}

K xmkdir(K x)
{
  //TODO should probably use kerf_mkdir, but in the interests of doing it quicklyuseful opts:
  //returns_output_as_{lines, string} and sync/async
  if(!IS_STRING(x)) ERROR(ERROR_STRING);
  return ex("shell \"mkdir -p \"#$1;",x);
}

K verb_split(K x, K y)
{
  return ex("$2[xvals part rsum (xkeys $2) in $1]", x,y);
}

K verb_ngram(K x, K y)
{
  //third argument could be a toggle dropping incomplete final list
  return ex("split(range(0,count($2),$1), $2)", x,y);
}

K verb_search(K x, K y) //needle, haystack
{
  I p = FINDER_FAIL;
  
  if(IS_MAP(y))
  {
    p = finder(x, yValues, NULL);

    if(FINDER_FAIL == p)
    {
      return Kn();
    }

    K0 o;
    return strong(LOOK_(yKeys, p, o));
  }

  p = finder(x, y, NULL);

  return Ki(p); 
}

K verb_filter(K f, K x)
{
  if (IS_MAP(x))
  {
    return ex("(xkeys $2)[t] map (xvals $2)[t: which $1 mapdown $2]", f,x);
  }
  return ex("$2[which $1 mapdown $2]", f,x);
}


K verb_help(K x)
{
  if(COUNT(x)==0)
    {
      fprintf(stderr, "\nHelp Menu. Try: help('list').\nPress return to see the next part of the table.\n");
      return ex("select unique subject from .Help");
    } else {
    return ex("{subjects: (flatten xvals select unique subject from .Help); \
                      if(in(enlist($1),subjects)[0]) { \
                         return(select usage,description from .Help where subject=$1);  \
                      } else {  \
                         return(select usage,description from .Help where namev=$1); }}",x);
    }
  }



