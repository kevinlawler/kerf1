#include "kerf.h"

//Table Level Traits
//--Column-Level Traits
//====Column Attributes (Key, Fkey, ...)
//====Autoincrement Counters
//====Foreign Key Incoming Links (for UPDATE referential integrity)
//====Default Values for Columns (none, type-specific defaults, constant, niladic function, NOW())
//====Column Display (Order, Width, Precision, ...)
//--Parallel Table Data (Distributed; want some idea of order)
//--Triggers (Views)
//--Table Indexes (Multi-column)
//--(Foreign Key Outgoing Links (for DELETE referential integrity))
//--Global Display (Order, Width, Precision, ...)

//([] id:(); bar:(); foo:())
//([] { id:(); bar:(); foo:()})
//({ id:(); bar:(); foo:()  })
//{{ id:`; bar:`; foo:`  }}
//{{ id:(); bar:(); foo:()  }}
//{{id:();...} {bar:();...} {foo:()}}
//{{column:id; vals:()} {column:bar; vals:()}{column:foo; vals:()}}
//[{atlas?}] : table: a ragged dict will reject (or transmute), not turn a table to an atlas
//             atlas: conforming dictionaries will not turn an atlas to a table 
//
//{{ PRIMARY KEY id; UNIQUE bar; foo }} //KEY, PRIMARY KEY, NOT NULL, UNIQUE, DEFAULT, ENUMERATION/HASH
//{{ txn_id; FOREIGN KEY (customers) customer_id;}}
//{{id:1 2 3;customer_name:("aye";"bee";"cee");time:2012Y 2013Y 2014Y}}
//{{id;customer_name;time}}

//code:     {[x]11+x} brackets must be present or is map (removes implicit. [] is niladic)
//map:      {a:1,b:2} and {a:1;b:2} and {"a":1,"b":2}... Commas parsed differently here (use parens for comma verb)
//list:     [1, "War & Peace", 2048] //alt comma.
//(atlas):  [{a:1,b:2},{b:2,c:3}] Ragged table (ragged list of maps (+indexed))
//table:    {{c,d,e}} and {{c;d;e}} Double braces. Rigid list of maps


#pragma mark - Atlas

K new_atlas()
{
  return new_atlas_from_K(kk);
}

K new_atlas_from_K(K x)
{
  if(IS_ATLAS(x)) return strong(x);

  K z = new_k(LIST, 0);

  K a = new_k(INTVEC, 0); 
  K b = new_intern();
  K c = new_k(LIST, 0);
  K4 o;
  K d = new_btree_from_strips(klist2(b,c,o));
  rd(b);rd(c);

  z = cow_add(z,a);
  z = cow_add(z,d);
  rd(a);rd(d);

  C n = zn;
  zn = 0;
  zt = ATLAS;
  z->na = 0;
  z->nn = n;
  z->nk = ATLAS_KIND_REGULAR;
  z->nf = 0;

  K g = strong(x);

  g = cow_coerce_array(g);
  work_push(g);
  work_push(z);

  //POTENTIAL_OPTIMIZATION_POINT:
  //Don't do this like this. Create the strips and let
  //new_btree_from_strips handle the initialization
  //this lets you A. perform a grade (faster)
  //instead of B. repeated landis-tree inserts

  ENUM(g, if(!IS_STRICT_MAP(v) || IS_ERROR(v)) ERROR(ERROR_ATLAS_MAP);
          work_pop();
          z = cow_atlas_add(z,v);
          work_push(z);
  )

  work_pop();
  work_pop_rd(true);

  return z; 
}

bool is_special_atlas_form(K x)
{
  //x is 'b' (as in ... where b)
  //y is atlas[...]
  //
  //We changed this to put the string first. Going string-first causes
  //errors to bubble faster, instead of potentially say adding 1 to the whole ATLAS or s/t
  //
  //produce [x, y] == ['b', atlas[...] ]
  //
  //alternative ways of proceeding include 
  //1. using a special attribute on the LIST
  //2. putting the atlas itself there (a copy of the top-level and ref incremented subelts),
  //   except with an additional item on the hacked atlas LIST (->nn), which is the string.
  //   this has the nice property of being sane whenever you're not looking for
  //   something special (but then you have to clean the thing later? which is bad)
  //3. we could hijack the LINK vector object

  bool is_special = (xt == LIST && xn == 2 && xIndex->t == CHARVEC && xKeys->t == ATLAS);
  return is_special;
}

ALWAYS_INLINE I atlas_count_keys_in_map_for_index(K x, I k)
{
  K fences = xIndex;
  K sort = xKeys;

  //[start, end) - half-open
  I start = kI(fences)[k];
  I end = start; 

  if(k < fences->n - 1)
  {
    end = kI(fences)[k+1];
  }
  else
  {
    end = COUNT(sort);
  }

  return end - start;
}

K atlas_preserving_integral_at(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //Faster to leverage existing pieces? (Existing AVL tree & so on?)
  //Note: Remember to reoffset reverse_indices when ATLAS_USES_REVERSE_KEYING

  //POTENTIAL_OPTIMIZATION_POINT
  //the landis/AVL/sort reindexing process may be slow. because we have the
  //original already-indexed tree, it's possible that we could  reuse that
  //somehow here for the subselection

  //atlas-index                   [INTVEC]
  //atlas-sort2-index             [INTVEC]
  //atlas-sort2-keys-ENUM-HASHSET [STRING ARRAY]
  //atlas-sort2-keys-ENUM-VALUES  [INTVEC]
  //atlas-sort2-values            [ARRAY]

  //If an index is bogus, make an empty map (w/ reverse key?)
  //  we could skip this for now if need be, since we can assert() won't happen
  //INDEX  will need to build
  //SORT2 new_btree_from_strips(klist2(HASH,VALUES))
  //  HASH   at(hash,  i) is fine
  //  VALUES at(values,i) plus fix reverse keys

  //Potential way forward:
  //expand atlas-index keys to sort2-keys and index?
  //  you can perform this calculation by subtracting indices
  //we can preserve the hashset and just index
  //linear scan with REVERSE KEYING to demarcate crossovers (atlas-index)
  //linear scan to fix the reverse keys
  //(or key counts)

  I n = lenI(x); 

  K fences      = xIndex;
  K sort        = xKeys;
  K intern      = kN(sort,   KEYS);
  K hashset     = kN(intern, INDEX);
  K indices     = kN(intern, KEYS);
  I reverse_key_index = lookupI(hashset, kcv(ATLAS_REVERSE_KEY));
  K payload = kN(sort, VALUES);

  I m = 0;

  K v = new_k(INTVEC, yn);
  DO(yn, I p = yI[i];
         vI[i] = m; 
         if(p < 0 || p >= n) continue;
         m += atlas_count_keys_in_map_for_index(x, p); ) 
  work_push(v);

  K expanded_indices = new_k(INTVEC, m);

  I total = 0;
  DO(yn, I p = yI[i]; 
         if(p < 0 || p >= n) continue;
         I keys = atlas_count_keys_in_map_for_index(x, p);
         DO2(keys, kI(expanded_indices)[total++] = kI(fences)[p] + j )
  )

  K hash = at(intern, expanded_indices);

  K values = at(payload, expanded_indices);

  values = cow(values); //gonna modify later

  rd(expanded_indices);
  work_push(hash);

  if(ATLAS_USES_REVERSE_KEYING)
  {
    //Fix reverse keys for the new regime
    DO(yn, 
            I start = kI(v)[i];
            //`values` can be a LIST, INTVEC, FLOATVEC, etc.
            values = update(values, ki(start), ki(i));
    )
  }

  work_push(values);

  K4 o;
  K sort2 = new_btree_from_strips(klist2(hash,values,o));

  work_pop_n_rd(2, true);

  work_push(sort2);

  K z = new_atlas();

  nestset_ri_rd(z, INDEX, v,     false, true);
  nestset_ri_rd(z, KEYS,  sort2, false, true);

  work_pop();//sort2
  work_pop();//v / fences

  return z;

  ////Hacky/Slow
  //K w = atlas_destroying_integral_at(x, y); 
  //K z = new_atlas_from_K(w);

  //rd(w);

  //return z;
}

K atlas_destroying_integral_at(K x, K y)
{
  if(IS_ATOM(y)) return simple_at(x,y);

  K z = new_k(LIST, cy);
  zn = 0;

  ENUM(y, K a = simple_at(x, v); z = cow_add(z, a); rd(a))

  return z;
}



K cow_atlas_add(K x, K y)
{
  I a =0; 
  if(!IS_STRICT_MAP(y) || IS_ERROR(y)) goto failed;

  x=cow(x);

  //There are two ways to do the fence-index:
  //A. i stores the start index of the map's keys in the sort
  //B. or i stores the end index of the map's keys in the sort
  //A is redundant since first value always 0
  //B is redundant since last value always -1 + COUNT(sort)
  //A&&B: deltas also retrievable from before/after entries in fence
  //lets go with A

  K fence = xIndex;
  K sort = xKeys;

  I nth_map = COUNT(fence);
  I start_key = COUNT(sort);

  //fence has all required changes now
  nestneu(x, INDEX, fence = cow_add(fence, ki(start_key)));

  //Flatten inserted map, resolving collisions of keys
  //collision resolution is officially undefined for now
  K flat = raze_map(y);

  //POTENTIAL_OPTIMIZATION_POINT
  //in the insert below...possibly...it is more efficient to cow_join all the keys,
  //cow_join all the values, then cause the SORT to index each of the added rows

  if(ATLAS_USES_REVERSE_KEYING)
  {
    K4 o;
    K entry = klist2(kcv(ATLAS_REVERSE_KEY), ki(nth_map),o);//splitting out kcv from klist2 causes crash under -Os
    nestneu(x, KEYS, sort = cow_sort_add_general(sort, entry));
  }

  ENUM(flat,K4 o; nestneu(x, KEYS, sort = cow_sort_add_general(sort, klist2(u,v,o))))
  rd(flat);

  return x;
failed:
  return x;
}

K deraze_map(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //no optimization thought put into this

  K z = new_map();
  ENUM(x, 
          K1 pair = denest_K1_from_start_and_key(z,u,true,false); 

          K parent = pair.u;
          I position = pair.v.i;

          if(parent) //success. && !IS_HASH_NULL(position) 
          {
            K0 o;
            K key = LOOK_(kN(parent,KEYS),position,o); 
            update(parent, key, v); 
          }
  )

  return z;
}

K raze_map(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //no optimization thought put into this

  bool already_flat = true;
  ENUM(xValues, if(IS_MAP(v)){already_flat = false; break;})
  if(already_flat) return strong(x);

  K z = new_map();

  ENUM(x, if(!IS_MAP(v))
          {
            z = update(z,u,v);
            
                  continue;
          }

          K y = raze_map(v);

          K front = u;
          ENUM(y, 
            K4 o;
            K key = implode(kc('.'), klist2(front, u,o));
            z = update(z, key, v);
            rd(key);
          )
          rd(y);
  )

  return z;
}

K atlas_at_string(K atlas, K string)
{
  //2016.08.19 kevin
  //our current thinking on this is that we should return an array of length
  //equal to the atlas' length, not necessarily of length equal to the number
  //of subelements where the string is populated as a key on the map at that
  //index. So for myatlas['b'] we would have values of length(myatlas), and not
  //length(select from myatlas where b ...), and null fill them with the type
  //of whatever the 'b' values appear to look like. The benefit of this
  //strategy is that ATLAS GROUP_BY methods seem to want to work this way, and
  //this plays nicely with other columnar formats in Kerf. The different string
  //indexings would after all otherwise be ragged, which means that none of
  //myatlas[['b','c','d']] could be expected to be conformable. Users who don't
  //want this uniformity property can always select out 'b' using `select from
  //myatlas where b ...` to get the other effect. I don't think it is an
  //absolute requirement that atlas string indexing should work this way, so if
  //you want to get rid of it, make sure you retain this method & related
  //methods for GROUP_BY and anything else that may still depend on it.

  //POTENTIAL_OPTIMIZATION_POINT
  //A. w.r.t the strategy to select out the real (keyed) values, then to
  //   build a fake index list, starting from 0N nulls to index into it. It may be
  //   faster to do this directly somehow. (Though you can't know what the
  //   type-specific null is without selecting out all of the items in the first
  //   place. So at most you would win by avoiding the intermediate index piece,
  //   and you'll still have to build the final return array.)
  //
  //B. When the count of the selected values is large relative to the total,
  //   it's probably faster to do a linear pass through the atlas,
  //   using the SORT2-sub-ENUM for fast key comparison, 
  //   (you could get fast bounds on the total by getting an I2 range from the SORT2's landis)
  //   since you are doing something the same size as the atlas anyway.
  //   OTOH, you don't know what the type null is ahead of time
  //   OTOOH, this does have the benefit of letting you skip the cow_sort above
  //   when preserving natural order, so it may win. You may even want to do
  //   the 'b' (populated key) collection linearly, then go back and do the null fill.
  //   It also avoids having to do the key-to-atlas index reversing. So that's two.
  //   I bet this method beats the other method and the existing method.
  //
  //C. For statements of the form "select a,b,c from myatlas" there may be a similar
  //   speedup to method B above, where you gather all of a,b,c in one linear pass.

  K sort   = kN(atlas, KEYS);
  I table_equal[3] = {0,1,0};
  K4 o;
  K indices = sort_type_compare_pre_whered(sort, klist1(string,o), table_equal, false, false);

  //not sorting indices returns atlases in sorted order
  bool atlas_string_index_preserves_natural_order = true; 
  if(atlas_string_index_preserves_natural_order)
  {
    indices = cow_sort(indices); 
  }

  work_push(indices);

  K sort_vals = kN(sort, VALUES);

  K keyed_vals = at(sort_vals, indices);
  work_push(keyed_vals);


  K deindexed = atlas_indices_from_key_indices(atlas, indices);
  work_push(deindexed);

  I n = lenI(atlas);
 
  //Expand the indexes where the key (string) is populated to have nulls
  //where it isn't.

  // 0N 0N 0N 0N 0N 0N...
  K full = take(ki(n), ki(IN));
  OFF_ATTR(full, ATTR_SORTED);
  work_push(full);

  // 0N 0N  2 0N 0N 5, if args are 2 5 ...
  DO(deindexed->n, I k = kI(deindexed)[i]; kI(full)[k] = i;)

  K z = at(keyed_vals, full);

  work_pop_n_rd(4, true);

  return z;
}

K atlas_map_at_index(K x, I k)
{
  K z = new_map();

  K fences = xIndex;
  K sort = xKeys;

  if(k < 0 || fences->n <= k) return z;  

  //POTENTIAL_OPTIMIZATION_POINT
  //try making list of keys, vals, then using new_map_from_k_k(.)
  //maybe update() is improved too since we know it's map

  //could factor with the newer function atlas_count_keys_in_map_for_index
  //[start, end) - half-open
  I start = kI(fences)[k];
  I end = start; 
  if(k < fences->n - 1)
  {
    end = kI(fences)[k+1];
  }
  else
  {
    end = COUNT(sort);
  }

  I i = 0;

  K keys = kN(sort, KEYS);
  K values = kN(sort, VALUES);

  for(i = start; i < end; i++)
  {
    if(ATLAS_USES_REVERSE_KEYING)
    {
      if(i == start) continue;
    }

    K0 o1,o2;
    K key   = AT2(keys,   ki(i), o1);
    K value = AT2(values, ki(i), o2);

    z = update(z, key, value);
  }

  K y = deraze_map(z);
  rd(z);

  return y;
}

I atlas_index_from_key_index(K x, I i)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //is this slow to repeat everytime?
  K fence = xIndex;
  K sort = xKeys;

  bool bounds_checking = true;
  if(bounds_checking)
  {
    if(i < 0 || i > lenI(sort)) //could 0pt1m1ze this lenI
    {
      return IN;
    }
  }

  if(ATLAS_USES_REVERSE_KEYING)
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //cache these kN indexings in the parent (or just move all this code there)

    //POTENTIAL_OPTIMIZATION_POINT
    //cache the ATLAS_REVERSE_KEY's index in the ENUM-hashset
    K intern  = kN(sort,   KEYS);
    K hashset = kN(intern, INDEX);
    K indices = kN(intern, KEYS);
    I reverse_key_index = lookupI(hashset, kcv(ATLAS_REVERSE_KEY));

    K payload = kN(sort, VALUES);

    I j = i;

    while( kI(indices)[j] != reverse_key_index) j--;

    K0 o;
    return LOOK_(payload, j,o)->i;
  }
  else //bin search
  {
    //Note: I didn't test this method (which we had disabled via ..._REVERSE_KEYING)
    //      in order to save time.... =X   2016.08.15

    //Fence: [0, count(keys(atlas[0]), ...]

    //POTENTIAL_OPTIMIZATION_POINT
    //repeated bin search probably isn't necessary/ideal
    //when in the calling parent, the number of key_indices is large
    //or close together and so on

    I2 range = ranger(fence, ki(i), NULL);
    I place = range.x;
    //Note: bounds checking is not necessary here because we can always depend on the item to be present
    return place - 1;
  }

}

K atlas_indices_from_key_indices(K x, K y)
{
  //Note: This doesn't dedupe at all 
  // so for {[ {a:10,b:20} ]}
  // if y==[0,1,1] output is [0,0,0]

  //POTENTIAL_OPTIMIZATION_POINT
  //b/c the calling functions don't need y back (I think)
  //we could do z in place with a cow_ version

  K z = new_k(INTVEC, yn);

  DO(yn, zI[i] = atlas_index_from_key_index(x, yI[i]));

  return z;
}

int atlas_go()
{
  return 0;
  K x = new_atlas();

  //K y = ex("{b:22, c:33, d:44}");
  K y  = ex("{b:2,    c:3,    d.e:-1,    d:{e:4, f:5, g:{h:6, i:7 }}}");
  K y2 = ex("{b:22,   c:33,   d.e:-11,   d:{e:44, f:55, g:{h:66, i:77 }}}");
  K y3 = ex("{b:222,  c:333,  d.e:-111,  d:{e:444, f:555, g:{h:666, i:777 }}}");
  K y4 = ex("{b:2222, c:3333, d.e:-1111, d:{e:4444, f:5555, g:{h:6666, i:7777 }}}");
  //K y = ex("{b:22, c:33, d.e:-11, f:55}");

  cow_add(x,y);
  cow_add(x,y2);
  cow_add(x,y3);
  cow_add(x,y4);

  rd(y2); 
  rd(y3); 
  rd(y4); 

  //show(x);

  K z = xKeys;
//show(xIndex);
//show(z);
//show(zKeys);
//show(zValues);

  K4 o;
  K v = kdoublet(o);
  vI[0]=1;
  vI[1]=2;
  K a = at(x, v);

  //show(a);

  I count = COUNT(x);

  rd(x);
  rd(y);
  rd(a);

  R 0;
}

#pragma mark -

K meta_table(K x)
{
  if(!IS_TABLE(x))ERROR(ERROR_TABLE);

  K y = new_table();

  ENUM(x, 
          K name = u; 
          K vector = v; 
          K type_name = xkerf_type_name(vector);
          work_push(type_name);
          K z = ex("{column:$1,type:$2, type_name:$3, is_ascending:$4, is_disk:$5}", name, ki(vector->t), type_name, ki(SORTED(vector)), ki(IS_DISK(vector)));
          work_push(z);
          y = cow_table_add(y,z);
          work_pop_n_rd(2, true);
  )

  return y;
}

K new_table_x()
{
  K x = new_k(LIST,0);
  K z = new_map_from_K_K(x,x,false,true);
  rd(x);

  zt = TABLE;
  z->na = 0;
  z->nn = z->nn;
  z->nk = TABLE_KIND_REGULAR;
  z->nf = 0;

  return z;
}

#pragma mark - COW

K same_table_but_empty_columns(K table)
{
  //We avoid a copy() of the table here since
  //that doesn't work well with mapped items
  K x = new_table();

  ENUM(table,
    K empty = take(ki(0), v); //causes empty list types to match
    work_push(empty);
    x = cow_table_add_column(x, u, empty);
    work_pop_rd(true);

    K k = table_trait_get_column(table, u);
    if(k)
    {
      x = cow_table_trait_set_column(x, u, k);
      rd(k);
    }
  )

  return x;
}

K cow_table_join_tuple(K x, K y)
{
  K4 o1;
  K0 o2;
  if(LIST != yt)
  {
    //ERROR(ERROR_TYPE);
    y = klist1(y,o1); //we did this instead for `insert into {{}} values 2`
  }

  if(ECOUNT(x) != ECOUNT(y)) ERROR(ERROR_LENGTH); //columns must equal tuple count

  I rows = SENTINEL;

  //Hacked count -- see cow_table_insert
  ENUM(y, I c = COUNT(v); if(IS_STRING(v) || !IS_ARRAY(v)) c = 1; if(SENTINEL==rows) rows = c; else if(rows != c) ERROR(ERROR_LENGTH)) //ragged tuple

  x = cow(x);
  nestneu(x, VALUES, cow(xValues));

  ENUM(y, 
    //TODO: what about MISMATCHES (INT v FLOAT, BTREES.VALUES + HASH.VALUES TOO), MISSING_ENTRIES? 
    //      may need special "base atom" or "base vector" thing to simplify checking HASH/SORT

    K column = AT2(xValues, ki(i),o2);
    K longer = NULL;
    
    if(IS_ARRAY(v) && !IS_STRING(v))
    {
      longer = cow_join(column, v);
    }
    else
    {
      K a = strong(v);
      a = cow_coerce_column(a);
      longer = cow_join(column, a);
      rd(a);
    }

    if(!IS_STRIPED(xValues) && !IS_DISK(xValues)) update_ri_rd(xValues, ki(i), longer, false, false);
  )

  return x;
}

K cow_table_mesh(K x, K y, bool fill)
{
  I xcols = table_columns(x);

  I xrows = table_rows(x);
  I yrows = table_rows(y);

  bool starting = (0 == xcols);
  bool unequal = (xrows != yrows);
  bool nounity = (xrows != 1 && yrows != 1);
  bool nofill  = (!fill);

  if(!starting && unequal && (nofill || nounity)) ERROR(ERROR_ROW); 

  I desired_n = xrows;
  if(starting || 1==desired_n) desired_n = yrows;

  x = cow(x);
  nestneu(x, VALUES, cow(xValues));

  ENUM(y, x = cow_table_receive_column_from_table(x, y, u, true))

  if(fill)
  {
    ENUM(xKeys,
      K0 o;
      K a = AT2(xValues, ki(i),o); 
      assert(IS_ARRAY(a));

      I current_n = COUNT(a);

      if(current_n != desired_n)
      {
        K revised = take(ki(desired_n), a); 
        work_push(revised);
        update_ri_rd(xValues, ki(i), revised, false, true);
        work_pop_rd(false);
      }
    )
  }

  return x;
}

K cow_table_add(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //track sort flag: here, cow_add, cow_table_join_tuple (remove OFF_ATTR)
  //ATTR_SORT for a table is *all the columns* *left to right* or 2? or 1?

  //we could leverage PLY for join()
  //atomicity might handle each column, etc. - it doesn't

  x = cow(x);
  OFF_ATTR(x, ATTR_SORTED);

  nestneu(x, VALUES, cow(xValues));

  I columns = table_columns(x);
  
  bool no_columns = (0 == columns);

  K a = NULL;

  //empty table, which we assume is practically useless. we'll override its columns
  //in effect, the first insert will specify columns
  if(no_columns)
  {
    SW(yt)
    {
      CS(TABLE,  return x = cow_table_mesh(x,y,false))
      CS(MAP, a = table_cast_from_map(y);
              work_push(a);
              x = cow_table_mesh(x,a,false);
              work_pop_rd(true); 
              return x;
      )
      CD: a = ex("{{col}}");
          work_push(a);
          DO(ECOUNT(y), x = cow_table_receive_column_from_table(x, a, kcv("col"), true))
          work_pop_rd(true); 
          break;//fallthrough to next case system
    }
  }

  SW(yt)
  {
    CS(TABLE,
      if(ECOUNT(x) != ECOUNT(y)) ERROR(ERROR_LENGTH);//Note: could extend to use default vals

      K tuple = new_k(LIST, ECOUNT(x));
      tuple->n = 0;

      //POTENTIAL_OPTIMIZATION_POINT
      //could keep a static threadsafe tuple which we expand
      //based on high watermark of column count 
      //here and below in CS(MAP)

      work_push(tuple);

      ENUM(x, 
        I p = lookupI(y, u);
        if(IS_HASH_NULL(p)) ERROR(ERROR_COLUMN);
        K0 o;
        K vals = AT2(yValues, ki(p),o);
        tuple = cow_add(tuple, vals);
      ) 

      x = cow_table_join_tuple(x, tuple);

      work_pop_rd(true);

      return x;
    )
    CS(MAP,

      //POTENTIAL_OPTIMIZATION_POINT

      //Let's try this instead
      //Whatever was going on before this did these wrong
      //a:{{}}
      //insert into a values {b:'de', f:'gh'} //first
      //insert into a values {b:'de', f:'gh'} //second
      //and/or
      //a:{{}}
      //insert into a values {b:['de','qr'], f:['gh','st']} //first
      //insert into a values {b:['de','qr'], f:['gh','st']} //second

      a = table_cast_from_map(y);
      work_push(a);
      x = cow_table_add(x,a);
      work_pop_rd(true); 
      return x;

      //copy-pasta from above
      if(ECOUNT(x) != ECOUNT(y)) ERROR(ERROR_LENGTH);//Note: could extend to use default vals

      K tuple = new_k(LIST, ECOUNT(x));
      tuple->n = 0;
      work_push(tuple);

      ENUM(x, 
        I p = lookupI(y, u);
        if(IS_HASH_NULL(p)) ERROR(ERROR_COLUMN);
        K0 o;
        K vals = AT2(yValues, ki(p),o);
        K list = enlist(vals);
        work_push(list);
        tuple = cow_add(tuple, list);
        work_pop_rd(true);
      ) 

      x = cow_table_join_tuple(x, tuple);

      work_pop_rd(true);

      return x;
    )
    CD: return cow_table_join_tuple(x, y); 
  }

  assert(1==0); //unreachable

  return x;
}

#pragma mark - Table Traits > Attributes

K cow_table_set_column_attributes(K table, K column_name, C attributes)
{
  K4 o;
  K keys = klist2(column_name, kcv(TRAIT_ATTRIBUTES),o);

  K z = cow(table);
  nestneu(z, TRAITS, cow(zTraits));
  cow_change(zTraits, kcv(TRAIT_COLUMNS), keys, NULL, kc(attributes), true);

  return z;
}

K cow_table_bitwise_or_column_attributes(K table, K column_name, C attributes)
{
  C existing = table_column_attributes(table, column_name);
  C revised = (existing | attributes);
  return cow_table_set_column_attributes(table, column_name, revised);
}

C table_column_attributes(K table, K column_name)
{
  C c = 0;

  K keys[] = {kcv(TRAIT_COLUMNS), column_name, kcv(TRAIT_ATTRIBUTES)};

  K x = kN(table,TRAITS);

  DO(ARRAY_LEN(keys), K0 o; x = LOOKUP_(x, keys[i],o); if(!x) return c)
  //DO(ARRAY_LEN(keys), I p = lookupI(x, keys[i]); if(IS_HASH_NULL(p)) return c; x = LOOK_xValues,p);)

  c = xc;

  return c;
}

C table_column_has_attributes(K table, K column_name, C attributes)
{
  return (attributes == (attributes & table_column_attributes(table, column_name)));
}

K table_trait_get_column(K table, K column_name)
{
  K x = table;
  K4 o;
  K keys = klist2(kcv(TRAIT_COLUMNS), column_name,o);

  return map_lookup(xTraits, keys);
}

K cow_table_trait_set_column(K table, K column_name, K payload)
{
  K4 o;
  K keys = klist1(column_name, o);

  K z = cow(table);
  nestneu(z, TRAITS, cow(zTraits));
  cow_change(zTraits, kcv(TRAIT_COLUMNS), keys, NULL, payload, true);

  return z;
}

#pragma mark - Table Column Keys

bool table_column_is_key(K table, K column_name)
{
  C b = table_column_attributes(table, column_name);

  return (b & COLUMN_ATTR_KEY);
}

bool table_column_is_key_or_fkey(K table, K column_name)
{
  C b = table_column_attributes(table, column_name);

  return (b & (COLUMN_ATTR_KEY | COLUMN_ATTR_FKEY));
}

bool table_keys_in_other_table(K haystack, K needle)
{
  ENUM(needle, 

    if(!table_column_is_key(needle, u)) continue;
  
    if(!table_has_column(haystack, u) || !table_column_is_key(haystack, u))
    {
      return false;
    }
  )

  return true;
}

bool tables_match_on_keys(K x, K y)
{
  return table_keys_in_other_table(x,y) && table_keys_in_other_table(y,x);
}

I table_count_keys(K x)
{
  I c = 0;
  ENUM(x, if(table_column_is_key(x,u)) c++)
  return c;
}

K table_keyed_columns_list(K x)
{
  K z = Kk();
  ENUM(x, if(table_column_is_key(x,u)) z = cow_add(z,u))
  return z;
}

#pragma mark - Table Columns

K table_cast_from_map(K x)
{
  K z = new_table();
  ENUM(x, z = cow_table_add_column(z, u, v))
  return z;
}

bool table_has_column(K table, K column_name)
{
  I p = lookupI(table, column_name);
  if(!IS_HASH_NULL(p)) return true;
  return false;
}

K cow_table_add_column_uncollided(K table, K column_name, K column_values)
{
  K original = alpha_uncollided_name(table, column_name);
  work_push(original);
  K z = cow_table_add_column(table, original, column_values);
  work_pop_rd(true);
  return z;
}

K cow_table_add_column(K table, K column_name, K column_values)
{
  if(table_has_column(table, column_name)) return table;

  //if(!IS_CHARVEC(column_name)) ERROR(ERROR_STRING);//optional
  //if(!IS_ARRAY(column_values)) ERROR(ERROR_ARRAY);//optional

  table = cow(table);

  ///////////////////////////////////////////// 
  //kevin 2016.05.16 
  //The sort attr on the table is different from the sort attr on the values
  //list (one means "the table is sorted", the other means that "columns are
  //ordered", which is not very useful in the context of tables). So because
  //this can cause an intense comparison on SELECT statements (eg two long
  //exactly same ZIP columns) we pre-disable the sort attr on the columns thing
  //for now. Should the need arise you can re-enable it, but be smart about it.
  nestneu(table, VALUES, cow(kN(table,VALUES)));
  OFF_ATTR(kN(table,VALUES),
  ATTR_SORTED);
  /////////////////////////////////////////////

  column_values = strong(column_values);
  column_values = cow_coerce_column(column_values);

  I p = insert_replace(table, column_name, column_values, false, false); 

  rd(column_values);

  return table;
}

K cow_table_add_column_uncollided_empty(K table, K column_name)
{
  K empty = Kk();
  work_push(empty);
  table = cow_table_add_column_uncollided(table, column_name, empty); 
  work_pop_rd(true);
  return table;
}

K alpha_column_name(K name)
{
  K prefix = kcv("_");

  if(!IS_STRING(name) || COUNT(name) < 1) return strong(prefix);

  C first = kC(name)[0];

  K z = NULL;

  if(isdigit(first))
  {
    z = join(prefix, name);
  }
  else
  {
    z = copy(name);
  }

  //replace unusual chars with underscores
  //strictly speaking, we don't have to do this as 
  //user can use "t['weird -name']" and "select t['weird -name'] as normal_name"
  DO(zn, C c = zC[i]; if('_'!=c && !isalnum(c))zC[i]='_'; )

  return z;
}

K uncollided_name(K table, K column_name)
{
  K final = strong(column_name);

  //POTENTIAL_OPTIMIZATION_POINT
  //don't linear increment i
  I i = 1;
  while(table_has_column(table, final))
  {
    K append = string_cast(ki(i));
    K temp = join(column_name, append);
    rd(append);
    rd(final);
    final = temp;
    i++;
  }

  return final;
}

K alpha_uncollided_name(K table, K name)
{
  K alpha = alpha_column_name(name);
  K original = uncollided_name(table, alpha);
  rd(alpha);
  return original;
}

K cow_table_receive_column_from_table(K destination, K origin, K column_name, bool rename_collisions)
{
  //add column + column attributes (fkey, ...)
  K x = destination;
  K y = origin;

  K final = NULL;

  if(!rename_collisions)
  {
    final = strong(column_name);
  }
  else
  {
    final = uncollided_name(destination, column_name);
  }

  K0 o;
  x = cow_table_add_column(x, final, LOOKUP_(origin, column_name, o));

  K k = table_trait_get_column(y, column_name);

  if(k)
  {
    x = cow_table_trait_set_column(x, final, k);
    rd(k);
  }

  rd(final);

  return x;
}

K table_ordered_subtable_on_column_keys(K table, K keys)
{
  K z = new_table(); 

  bool sort_order_keys = true;
  ENUM(keys, if(i != lookupI(table, v)) sort_order_keys = false;
             z = cow_table_receive_column_from_table(z, table, v, true))

  if(SORTED(table) && sort_order_keys)
  {
    SET_ATTR(z, ATTR_SORTED);
  }

  return z;
}

K table_column_default_value(K table, K column_name)
{
  //TODO: lookup default / execute default function

  I p = lookupI(table, column_name);
  K0 o;
  K vals = AT2(kN(table, VALUES), ki(p),o);

  //fall back to this if default is not specified
  K def = default_column_entry_for_type(vals);

  return def; 

}

K default_column_entry_for_type(K x)
{
  return null_type(x);
}

#pragma mark - Table Rows

I table_columns(K table)
{
  return ECOUNT(table);
}

I table_rows(K table)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //Only needs O(1) not O(columns) if never ragged
  I rows = 0;
  ENUM(table, rows = MAX(rows, ECOUNT(v)))
  return rows;
}

bool table_ragged(K table)
{
  I rows = SENTINEL;

  ENUM(table, 
              if(!IS_ARRAY(v)) return true;
              I c = ECOUNT(v); 
              if(SENTINEL == rows) rows = c;
              if(rows != c) return true;
  )

  return false;
}

K table_row_as_vlist(K table, I row)
{
  K z = Kk();
  
  ENUM(table, K0 o; z = cow_add(z,LOOK_(v,row,o)))

  return z;
}

K cow_table_rename_column(K table, K old_name, K revised_name)
{
  //TODO:
  //Renaming a column will also necessitate renaming the trait-column-column_name-attribute key
  //one rename-dictionary-key function should solve all our problems here
  //use it on the table, then on the table-traits wherever necessary

  table = cow(table);
  bool result = renameKey(table, old_name, revised_name); 

  return table;
}

void cow_table_drop_column()
{
  //Removing a column should also drop trait-column-column_name
}

K sample_map_from_table(K x)
{
  K y = take(ki(COUNT(xKeys)), kn);
  work_push(y);
  K map = new_map_from_K_K(xKeys, y, false, false);
  work_pop_rd(true);
  if(!map)map = new_map();
  return map;
}

#pragma mark -

K practice_table_1()
{
  //pkey customer_id, name, ... 
  K z = new_table_x();

//  z = cow(z);
//  nestneu(z, VALUES, cow(zValues));
//  
//  S fun[]={"customer_id", "name", "visits"};
//  
//  S names[] = {"Noah", "Liam", "Jacob", "Mason", "William", "Ethan", "Michael", "Alexander", "Jayden", "Daniel",};
//
//  I namesC = sizeof(names)/sizeof(names[0]);
//
//  S s = NULL;
//
//  DO(sizeof(fun)/sizeof(fun[0]), 
//    s=fun[i];
//    z = cow_table_add_column_empty(z,kcv(s)); 
//  )
//
//  I n = 6;
//
//  DO(n, nestneu(zValues, 0, cow_add(LOOK_(zValues,0),ki(1+i)));
//  )
//
//  DO(n, K m = charvec_from_cstring(names[i%namesC]); 
//        nestneu(zValues,1,cow_add(LOOK_(zValues,1),m));
//        rd(m)
//    )
//
//  DO(n, I rand =(rI()%100); rand = ABS(rand); nestneu(zValues,2,cow_add(LOOK_zValues,2),ki(33))))

  return z;
}

K practice_table_2()
{
  //fkey customer_id, (fkey) interaction_type, time_date

  K z = new_table_x();

//  z = cow(z);
//  nestneu(z, VALUES, cow(zValues));
//  
//  S fun[]={"customer_id", "interaction_type", "visits"};
//  
//  S s = NULL;
//
//  DO(sizeof(fun)/sizeof(fun[0]), 
//    s=fun[i];
//    z = cow_table_add_column_empty(z,kcv(s)); 
//  )
//
//  I n = 3;
//
//
//  DO(n, nestneu(zValues,0,cow_add(LOOK_zValues,0),ki(1+i))))
//  DO(n, I r =(I)abs(rI()%4); nestneu(zValues,1,cow_add(LOOK_zValues,1),ki(2))))
//  DO(n, I r =(I)abs(rI()%100); nestneu(zValues,2,cow_add(LOOK_zValues,2),ki(r))))

  return z;
}

int table_fun()
{

  I n = POW2(2);
  K x = til(ki(n));
  K y = til(ki(n));
  K z = take(ki(n),ki(55));

  DO(yn, yI[i] = 2 + i);

  //TIME(z = new_map_from_K_K(x,ki(IN),true,false))

  K a = new_map_from_K_K(x,x,false,false);
  K b = new_map_from_K_K(y,z,false,false);

  //K c = ply(a,b,NULL,NULL,2,VERB_DISPATCH[VERB_MERGE]);
  //show(c);
  //rd(c);

  rd(a);
  rd(b);

  rd(x);
  rd(y);
  rd(z);
return 0;
}

int table_go()
{

  //play("n:3; t:{{a:til n, b: 1 + til n}}");
  //play("n:3; t:{{a:til n, b: 1 + til n}}; select from t");
  //play("t:{{a,b}}");


  return 0; 
  //return table_fun();
  K x; 
  K y; 

  I n = 1000000;

  //x=til(ki(100)); y=take(ki(100),ki(3)); TIME(rd(show(greater(x,y)))) rd(x);rd(y);

  x = practice_table_1();
  y = practice_table_2();

  S cname = "customer_id";
  K col = kcv(cname);

  x = cow_table_bitwise_or_column_attributes(x, col, COLUMN_ATTR_KEY);
  y = cow_table_bitwise_or_column_attributes(y, col, COLUMN_ATTR_FKEY);
  dd(table_column_is_key(x,col))
  dd(table_column_has_attributes(x,col,COLUMN_ATTR_KEY))

  show(x);
  show(y);

  rd(x);
  rd(y);
  return 0;
}

