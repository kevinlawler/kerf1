#include "kerf.h"

#pragma mark - Alter/Amend/Change

#pragma mark - Index/At/Of

K values(K x)
{
  //NOTE: see note at ALTER_GLOBAL
  //You want to not copy the values, but there is a very
  //long winded reason why you can't do that for CREFS.
  //The demote is because we don't always demote previously
  //when inserting into maps for performance reasons.
  if(IS_CREFED(x))
  { 
    return cow_demote(copy(xValues));
  }
  nestneu(x,VALUES,cow_demote(xValues)); 
  return strong(xValues);
}

//Note: For LISTs, null_type / empty_atom may be better to be
//  the 0-count empty LIST instead of NIL atom since, for instance
//  if I use out-of-bounds indexing to do shift for a moving sum (multiplying by 0)
//  then empty LISTs are going to preserve shape (for a moving sum over a LIST of arb. shaped LISTs)
//  (without specifying a fill-item) whereas NIL is going to throw a type error I think.
//  Note that the empty LIST is atom sized (and in places we've used `kk`) so it would
//  make a natural fit; however, I don't remember if we ran into trouble with kk because it
//  is a TENANT list type instead of a TENTANT atom. you'd need to verify (postive or negative) and test.
//We could do verbs << and >> left shift and right shift. which are shift in one direction or the other.
//maybe only right shift is useful? 
K0 empty_atom(K x)
{
  SW(abs(xt))
  {
     CR(CHAR,  kc0(' '))
     CR(INT,   ki0(IN))
     CR(FLOAT, kf0(FN))
     CR(STAMP, ks0(SN))
     CR(LIST,  kn0)
     CR(BTREE,  empty_atom(xKeys))
     CR(HASH,  empty_atom(kN(xIndex,KEYS)))
     CD:       R kn0;
  }
}

K0 null_type_K0(K x)
{
  return empty_atom(x); //i put empty_atom in to stub. null_type can have its own logic
}

K null_type(K x)
{
  K0 j = null_type_K0(x);
  return strong(&j);
}

I is_null_atom(K x)
{
  SW(xt)
  {
    CR(CHAR,   ( ' '==xc) ?1:0)
    CR(INT,    (  IN==xi) ?1:0)
    CR(FLOAT,  (isnan(xf))?1:0)
    CR(STAMP,  (  SN==xi) ?1:0)
    CR(NIL,        1      ?1:0)
    CR(FUNC,       0      ?1:0)
  }

  return 0;
}


K1 denest_K1_from_start_and_key(K start_map, K key_charvec, bool creates, bool treelike)
{
  //NOTE: we could merge this with compiled_var_path_increment
  //to create path_tool(). The kind of toggles we want to add
  //are: creates_maps, splits_maps/tables, allows_table_descent,
  //     allows_tenant/disk_descent, throws_errors

  K x = key_charvec;

  if(!IS_CHARVEC(x)) goto nopop_failure;

  K y = NULL;

  if(!treelike)
  {
    y = strong(x);
  }
  else if(xn > 0 && xC[0] == '.') //is absolute
  {
    y = strong(x);
  }
  else
  {
    //The question is, if .["v";();:;111] is inside a lambda, does v mean
    //global or does v mean local-variable?  We take it to mean global and
    //we append the KERFDIR to the front so you're operating in the same
    //directory (but outside of the function).
    //
    //You could change this by instead returning something like the KVM->stack
    //as the list and the position of the arglocal on the function application
    //frame. You'd have to get rid of this dot append below as well. We made
    //some kind of other decision that doesn't lend itself well to map-locals
    //(local trees) so probably this direction is inadvisable. (Maybe in the
    //way closures are handled?  I think we decided {[x] i.j.k:1} inside of a
    //function was more trouble than it was really worth and so decided not to
    //do that again. It may have some hidden annoyance like moving maps around
    //or maintaining compiled reference count or closures or something. Also,
    //some of the de-name lookup code floating around may depend on properly
    //separated and compile-referenced maps.)

    K4 o;
    y = implode(kc('.'), klist2(KVM->KERFDIR, x, o));
  }
 
  work_push(y);
  K e = explode(kc('.'),y);
  work_push(e);

  K z = start_map;
  I p = HASH_NULL;

  ENUM(e, 
    
    if(0==i && 0==cv)//kludge so we don't have to 1_ one-drop explode('.', ".a.b.c")
    {
      continue;
    }

    if(!IS_MAP(z) && !IS_TABLE(z)) //IS_TABLE is tentatively left in
    {
      goto failure;
    }

    p = lookupI(z,v);

    I more = i < _i - 1;

    if(IS_HASH_NULL(p))
    {
      if(creates)
      {
        K map = new_map();
        work_push(map);
        p = insert_replace(z,v,map,true,false);
        work_pop_rd(true);
      }
      else
      {
        goto failure;
      }
    }
    else
    {
      //cow all the keys and values and indexes and traits and whatever...
      //i think we need this though not sure
      //The_Kerf_Tree compiled parts will be split already, but the places you can 
      //reach with dynamic subindexes may not be
      //at any rate I don't think this can hurt you; worst case is micro slowdown
      //NEST(z, nestneu(z, i, cow(v)))
      //NEST(z, nestset_ri_rd(z, i, cow0(v), false, false))
      NEST(z, if(!IS_TENANT(v)) nestneu(z, i, cow(v)))
    }
    
    if(more)
    {
      K0 o;
      z = LOOK_(zValues,p,o);
      p = HASH_NULL;
    }
  )

  if(IS_HASH_NULL(p))goto failure;//force null z

success:
  work_pop_n_rd(2, true);
  return (K1){z, ki0(p)};
failure:
  work_pop_n_rd(2, true);
nopop_failure:
  return (K1){NULL, ki0(HASH_NULL)};
}

K0 kat(K x, K y, bool creates_maps, bool throws_errors)
{
  assert(IS_SATOM(y));

  SW(xt)
  {
   CD://VECTORs
   CSF(HASH,)
   CSF(BTREE,)
    CS(LIST,
        I n = ECOUNT(x);
        I i = -1;

        SW(yt)
        {
          CS(NIL, return kl0(x))
          CS(CHAR,  i = yc)
          CS(INT,   i = yi)
          CS(FLOAT, i = yf)
        }

        if(i < 0 || n <= i)
        {
          if(throws_errors) ERROR(ERROR_INDEX);
          else return null_type_K0(x); //or fall down to error
        }
        else
        {
          return klook(x,i);
        }
    )
    CS(STAMP, 
        SW(yt)
        {
          CS(-CHAR, //POTENTIAL_OPTIMIZATION_POINT: we can put all the relevant functions in a hashtable lookup
            
            //"a: 2015.01.01T01:02:03; a['date']"
            bool local = false;
            TM_NANOS t = TM_NANOS_from_stamp(x, local);

            if(EQUAL == KC(y,kcv("date")))        return ks0(DATE_ONLY(xi));
            if(EQUAL == KC(y,kcv("time")))        return ks0(TIME_ONLY(xi));
            if(EQUAL == KC(y,kcv("year")))        return ki0(t.tm_year + 1970);
            if(EQUAL == KC(y,kcv("month")))       return ki0(t.tm_mon + 1);
            if(EQUAL == KC(y,kcv("day")))         return ki0(t.tm_mday + 1);
            if(EQUAL == KC(y,kcv("hour")))        return ki0(t.tm_hour);
            if(EQUAL == KC(y,kcv("minute")))      return ki0(t.tm_min);
            if(EQUAL == KC(y,kcv("second")))      return ki0(t.tm_sec);
            //Er, 'millisecond' and 'microsecond' are not right, right?
            //Well... i guess it doesn't make sense to extract the modular unit
            //So it does sort of make sense as long as you consider
            //each sub-second division as forming a unit-second before rolling over
            //So each is a subdivision of a second and NOT a subdivision of each other.
            if(EQUAL == KC(y,kcv("millisecond"))) return ki0(t.tm_nanos/MILLION);
            if(EQUAL == KC(y,kcv("microsecond"))) return ki0(t.tm_nanos/1000);
            if(EQUAL == KC(y,kcv("nanosecond")))  return ki0(t.tm_nanos);
            if(EQUAL == KC(y,kcv("week")))        return ki0(week_of_year_from_TM_NANOS(t));
          )
        }
    )
   CSF(TABLE, //fallthrough, column lookup
        SW(yt)
        {
          CS(INT,)//Rows? Or fallthrough? (we can special case higher for int vectors)
          //CS(NIL,return kl0(x))//or fallthrough
        }
    )
    CS(MAP, 
        SW(yt)
        {
          //keeping NIL for vals is not a requirement, as
          //if ! gives keys, then "map . ,!map" or "m[!m]" gives vals
          CR(NIL, kl0(xValues))
        }

        I p = lookupI(x, y);

        if(!IS_HASH_NULL(p))
        {
          return klook(x, p);
        }
        else if(creates_maps) 
        {
          K z = new_map(); 
          work_push(z);
          insert_replace(x, y, z, true, false);
          //normally would rd(z) but in this case we need the r+1
          work_pop_rd(true);
          return kl0(z);
        }
        else
        {
          if(throws_errors) ERROR(ERROR_INDEX);
          else return null_type_K0(x); //or fall down to error
        }
    )
  }
  
  //From here on, it's whatever we want to happen basically:
  //we can make atoms return themselves, or error, or whatever

  if(IS_NIL(y))return kl0(x);//shrug, catch if we want

  if(throws_errors) ERROR(ERROR_INDEX);
  else return kn0;
}

K simple_at(K x, K y)
{
  return simple_at_creates(x, y, false);
}

K simple_at_creates(K x, K y, bool creates_maps)
{
  assert(IS_SATOM(y));
  bool throws_errors = false;

  //Catch & perform certain operations which do not lend themselves to K0
  //format. For instance, function execution which tends to create new K.
  SW(xt)
  {
    CS(FUNC, return MONAD_EX(x,y))
    CS(ATLAS, 
        SW(yt)
        {
          CS(INT,     return atlas_map_at_index(x, yi))
          CS(CHARVEC, return atlas_at_string(x, y))
        }
    )
    CS(PARTABLE,
        SW(yt)
        {
          CS(CHARVEC,
            K0 o;
            if(EQUAL == KC(y,kcv("domain_table")))  return strong(LOOKUP_(x, kcv("parcel_names_evaled"), o));
            if(EQUAL == KC(y,kcv("domain")))        return ex("first xkeys $1['domain_table']", x);
            if(EQUAL == KC(y,kcv("domain_values"))) return ex("first xvals $1['domain_table']", x);
          )

        }

    )
    CS(TABLE, //fallthrough, column lookup
        SW(yt)
        {
          CS(INT,)//rows (we can special case higher (in at() or of()) for int vectors)
        }
    )
    CS(MAP, 
        SW(yt)
        {
          //keeping NIL for vals is not a requirement, as
          //if ! gives keys, then "map . ,!map" or "m[!m]" gives vals
          CS(NIL, return values(x);)
        }
    )
    CS(-STAMP,
        if(-CHAR==yt)
        {

          if(EQUAL == KC(y,kcv("date")))        return ex("bars(1d,$1)", x);
          if(EQUAL == KC(y,kcv("time")))        return ex("stamp_diff($1, $1['date'])", x);
          if(EQUAL == KC(y,kcv("nanosecond")))  return bar_subsecond_modular_with_divisor(x, 1);
          if(EQUAL == KC(y,kcv("microsecond"))) return bar_subsecond_modular_with_divisor(x, 1000);
          if(EQUAL == KC(y,kcv("millisecond"))) return bar_subsecond_modular_with_divisor(x, MILLION);
          if(EQUAL == KC(y,kcv("year")))        return bar_modular_time_index_by_unit_code(x, 'y');
          if(EQUAL == KC(y,kcv("month")))       return bar_modular_time_index_by_unit_code(x, 'm');
          if(EQUAL == KC(y,kcv("day")))         return bar_modular_time_index_by_unit_code(x, 'd');
          if(EQUAL == KC(y,kcv("hour")))        return bar_modular_time_index_by_unit_code(x, 'h');
          if(EQUAL == KC(y,kcv("minute")))      return bar_modular_time_index_by_unit_code(x, 'i');
          if(EQUAL == KC(y,kcv("second")))      return bar_modular_time_index_by_unit_code(x, 's');

          //POTENTIAL_OPTIMIZATION_POINT: 'week'
          //I assume this will work but have not looked at it closely
          //Use 7d bars *with the start date set appropriately* (for the year?)
          //Reuse the week number while the stamps are in the same bucket
          //Otherwise recalc from TM_NANOS
          
          //If we didn't optimize it out above, catch whatever falls to here and use 
          //the atom method on the stampvec individually (assuming it exists)
          //POTENTIAL_OPTIMIZATION_POINT: generate directly with vector of pre-reserved size
          K z = new_k(LIST, 0);
          work_push(z);
          K0 o; 
          ENUM(x, K a = _AT(v,y,creates_maps,throws_errors, o); work_pop(); z = cow_join(z,a); work_push(z);)
          work_pop();
          return z;
        }
    )
    CD: break;//fallthrough
  }

  //K0 k = kat(x, y, creates_maps, throws_errors); return strong(&k);
  K0 o1;
  return strong(_AT(x,y,creates_maps,throws_errors, o1));
}

K table_integral_at(K x, K y)
{
  //You can make the argument that an atom integer index
  //into a table should return a map, and that a list
  //of maps should form a table, via demote.
  //We choose to do something else: integer atoms and vectors
  //produce tables: single or multiple rows. Table column values
  //are always arrays regardless of whether they contain one row.
  //Benefits:
  //  i) handles table attributes transparently. our maps don't
  //     necessarily have attributes.
  // ii) preserves the commonly used JSON array of JSON maps
  //iii) less jarring. map popping out of a table is cognitive
  //     overhead most people don't want or expect, however
  //     theoretically sound.


  //We don't copy() the table because this is bad with mapped tables
  K table = same_table_but_empty_columns(x);

  work_push(table);

  ENUM(x, K a = at(v,y);
          K column = cow_coerce_column(a);
          update_ri_rd(table, u, column, false, true)) 

  work_pop();

  //There may be some fast way to detect if you sorted the table inadvertantly or not,
  //but I doubt it.
  
  return table;
}

K atlas_integral_at(K x, K y)
{
  return atlas_destroying_integral_at(x, y);
}

K at(K x, K y)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //in TABLE clause here (special case), and INTVEC/etc clause below
  //if left hand is sorted, then indexing with
  //sorted (actually merely non-decreasing) right hand intvec yields 
  //sorted output - mark ATTR_SORTED (table & vectors)

  if(IS_TABLE(x) && INT == ayt)
  {
    return table_integral_at(x,y);
  }

  if(IS_ATLAS(x) && INT == ayt)
  {
    return atlas_integral_at(x,y);
  }

  //POTENTIAL_OPTIMIZATION_POINT ?
  //We're returning real objects here (Ki or strong(ki) not ki)  
  //But we could potentially tune the At/Of stuff to use ki style
  //everywhere, and only do strong at the end.
  if(IS_SATOM(y))
  {
    return simple_at(x,y);
  }

  SW(yt)
  {
    CS(INTVEC, 
      SW(xt)
      {
        //POTENTIAL_OPTIMIZATION_POINT: can mantain sorted here (this is interesting for say the second column in a parted pair)
        CS(INTVEC,   K z = new_k(INTVEC,   cy); DO(cz, I k = yI[i];  zI[i] = 0<=k && k<xn? xI[k] : IN) return z)
        CS(FLOATVEC, K z = new_k(FLOATVEC, cy); DO(cz, I k = yI[i];  zF[i] = 0<=k && k<xn? xF[k] : FN) return z)
        CS(STAMPVEC, K z = new_k(STAMPVEC, cy); DO(cz, I k = yI[i];  zI[i] = 0<=k && k<xn? xI[k] : SN) return z)
        CS(LIST,     K z = new_k(LIST, cy); DO(cz, I k = yI[i]; nestset_ri_rd(z,i,0<=k && k<xn?kN(x,k):kn,true,false)) z = cow_demote(z); return z;) 

        CS(HASH,     //POTENTIAL_OPTIMIZATION_POINT
                     //this reuses ALL the INDEX-items from x but you could see how we could reduce it to just the necessary values
                     //(eg, if I'm selecting where symbol='A', we just keep 'A', not all symbols in the index - this is basic integer math)
                     //Strategy:
                     //*Count the uniques (allocate bins = new_k(INTVEC,|old_uniques|) )
                     //*Decide whether to vacuum (perhaps if |new_uniques| < 0.2*|old_uniques| ?)
                     //*Decide which order to preserve (hashset-subset-natural or key-subset-natural):
                     //--numbering by count++ first appearance in the bins gives key-subset-natural
                     //  and you can revise the intern-indices during the same pass.
                     //  then you have to reorder the intern-hashset-keys according to this order.
                     //--boolean yes'ing by first appearance then incrementing to count++ in order
                     //  from left to right in the array when true gives hashset-subset-natural
                     //  you need a second pass on the intern-indices, but you avoid
                     //  having to reoder the intern-hashset-keys.
                     //*Reindex the hashset (if you dropped even one item from it)
                     //--if we implemented deleting keys from hashsets that could be faster?
                     //*Dropping any hashset-keys item but the last-ones-contiguously necessitates relabeling intern-keys

                     K a = at(xKeys,y); 
                     K z = new_intern(); 
                     nestset_ri_rd(z,KEYS,a,false,true); 

                     //Doing this fixed a bug where we were taking a HASH's index that was a striped disk object in a table
                     //and writing to it, corrupting the original table's symbol enumeration index. that was bad
                     //and this fixed it. although it makes me worry if there are any other ways to do something like that.
                     //what we want to do is cause COPY/CAN_WRITE/COW and so on to copy when we want it to and to
                     //not copy when we don't (eg pushing it on the stack and so on)
                     //***
                     //If we find this issue causing a lot of problems, we may just want to turn on
                     //copy (to in-memory object) whenever a striped item is reference incremented. We can handle
                     //certain cases like pushing/removing to/from stack by checking for IS_STRIPED and avoiding
                     //regular reference increment/decrements. I think overall this works and there is less
                     //issue (than with non-striped-disk-tables) because they are only proportional to column-size
                     //Another idea is to always make hash*sets* do a full copy
                     //**
                     //nestset_ri_rd(z,INDEX,xIndex,true,true);
                     K c = copy_override(xIndex);

                     //this fixes a bug where y contains ints outside the range of hash's xIndex
                     bool null_ceremony = false;// could also leave on by default and skip check below...
                     DO(a->n, if(kI(a)[i]==IN){null_ceremony = true; break;}) //check for null_ceremony

                     if(null_ceremony)
                     {
                      K empty = null_type(kN(c,KEYS));
                      I p = insert_replace_ri_rd(c, empty, NULL, false, true, true, true);
                      rd(empty);

                      DO(a->n, if(kI(a)[i]==IN)kI(a)[i]=p)
                     }


                     nestset_ri_rd(z,INDEX,c,false,true);

                     OFF_ATTR(z,ATTR_SORTED); //this fixed a bug
                     //POTENTIAL_OPTIMIZATION_POINT: can mantain sorted here (for hash)
                     //a few O(1) time checks. can always O(n) check
                     //if (y is sorted and in bounds) and (x is sorted || the index-hash is sorted ) sort is maintained on z
                     return z
        ) 
        CS(BTREE, if(!SORT_INDEX_YIELDS_SORT_NOT_LIST) return at(xKeys, y); K a = at(xKeys,y); work_push(a); K z = new_btree_from_K(a); work_pop_rd(true); return z)
      }
    )
  }

  K z = take(ki(0), x);//better than K z = new_k(LIST, 0);

  work_push(z);

  if(TABLE==zt)
  {
    //Note: ATTR_SORTED [should only be] preserved on TABLE
    //if the column [index] subselection is of the form 0+,1+,2+,3+,k+ with k <= #columns-1
    //and + indicating one or more times (which is admittedly a strange use case)
    //See table_ordered_subtable_on_column_keys for the relevant check

    //POTENTIAL_FEATURE_POINT
    //keyword: gnsdkjdnfk92222
    //capture table attributes here for the relevant columns?
    //It may simply be cow_table_receive_column_from_table instead of at(), perhaps w/ checks for existence


    work_pop_rd(true);
    z = new_table();
    work_push(z);
    ENUM(y, K w=at(x,v); if(IS_NIL(w)){rd(w); ERROR(ERROR_COLUMN);} work_pop(); z = update(z,v,w); work_push(z); rd(w);)
  }
  else
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //this is slow
    ENUM(y, K w=at(x,v); work_pop(); z = cow_add(z, w); work_push(z); rd(w);)
  }

  return work_pop();
}

K of(K x, K y)
{
  return of_i_w(x,y,0,0,true);
}

K of_i_w(K x, K y, I k, K w, bool denest_charvec)
{
  if(IS_CHARVEC(x) && denest_charvec)
  {
    K0 o;
    K1 k1;
    x = denest_of(x,o,k1);
  }

  if(FUNC == xt)
  {
    ERROR(ERROR_TYPE);

    //this is broken on x={[x,y]x+y} y={[x]0} for some reason...
    K iter = NULL;
    bool reverso = true;

    if(reverso) iter = reverse(y);
    else iter = strong(y);

    frame_open(KVM);

    x = strong(x);

    if(IS_ATOM(y))push_ri(KVM, y, true);
    else ENUM(iter, push_ri(KVM, v, true))
    rd(iter);

    _local_ex(KVM, x);

    K0 o;
    K k = peeko(KVM,0,o);
    scrub(KVM);

    frame_close(KVM);

    K result = strong(k);
    rd(k);
    k = result;

    return k;
  }

  I c = COUNT(y);

  if(0==c) //.[a;()] or .[a;0#0] or ...
  {
    return strong(x);
  }  

  if(IS_SATOM(y)) //no ".[a;map]", also turn ".[a;string]" into ".[a;,string]"
  {
    return at(x,y);
  }

  K0 o;
  if(!w) w = LOOK_(y,k,o);

  if(k >= c - 1)
  {
    return at(x, w);
  }
  else if(IS_SATOM(w))
  {
    //vvv NIL cases not necessary if at() handles nil case and of() 'matches'
    //if(IS_NIL(w)) { if(IS_MAP(x)) { x = xValues; } return of(x,y,i+1); }
    K t = at(x,w);
    work_push(t);
    K z = of_i_w(t,y,k+1,0,denest_charvec);
    work_pop_rd(true);
    return z;
  }
  else
  {
    K z = new_k(LIST, 0);
    work_push(z);
    ENUM(w, K t=of_i_w(x,y,k,v,denest_charvec); work_pop(); work_push(t); z = cow_add(z, t); work_pop_rd(true); work_push(z);)
    return work_pop();
  }
}

#pragma mark - Alter/Change/Update

K alter(K w, K x, K y, K z)
{
  if(!y || !z) return of(w, x);
  return change(w, x, y, z);
}

K change(K target, K indices, K modifier, K supplies)
{
  //Note: By doing cow_change the way we do, that is, in place when modifying
  //variables by name, if we pass something in the middle that's going to cause
  //an error, we won't get to roll changes back (unless we build in error
  //checking first, and that's going to be hard or impossible to do correctly
  //because you can be changing types and such intermediately in the
  //middle...so it's basically equivalent to not doing in place at all.) So for
  //instance:
  // a: (1; 2.0; "fun";"time")
  //.[`a;,0 1 2 3;+;1] fails midway at the chars
  //but the first two positions are now 2 and 3.0

  //BIG CONCERNS
  //1. Must work for ARGLOCAL_ALTER, GLOBAL_ALTER, the ALTER verb, and for tool purposes
  //2. Any ERROR thrown must not "damage" tree or create a leak
  //    In other words, we need to be in a STABLE STATE any time it's possible
  //    for an error to be thrown. (Note that eliminating errors satisfies this.)

  if(IS_CHARVEC(target)) //If it's a variable name, change it directly on the tree
  {
    work_push(strong(target));

    K1 k = denest_K1_from_start_and_key(The_Kerf_Tree, target, true, true); 
    
    K parent = k.u;
    if(!parent)
    {
er(absent/null parent in alter)
      ERROR(ERROR_REFERENCE); 
    }
    I position = k.v.i;
    K0 o;
    K key = LOOK_(kN(parent,KEYS),position,o); 
    
    parent = cow_change(parent, key, indices, modifier, supplies, true);
    return work_pop();
  }
  else //Otherwise merely change a copy of the object
  {
    K parent = enlist(target);
    work_push(parent);
    parent->r = 1;//hack for cow()
    cow_change(parent, ki(0), indices, modifier, supplies, false);
    parent = work_pop();
    K result = strong(kN(parent,0));
    rd(parent);
    return result;
  }
}

K cow_change(K parent, K key, K indices, K modifier, K supplies, bool demotes)
{
  return cow_change_i(parent, key, indices, modifier, supplies, 0, demotes);
}

K cow_change_i(K parent, K key, K indices, K modifier, K supplies, I deep, bool demotes)
{
  //We assume that the nest and the path to the nest are
  //all properly split, that is CAN_WRITE(node)

  assert(CAN_WRITE(parent));//don't pass us items from a non-split tree

  bool creates_maps = true;

  if(parent->t == TABLE)
  {
    creates_maps = false;
  }
  
  K0 o;
  K target = _AT(parent, key, creates_maps, 1, o);

  I tiers = ECOUNT(indices);

  if((1||0==deep) && 0==tiers) //.[target;();?;supplies]
  {
    if(IS_CREFED(target))
    {
      //er(change_i compiled referenced error)
      ERROR(ERROR_REFERENCE);//compiled ref error 
    }

    //For DISK we might let this overwrite tenants (to disk), but we should 
    //probably not let it get mapped masters (to disk) maybe?
    //Does it make things weird?
    //Does it make it tricky to unmap something?
    //Maybe we don't need this error at all?

    if(ALPHA_DISK_REFCOUNT)
    {
      //noop
    }
    else
    {
      if(IS_DISK(target) && !IS_TENANT(target)) ERROR(ERROR_DISK);
    }

    ///////catch nil
    //if(modifier && NIL == modifier->t) modifier = NULL;
    /////////////////

    K replacement = NULL;
    if(modifier && supplies) replacement = *work_push(DYAD_EX(modifier, target, supplies));
    else if(modifier && !supplies) replacement = *work_push(MONAD_EX(modifier, target));
    else if(supplies) replacement = *work_push(strong(supplies));

    parent = update_ri_rd(parent, key, replacement, true, true);

    if(demotes) parent = cow_demote(parent);

    work_pop_rd(true);
    ///////////////////////////////

    return parent;
  }

  //if(x is too wimpy for descend) -> error
  if(!(IS_ARRAY(target) || IS_MAP(target) || IS_TABLE(target)))
  {
    if(IS_DEFINED(DEBUG))
    {
      er(cannot descend);
      show(target);
      show(key);
      show(indices);
    }

    ERROR(ERROR_RANK);
  }

  SW(target->t)
  {
    CS(TABLE, 
    )
  }

  //At this point we are going to descend into the parent, maybe without
  //altering it, and so if the parent is a MAP (or similar; TABLE, BTREE) we need to guarantee
  //that the VALUES (or KEYS) list is properly writable (if we pass on the way a reference count 2
  //without splitting it, that means we're modifying two parents, incorrectly). So we need
  //to split the values, probably using cow_split_map. As a side effect of the next call,
  //we'll actually get that to happen:
  parent = update_ri_rd(parent, key, target = cow(target), false, false); //should happen after 0 tiers case
  //but if we didn't, then we'd need to do it manually, for MAPS, TABLES, HASHES, BTREES, ...

  K0 o0;
  K tier = LOOK_(indices, deep, o0);

  if(deep < tiers - 1) //still descending
  {
    OFF_ATTR(target, ATTR_SORTED);//we could be more intelligent, but for now, safe

    //2014.11.10 no time for more generality - at "!IS_SATOM" errors below
    //have .[;(1 2;3 4);;] but not the very abstract .[;((1 2;5 6);(3 4;7 8));;]
    //I guess also affects nil thing here which would need to go inside
    //a list as well

    K other = supplies ? supplies : kn;

    //It's possible to factor the nil case together with the non-nil,
    //but probably a really bad idea.

    if(IS_NIL(tier))
    {
      bool is_keyed = IS_MAP(target) || IS_TABLE(target);
      K4 o;
      if(is_keyed) other = klist1(other, o); //putting something here to make it work

      SATOM2(target, other, 
        K0 o; 
        K fake_index = is_keyed ? LOOK_(kN(target,KEYS),i,o) : ki(i);
        K supply = supplies ? v : NULL;
        parent = cow_change_i(target, fake_index, indices, modifier, supply, deep+1, 0)
      )
    }
    else
    {
      SATOM2(tier, other, 
        if(!IS_SATOM(u)) ERROR(ERROR_INDEX); 
        K supply = supplies ? v : NULL;//POTENTIAL_OPTIMIZATION_POINT: we can expand this to 2 cases
        parent = cow_change_i(target, u, indices, modifier, supply, deep+1, 0)
      )
    }
  }
  else //last tier, begin modifying
  {
    parent = cow_change_at(parent, key, tier, modifier, supplies, true);
  }

  return parent;
}

K cow_change_at(K parent, K key, K indices, K modifier, K supplies, bool demotes)
{
  assert(CAN_WRITE(parent));//don't pass us items from a non-split tree
  K0 o;
  K target = _AT(parent, key, 1, 1, o);

  //Overhead: O(n) - Fix the target for writing
  parent = update_ri_rd(parent, key, target = cow(target), false, false); //insert_replace will handle cow(map_sublists)

  //Do modifications without overhead
  parent = cow_change_at_no_overhead(parent, key, indices, modifier, supplies);

  //Overhead: O(n) - Demote the target
  if (demotes)
  {
    K0 o;
    target = _AT(parent, key, 1, 1, o);//sic, refresh
    parent = update_ri_rd(parent, key, target = cow_demote(target), false, false);//demote lists, map-sublists; all at once
  }

  return parent;
}

K cow_change_at_no_overhead(K parent, K key, K indices, K modifier, K supplies)
{
  if(!IS_SATOM(indices))
  {
    K other = supplies ? supplies : kn;
    LIST2(indices, other, 
            K supply = supplies ? v : NULL;
            parent = cow_change_at_no_overhead(parent, key, u, modifier, supply)
    )

  }
  else if(IS_NIL(indices))
  {
    K0 o1;
    K target = _AT(parent, key, 1, 1, o1);
    K other = supplies ? supplies : kn;

    bool is_keyed = IS_MAP(target) || IS_TABLE(target);
    K4 o;
    if(is_keyed) other = klist1(other, o); //putting something here to make it work

    LIST2(target, other, 
            K0 o;
            K fake_index = is_keyed ? LOOK_(kN(target,KEYS),i,o) : ki(i);
            K supply = supplies ? v : NULL;
            parent = cow_change_at_no_overhead(parent, key, fake_index, modifier, supply)
    )
  }
  else
  {
    //POTENTIAL_OPTIMIZATION_POINT - we can pre-calc target and pass it around (make sure
    //to get the new one as it changes), possibly make this function return target
    //see also loops above. may have to modify calling function

    assert(CAN_WRITE(parent));//don't pass us items from a non-split tree
    K0 o;
    K target = _AT(parent, key, 1, 1, o);

    K index = indices;//name change

    bool creates = false;//2016.01.05 we needed `creates=false` here so that striped_table['new_column']: 1 2 3 worked
    K existing = *work_push(simple_at_creates(target, index, creates));

    //POTENTIAL_OPTIMIZATION_POINT: skip if modifier is ":"

    //POTENTIAL_OPTIMIZATION_POINT:
    //would be nice if "m:{a:[1]}; m['b'] ,: 2" worked giving {a:[1],b:[2]}
    //we can probably do it here
    //(if existing is NULL for a map, just assign supply instead of using dyadic verb)
    K replacement = NULL;

    if(modifier && supplies) replacement = *work_push(DYAD_EX(modifier, existing, supplies));
    else if(modifier && !supplies) replacement = *work_push(MONAD_EX(modifier, existing));
    else replacement = *work_push(strong(supplies));

    assert(CAN_WRITE(target));//cow(target) handled in overhead

    //POTENTIAL_OPTIMIZATION_POINT:
    //maybe it's possible, with a lot of extra logic, to not put this check so deep
    //if you move it up, you can handle cases like `table[null]: longer_columns`
    //where you need to make sure the lists are the same (modifiers like `join` may blow this idea up though?)
    //another possibility is to always force people to use sql UPDATE instead.
    {
      bool checks_ragged = true || true; //you really want this

      bool checks_striped_modify = true; //2016.01.05 we should be able to disable this, but you need to find bug mentioned above
      bool checks_striped_add    = false;
      bool checks_striped = checks_striped_modify || checks_striped_add;;

      SW(target->t)
      {
        CS(TABLE, 

                  if(checks_striped && IS_STRIPED(target) && IS_DISK(target))
                  {

                    bool existing_key = false;
                    
                    existing_key = table_has_column(target, index);  //just a lookupI that would work with MAP...

                    if(checks_striped_modify && existing_key)
                    {
                      //we could turn this on of course, via various overwriting methods
                      fprintf(stderr, "Error: Attempted to overwrite column to striped table. Try SQL UPDATE.\n");
                      ERROR(ERROR_COLUMN);
                      return parent;
                    }
                    else if(checks_striped_add)
                    {
                      fprintf(stderr, "Error: Attempted to add column to striped table. Try SQL UPDATE.\n");
                      ERROR(ERROR_COLUMN);
                      return parent;
                    }
                  }


                  I starting_cols = table_columns(target);
                  if(checks_ragged && starting_cols > 0)
                  {
                    K col = kN(kN(target, VALUES),0);
                    assert(IS_ARRAY(col));

                    I existing = lenI(col); //checking the last existing column should be enough
                    I proposed = lenI(replacement); 

                    bool mismatched = existing != proposed;

                    //we can also allow 1 col tables to change length IF changing same key
                    bool single_column_length_change = (1 == starting_cols) && table_has_column(target, indices);

                    bool ragged = mismatched && !single_column_length_change;

                    if(ragged)
                    {
                      ERROR(ERROR_RAGGED);
                    }
                  }
        )
      }
    }
  
    K revised = update(target, index, replacement);
    //can update parent only if target pointer actually changed
    if(revised != target) parent = update_ri_rd(parent, key, revised, false, false);

    work_pop_n_rd(2, true);
  }

  return parent;
}

K update(K target, K index, K replacement)
{
  return update_ri_rd(target, index, replacement, true, true);
}

K update_ri_rd(K target, K index, K replacement, bool increment, bool decrement)
{
  //POTENTIAL_OPTIMIZATION_POINT: maybe a micro-op: we can drop cow everywhere
  //                              (including name) and handle in calling functions
  //                              this is actually a p.i.t.a and will need 
  //                              some tools to handle recursion in BTREE/HASH
  //
  //POTENTIAL_OPTIMIZATION_POINT: we might want a toggle for sort checking here
  //                              if so, just set the ATTR_SORT flag off on x
  //                              instead of doing the branch to check_attr_...

  //Warning: we don't want to throw any errors *after* doing a cow_ call
  //or mucking with references/reference counts or generating an object,
  //but but *before* returning the pointer that lets us reconcile the
  //new situation with the parent objects/tree/etc. Sometimes we
  //may be able to fix this by using the work stack, but not always, as
  //certain cow_ operations can never bubble back to parent in this way.
  //Not all calls to this function are in the context of parents/trees
  //but some are.
  //
  //we can't cow here because that would lose references if we throw an error
  //i mean, technically we could by pushing the cow() down below any error
  //throwing (we get a guarantee recursing won't trigger any...?). some
  //places not needed (eg if promote imminent)
  //We kind of don't want to cow here since that gets handled by many
  //parents


  if(IS_DEFINED(DEBUG) && (target->t == TABLE || target->t == MAP))
  {
    if(!CAN_WRITE(target))
    {
      er(target)
      if(target)show(target);
      er(index)
      if(index)show(index);
      er(replacement);
      if(replacement)show(replacement);
      dd(increment)
      dd(decrement)
      dd(target->t);
      dd(target->r);
    }
    assert(CAN_WRITE(target));
  }

  SW(target->t)
  {
    CS(ZIP, fprintf(stderr, "Error: Writing to ZIP currently disabled. If in a table, try replacing column.\n");
            ERROR(ERROR_TYPE))
    CS(TABLE,

        bool rejects_subtables = true;

        if(rejects_subtables)
        {
          SW(replacement->t)
          {
            CS(TABLE, ERROR(ERROR_COLUMN));
            CD: break;//noop
          }
        }

        //Only arrays can be set as table columns
        K fix = strong(replacement);
        fix  = cow_coerce_array(fix);
        work_push(fix);

        insert_replace_ri_rd(target, index, fix, true, false, increment, decrement);

        work_pop_rd(true);
      )
    CS(MAP,

        insert_replace_ri_rd(target, index, replacement, true, false, increment, decrement);
      )
   CSF(-LINK,)
   CSF(-STAMP,)
   CSF(-FLOAT,)
   CSF(-INT,)
    CS(-CHAR,
        K x = target;
        K y = replacement;
        I k = indexify(index); 
        if(k < 0 || COUNT(target) <= k)ERROR(ERROR_INDEX);

        if(xt==-yt && VECTOR_ATOM(yt))//POTENTIAL_OPTIMIZATION_POINT: vector_atom redundant now?
        {
          x = cow(x);

          SW(yt)
          {
            CS(LINK, xP[k] = yk)
            CS(CHAR, xC[k] = yc)
            CD:      xI[k] = yi; break;
          }
          check_attr_sort_at_i(x,k);
          return x;
        }

        x = cow_promote(x);
        x = update_ri_rd(x, index, y, increment, decrement);//recurse on LIST
        if(1==xn)x = cow_demote(x);
        target = x;
      )
    CS(LIST,
        I k = indexify(index); 
        if(k < 0 || COUNT(target) <= k)ERROR(ERROR_INDEX);
        target = cow(target);
        nestset_ri_rd(target,k,replacement,increment,decrement);//eventual demote needed
        check_attr_sort_at_i(target,k);
      )
    CS(BTREE,
        I k = indexify(index); 
        if(k < 0 || COUNT(target) <= k)ERROR(ERROR_INDEX);
        target = cow(target);
        target = _alter_tree_single(target, k, replacement);
        check_attr_sort_at_i(target,k);
      )
    CS(HASH,
        I k = indexify(index); 
        if(k < 0 || COUNT(target) <= k)ERROR(ERROR_INDEX);
        target = cow(target);
        target = _alter_intern_single(target, k, replacement);
        check_attr_sort_at_i(target,k);
      )
    CD: ERROR(ERROR_RANK);
  }

  return target;
}

I indexify(K x) //convert to array-friendly index
{
  SW(xt)
  {
    CR(FLOAT, xf)
    CR(INT, xi)
  //CR( CHAR,)  //we can parse these to allow 4 5 6[,"1"] -> 5
  //CR(-CHAR,) 
  }

  return -1;
}


