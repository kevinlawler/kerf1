#include "kerf.h"

//Conjecture: as a rule, cow_ methods should not ERROR as normal

#pragma mark - Regular Reference Counting
K strong(K x)//retain
{
  //if you don't copy you reference increment

  I copies_funcs = false;//possibly copy for the same reason as map (esp. if read-only closures)
  I copies_maps  = false;//what use case would ever need repeated same map?? (closures maybe)
  I copies_atoms = false;
  I copies_lists = false;
  //if all copies stay off you can get rid of strong() and use xr++ or ri()

  if(IS_TENANT(x))
  {
    R copy(x);
  }
  else if(IS_DISK(x))
  { 
    if(ALPHA_DISK_REFCOUNT)
    {
      //I think you can do this but you'll have to spend an hour proving it:
      xr++; R x;
      //You can also do shared-disk-copy-using-anonymous-tempfile and notify the user
    }
    else
    {
      er(Cowardly refusing to reference increment mapped disk object. Suggested: unmap existing file or save a new file and open that.)
      return Kn();
    }

  }

  //POTENTIAL_FEATURE_POINT
  //*if refcount is about to max out, you could copy() a new one (or crash)
  //*given that, we could've also used a 2-byte (or 1-byte) reference count instead, & that would be OK

  SW(xt)
  {
     CS( MAP,  if(xw>0 && !IS_DISK(x))R compiled_ref_safe_copy_map(x); if(!copies_maps){xr++; R x;}else R copy(x))
     CS( FUNC, if(!copies_funcs){xr++; R x;}else R copy(x))
    CSF( NIL,)
    CSF( LINK,)
    CSF( STAMP,)
    CSF( CHAR,)
    CSF( FLOAT,)
     CS( INT,  if(!copies_atoms){xr++; R x;}else R copy(x)) 
     CS( LIST, if(!copies_lists){xr++; R x;}else R copy(x))
    CSF(-INT,)
    CSF(-FLOAT,)
    CSF(-CHAR,)
    CSF(-STAMP,)
    CSF(-LINK,)
    CSF( TABLE,)//use or do not use map's xw protection?
    CD: xr++; R x;
  }

  R x;
}

void rd(K x)//release
{
  if(TEST_TRY_NO_REFERENCE_DECREMENT) return;

  SW(xr)
  {
    CS(0,if(IS_EMPTY_ATOM(x)) break; er(Warning: decrementing reference count of 0.))
    CS(TENANT_REF_SIGNAL,)//noop, allow < 0 to keep going wo/ decrement (prob -1)
    CS(1, 

      if(IS_DISK(x))
      {

        if(IS_STRIPED(x))
        {
          decrement_subelements(x);
        }

        disk_mmap_registry_remove_entry_for_mapped_origin_pointer(x);

        break;
      }

      if(TEST_TRACK_DEALLOC_IN_WORK_STACK)
      {
        K w = KVM->workspace;
        if(w)
        {
          ENUM(w, 
                  if(x==(V)vi)
                  {
                    er(Warning: dealloc of object in work_stack)
                    show(x);
                  }
          )
        }
      }

      --xr;//put after DISK so that 1==xr is endianness flag  (0 won't work)

      decrement_subelements(x);
      
      pool_dealloc(x);
    )
    CD: --xr;break;
  }
}

void decrement_subelements(K x)
{
  //repool in reverse to attempt to maintain order 
  I n = xn;
  SW(xt)
  {
   CSF( LINK,)
    CS(-LINK,)//noop. NOTE: do not rd -LINK vector pointers like NEST
   CSF( FUNC, 
              SW(x->nn)
              {
                CS(0,if(IS_EMPTY_ATOM(x))break;)//noop - NULL object
                //funcs need compiled reference decrement
                CD:_rd_function_helper(x);//fallthrough
              }
      )
   CSF( DEQUE,)
   CSF( PARTABLE,)
   CSF( ATLAS,)
   CSF( ZIP, )
   CSF( HASH,)
   CSF( BTREE,)
   CSF( TABLE,)
   CSF( MAP, n = box_count_K(x))//future-proof: maps indicate size there
    CS( LIST, DO(n, I j = _i-i-1; 
                    SW(x0[j].t)
                    {
                      CS(JUMP, K g = kJ(x,j); if(IS_STRIPED(x) && g->t == LINK && g->k){ rd(kN(x,j)); }) //set g->k=NULL ?
                      CS(LINK, rd(x0[j].k))
                    }
                )
      )
    CD: break;//noop
  }
}

void _rd_function_helper(K x)
{
  if(kN(x,FUNC_GLOBALS)->r > 1) return;//don't need to decrement yet

  func_wipe_compiled_globals_no_decrement(x);

  K absolutes = NULL;

  SW(x->nk)
  {
    CS(FUNC_KIND_DYNAMIC_LIB, ) //noop - globals are dylib pointers
  
    CD:
        absolutes = func_absolute_longform_globals(x);
        work_push(absolutes);
        if(GET_ALT_ATTR(x, FUNC_ATTR_CREFED)) //finished compiling? so that all vars work?
        {
          ENUM(absolutes, 
           K path = explode(kc('.'), v);
           work_push(path);
           compiled_ref_decrement_path_in_map(The_Kerf_Tree, path);
           work_pop();
           rd(path);
          )
        }
        work_pop();
        rd(absolutes);
        break;
  }

}

#pragma mark - Compiled Reference Counting

//for writes, no hop may have > 1 regular refs (maps or map-values^0)
//for reads, DV is forced b/c of rref==1 guarantees
//do reads also get DV^0? yes: it will happen anyway because
//the child dictionary causes cow
//
//also, parents of vars must be split like this
//but not the final guy (the key)
//
//0. must guarantee all dictionaries in path are 1==refcount after
//0. hash-writes must split all D and DV^0 in path
//0. hash-reads must split at least the maps in path. do they need to do DV^0 as well?
K cow_compiled_ref_increment_map(K x)
{
  x = cow_compiled_ref_split_map(x);
  xw++;
  return x;
}

K cow_compiled_ref_split_map(K x)
{
  x = cow(x);
  
  bool splits_all_values_types = true; //if it's a TWIN_ARRAY you'd need
  if(splits_all_values_types ) nestneu(x, VALUES, cow_special_promote(xValues));
  else if(IS_LIST(xValues)) nestneu(x, VALUES, cow(xValues));//probably obviated
  return x;
}

K1 compiled_var_path_increment(K x, K y)//makes path
{

  //At the moment this looks like it will only work on starting dictionaries x with r->1.
  //To expand it you would need to do like a nestneu thing on the parent or something
  //(which means passing the parent and ensuring all ancestors are r->1)

  I p = HASH_NULL;

  K original = x;
  K parent_list = NULL;
  I parent_index = -1;
  K0 o;

  ENUM(y, 

    if(0==i && 0==cv)//kludge so we don't have to 1_ one-drop explode('.', ".a.b.c")
    {
      x=The_Kerf_Tree;
      continue;
    }

    if(!IS_MAP(x) && !IS_TABLE(x))
    {
      goto failure;//freeing func from work stack would handle freeing...
    }

    if(IS_TENANT(x) && IS_DISK(x))
    {
      goto failure;//TODO: needs to be dynamically referenced from this point
    }

    if(!parent_list) x = cow_compiled_ref_increment_map(x);
    else nestneu(parent_list, parent_index, x = cow_compiled_ref_increment_map(x));

    p = lookupI(x,v);

    if(IS_HASH_NULL(p))
    {
      if(IS_TABLE(x)) goto failure;

      K map = new_map();
      p = insert_replace(x,v,map,true,false);
      rd(map);
    }

    if(i<cy-1)//deeper
    {
      K z = xValues;
      if(!IS_LIST(z)) goto failure;//ref: parent not map error
      parent_list = z;
      parent_index = p;
      x = LOOK_(z,p,o);
    }
  )
  
success:
  return (K1){x,ki0(p)};

failure:
  ERROR(ERROR_REFERENCE); //reference/symbol/lookup error
  //compiled_ref_decrement_path_in_map(original, y); //not needed because function release handles
  return (K1){x,ki0(HASH_NULL)};
}

void compiled_ref_decrement_path_in_map(K map, K y)
{
  K z = map;
  K0 o;

  ENUM(y, 

    if(0==i && 0==cv)//kludge so we don't have to 1_ one-drop explode('.', ".a.b.c")
    {
      z=The_Kerf_Tree;
      continue;
    }

    if(!IS_MAP(z) && !IS_TABLE(z)) return;

    zw--;

    assert(zw>=0);

    if(i==yn-1) break;

    I p = lookupI(z,v);
    if(IS_HASH_NULL(p)) return;

    K y = zValues;
    if(!IS_LIST(y)) return;
    z = LOOK_(y,p,o);
  )
}

#pragma mark - Reference Management

K cow(K x)//copy-on-write
{
  if(CAN_WRITE(x))
  {
    return x;
  }

  K y = copy(x);
  rd(x);
  return y;
}

K cow0(K x)
{
  if(IS_TENANT(x))return x;//for when we don't want to alloc atoms (eg setting inline)

  return cow(x);
}

#pragma mark - Copying

K copy(K x)//the requirement is that we get something we can write to
{
  R copy_m(x,0,false,false);
}

K copy_override(K x)
{
  R copy_m(x,0,false,true);
}

K copy_m(K x, I m_min, I zero_new, bool allow_mmap_master_copy)
{

  //it should be OK to copy eg atoms and [the overhead of/the top-level of] striped tables and so on.
  bool small_guy = (TABLE==xt);  

  if(!allow_mmap_master_copy && !IS_TENANT(x) && IS_DISK(x) && !small_guy)
  {
    er(Cowardly refusing to copy mapped disk object. Suggested: unmap existing file or save a new file and open it.)
    //in-ram copy or anonymous-disk-shared-map-copy could conceivably go here
    //should really do anonymous-disk-shared-map-copy and notify user to resave
    //note: any mmap copy needs dup() on filehandle since registry remove closes()
    return Kn();
  }

  if(IS_DISK(x) && IS_TENANT(x))
  {
    //er(Tenant Copy)
  }

  I b = any_size_K(x);

  K y;

  I request = MAX(POW2(m_min),b);
  
  y = pool_alloc_struct(request); 
  I m = ym;//alloc may change ->m
  
  *y = *x;
  ym = m;
  yr = 1; 
  OFF_ATTR(y, ATTR_DISK);
  OFF_ATTR(y, ATTR_STRIPED);

  SW(xt)
  {
    //Note: ref incrementing globals does not need compiled increment. new globals do
    //      (provided you don't share the globals outside functions)
   CSF( TABLE,)
   CSF( MAP,  yw=0)
   CSF( PARTABLE,)
   CSF( DATABASE,)
   CSF( DEQUE,)
   CSF( ATLAS,)
   CSF( ZIP,)
   CSF( FUNC,)
   CSF( HASH,)
   CSF( BTREE,)
    CS( LIST, DO(box_count_K(x), y0[i]=kn0; //must wipe here or LINKs cause crashes
                                 nestset(y,i,kN(x,i));
                )

              if(allow_mmap_master_copy && IS_DISK(x) && IS_STRIPED(x))
              {
                //POTENTIAL_OPTIMIZATION_POINT
                //go back over them to make sure they aren't writing to somebody else's shared column
                DO(box_count_K(x), nestset_ri_rd(y,i, copy_override(kN(x,i)), false, true); )
              }
      )
    CD: memcpy(y,x,b); 
        ym = m;
        yr = 1; 
        OFF_ATTR(y, ATTR_DISK);
        OFF_ATTR(y, ATTR_STRIPED);
        break;
  }

  if(zero_new) bzero(b+(V)y, POW2(ym) - b);

  return y;
}

//called from strong(). if >0 cref map
//must fix 1. the dictionary
//         2. map-values^0 
//         3. recurse on any top level maps in dv^0 with >0 crefs
//We don't need to check other kinds of 0-lists or maps because
//cref>0 guarantees an initial 1==refcount after splitting
K compiled_ref_safe_copy_map(K x)
{
  if(!IS_MAP(x) && !IS_TABLE(x))
  {
    //function should only be called on maps
  }

  x=copy(x);
  K values = kN(x,VALUES);
  if(IS_LIST(values))
  {
    nestneu(x,VALUES,values=cow(values));
    //I think this line is redundant (b/c cow(values)->copy->nestset->strong copies subdicts):
    //ENUM(values, if(IS_CREFED(v)) nestset(values,i,compiled_ref_safe_copy_map(v))) 
  }

  return x;
}
#pragma mark - Expand

K _expand_in_memory_struct(K x, I m_requested, I zero_new)
{
  if(m_requested <= xm) return x;//success, already compliant

  //assert(!IS_DISK(x) && 1==xr)

  //POTENTIAL_OPTIMIZATION_POINT
  // #ifdef mremap ...  defined on Linux, etc.

  //K z = copy_m(x, m_requested, zero_new); 
  //^^ this has a problem because compile-referenced dictionaries only copy
  //   on reference incremement but we really need them to stay unchanged

  //HACK
  K z = pool_alloc_struct(POW2(m_requested)); 
  I m = zm;//alloc may change ->m
  memcpy(z,x,any_size_K(x));
  zm = m;
  OFF_ATTR(z, ATTR_DISK);
  OFF_ATTR(z, ATTR_STRIPED);
  if(zero_new)zero_unused_space(z);
  //this hack lets us free known 1==r in-mem quickly
  //without causing reference dec/incrementing of cref'd subdictionaries
  xt=-CHAR;
  //

  rd(x);
  return z;
}

K expand_zero(K x, I m_requested, I zero_new)
{
  if(IS_DISK(x))
  {
    return _expand_disk_backed(x, m_requested, zero_new);
  }

  return _expand_in_memory_struct(x, m_requested, zero_new);
}

K expand(K x, I m_requested)
{
  return expand_zero(x, m_requested, false);
}

#pragma mark - Nestset

K nestneu(K x, I i, K y)
{
  //use this to update a list when you're writing on the subelements
  //we assume the neu/cow methods are invariant on reference count
  //   (technically this isn't absolutely true since PROMOTE necessarily 
  //    may add to refcount...it's invariant on the "top-level" items,
  //    so if we promote an object to an enlisted object, we've created an
  //    add'l reference, but the same rd() pattern suffices to free everything.
  //    something like that.)
  //so swap in the new pointer without any add'l reference management
  //(it may be the old pointer)
  //IS_DISK / JUMP==zt does not need change

  //nestset_ri_rd(x,i,y,false,false);//actually different if DISK causes writing

  K z = k0i(x,i);
  if(LINK==zt)zk = y;
  return y;
}

void nestset(K z, I i, K y)//prepare as 0-list entry
{
  nestset_ri_rd(z,i,y,true,true);
}

void nestset_ri_rd(K z, I i, K y, I increment, I decrement)//prepare as 0-list entry
{
  if(IS_DISK(z)){ _disk_nestset(z,i,y); return;}
  K x = k0(z)+i;
  K0 k = *x;

  SW(yt)
  {
    CSF( NIL,)
    CSF( STAMP,)
    CSF( CHAR,)
    CSF( FLOAT,)
     CS( INT, *x=*y; xm=LOG2_SIZEOF_K0; OFF_ATTR(x,ATTR_DISK); xr=TENANT_REF_SIGNAL;)
    CSF( (char)0, if(IS_EMPTY_ATOM(y)){*x=*y;break;})//fallthrough for 0==FUNC
     CD: *x=(increment?kl0(strong(y)):kl0(y));
  }

  if(decrement && k.t==LINK)rd(k.k);//do this after in case k.k==y
}

void _disk_nestset(K z, I i, K y)
{
  if(IS_STRIPED(z))
  {
    K old = kN(z,i);
    
    //For a striped object (table) z, we'll capture the z[i] nestset
    //and cause it to overwrite the separate disk object the z[i] LINK points to
    //instead of overwriting the dummy LINK inside of z.

    if(y == old) return;

    if(!IS_TENANT(old) && IS_DISK(old))
    {
      assert(JUMP==k0i(z,i)->t);
      assert(LINK== kJ(z,i)->t);
      char expand_m = ceiling_log_2(wire_size_K(y));
      _expand_disk_backed(old, expand_m, false);
      copy_from_k_to_good_disk_membuf_temp(old,y);
      return;
    }


  }

  K x = k0i(z,i);
  K0 k = *x;
  //POTENTIAL_OPTIMIZATION_POINT - maybe we want to return early if x==y
  //                               maybe recheck m sizes and stuff though

  //POTENTIAL_OPTIMIZATION_POINT
  //for disk(y) at least you could instead use POW2(ym) - faster, potential space overuse
  I request = wire_size_K(y);
  C request_m = ceiling_log_2(request);

  I nestcount = box_count_K(z);

  //not using k.m lets us go by index-JUMPs instead of having payload populated already
  //C available_m = k.m;
  C available_m = -1;
  if (i < nestcount - 1) available_m = floor_log_2(k0i(z,i+1)->n - k0i(z,i)->n);
  else if(0 <  i)        available_m = floor_log_2(kENDM(z) - (kENDM(kJ(z,i-1)))); 
  else if(0 == i)        available_m = floor_log_2(nest_payload_total_space(z));

  if(request_m > available_m) 
  {
    I delta = request - POW2(available_m);  
    I remain = nest_payload_remaining(z);

    if (delta > remain)
    {
      C expand_m = ceiling_log_2(POW2(zm) + (delta - remain));
      _expand_disk_backed(z, expand_m, false);
    }

    V split = kENDM(kJ(z,i));  
    V end = nest_payload_end(z);

    assert(end >= split);

    memmove(split + delta, split, end - split);
    I k = i+1;
    DO(nestcount - k, k0i(z,k+i)->n += delta)//update following JUMPs
  }

  //assert: we have |request| space reserved and ready for writing (at minimum)
  char copied;
  copied = copy_from_k_to_good_disk_membuf_temp(kJ(z,i), y);

  kJ(z,i)->r = TENANT_REF_SIGNAL;

  //assert: everybody except the last item has to stay flush on both sides
  //         so if ym is smaller, you need to preserve existing m

  C put_m = -1; 

  if(i<nestcount-1)
  {
    put_m = MAX(request_m, available_m);//at least existing slot
    kJ(z,i)->m = put_m;
  }
  else
  {
    put_m = MAX(request_m, LOG2_SIZEOF_K0);//last item, slot is anything at all
    kJ(z,i)->m = put_m;
    I zero_new = false;
    if(zero_new)
    {
      if(put_m < available_m)
      {
        V end = kENDM(kJ(z,i));  
        bzero(end, POW2(available_m)-POW2(put_m));
      }
    }
  }
}

#pragma mark - Writes

K cow_sort(K x)
{
  K0 o;
  //POTENTIAL_OPTIMIZATION_POINT
  //*in-place without intermediate grades or at()
  //*for in-place  you can use *unstable* sorts? (certainly on 1D vecs)
  // let employee #1 do that though

  //Note: when sorting table, leftmost column should be marked ATTR_SORTED, here and wherever else it occurs
  //      potentially also we can detect this in at(), if we're still using at()

  if(SORTED(x)) return x;

  K grades = grade_up(x);
  work_push(grades);
  K y = at(x, grades);
  SET_ATTR(y, ATTR_SORTED);

  if(IS_TABLE(y))
  {
    if(table_columns(y) > 0)
    {
      //mark first column sorted, too
      K first = LOOK_(yValues,0,o);
      assert(CAN_WRITE(first));
      if(CAN_WRITE(first))
      {
        SET_ATTR(first,ATTR_SORTED);
      }
    }
  }

  work_pop_rd(true);
  rd(x);
  return y;
}

K cow_coerce_vlist(K x)//probably you usually want cow_coerce_array instead
{
  //possibly we will want to turn "atoms" like SORTs and HASHs into
  //LISTs in a way that's smarter / preserves subelements as list
  //roundtripping promote/demote will be slow for that
  if(IS_VLIST(x))R x;//if it's a vlist, return it, no change in refcount

  x = cow_promote(x);
  return x = cow_demote(x);
}

K cow_coerce_array(K x)
{
  if(IS_ARRAY(x))R x;//if it's an array, return it, no change in refcount

  x = cow_promote(x);
  return x = cow_demote(x);
}

K cow_coerce_column(K x)
{
  if(IS_STRING(x))
  {
    K z = enlist(x);
    rd(x);
    return z;
  }

  if(IS_ARRAY(x))R x;//if it's an array, return it, no change in refcount

  x = cow_promote(x);
  return x = cow_demote(x);
}

K cow_join(K x, K y)//always returns a list
{
  //POTENTIAL_OPTIMIZATON_POINT
  //expanding the left argument m to the eventual m
  //should speed this up significantly
  //alternatively, you could write a version which does not rely on cow_add()

  //TODO
  //when upgrading join to be its own faster thing without cow_add
  //remember that join needs to check+maintain ATTR_SORTED when possible

  // 0. always returns an array
  // 1. if left side is empty, right right
  // 2. if right side is empty, return left
  // 3. now that neither side is an empty list, determine list type (5 options: -4...0)
  // 4. compute needed space
  // 4. alloc space: maybe reuse arg
  // 5. fill

  if (0==cy) {R cow_coerce_array(x);}

  if(MAP==xt || MAP==yt)
  {
    return cow_add(x, y);//quick fix, maybe not best. consider TABLE.
  }

  if(TABLE==xt && TABLE==yt)
  {
    //perhaps this should be some kind of union thing? eg null fill if the column is missing?
    return cow_add(x, y);//quick fix, maybe not best. consider TABLE.
  }

  bool lower_bound = 2;

  SW(xt)
  {
   CSF(CHARVEC,)
   CSF(STAMPVEC,)
   CSF(FLOATVEC,)
    CS(INTVEC, if(xt==yt)
               {
                 x = cow(x);
                 I n = xn + yn;
                 C m = list_size_m(xt, n);
                 if(m > xm) x = cow_expand(x,m);

                 S xend = xC + (xn * size_of_type_element[abs(xt)]);

                 memcpy(xend, yC, yn * size_of_type_element[abs(yt)]);
                 
                 xn = n;

                 //TODO fix sort check (easy)
                 //if both are (sorted or empty)
                 //use [a variation of] check_attr_sort_at_last_item to check in the middle (if both size >= 1)
                 OFF_ATTR(x,ATTR_SORTED);

                 return x;
               }
    )

    CS(ZIP,
      SW(x->nk)
      {
        CS(-STR32,
          SW(yt)
          {
            CS(LIST, 
              
              //Because this was not a win, no sense in increasing danger yet by enabling it
              break;

              //This method takes a list of CHARVEC, adds all the straggler bytes to the ZIP
              //then compresses them.
              //This doesn't speed up anything really [in the CSV parser], so it probably wasn't the bottleneck
              //like I though
              I width = 32;

              //POTENTIAL_OPTIMIZATION_POINT
              //1. pre-compress all RHS into in-memory ZIP, then join(ZIP-x, ZIP-y). (if no stragglers in left-hand side)
              //2. batch #1 into multiple in-memory zips. probably best.
              bool list_of_charvec = true;
              ENUM(y, if(vt==CHARVEC)continue; list_of_charvec=false; break;)
              if(!list_of_charvec) break;

              I bytes = width * cy;

              x = cow(x);
              nestneu(x,INDEX, cow(xIndex));

              C m = list_size_m(CHARVEC, xIndex->n + bytes);

              //Maybe expand the index to hold the uncompressed bytes
              if(m > xIndex->m)
              {
                nestneu(x,INDEX, cow_expand(xIndex,m));
              }

              zero_unused_space(xIndex);

              //Add the bytes
              S p = kC(xIndex)+xIndex->n;


              ENUM(y, DO2(vn, p[j]=vC[j])  p+=width;)

              xIndex->n += bytes;
            
              x = cow_zip_maybe_compress(x);
              return x;
            )
          }
        )
      }
    ) //Be careful here that you don't clobber any special join(-INT4,INT) or join(-FLOAT4,FLOAT) style stuff that
             //we have in cow_zip_add (the zip types will accept a wider range of input)

    case HASH: //POTENTIAL_OPTIMIZATION_POINT
             //making this bigger but not too big (10? 100?)
    
              if(xt==yt && lenI(y) > lower_bound)
              {
                bool will_be_sorted = check_attr_sort_for_join(x,y);
                
                K y_keys = kN(yIndex, KEYS);

                //Keys in hashset_y not in hashset_x

                //This silliness replaces
                //K glom = ex("$1 except $2", y_keys, xIndex);
                //which we replaced for thread-safety reasons (csv-test)
                K caught = xin(y_keys, xIndex);
                K flip = xnot(caught); 
                K where = which(flip);
                rd(caught);
                rd(flip);

                K glom = at(y_keys, where);
                rd(where);


                //Add the missing keys to the hashset
                x=cow(x);
                nestneu(x,INDEX,cow(xIndex));
                K hashset = xIndex;
                ENUM(glom, I p = insert_replace(hashset, v, 0, false, true))
                rd(glom);

                //Find the conversion table for y
                I c = lenI(y_keys);
                K z = new_k(INTVEC, c);

                hashset = xIndex;
                ENUM(y_keys, I p = lookupI(hashset, v); 
                             assert(!IS_HASH_NULL(p));
                             zI[i] = p; )

                //Convert the y INTVEC / Add it to the x INTVEC
                nestneu(x, KEYS, cow(xKeys));

                //POTENTIAL_OPTIMIZATION_POINT
                //convert while you join INTVECs instead of after
                I old_n = xKeys->n;
                I added_n = yKeys->n;

                nestneu(x, KEYS, cow_join(xKeys, yKeys));
                K w = xKeys; //kN(x,KEYS);

                DO(added_n, I k = old_n + i; wI[k] = zI[wI[k]])

                rd(z);

                if(will_be_sorted) SET_ATTR(x, ATTR_SORTED);
                else OFF_ATTR(x, ATTR_SORTED);

                return x;
              }
              break;

    CS(BTREE,  //POTENTIAL_OPTIMIZATION_POINT - postpone indexing [insert into tree],
              //**maybe use attribute**
              if(xt==yt)
              {

              }
    )

    CD: break;//default: fallthrough
  }

  ENUM(y, x=cow_add_funny(x, v))//changed this to fix kyle spaan's INSERT int to floatvec bug

  return x;
}

K cow_expand_zero(K x, I m, I zero_new)
{
  I use_expand = IS_DISK(x) || 1==xr;
  
  if(use_expand)
  {
    return expand_zero(x,m,zero_new);
  }

  //make a (potentially bigger) copy
  K z = copy_m(x,m,zero_new,false);
  rd(x);
  return z;
}

K cow_expand(K x, I m)
{
  return cow_expand_zero(x,m,false);
}

K cow_add_funny(K x, K y)
{
  //TODO Note: this hasn't yet been updated to work for DISK
  //           so we've walled it off
  //           we may want to reject on disk??
  if(!IS_DISK(x) && -INT == xt && FLOAT == yt)
  {
    K z = new_k(-FLOAT, cx);
    DO(cx, zF[i] = xI[i]);
    rd(x);
    x = z;
  }
  else if(-FLOAT == xt && INT == yt)
  {
    return cow_add(x, kf((F)yi));
  }

  return cow_add(x, y);
}

K cow_add(K x, K y)//always returns a list
{
  //POTENTIAL_OPTIMIZATION_POINT: 
  //maybe not here, but 
  //somewhere we can promote -INT to -FLOAT in place
  //(if you don't go to LIST instead, you'll lose some
  // high order INT values)

  SW(xt)
  {
    CS( HASH,  x = cow_intern_add(x,y); 
               check_attr_sort_at_last_item(x);
               return x;)

    CS( ZIP,   return cow_zip_add(x,y))
    CS( BTREE, return cow_sort_add_single(x,y))
    CS( TABLE, return cow_table_add(x,y))
    CS( ATLAS, return cow_atlas_add(x,y))
                                        
   CSF(-LINK,)
   CSF(-STAMP,)
   CSF(-FLOAT,)
   CSF(-INT,)
   CSF(-CHAR, if (yt==-xt)
              { 
                I m = (xn < max_possible_count_K(x))?xm:xm+1;

                x = cow_expand(x, m);

                SW(xt)
                {
                  CS(-LINK, xP[xn] = yk)
                  CS(-CHAR, xC[xn] = yc)
                  CD: xI[xn] = yi; break;
                }

                xn++;

                SW(xn)
                {
                  CS(1, SET_ATTR(x, ATTR_SORTED))
                  CD: check_attr_sort_at_last_item(x); break;
                }

                break;
              }
      )


   CSF( MAP,) //We can reclaim this if we want, to do say MAP-merge instead of [x,y]
   CSF( PARTABLE,)
   CSF( DATABASE,)
   CSF( FUNC,)
    CS( NIL,  x = cow_promote(x); return cow_add(x,y);)

   CSF( STAMP,)
   CSF( FLOAT,)
   CSF( INT,)
    CS( CHAR, x = cow_promote(x); x = cow_add(x,y); return cow_demote(x);)

    CS( LIST,
              if(IS_DISK(x))
              {
                //POTENTIAL_OPTIMIZATION_POINT
                //if you don't mind (potential) overallocation
                //you can use faster methods than wire_size
                //which recalcs the minimum but is O(n)

                I add = wire_size_K(y);
                I used = nest_payload_used(x);
                I revised_payload = used + add;

                _expand_disk_nest_special(x, 1+xn, revised_payload, true);

                k0(x)[xn] = kj0(nest_payload_used(x));

                K split = nest_payload_end(x);

                *(membuf*)split = (membuf){ceiling_log_2(add),ATTR_DISK,0,0};

                //POTENTIAL_OPTIMIZATION_POINT
                //this method uses pipes - unknown whether that will be slow
                //it is. like 50x
                copy_from_k_to_good_disk_membuf_temp(split, y);
                split->r = TENANT_REF_SIGNAL;

                xn++;

                SW(xn)
                {
                  CS(1, SET_ATTR(x, ATTR_SORTED); cow_demote(x))
                  CD: check_attr_sort_at_last_item(x); break;
                }


                break;
              }

              I m = (xn < max_possible_count_K(x))?xm:xm+1;

              x = cow_expand(x, m);

              x0[xn]=kn0;//you must wipe here or LINKs in reclaimed memory create crashes
              nestset(x,xn,y);
              xn++;

              SW(xn)
              {
                CS(2,check_attr_sort_at_last_item(x);)//TODO: matching maps we can convert to a table
                CS(1, SET_ATTR(x, ATTR_SORTED); x = cow_demote(x))
                CD: check_attr_sort_at_last_item(x); break;
              }
      )
  }

  return x;
}

K cow_delete(K x, K y)
{
  if(IS_ARRAY(x)) return cow_delete_indices(x,y);
  else if(IS_MIXED_KEYED(x)) return cow_delete_keys(x,y);
  else if(IS_ATLAS(x)){ }//TODO

  return x;
}

K cow_delete_keys(K x, K y)
{
  // make a copy; don't alter a table/map in place.
  x = cow(x);
  x = in_place_delete_keys(x, y);
  return x;
}

K in_place_delete_keys(K x, K y)
{
  // this routine will modify a map/table in place, like an ALTER TABLE DROP COLUMNS.
  // if you want to make a copy with keys removed, use cow_delete_keys().

  if (!IS_MAP(x) && !IS_TABLE(x)) { return x; }

  // find the (distinct) indices of the columns which should be removed:
  K z = Kk();
  ENUM(y, I p = lookupI(x, u); if(!IS_HASH_NULL(p)) { z = cow_add(z, ki(p)); })
  K dead_cols = distinct(z);
  rd(z);

  // strip key/value lists:
  nestneu(x, KEYS,   cow_delete_indices(xKeys,   dead_cols));
  nestneu(x, VALUES, cow_delete_indices(xValues, dead_cols));
  x = cow_reindex(x);
  rd(dead_cols);

  return x;
}

K cow_delete_indices(K x, K y)
{
  if(INTVEC!=yt || 0==yn) return x;
  x = cow(x);

  y = cow_sort(strong(y));
  work_push(y);

  //potential_feature_point
  //1. I think there is a simple way to handle repeats?
  //2. should we ignore out-of-bounds indexes wo/ error?
  //   (may affect total cx-cy)
  DO(yn, if(i>0 && yI[i] == yI[i-1])ERROR(ERROR_REPEAT);
         if(yI[i] < 0 || yI[i] >= xn)ERROR(ERROR_INDEX);
  )

  I n = MAX(0, cx - cy);

  SW(xt)
  {
    CS(HASH, x = cow(x); 
             nestneu(x, KEYS, cow_delete_indices(xKeys, y));
             work_pop_rd(true);//y
             return x;
      )
    CS(BTREE, x = cow(x);
             nestneu(x, KEYS,  cow_delete_indices(xKeys,  y));
             nestneu(x, INDEX, cow_delete_indices(xIndex, y));
             cow_landis_reindex(x);
             work_pop_rd(true);//y
             return x;
      )
    CS(TABLE,)//TODO: delete rows (simply recurse on each column)
  }

  K z;

  if(CAN_WRITE(x))
  {
    z = x;
  }
  else
  {
    z = new_k(xt, n); //smaller vector than cow(x) would create
    za = xa; //see copy() for logic on setting attributes. probably should refactor
    OFF_ATTR(z, ATTR_DISK);
    OFF_ATTR(z, ATTR_STRIPED);
  }
  work_push(z);

  //POTENTIAL_OPTIMIZATION_POINT
  //break out by types LIST CHARV INTV STAMPV FLOATV
  I skip = 0;
  I store = 0;

  DO(cx,  
          if(skip<yn && i == yI[skip])
          {
            if(z == x && LIST == zt)
            {
              if(IS_STRIPED(z) && IS_DISK(z))// && LINK==kJ(z,i)->t)
              {
                //Currently, if you use nestset to nil out a striped column,
                //you'll overwrite the column's values without actually blasting
                //(i.e. releasing and deleting) the column.
                //I think this all makes sense given delete is a special cow operation.
                //We had to split out the kJ thing b/c it makes the if statement die even though it shouldn't
                K k = kJ(z,i);
                if(LINK==k->t && GET_ATTR(k,ATTR_STRIPED))
                {
                  //TODO?: here and below, if this was the last guy (ie k->r==1)
                  //probably (I think) want to get the filehandle and ftruncate it to 0
                  //(and the mapping registry and so on) before release, to 
                  //free up more space. but this should be ok for now
                  if(k->k)rd(k->k);
                  k->k = NULL;
                  k->t=NIL;
                  OFF_ATTR(k,ATTR_STRIPED);
                }
                else
                {
                  nestset_ri_rd(z, i, kn, false, true);
                }
              }
              else
              {
                nestset_ri_rd(z, i, kn, false, true); //fixed a memory leak
              }
            }

            skip++;
            continue;
          }

          K0 o;
          update_ri_rd(z,ki(store),LOOK_(x,i,o),true,true);

          //Alternatively, we could strong() the LOOK above, wipe, and place (with decrement)
          if(i != store && z == x && LIST == zt)// && LINK==kJ(z,i)->t)
          {
              if(IS_STRIPED(z) && IS_DISK(z))
              {
                K k = kJ(z,i);
                if(LINK==k->t && GET_ATTR(k,ATTR_STRIPED))
                {
                  if(k->k)rd(k->k);
                  k->k = NULL;
                  k->t=NIL;
                  OFF_ATTR(k,ATTR_STRIPED);
                }
                else
                {
                  nestset_ri_rd(z, i, kn, false, true);
                }
              }
              else
              {
                nestset_ri_rd(z, i, kn, false, true); //fixed a memory leak
              }
          }

          store++;
  )

  work_pop_n_rd(2,false);
  rd(y);

  if(CAN_WRITE(x))
  {
    xn = n;
  }

  //optional: shrink disk members

  return z;
}


K cow_demote(K x)//make 0-lists into vectors -4...-1
{
  if(IS_MAP(x) || IS_TABLE(x))//we're sticking this in cause it's nice for change/alter/amend
  {
    assert(!IS_CREFED(x));
    nestneu(x, KEYS, cow_demote(xKeys));
    nestneu(x, VALUES, cow_demote(xValues));
    return x;
  }

  if(!IS_LIST(x)||!cx)R x;//only operate on non-empty 0-lists

  I t=0;

  K0 o;
  t = LOOK_(x,0,o)->t;//first element basic atom?
  if(!VECTOR_ATOM(t))R x;

  //TODO: demote LIST of MAPs to TABLE?

  bool all_same = true;
  bool numeric = true;

  ENUM(x, if(vt!=INT && vt!=FLOAT){numeric = false;} if(vt != t){all_same = false;} if(!all_same && !numeric) return x )//all elements the same type? 

  //POTENTIAL_OPTIMIZATION_POINT
  //This `numeric` business is good enough to preserve at least FLOATVEC
  //when altering (INTVEC with FLOAT) or (FLOATVEC with INT)
  //A further improvement would be to short-circuit the promote(),
  //and change, in place, any incoming INT[VEC] to FLOAT[VEC], or any existing INTVEC
  //to FLOATVEC

  if(numeric && !all_same) t = FLOAT;

  I can_write = IS_DISK(x) || 1==xr;

  K y = NULL;

  if(can_write)
  {
    y = x;
  }
  else
  {
    y = new_k(-t, cx);
  }

  ya |= xa & ATTR_SORTED;

  if(numeric && !all_same)
  {
    ENUM(x, SW(vt){CS(INT,yF[i] = si)CS(FLOAT, yF[i] = sf)} )
  }
  else SW(t)
  {
    CS(CHAR,ENUM(x, yC[i] = vc;))
    CD:     ENUM(x, yI[i] = vi;)
  }

  yt = -t;

  if(!can_write)
  {
    rd(x);
  }

  R y;
}

K cow_special_promote(K x)
{
  //Summary: eventually this should be merged with cow_promote
  //We rely on BTREE,HASH being promoted to LIST
  //at least in map CREF splitting. (Instead of just enlisted() with count
  //always 1 instead of n.)
  //What we should do is update cow_promote to handle IS_DISK
  //BTREES and HASHES and make them LISTs.
  //But we're holding off on merging these two functions until we do
  //since it breaks consistency

  if(IS_BTREE(x)||IS_HASH(x))
  {
    if(IS_DISK(x))
    {
      fprintf(stderr, "Unimplemented feature: promote on disk BTREE or HASH to LIST.\n");
      kerf_exit(0);
    }

    //This is copy-pasta from cow_promote
    K z = new_k(LIST,COUNT(x));

    //if(IS_ARRAY(x))
    ENUM(x, nestset_ri_rd(z,i,v,false,false))

    if(1==cx || GET_ATTR(x,ATTR_SORTED))
    {
      SET_ATTR(z, ATTR_SORTED);
    }

    rd(x);
    return z;
  }

  return cow_promote(x);
}

K cow_promote(K x)//always returns LIST
{
  //possibly we will want to turn "atoms" like SORTs and HASHs into
  //LISTs in a way that's smarter / preserves subelements as list

  if(IS_LIST(x))R x;

  if(IS_DISK(x))
  {
    K z = x;

    if(IS_VECTOR(z))
    {
      K0 k = *z;

      I payload = sizeof(K0)*zn;
      zh = LOG2_SIZEOF_K0;
      I a = any_size_K(z) - sizeof(K0);
      _expand_disk_nest_special(x,zn+1,payload,false);
      DO(zn, k0(z)[i] = kj0(sizeof(K0)*i))
      V s = nest_payload_start(z);
      V t = kENDM(z) - a;
      K y = t - sizeof(K0);
      *y = k;
      memmove(t, s, a); 

      //this is a hack
      //don't do a regular enum because y headers mutate midway through (overwritten by data)
      zt = LIST;
      SW(yt)
      {
        CS(-CHAR,DO(yn,nestset(z,i,kc(yC[i]))))
        CD:      DO(yn,nestset(z,i,ki(yI[i])))
      }

      I zero_new = false;
      if(zero_new) zero_unused_space(z);
    }
    else if(0 && (IS_BTREE(x) || IS_HASH(x)))
    {
     //not yet implemented - see in memory note below
     //don't do until we have a use case that forces
     //we'll just enlist them for now
     //whoops
    }
    else 
    {
      I shift = 2 * sizeof(K0);
      I w = wire_size_K(x);
      I oldm = xm;
      I m = ceiling_log_2(shift + w);
      _expand_disk_backed(x,m,false);
      xm = oldm;
      xr = TENANT_REF_SIGNAL;
      memmove(shift+(V)x,x,w);  
      *x = (K0){.m=m,.a=ATTR_DISK,.h=1+LOG2_SIZEOF_K0,.t=LIST,.r=xr,.n=1};
      k0(x)[0]=kj0(0);
    }

    return x;
  }
  else
  {
    K z = new_k(LIST,COUNT(x));

    //*z0=K00;//must zero memory or use nestset_ri_rd

    if(IS_VECTOR(x)) ENUM(x, nestset_ri_rd(z,i,v,false,false))
    else nestset_ri_rd(z,0,x,true,false);

    if(1==cx || GET_ATTR(x,ATTR_SORTED))
    {
      SET_ATTR(z, ATTR_SORTED);
    }

    rd(x);
    return z;
  }
}

K cow_ensure_null_terminated_chars(K x)
{
  SW(xt)
  {
    CS( CHAR, C c = xc; xn = 0; xc = c)
    CS(-CHAR, x=cow_add(x,kc('\0')); xn--)
  }

  return x;
}

#pragma mark - ATTR_SORT


ALWAYS_INLINE bool check_attr_sort_at_i(K x, I i)//POTENTIAL_OPTIMIZATION_POINT
{
  //check left and right of i (safely)
  //unset sort if necessary

  if(!GET_ATTR(x,ATTR_SORTED)) return false;

  I c = ECOUNT(x);

  K0 o1,o2;

  if(i > 0)
  {
    if(DESCENDING == KC(LOOK_(x,i-1,o1),LOOK_(x,i,o2)))
    {
      OFF_ATTR(x,ATTR_SORTED);
      return false;
    }
  }

  if(i < c - 1)
  {
    if(DESCENDING == KC(LOOK_(x,i,o1),LOOK_(x,i+1,o2)))
    {
      OFF_ATTR(x,ATTR_SORTED);
      return false;
    }
  }

  return true;
}

ALWAYS_INLINE bool check_attr_sort_at_last_item(K x)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //probably several ways. one is to make a
  //specific "compare last 2" function without LOOK
  //see also check_attr_sort_at_i

  if(!GET_ATTR(x,ATTR_SORTED)) return false;

  I c = ECOUNT(x);

  K0 o1,o2;

  if(c > 1)
  {
    if(DESCENDING == KC(LOOK_(x,c-2,o1),LOOK_(x,c-1,o2)))
    {
      OFF_ATTR(x,ATTR_SORTED);
      return false;
    }
  }

  return true;
}

ALWAYS_INLINE bool check_attr_sort_for_join(K x, K y)
{
  bool x_sorted = GET_ATTR(x, ATTR_SORTED);
  bool y_sorted = GET_ATTR(y, ATTR_SORTED);

  if(!x_sorted || !y_sorted) return false;

  I x_count = ECOUNT(x);
  I y_count = ECOUNT(y);

  bool any_empty = 0==x_count || 0==y_count;

  if(any_empty) return true; //ensures only populated array follow

  K0 o1,o2;
  K widow  = LOOK_(x, x_count-1,o1);
  K orphan = LOOK_(y, 0,o2);

  bool correct_seam = (DESCENDING != KC(widow, orphan));

  if(!correct_seam) return false;

  return true;
}


