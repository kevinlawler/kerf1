#include "kerf.h"

K extract(K x)
{

  if(IS_ATLAS(x)) return at(x,ki(0)); 

  if(!IS_TABLE(x)) return strong(x);

  assert(xt==TABLE);

  I cols = table_columns(x);
  I rows = table_rows(x);

  //If x is table
  //if x has 
  //0  columns: return strong x
  //1  column : 
  //          :   0  rows: array [empty]
  //          :   1  row : first item
  //          :   2+ rows: array
  //2+ columns: 
  //          :   0  rows: array of arrays [empty]
  //          :   1  row : array of items
  //          :   2+ rows: array of arrays

  SW(cols)
  {
    CR(0, Kk()); //or Kn() or strong(x)
    CS(1, 
          SW(rows)
          {
            CR(1, ex("first first xvals $1", x))
            CD:
           CSF(0,)
            CR(2, ex("      first xvals $1", x))
          }
    )
    CSF(2,)//2+ cols
    CD:   SW(rows)
          {
            CR(1, ex("first mapright xvals $1", x))
            CD: break;
          }
          break;
  }

  return strong(xValues);  
}

K cow_sorted_arrays_intervals_tool(K interval, K left, K right, C kind, bool fuzzy, K lowers_dupe) //kind - 0:left-closed 1:right-open
{
  //POTENTIAL_OPTIMIZATION_POINT
  //*binary/interpolation search (not linear): faster if RHS is comparatively huge?
  //*break (in either direction-kind) when interval is 0 width (?eg? current_key_in_lhs > final_key_in_rhs)

  //This isn't going to work exactly like this b/c you need a stop-work indicator if no key match
  //if you have the terminal chase vector (pre-populated with zeroes) you can mark it NAN, shrug

  I left_n  = COUNT(left);
  I right_n = COUNT(right);

  bool two_hashes = left->t == HASH && right->t == HASH;
  K leftish  = NULL;
  K rightish = NULL;
  
  if(two_hashes)
  {
    //*Profiler said "enum (string) comparisons" is 1/2 of time spent (most of slowdown) - easy fix.
    //   sort the unique values for both ahead of time, develop an integer lookup/comparison thing
    //
    //2016.09.13 the hash method that we used for the above is *slow* on small
    //tables if they were subselected from tables with big intern-hashsets
    //there...probably...isnt a good way to speed this up other than
    //reducing the size of such things before they get to us. 

    //take union. order it. hashsets replaced by indexing into orders.
    K left_unique  = kN(kN(left,INDEX),KEYS);
    K right_unique = kN(kN(right,INDEX),KEYS);

    K left_indices  = kN(left, KEYS);
    K right_indices = kN(right,KEYS);

    K code = NULL;
    
    code = ex("{[x,y] u:union(x,y); map(u,order u)}($1,$2)", left_unique, right_unique);
    work_push(code);

    K left_order  = NULL;
    K right_order = NULL; 

    left_order  = at(code, left_unique);
    right_order = at(code, right_unique); 

    work_pop_n_rd(1, true);

    leftish = new_intern(); 
    OFF_ATTR(leftish, ATTR_SORTED);
    K leftish_index = kN(leftish,  INDEX);
    nestset_ri_rd(leftish_index,   KEYS, left_order,   false, true); 
    nestset_ri_rd(leftish,         KEYS, left_indices, true,  true);

    rightish = new_intern(); 
    OFF_ATTR(rightish, ATTR_SORTED);
    K rightish_index = kN(rightish, INDEX);
    nestset_ri_rd(rightish_index,   KEYS, right_order,   false, true); 
    nestset_ri_rd(rightish,         KEYS, right_indices, true,  true);

    work_push(leftish);
    work_push(rightish);

    left = leftish;
    right = rightish;
  }

  bool vectory = false;

  if((IS_VECTOR(left) && IS_VECTOR(right) && left->t == right->t )  || two_hashes)
  {
    vectory = true;
  }

  if(0 == kind && !fuzzy)//raise floor, not on fuzzy columns
  {
    I i;

    for(i = 0; i < left_n; i++)
    {
      K0 o;
      K a = LOOK_(left, i,o);

      I self_j = kI(interval)[i];
      I neighbor_j = self_j;
      if(i > 0)
      {
        neighbor_j = kI(interval)[i-1];
      }
      I j = MAX(self_j, neighbor_j);

      for(j = j; j < right_n; j++)
      {

        K0 o2;
        K b = LOOK_(right, j, o2);

        if(vectory)
        {
          if(DESCENDING != AC(a,b)) break;
        }
        else
        {
          if(DESCENDING != KC(a,b)) break;
        }

        //POTENTIAL_OPTIMIZATION_POINT
        //this is where you would exp. increase j for faster search
      }

      kI(interval)[i] = j;
    }
  }
  else if(1 == kind)//lower ceiling
  {
    I i;
    for(i = left_n - 1; i >= 0; i--)
    {
      K0 o;
      K a = LOOK_(left, i, o);

      I self_j = kI(interval)[i];
      I neighbor_j = self_j;
      if(i < left_n - 1)
      {
        neighbor_j = kI(interval)[i+1];
      }
      I j = MIN(self_j, neighbor_j);

      for(j = j; j > 0 && j > kI(lowers_dupe)[i]; j--)
      {
        K0 o2;
        K b = LOOK_(right, j - 1, o2);

        if(vectory)
        {
          if(ASCENDING != AC(a,b)) break;
        }
        else
        {
          if(ASCENDING != KC(a,b)) break;
        }
        //POTENTIAL_OPTIMIZATION_POINT
        //this is where you would exp. increase j for faster search
      }

      kI(interval)[i] = j;
    }
  }

  if(two_hashes)
  {
    work_pop_n_rd(2, true);
  }
  
  return interval;
}

K sorted_tables_chase(K left, K right, I fuzzy_index) //both sorted, 
{
  assert(IS_TABLE(left));
  assert(IS_TABLE(right));
  assert(SORTED(left));

  K x = left;
  K y = right;

  I n = table_rows(left);
  I m = table_rows(right);
  I c = table_columns(left);//ignore extra columns in right

  K chase  = take(ki(n), ki(0));
  K lowers = take(ki(n), ki(0));
  K uppers = take(ki(n), ki(m));

  work_push(chase);
  work_push(lowers);
  work_push(uppers);
  OFF_ATTR(chase,ATTR_SORTED);

  DO(c,
    K left_col  = kN(xValues, i);
    K right_col = kN(yValues, i);

    bool final = (i == c - 1);
    bool fuzzy = fuzzy_index <= i; 
    bool last_exact = (final && !fuzzy) || (i == fuzzy_index - 1);

    lowers = cow_sorted_arrays_intervals_tool(lowers, left_col, right_col, 0, fuzzy, lowers);
    uppers = cow_sorted_arrays_intervals_tool(uppers, left_col, right_col, 1, fuzzy, lowers);

    if(last_exact) //step through and NAN out 0-width ranges (when no exact key match)
    {
      DO(n, I down = kI(lowers)[i];
            I up = kI(uppers)[i];
            I width = up - down;
            if(width <= 0)
            {
              kI(chase)[i] = IN;
            }
      )
    }

    if(final) //populate all chases not already NAN, using intervals
    {
      DO(n, 
            I down = kI(lowers)[i];
            I up = kI(uppers)[i];
            I width = up - down;
            if(0==width) kI(chase)[i] = IN;
            if (IN == kI(chase)[i]) continue;
            if(!fuzzy) kI(chase)[i] = down;
            else kI(chase)[i] = up - 1;
        )
    }
  )

  work_pop_rd(true);
  work_pop_rd(true);
  work_pop_rd(false);

  return chase;
}

K join_left(K x, K y, K z)//x:left-hand-side table, y:RHS, z:keys array or keys map
{
  return join_asof(x, y, z, kk);
}

K join_asof(K left_table, K right_table, K match_keys, K fuzzy_keys)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //Might be faster to use a hash function on [each row of] the key columns and then
  //grade the hashes. (I don't think you even need 128-bit hashes
  //so long as you verify equality later.)

  K x = left_table;
  K y = right_table;
  K z = match_keys;
  K w = fuzzy_keys;

  if(!IS_TABLE(x) || !IS_TABLE(y)) ERROR(ERROR_TABLE); //later we could update to sort maps and such

  K match_links = xmap(z, NULL);
  work_push(match_links);
  K fuzzy_links = xmap(w, NULL);
  work_push(fuzzy_links);

  K match_map_keys   = kN(match_links, KEYS);
  K match_map_values = kN(match_links, VALUES);
  K fuzzy_map_keys   = kN(fuzzy_links, KEYS);
  K fuzzy_map_values = kN(fuzzy_links, VALUES);

  K left_keys  = join(match_map_keys,   fuzzy_map_keys);
  work_push(left_keys);
  K right_keys = join(match_map_values, fuzzy_map_values);
  work_push(right_keys);

  ENUM(left_keys,  if(!table_has_column(x,v)) ERROR(ERROR_COLUMN))
  ENUM(right_keys, if(!table_has_column(y,v)) ERROR(ERROR_COLUMN))

  K right_subtable = table_ordered_subtable_on_column_keys(y, right_keys);
  K right_grades = grade_up(right_subtable); 
  K right_subtable_sorted = at(right_subtable, right_grades);
  SET_ATTR(right_subtable_sorted, ATTR_SORTED);
  //POTENTIAL_OPTIMIZATION_POINT
  //you don't need both the sorted subtable and the potable
  //if you reorder the columns in potable, so that all the columns
  //in subtable are at the front and in the same order as in subtable
  K right_potable = at(y, right_grades);//partially-ordered table, all columns
  rd(right_subtable);
  rd(right_grades);
  work_push(right_subtable_sorted);
  work_push(right_potable);

  I n = lenI(x);

  K left_subtable = table_ordered_subtable_on_column_keys(x, left_keys);
  K left_grades   = grade_up(left_subtable);
  K left_subtable_sorted = at(left_subtable, left_grades);//sorted subtable, key columns only
  SET_ATTR(left_subtable_sorted, ATTR_SORTED);
  rd(left_subtable);
  work_push(left_grades);
  work_push(left_subtable_sorted);

  bool keysort_left_table = false;

  I fuzzy_index = ECOUNT(match_links);
  K table_chase = NULL;

  table_chase = sorted_tables_chase(left_subtable_sorted, right_subtable_sorted, fuzzy_index);

  K regrade = NULL;
  K compound_chase = NULL;
  regrade = grade_up(left_grades);
  compound_chase = at(table_chase, regrade);
  work_push(compound_chase);
  rd(regrade);
  rd(table_chase);

  K unshared = new_k(LIST, 0);//unjoined non-link keys
  ENUM(right_potable, if(IN == finder(u, right_keys, NULL)) unshared = cow_add(unshared, u))
  work_push(unshared);

  K pre = table_ordered_subtable_on_column_keys(right_potable, unshared);
  ENUM(unshared,  K0 o; K picked = at(LOOKUP_(pre, v, o), compound_chase); insert_replace_ri_rd(pre, v, picked, true, false, false, true);)
  work_push(pre);
  
  K joined = strong(x);

  ENUM(unshared, joined = cow_table_receive_column_from_table(joined, pre, v, true))

  work_pop_n_rd(11, true);

  return joined;
}

#pragma mark - SQL DELETE

K placeholder_sql_delete(K target, K where_clauses, K select_clauses, K order_clauses, K limit_clauses)
{
  if(IS_CHARVEC(target)) //If it's a variable name, change it directly on the tree
  {
    work_push(strong(target));

    K1 k = denest_K1_from_start_and_key(The_Kerf_Tree, target, true, true); 
    
    K parent = k.u;
    if(!parent)
    {
      er(sql delete no parent);
      ERROR(ERROR_REFERENCE); 
    }
    I position = k.v.i;
    K0 o,o2; 
    K key = LOOK_(kN(parent,KEYS),position, o); 
    
    K x = AT2(parent, key, o2);

    SW(xt)
    {
     CSF(ATLAS, )
      CD:ERROR(ERROR_TABLE);
      CS(TABLE, x = cow_table_delete(x, where_clauses, select_clauses, order_clauses, limit_clauses))
    }

    update_ri_rd(parent, key, x, false, false);

    return work_pop();
  }
  else //Otherwise merely change a copy of the object
  {
    SW(target->t)
    {
     CSF(ATLAS, )
      CD:ERROR(ERROR_TABLE);
      CR(TABLE, cow_table_delete(strong(target), where_clauses, select_clauses, order_clauses, limit_clauses))
    }
  }
}

K cow_table_delete(K table, K where_clauses, K select_clauses, K order_clauses, K limit_clauses)
{
  table = cow(table);
  nestneu(table, VALUES, cow(kN(table,VALUES)));

  bool contains_star = false;

  bool picks_columns = COUNT(select_clauses) != 0 && !contains_star;
  bool has_wheres = COUNT(where_clauses) != 0;

  if(has_wheres && picks_columns) ERROR(ERROR_RAGGED);

  if(picks_columns)
  {
    //potential_feature_point
    //remove selected columns
    //just cow remove them here
    //see cow_table_drop_column 
    //(table keys and values)
    //return table;
  }

  if(!has_wheres) //Delete all rows
  {
    ENUM(table, update_ri_rd(kN(table,VALUES), ki(i), take(ki(0), v), false, true))
    return table;
  }

  //Delete according to WHEREs

  //Method A: we can add a final column to the subtable of i

  K subtable = NULL;
  subtable = strong(table);
  work_push(subtable);

  assert(COUNT(where_clauses)>0);//guaranteed by !has_wheres

  ENUM(where_clauses,  

    K function = v;

    I optimized = GET_ALT_ATTR(function, FUNC_ATTR_PRE_WHERED);

    K hits = MONAD_EX(function, subtable);

    work_push(hits);

    if(!optimized)
    {
      K whered = which(hits);
      work_pop_rd(true);
      hits = whered; 
      work_push(hits);
    }
    else
    {
      if(BYTEY(hits))
      {
        K whered = which(hits);
        work_pop_rd(true);
        hits = whered; 
        work_push(hits);
      }
    }

    //hits is now the virtual column `i`

    K reduce = at(subtable, hits);

    if(0==i)
    {
      reduce = cow_table_add_column_uncollided(reduce, kcv("i"), hits);
    }

    work_pop_n_rd(2, true);
    subtable = reduce;
    work_push(subtable);
  )

  K hits = last(kN(subtable,VALUES),NULL);
  SET_ATTR(hits, ATTR_SORTED);

  work_pop();
  rd(subtable);
  work_push(hits);

  ENUM(table, update_ri_rd(kN(table,VALUES), ki(i), cow_delete(v,hits), false, false))

  work_pop();
  rd(hits);

  return table;
}

#pragma mark - SQL UPDATE

K placeholder_sql_update(K target, K where_clauses, K groupby_clauses, K select_clauses, K as_clauses, K order_clauses, K limit_clauses)
{
  if(IS_CHARVEC(target)) //If it's a variable name, change it directly on the tree
  {
    work_push(strong(target));

    K1 k = denest_K1_from_start_and_key(The_Kerf_Tree, target, true, true); 
    
    K parent = k.u;
    if(!parent)
    {
      er(sql delete no parent);
      ERROR(ERROR_REFERENCE); 
    }
    I position = k.v.i;
    K0 o, o2;
    K key = LOOK_(kN(parent,KEYS),position,o); 
    
    K x = AT2(parent, key, o2);

    SW(xt)
    {
     CSF(ATLAS, )
      CD:ERROR(ERROR_TABLE);
      CS(TABLE, x = cow_table_update(x, where_clauses, groupby_clauses, select_clauses, as_clauses, order_clauses, limit_clauses, 0))
    }

    update_ri_rd(parent, key, x, false, false);

    return work_pop();
  }
  else //Otherwise merely change a copy of the object
  {
    SW(target->t)
    {
     CSF(ATLAS, )
      CD:ERROR(ERROR_TABLE);
      CS(TABLE, target = strong(target);
                target = cow(target);
                work_push(target);
                cow_table_update(target, where_clauses, groupby_clauses, select_clauses, as_clauses, order_clauses, limit_clauses, 0);
                work_pop();
                return target;)
    }
  }
}

K junky_table_copy(K table) //we're using this to get a semi-decent copy of striped table to play with
{
  //POTENTIAL_FEATURE_POINT
  //keyword: gnsdkjdnfk92222

  K z = new_table();
  ENUM(table, update(z, u, v)) 
  return z;
}

K cow_table_update(K table, K where_clauses, K groupby_clauses, K update_clauses, K as_clauses, K order_clauses, K limit_clauses, I groupby_subprefixes)
{
  if(0==COUNT(update_clauses))
  {
    return table;
  }

  K subtable = NULL;
  //subtable = copy(table);
  subtable = junky_table_copy(table);


  //On error (eg "update t set b=arity_error(c) group by a") this leaks something (table?)
  //If it's table, then the following argument holds (if not it's nonsense):
  //cow discussion
  //i. if we don't push table then it leaks because it's 
  //ii. if we do it gets freed incorrectly on error (cow'ing)
  //iii. we may need a special work stack for cow items???? (how??)
  //iv. we can get it right by "strong'ing" table I think but then it triggers warnings in `update` !CAN_WRITE
  //v. we may need to split all our 'cow' tools into two versions: raw wo/cow and w/cow

  table = cow(table); 
  nestneu(table, KEYS,   cow(kN(table,KEYS)));
  nestneu(table, VALUES, cow(kN(table,VALUES)));
  //work_push(table); //don't do this or you'll free table on error
  work_push(subtable);

  /////////////////////////////////////
  //
  //WHERE Clauses
  //
  /////////////////////////////////////

  ENUM(where_clauses,  

    K function = v;

    I optimized = GET_ALT_ATTR(function, FUNC_ATTR_PRE_WHERED);

    K hits = MONAD_EX(function, subtable);

    work_push(hits);

    if(!optimized)
    {
      K whered = which(hits);
      work_pop_rd(true);
      hits = whered; 
      work_push(hits);
    }
    else
    {
      if(BYTEY(hits))
      {
        K whered = which(hits);
        work_pop_rd(true);
        hits = whered; 
        work_push(hits);
      }
    }

    //hits is now the virtual column `i`

    K reduce = at(subtable, hits);

    if(0==i)
    {
      reduce = cow_table_add_column_uncollided(reduce, kcv("i"), hits);
    }

    work_pop_n_rd(2, true);
    subtable = reduce;
    work_push(subtable);
  )

  K hits = Kn(); //null==hits lets us skip that business & set everything to that (in alter())

  if(lenI(where_clauses) > 0)
  {
    rd(hits);
    hits = last(kN(subtable,VALUES),NULL);
    SET_ATTR(hits, ATTR_SORTED);
  }

  //reject any new columns if the WHERE clauses restrict us to less than the whole table
  //POTENTIAL_FEATURE_POINT
  //we could sidestep this by making a lot of NANs nans nulls
  if(!IS_NIL(hits) && lenI(hits) != table_rows(table))
  {
    if(lenI(where_clauses)>0)
    {
      ENUM(as_clauses, K0 o; if(!LOOKUP_(table, v, o)) ERROR(ERROR_RAGGED); )
    }
  }

  work_pop();
  work_push(hits);
  work_push(subtable);

  /////////////////////////////////////
  //
  //GROUP_BY Clauses
  //
  /////////////////////////////////////

  if(COUNT(groupby_clauses) > 0 && table_rows(subtable) > 0)
  {
    K group_columns = new_k(LIST, COUNT(groupby_clauses));
    group_columns->n = 0;
    work_push(group_columns);

    ENUM(groupby_clauses,
      K function = v;
      K group_slurry = MONAD_EX(function, subtable);
      group_columns = cow_add(group_columns, group_slurry);
      rd(group_slurry);
    )

    K parts = NULL;

    if(1)
    {
      //NOTE: This is copy-pasted from SQL-SELECT and needs any changes that go there
      parts = ex("(enlist range count first $1) {[x,y] flatten {[z] z[xvals part y[z]]} mapright x} fold $1", group_columns);
    }
    work_pop_rd(true);
    work_push(parts);

    //POTENTIAL_OPTIMIZATION_POINT
    //Two ways to do this for^2 loop:  (parts, update_clauses) or (update_clauses, parts)
    //didn't test which was faster
    ENUM(parts, 

      K part_table = at(subtable, v);
      K part_limits = kk; //we could add limits here if we wanted
      K no_wheres   = kk;
      K no_groupbys = kk;
      K part_update = cow_table_update(part_table, no_wheres, no_groupbys, update_clauses, as_clauses, order_clauses, part_limits, COUNT(groupby_clauses));
      work_push(part_update);

      K reindexed = NULL;
      
      if(IS_NIL(hits))
      {
        reindexed = strong(v);
      }
      else
      {
        reindexed = at(hits, v); 
      }

      work_push(reindexed);

      DO3(as_clauses->n, 
      
        K0 o;
        K current_as = LOOK_(as_clauses,k,o); 

        if(IS_NIL(current_as))
        {
          current_as = kcv("col");
        }

        K col = NULL;
        K0 o1;
        K payload = LOOKUP_(part_update, current_as, o1);

        I p = lookupI(table, current_as);

        if(!IS_HASH_NULL(p)) //table has column
        {
          col = kN(kN(table,VALUES), p);
          col = cow(col);
          nestneu(kN(table,VALUES), p, col);
        }
        else //create column for first time
        {
          //pre-load, overwrite later
          K0 col_nan = null_type_K0(payload);
          col = take(ki(table_rows(table)), &col_nan);

          cow_table_add_column_uncollided(table, current_as, col);
          
          if(IS_DISK(table))
          {
            rd(col);
            K v = kN(table,VALUES);
            col = kN(v,COUNT(v)-1); 
          }
          else
          {
            rd(col);
          }
        }
        
        K4 o2;
        col = change(col, klist1(reindexed, o2), NULL, payload);

        update_ri_rd(table, current_as, col, true, true);

        rd(col);
      )

      work_pop_n_rd(2, true);
    )

    work_pop();rd(parts);

    work_pop_n_rd(2, true);

    return table;
  }

  /////////////////////////////////////
  //
  //UPDATE Column & Aggregation Clauses
  //
  /////////////////////////////////////

  LIST2(as_clauses, update_clauses,

    K as_name = u;

    K function = v;

    if(IS_NIL(as_name))
    {
      as_name = alpha_uncollided_name(table, kcv("col"));
    }
    else
    {
      as_name = strong(u);
    }

    work_push(as_name);

    K single = NULL;
    
    single = MONAD_EX(function, subtable);

    if(IS_TABLE(single))
    {
      //when it's set c1={{c2:1 2 3}} one column table other SQLs allow that
      K temp = ex("last xvals $1", single);
      rd(single);
      single = temp;
    }

    K col = NULL;
    K payload = NULL;

    I p = lookupI(table, as_name);

    if(!IS_HASH_NULL(p))
    {
      col = kN(kN(table,VALUES), p);
      nestneu(kN(table,VALUES), p, col=cow(col));
    }

    if(IS_STRING(single))
    {
      K temp = enlist(single);
      rd(single);
      single = temp;
    }

    if(col)
    {
      K4 o;
      payload = change(col, klist1(hits, o), NULL, single);
    }
    else
    {
      payload = take(ki(table_rows(table)), single);
      rd(single);
      single = strong(payload);
    }

    work_pop();
    update_ri_rd(subtable, as_name, single, true, true);
    work_push(subtable);

    //I think this IS_STRIPED check could be artifical, and that the problem
    //is that we aren't correctly handling disk_nestset for striped somehow
    //when it writes the same thing in place
    //but i don't know the reason for sure yet
    //you should test in the terminal causing these lines to execute to see what I mean
    //anyway. this `if` pattern appears elsewhere in the code and can probably
    //be gotten rid of. `update` isn't happy about overwriting something with itself for STRIPED
    if(!IS_STRIPED(table) && !IS_DISK(table)) update_ri_rd(table, as_name, payload, true, true);

    rd(payload);
    rd(single);
    work_pop();rd(as_name);
  )

  work_pop();
  rd(subtable); 

  work_pop();
  rd(hits);

  return table;
}

#pragma mark - SQL SELECT

//POTENTIAL_FEATURE_POINT
//1. Column clauses in select (& other) statements could preserve local variables reasonably easy
//   from one clause to the next. eg. select (1+p)/p:1+1, 2+p, ...
//2. Columns can also use "as" names from the previous, which is *really* helpful
//   select blah*bleh as revenue, (revenue-prev)/prev as growth, ...
//   for this it may be helpful to use symbolic indexing
//3. updating symbolic indexing to support a virtual i column should be possible

K placeholder_sql_select(K table, K where_clauses, K groupby_clauses, K select_clauses, K as_clauses, K order_clauses, K limit_clauses)
{
  K z = NULL;

  if(IS_ATLAS(table))
  {
    z = atlas_select(table, where_clauses, groupby_clauses, select_clauses, as_clauses, order_clauses, limit_clauses, 0);
    return z;
  }
  else if(IS_PARTABLE(table))
  {
    K passed = new_k(INTVEC, 7);
    kI(passed)[0] = (I)table;
    kI(passed)[1] = (I)where_clauses;
    kI(passed)[2] = (I)groupby_clauses;
    kI(passed)[3] = (I)select_clauses;
    kI(passed)[4] = (I)as_clauses;
    kI(passed)[5] = (I)order_clauses;
    kI(passed)[6] = (I)limit_clauses;
    work_push(passed);

    K0 o1,o2;
    K parcel_paths = LOOKUP_(table, kcv("pre"), o1);
    K domain_table = LOOKUP_(table, kcv("parcel_names_evaled"), o2);

    if(!domain_table)
    {
      fprintf(stderr, "List of directories in parceled table did not eval correctly. Check naming.\n");
      ERROR(ERROR_TABLE);
    }

    K parcel_intvec = NULL;

    bool domain_match = false;

    //Look to see if parcel domain name is present in first WHERE clause
    if(COUNT(where_clauses) > 0)
    {
      K i = xin(kN(domain_table,KEYS), kN(kN(kN(where_clauses,0),FUNC_GLOBALS),KEYS));
      domain_match = INTIFY(i);
      rd(i);
    }

    K usable_wheres = strong(where_clauses);

    if(domain_match)
    {
      K function = kN(where_clauses,0);
      I optimized = GET_ALT_ATTR(function, FUNC_ATTR_PRE_WHERED);

      K hits = MONAD_EX(function, domain_table);

      if(!optimized || INTVEC!=hits->t)
      {
        K w = which(hits);
        rd(hits);
        hits = w;
      }

      parcel_intvec = hits;

      //Replace WHERE clauses by dropping the parcel domain one off front
      rd(usable_wheres);
      usable_wheres = drop(ki(1), where_clauses);
      kI(passed)[1] = (I)usable_wheres;
    }
    else
    {
      parcel_intvec = til(ki(table_rows(domain_table)));
    }

    K using_paths = at(parcel_paths, parcel_intvec);

    //POTENTIAL_OPTIMIZATION_POINT
    //when partable is loaded, determine which paths reside on which
    //devices/disks using device_major_for_file_handle

    bool parallel = true;

    if(COUNT(parcel_intvec) <= 50)
    {
      parallel = false;
    }

    rd(parcel_intvec);

    work_push(using_paths);
    work_push(usable_wheres);

    C ex_string[256] = {0};
    snprintf(ex_string, sizeof(ex_string), "join fold {[path] _debug_partable_select($1, path) } %s $2", parallel?"mapcores":"mapright");

    z = ex(ex_string, passed, using_paths);
    work_pop_n_rd(3, true);

    return z; 
  }
  else if(!IS_TABLE(table))ERROR(ERROR_TABLE);  //could also table-ize() here...

  z = table_select(table, where_clauses, groupby_clauses, select_clauses, as_clauses, order_clauses, limit_clauses, 0, NULL, 0);

  return z;
}

K _debug_partable_select(K passed, K path)
{
  K partable        = (K) kI(passed)[0]; 
  K where_clauses   = (K) kI(passed)[1]; 
  K groupby_clauses = (K) kI(passed)[2]; 
  K select_clauses  = (K) kI(passed)[3]; 
  K as_clauses      = (K) kI(passed)[4]; 
  K order_clauses   = (K) kI(passed)[5]; 
  K limit_clauses   = (K) kI(passed)[6]; 

  K table = verb_open_table(path);
  work_push(table);
  
  K z = table_select(table, where_clauses, groupby_clauses, select_clauses, as_clauses, order_clauses, limit_clauses, 0, NULL, 0);

  //If you don't do [something like] this, you will
  //have too many open files when doing `select * from big_parceled_table`
  if(IS_DISK(z))
  {
    K temp = copy_override(z);
    rd(z);
    z = temp;
  }
  else
  {
    z = cow(z);
    nestneu(z,VALUES,cow(zValues));

    DO(COUNT(zValues),

      K v = kN(zValues, i);
      if(IS_DISK(v))
      {
        nestset_ri_rd(zValues, i, copy_override(v), false, true);
      }
    )


  }

  work_pop_n_rd(1, true);

  return z;
}

K atlas_select(K atlas, K where_clauses, K groupby_clauses, K select_clauses, K as_clauses, K order_clauses, K limit_clauses, I groupby_subprefixes)
{
  K subatlas = strong(atlas);
  work_push(subatlas);

  //TODO
  // x0. where
  // x0. groupby
  // x0. select columns
  // x0. select aggregates
  //  0. Note: even though we're doing table-selects in the atlas methods here, maybe at the end
  //           we should pop them out into MAPs instead of TABLEs? for funsies
  //  0. there's going to be a tendency to want to fix all the WHERED stuff but we may want to leave it alone
  //      (meaning, we don't really need `where b+1 < 2` functionality do we?)
  //      that said, it may be smart to change the "special atlas form". here's a for instance:
  //      we can change the stack thingy to detect when an atlas (with a key embedded on it)
  //      is being returned and force the index. but this doesn't solve all our problems does it?
  //      you'd need to hook into the verb executor/ply too and ...
  //      probably better to just force an index EXCEPT when there's a special optimized comparator verb coming
  //  0. DELETE
  //  0. UPDATE


  /////////////////////////////////////
  //
  //WHERE Clauses
  //
  /////////////////////////////////////
  ENUM(where_clauses,  

    K function = v;

    I optimized = GET_ALT_ATTR(function, FUNC_ATTR_PRE_WHERED);

    K hits = MONAD_EX(function, subatlas);

    work_push(hits);

    //Questions:
    //  How should WHERE reduce? (in context of multiple items)
    //  Output is another atlas? or list? or table?
    //Attempted Strategy Passes
    //Pass 1: Produces indices, produce new ATLAS, recurse

    if(is_special_atlas_form(hits))
    {
      //todo:  implement "select from atlas where b" (with no comparator for 'b')
      //       we could also just force them to use 'where b != nil' or somesuch

      //Note: This select may be dependent on LIKE. Because we want
      //      select from atlas where b
      //      even if {[ ... {b:{c:4,d:5}}, ... ]}
      //      which has keys b.c and b.d only (not 'b')
      //
      //      However, the hacky way is to just ignore this for now.
      //      Which we should be able to get away with. (Force them
      //      to use 'b.c' not merely 'b')
      //
      //      There is a probable way forward on this. See `atlas_key_required`

      ERROR(ERROR_INDEX); //this would get throw below, but anyway...for now
    }
    else if(!optimized)
    {
      K whered = which(hits);
      work_pop_rd(true);
      hits = whered; 
      work_push(hits);
    }
    else
    {
      if(BYTEY(hits))
      {
        K whered = which(hits);
        work_pop_rd(true);
        hits = whered; 
        work_push(hits);
      }
    }

    if(hits->t != INTVEC)
    {
      ERROR(ERROR_INDEX);
    }

    //see comments at `table_select` for incorporation virtual `i` column

    K reduce = NULL;

    reduce = atlas_preserving_integral_at(subatlas, hits);

    work_pop_n_rd(2, true);
    subatlas = reduce;
    work_push(subatlas);
  )

  /////////////////////////////////////
  //
  //GROUP_BY Clauses
  //
  /////////////////////////////////////

  if(COUNT(groupby_clauses) > 0 && lenI(subatlas) > 0)
  {
    K group_columns = new_k(LIST, COUNT(groupby_clauses));
    group_columns->n = 0;
    work_push(group_columns);

    ENUM(groupby_clauses,

      K function = v;

      K group_slurry = MONAD_EX(function, subatlas);

      group_columns = cow_add(group_columns, group_slurry);

      rd(group_slurry);
    )

    K parts = NULL;

    if(1)
    {
      //NOTE: this is copy-pasted, see the stuff at the TABLE version
      parts = ex("(enlist range count first $1) {[x,y] flatten {[z] z[xvals part y[z]]} mapright x} fold $1", group_columns);
    }

    work_pop_rd(true);
    work_push(parts);

    K bigger_selects = strong(groupby_clauses);
    K bigger_ases    = take(ki(COUNT(bigger_selects)), kn);//Note: we could do groupy_as_clauses

    bigger_selects = cow_join(bigger_selects, select_clauses);
    bigger_ases    = cow_join(bigger_ases, as_clauses);  

    work_push(bigger_selects);
    work_push(bigger_ases);

    K build = NULL;

    //build = same_table_but_empty_columns(subatlas);
    build = new_atlas();

    work_push(build);

    K build_select = atlas_select(build, kk, kk, bigger_selects, bigger_ases, kk, kk, 0);
    work_pop_rd(true);
    build = build_select;
    work_push(build);

    ENUM(parts, 


      K part_atlas = atlas_preserving_integral_at(subatlas, v);

      K part_limits = kk; //we could add limits here if we wanted
      K no_wheres = kk;
      K no_groupbys = kk;

      K part_select = atlas_select(part_atlas, no_wheres, no_groupbys, bigger_selects, bigger_ases, order_clauses, part_limits, COUNT(groupby_clauses));

      work_pop();
      build = cow_table_add(build, part_select);
      work_push(build);

      rd(part_atlas);
      rd(part_select);
    )

    build = work_pop();
    work_pop_n_rd(1, true);
    work_pop_n_rd(1, true);
    work_pop_n_rd(1, true);
    subatlas = work_pop();
    rd(subatlas);

    return build;
  }

  /////////////////////////////////////
  //
  //SELECT Column & Aggregation Clauses
  //
  /////////////////////////////////////

  //2016.08.19 so far this is basically exactly the same as the table
  //           version, so maybe we should factor with that

  if(0==COUNT(select_clauses))
  {
    work_pop();
    return subatlas;
  }

  K select_columns = new_k(LIST, COUNT(select_clauses));
  select_columns->n = 0;
  work_push(select_columns);

  K build = new_table();
  work_push(build);

  LIST2(as_clauses, select_clauses,

    K name = u;
    K function = v;

    K single = NULL;
    
    //bit of a hack, not necessarily this but the general idea
    KVM->sql_peek = NULL; //before function execution

    single = MONAD_EX(function, subatlas);

    if(!IS_TABLE(single)) //was not a subselect, eg, not 2nd clause in "select a, (select from u), c from t"
    {
      K clause_text = NULL;
      K implied_name = NULL;
      K as_name = NULL;
      
      if(!IS_NIL(u))
      {
        as_name = u;
      }

      if(KVM->sql_peek)
      {
        //a weak reference dependent on clause function 
        //still being in scope
        implied_name = KVM->sql_peek;
      }

      clause_text = kN(function, FUNC_TEXT);

      bool squash_groupby = true;
      if(squash_groupby)
      {
        if(groupby_subprefixes > 0 && i >= groupby_subprefixes)
        {
          if(!IS_ATOM(single))
          {
            K list = enlist(single);
            rd(single);
            single = list;
          }

        }
      }

      //Note that subselections are not given a chance to be renamed

      K parent = NULL;

      //parent = subatlas; //<-- we're not doing this since atlas doesn't have any attribute stuff yet anyway
                           //    and we haven't even updated TABLE to have that yet really

      K tab = tableize(parent, single, clause_text, implied_name, as_name); 
      rd(single);
      single = tab;
    }

    if(i < groupby_subprefixes) //compress to 1 row
    {
      I rows = table_rows(single);

      if(rows > 0)
      {
        K top = car(single);
        rd(single);
        single = top;
      }
    }

    K build2 = cow_table_mesh(build, single, true);
    work_pop();
    build = build2;
    work_push(build);

    select_columns = cow_add(select_columns, single);

    rd(single);
  )

  build = work_pop();

  work_pop();
  rd(select_columns);

  work_pop();
  rd(subatlas); 

  return build;
}

I binning_magic_number = 87654321;
K single_column_binner(K x, I *bin_count) //distinct-bins-but-not-necessarily-sorted, preferably natural order
{
  assert(IS_ARRAY(x));

  I n = lenI(x);

  K y = NULL;
  K z = NULL;

  SW(xt)
  {
    //to xrank ints, first we need the #distincts (counting the hashset->keys won't do that)
    CS(HASH, K w = xKeys;
             K special = kN(xIndex,KEYS);
             I possible = COUNT(special);

             I count = 0;

             K y = new_k(INTVEC, possible);
             z = new_k(INTVEC, n);
             assert(wn==n);
             assert(zn==n);

             zero_list_payload(y);

             DO(n, I k = wI[i]; if(!yI[k]) yI[k] = ++count; zI[i] = yI[k] - 1)
             rd(y);
             if(bin_count) *bin_count = count;
      )

    CS(BTREE, //POTENTIAL_OPTIMIZATION_POINT - leverage landis-index ?
             return single_column_binner(xKeys, bin_count);
      )

   CSF(STAMPVEC,)//will drop into not-magic version.
    CS(INTVEC,
   
        //copy-pasta from grade_updown
        I t,the_min=II,the_max=-II;//MIN,MAX
        DO(xn, t=xI[i]; if(t<the_min)the_min=t; if(t>the_max)the_max=t;)  
        //POTENTIAL_OPTIMIZATION_POINT
        //dynamically set this based on system memory, similar to POOL_LANE_MAX
        I allowed = binning_magic_number;
        bool ok = false;
      
     #ifdef __int128
        __int128 diff = ((__int128)the_max) - (__int128)the_min;//or check diff>=0 for really big int64 differences
        ok = diff < allowed;
     #else
        I diff = the_max-the_min;
        ok = diff < allowed && diff >= 0;
     #endif
      
        if(ok)
        {
          I count = 0;
          y = new_k(INTVEC, diff);
          zero_list_payload(y);

          z = new_k(INTVEC, n);
          //Note: this is effectively the same as what we do for HASH above.
          //We could refactor it on that basis, substituting 0 for min and COUNT(hashset->keys) for max there
          //Benefit is marginal. Worth knowing
          DO(zn,  I k = xI[i] - the_min; if(!yI[k]) yI[k] = ++count; zI[i]=yI[k]-1; )
          if(bin_count) *bin_count = count;

          rd(y);
          return z;
        }
        else
        {
          //POTENTIAL_OPTIMIZATION_POINT
          //you could assign w=grade_up(y) and use order. this might not help though
          //Note however that this would be "natural ordered" and the current strategy isn't
          //floatvec uses a copy-pasta of this strategy so update there as well

          //NOTE: this non-w-grade_up(y) method breaks bucket natural order (first bucket may not be 0)
          //      a second grade doubles the time however (2^20:  70ms -> 140ms)
          y = grade_up(x);
          z = new_k(INTVEC, n);

          I count = 0;
          DO(zn, I k = yI[i]; if(i==0 || xI[k] != xI[yI[i-1]])
                              {
                                zI[k] = count++;
                              }
                              else //i>0, ==
                              {
                                zI[k] = zI[yI[i-1]];
                              }
          )
          
          if(bin_count) *bin_count = count;
          rd(y);
          return z;
        }
   
    )
    CS(FLOATVEC, 
          //POTENTIAL_OPTIMIZATION_POINT
          //you could assign w=grade_up(y) and use order. this might not help though
          //Note however that this would be "natural ordered" and the current strategy isn't
          //intvec uses a copy-pasta of this strategy so update there as well

          //NOTE: this non-w-grade_up(y) method breaks bucket natural order (first bucket may not be 0)
          //      a second grade may nearly double the time however
          y = grade_up(x);
          z = new_k(INTVEC, n);

          I count = 0;
          DO(zn, I k = yI[i]; if(i==0 || EQUAL != FC(xF[k], xF[yI[i-1]]))
                              {
                                zI[k] = count++;
                              }
                              else //i>0, ==
                              {
                                zI[k] = zI[yI[i-1]];
                              }
          )
          
          if(bin_count) *bin_count = count;
          rd(y);
          return z;
   )
   CSF(LIST,)//POTENTIAL_OPTIMIZATION_POINT: I'm sure this can be faster. Worst case: copy FLOATVEC strategy
   CSF(ZIP,) //POTENTIAL_OPTIMIZATION_POINT: for zipped INTs, STAMPs, etc you can unzip the column
             //                              into memory and recurse. this is *fine* for *group by*
    CD:
        y = hashed(x);
        work_push(y);
        z = single_column_binner(y, bin_count);
        work_pop_rd(true);
        if(bin_count) *bin_count = *bin_count;
  }

  return z;
}

void combine_column_bins(K x, K y, K *output, I *bin_count)
{
  //If implemented, this technique should be chosen to maintain natural order.

  //this is actually pretty easy to implement
  //z and y are both intvec
  //potential strategy:
  //grade z, grade y, step through checking where they're different
  //this should maintain natural order I think
  //
  //do it in place in z

  K w = grade_up(x);
    
  //xI[ ]; 

  rd(w);

  return;
}

K group_by_binner(K x, I *bin_count)
{
  //input:   list of columns (1 column for now)
  //output:  buckets-column [distinct-but-not-necessarily-sorted, preferably natural order]

  //Note: if you ever have to hash entire [sub-]tables, the fastest way to do this
  //      seems to be in column-major order, by a factor of 2x. Spitting it into a 
  //      separate intvector versus doing it implicitly (eg iterator) doesn't
  //      seem to meaningfully affect the time

  assert(LIST==xt);
  assert(0 < xn);

  //Multiple column binner
  //5.1  how do we handle more than one column buckets?
  //     if sorta small, you can do a list of bins. each unique bucket in the first
  //     array gets an intvec which it will expand using items from the second
  //     lame but ok. then collapse this [back into a single bucket number] so it never gets too bad
  //     as you iterate over more columns

  K0 o;
  K z = single_column_binner(LOOK_(x,0,o), bin_count);

  DO(xn, if(0 == i) continue;
         I y_bin_count = 0;
         K0 o2;
         K y = single_column_binner(LOOK_(x,i,o2), &y_bin_count);

         //Optimization
         bool small_enough = log(y_bin_count) + log(*bin_count) < log(binning_magic_number);
         bool use_optimization = small_enough;

         //Until we implement the below [and check it works better in those cases], let's always use this version
         use_optimization = true;

         if(use_optimization)
         {
           //This technique mantains natural order
           DO(yn, yI[i] = zI[i]*y_bin_count + yI[i]) //update y in place
           rd(z);
           z = single_column_binner(y, bin_count); //necessary since can't guarantee all possible z[i]*y[j] are present
           rd(y);
           continue;
         }
         else
         {
           fprintf(stderr, "Reasonable GROUP BY combinations exceeded: %lld. To continue request latest binary.\n", (y_bin_count * (*bin_count)));
           ERROR(ERROR_MISSING);
           combine_column_bins(z, y, &z, bin_count);
           rd(y);
         }
  )

  return z;
}

bool gby_hacktivated(K arg)
{
  if(NULL == gby_hack) return false;
  if(!IS_ARRAY(arg) && !IS_TABLE(arg)) return false;
  return true;
}

void whichify(K *x)
{
  work_push(*x);
  K whered = which(*x);
  work_pop_rd(true);
  *x = whered;
}

K _where_type_combiner(K x, K y) //warning: may modify its argument x
{
  if(x == y)
  {
    return strong(x);
  }

  //At the moment we assume only
  //INTVEC of indices
  //CHARVEC of 0-1
  //and not
  //INTVEC of 0-1
  assert(CHARVEC==xt || INTVEC==xt);
  assert(CHARVEC==yt || INTVEC==yt);
  assert(xn == yn);

  if(CHARVEC==xt && CHARVEC==yt)
  {
    K z = (1==xr) ? strong(x) : new_k(CHARVEC, xn); 
    DO(zn, xC[i] &= yC[i])
    return z;
  }

  K w = INTVEC==xt?strong(x):which(x);
  K z = INTVEC==yt?strong(y):which(y);

  work_push(w);
  work_push(z);

  //POTENTIAL_OPTIMIZATION_POINT
  //see note at xintersect
  K v = xintersect(w,z);

  work_pop_n_rd(2,true);

  return v;
}


K potential_column_names_from_functions(K x, bool *has_star)
{
  K z = new_k(LIST, 0);

  DO(xn, K funcs = kN(x,i);
         ENUM(funcs, 
                     if(has_star && GET_ALT_ATTR(v, FUNC_ATTR_HAS_STAR)) *has_star = true;
                     z = cow_join(z, kN(kN(v,FUNC_GLOBALS),KEYS))   
         )
  )

  return z;
}


//K The_Where_Cache = NULL;
K table_select(K table, K where_clauses, K groupby_clauses, K select_clauses, K as_clauses, K order_clauses, K limit_clauses, I groupby_subprefixes, K groupby_bins, I groupby_bin_count )
{
  K subtable = NULL;

  subtable = strong(table);
  work_push(subtable);

  //(NOTE: should you decide to actually support the virtual
  // column `i` there are a few ways to go about it.
  // You can have the table check if it already has an i, and if not
  // return the til(|rows|) value of the table.
  // You may also want to create a very special object which we can pass
  // to optimized WHERE verbs which cheats and tells them the answer directly.
  // e.g., for the query i<10, instead of generating a large til column, it
  // tells this to the WHERE verbs which generate only the necessary values.)


  /////////////////////////////////////
  //
  //RELEVANT COLUMNS
  //
  /////////////////////////////////////

  ////////////////////////////////////////////////////////////////
  //Reduce by selecting out only in-use column names
  //(sub-indexing everything [into the subtable] is expensive)
  //POTENTIAL_OPTIMIZATION_POINT
  //copy this logic elsewhere
  bool no_clauses = (0 == COUNT(select_clauses));
  bool has_star = false;

  //POTENTIAL_OPTIMIZATION_POINT
  //WHERE: If you wanted to get really fancy, you could drop add'l columns
  //after there was no longer any chance of them appearing, for instance `...
  //WHERE bidSz > 20, bidPx < 10.1, xyz > 123`  if bidSz only appears there
  //(and there are no stars and so on) you could drop it. This makes the next
  //two WHERE reduction indexings faster. One strategy for doing so is to
  //maintain a count `column_name => #appearances`
  //
  //Similarly, GROUP BY sub-selections can benefit from this same column
  //omission, probably more so.

  K4 o;
  K potential_cols = potential_column_names_from_functions(klist3(select_clauses, groupby_clauses, where_clauses, o), &has_star);
  K cols = kN(subtable, KEYS);

  K keepers = ex("$1[which $1 in $2]", cols, potential_cols);
  rd(potential_cols);

  work_push(keepers);

  bool col_reduce = true;
  if(no_clauses || has_star)
  {
    col_reduce = false;
  }

  if(col_reduce)
  {
    K reduce = at(subtable, keepers);
    work_pop_n_rd(2, true);
    subtable = reduce;
    work_push(subtable);
  }
  else
  {
    work_pop_rd(true);
  }
  ////////////////////////////////////////////////////////////////


  /////////////////////////////////////
  //
  //WHERE Clauses
  //
  /////////////////////////////////////

  K cache = kn;

  ENUM(where_clauses,  

    work_push(cache);

    bool last_pass = i >= COUNT(where_clauses) - 1;

    K hits = NULL;

    K function = v;

    //POTENTIAL_OPTIMIZATION_POINT
    //What we really want here is for the pre_whering
    //INTVEC generating function to "time out" if it's getting
    //too big, and then to fall back to boolean byte presence.
    //
    //Maybe we could also play the opposite game with bools
    //but less likely.
    //
    //Alternatively, don't use pre-whering if the vector isn't
    //ATTR_SORTED or indexed and so on

    I optimized = GET_ALT_ATTR(function, FUNC_ATTR_PRE_WHERED);

//K temp = The_Where_Cache;
//The_Where_Cache = cache;
    hits = MONAD_EX(function, subtable);
//The_Where_Cache = temp;

    //TODO: if hits is not INTVEC (or CHARVEC/BOOLVEC), error
    //      prevents bad interaction with comparison verbs?

    if(!optimized && INTVEC==hits->t)
    {
      if(CHARVEC==hits->t) {/*keep it, noop */ } 

      //POTENTIAL_OPTIMIZATION_POINT
      //If a large percentage of the INTs are 1 and not 0 (say >= x% and col
      //size > y ???) We may want to downsample this into a bytevec of the same
      //values (to not trigger a reduce) PROVIDED !last_pass (Of course, having
      //the functions return a bytevec to begin with would be better...)

      if(INTVEC==hits->t)  { whichify(&hits); }//INTVEC of 0s and 1s 64-bit boolean
    }

    if(optimized)
    {
      //POTENTIAL_OPTIMIZATION_POINT
      //If the I2 range(s) that would be returned from an ATTR_SORTED
      //array is "too large" (> 5% maybe?), hook into the function, 
      //decide at that time to do bytes, and instead
      //of converting it to an INTVEC-of-indices, convert it
      //into a boolean byte vector of length equal to the length
      //of the array. This should be really easy.

      if(INTVEC==hits->t) { /* keep it, noop */ } //INTVEC of indices eg 33,999,1080274

      if(BYTEY(hits)) {     /* keep it, noop */ } //CHARVEC of 0s and 1s 8-bit bytes
    }


    //COMBINE
    if(!IS_NIL(cache))
    {
      work_push(hits);
      K z = NULL;

      //POTENTIAL_OPTIMIZATION_POINT
      //Instead of combining here, you can expose
      //the cache [boolean vector] as a global somehow
      //and either `continue` against 0 positions
      //or `and` against them (same effect)
      //May choose to begin with an all-ones vector
      //somehow, though be careful about ignoring
      //that starting value in this loop
      //(it's no longer "NIL" the current thing we avoid)

      z = _where_type_combiner(cache, hits);
      work_pop(); rd(hits);
      hits = z;

      work_pop_rd(true);//cache
      cache = strong(hits);
      work_push(cache);
    }

    //POTENTIAL_OPTIMIZATION_POINT
    //INTVEC should not [immediately] trigger reduce if too much percent of the table
    //easy.
    //[last_pass case is an exception, but already handled]
    bool reducing = (INTVEC == hits->t);

    if(last_pass)
    {
      reducing = true;
    }


    if(reducing)
    {
      if(INTVEC != hits->t)
      {
        whichify(&hits);
      }

      work_push(hits);
      
      K reduce = NULL;

      reduce = at(subtable, hits);

      work_pop_n_rd(1, true);  //hits
      work_pop_n_rd(1, false); //subtable
      subtable = reduce;

      work_pop_rd(true);       //cache
      work_push(subtable);
      cache = kn;

      continue;
    }
    else
    {
      work_pop_rd(true);//cache
      cache = hits;
      work_push(cache);

      continue;
    }
  )

  /////////////////////////////////////
  //
  //GROUP_BY Clauses
  //
  /////////////////////////////////////

  if(COUNT(groupby_clauses) > 0 && table_rows(subtable) > 0)
  {
    K group_columns = new_k(LIST, COUNT(groupby_clauses));
    group_columns->n = 0;
    work_push(group_columns);

    ENUM(groupby_clauses,

      K function = v;

      K group_slurry = MONAD_EX(function, subtable);

      group_columns = cow_add(group_columns, group_slurry);

      rd(group_slurry);
    )

    //TODO: I think either you get to do it this way [hacktivated bin functions], or you split it up into groups and run the lambda on the groups
    //eg, for unhacktivatable bin functions like `select msum(4,blah) from table group by a`
    //maybe there's an allowable function list and a restricted function list (bit in the table), and we can use that to set an attribute

    //TODO: restore "sublist" group_by functionality for non-aggregate functions. Notes:
    //
    //try: you may be able to set a flag inside of PLY to hack the way groupby works.
    //try: subtable reduction may unlock better group_by. old way improved, can you carry it further?
    //try: part [sub-]column by binner, then do table_selects
    //try: Fastest may be to mark bad VERBs & fall back to old code?
    //try: may be fast enough to separate table via gby in one pass (cache-efficient). the way
    //     this would work is to use the binning list to build the (small first list) LIST of LISTS
    //     corresponding to the values in the table column(s)
    //unit test: make sure solution solves `mavg(mavg(col))` group by problem. 

    bool use_group_by_binner = true;//1==COUNT(group_columns);// && HASH == LOOK_(group_columns,0)->t;
    if(use_group_by_binner)
    {
      I bin_count = 0;
      K bins = group_by_binner(group_columns, &bin_count);
      work_push(bins);

      K bigger_selects = strong(groupby_clauses);
      K bigger_ases    = take(ki(COUNT(bigger_selects)), kn);//Note: we could do groupy_as_clauses

      bigger_selects = cow_join(bigger_selects, select_clauses);
      bigger_ases    = cow_join(bigger_ases, as_clauses);  

      work_push(bigger_selects);
      work_push(bigger_ases);

      K part_limits = kk; //we could add limits here if we wanted
      K no_wheres = kk;
      K no_groupbys = kk;

      K build = table_select(subtable, no_wheres, no_groupbys, bigger_selects, bigger_ases, order_clauses, part_limits, COUNT(groupby_clauses), bins, bin_count);
    gby_hack = NULL;
    gby_hack_count = 0;
      
      //K build = same_table_but_empty_columns(subtable);

      //work_push(build);
  
      //K build_select = table_select(build, kk, kk, bigger_selects, bigger_ases, kk, kk, 0);
      //work_pop_rd(true);
      //build = build_select;
      //work_push(build);

      //ENUM(parts, 
      //  K part_table = at(subtable, v);

      //  K part_select = table_select(part_table, no_wheres, no_groupbys, bigger_selects, bigger_ases, order_clauses, part_limits, COUNT(groupby_clauses));

      //  work_pop();
      //  build = cow_table_add(build, part_select);
      //  work_push(build);

      //  rd(part_table);
      //  rd(part_select);
      //)

      //build = work_pop();

      work_pop_n_rd(2, true);

      gby_hack = NULL;
      gby_hack_count = 0;
      work_pop_n_rd(1, true);
 
      work_pop_rd(true);//group_columns
      subtable = work_pop();
      rd(subtable);
      return build;
    }

    //POTENTIAL_OPTIMIZATION_POINT
    // Future directions may want to avoid the transpose (which creates a lot of
    // vectors) and create the "PART" structure directly.
    // This could be useful on say a large mapped table, who groups on an entire
    // mapped column, where the column contains only a few distinct values.
    // You'll still create a column-sized in-memory-only structure, unavoidably.
    // I suppose unless you come up with a way to iterate. Then you don't
    // need the column sized structure in memory. Provided you apply
    // the aggregator to all the rows along the way. Won't avoid a mapped table scan
    // however.
    //POTENTIAL_OPTIMIZATION_POINT^^^
    //(Now that we had to add the $1 I think this triggers an additional copy?)

    //Abandoned Strategy A
    //K distincts = new_k(LIST, 0);
    //ENUM(group_columns, K d = distinct(v); distincts = cow_add(distincts, d); rd(d))
    //K group = ex("join fold (join mapright mapleft) fold", distincts);
    //work_push(distincts);

    K parts = NULL;

    //POTENTIAL_OPTIMIZATION_POINT
    //1. We can still make groubpy 10x as fast: sort the table (Strategy F)
    //Note that it *may* be a win to sort on *keys only* and not on *non-keys*
    //There is a another potential speedup there which is passing
    //all-consecutive-interval-ranges into a fixed sorted table to run
    //aggregates, instead of at'ing the table to get something bubbly.
    //2. Note that the sorting method sacrifices the natural order. Maybe we
    //can get ours closer to 10x by profiling/fixing adverb/rewriting in C/etc.
    //3. If it matters, we can probably "inverse" the sorting after by retaining grades (we do similar in part() )
    //x4. Actually, looks like part() is the bottleneck, and we can speed that up 10x via sorting 
    //    we did this. it's still 10x+ faster to sort
    // 5. Group By is a lot like xrank - does this mean anything?
    
    if(1)
    {
      //NOTE: SQL-UPDATE [and maybe ATLAS-SELECT,...] is copy-pasted from this and changes must go there as well
      //Strategy E
      //POTENTIAL_OPTIMIZATION_POINT: improve strategy E: mantain two count-N lists: bool and INT. INT is indices. bool true indicates next group
      parts = ex("(enlist range count first $1) {[x,y] flatten {[z] z[xvals part y[z]]} mapright x} fold $1", group_columns);
      //note: multiple-list per depth at() would've actually been useful here^ (avoids mapright)
    }
    
    work_pop_rd(true);
    work_push(parts);

    K bigger_selects = strong(groupby_clauses);
    K bigger_ases    = take(ki(COUNT(bigger_selects)), kn);//Note: we could do groupy_as_clauses

    bigger_selects = cow_join(bigger_selects, select_clauses);
    bigger_ases    = cow_join(bigger_ases, as_clauses);  

    work_push(bigger_selects);
    work_push(bigger_ases);

    K build = same_table_but_empty_columns(subtable);

    work_push(build);
  
    K build_select = table_select(build, kk, kk, bigger_selects, bigger_ases, kk, kk, 0, NULL, 0);
    work_pop_rd(true);
    build = build_select;
    work_push(build);

    //POTENTIAL_OPTIMIZATION_POINT
    //you could parallelize this, probably

    ENUM(parts, 
      K part_table = at(subtable, v);

      K part_limits = kk; //we could add limits here if we wanted
      K no_wheres = kk;
      K no_groupbys = kk;

      K part_select = table_select(part_table, no_wheres, no_groupbys, bigger_selects, bigger_ases, order_clauses, part_limits, COUNT(groupby_clauses), NULL, 0);

      work_pop();
      build = cow_table_add(build, part_select);
      work_push(build);

      rd(part_table);
      rd(part_select);
    )

    build = work_pop();
    work_pop_n_rd(3, true);
    subtable = work_pop();
    rd(subtable);

    return build;
  }

  /////////////////////////////////////
  //
  //SELECT Column & Aggregation Clauses
  //
  /////////////////////////////////////

  if(0==COUNT(select_clauses))
  {
    work_pop();
    return subtable;
  }

  K select_columns = new_k(LIST, COUNT(select_clauses));
  select_columns->n = 0;
  work_push(select_columns);

  K build = new_table();
  work_push(build);

  LIST2(as_clauses, select_clauses,

    K name = u;
    K function = v;

    K single = NULL;
    
    //bit of a hack, not necessarily this but the general idea
    KVM->sql_peek = NULL; //before function execution

    //POTENTIAL_FEATURE_POINT
    //if we changed this to run on the build, order something similar,
    //we could reuse columns we just made, eg,   select a+a as b, b+b as c from {{a:1 2}}
    //as we do with sql-update
    //note: probably lots of name stuff to handle
    //Note: probably we *don't* want this, since those columns never existed in the table & that's
    //      not how select works otherwise (it looks only at the pre-existing table)

    //TODO: we SHOULD model this [gby_hack] after KVM->workspace (in the long term). so it's a little annoying
    //      since you need to keep track of height and so on on the regular stack. try to generalize
    //      maybe for the next time we need to do a stack hack.
    //      (without something like this you can't do recursive group by)
    //group_by_stack_hack_push(bins);
    //KVM->group_by_stack_hack;
    //maybe we can cheat and use space on the current function instead....?
gby_hack = groupby_bins;//this stack is only 90%, you'd need a real stack on KVM. see above in "group by" section
gby_hack_count = groupby_bin_count;
    single = MONAD_EX(function, subtable);

    if(!IS_TABLE(single)) //was not a subselect, eg, not 2nd clause in "select a, (select from u), c from t"
    {
      K clause_text = NULL;
      K implied_name = NULL;
      K as_name = NULL;
      
      if(!IS_NIL(u))
      {
        as_name = u;
      }

      if(KVM->sql_peek)
      {
        //a weak reference dependent on clause function 
        //still being in scope
        implied_name = KVM->sql_peek;
      }

      clause_text = kN(function, FUNC_TEXT);

      bool squash_groupby = true;
      if(squash_groupby && !groupby_bins)
      {
        if(groupby_subprefixes > 0 && i >= groupby_subprefixes)
        {
          if(!IS_ATOM(single))
          {
            K list = enlist(single);
            rd(single);
            single = list;
          }

        }
      }

      //Note that subselections are not given a chance to be renamed
      K tab = tableize(subtable, single, clause_text, implied_name, as_name); 
      rd(single);
      single = tab;
    }

    if(i < groupby_subprefixes) 
    { 
      if(groupby_bins) //grab the last
      {
        K top = last(single, NULL);
        rd(single);
        single = top;
      }
      else //compress to 1 row
      {
        I rows = table_rows(single);

        if(rows > 0)
        {
          K top = car(single);
          rd(single);
          single = top;
        }
      }
    }


    K build2 = cow_table_mesh(build, single, true);

    work_pop();
    build = build2;
    work_push(build);

    select_columns = cow_add(select_columns, single);

    rd(single);

gby_hack = NULL;
gby_hack_count = 0;
  )

  build = work_pop();

  work_pop();
  rd(select_columns);

  work_pop();
  rd(subtable); 

  return build;
}

K tableize(K parent, K payload, K clause_text, K inferred_name, K as_name)
{
  K build = new_table();

  K name = NULL; 

  clause_text = clause_text;//ignored for now but we could use...

  if(as_name)
  {
    name = as_name;
  }
  else if(inferred_name)
  {
    name = inferred_name;
  }
  else
  {
    name = kcv("col");
  }

  build = cow_table_add_column(build, name, payload);

  //try to retain eg foreign key traits
  if(inferred_name && parent)
  {
    K k = table_trait_get_column(parent, inferred_name);

    if(k)
    {
      build = cow_table_trait_set_column(build, name, k);
      rd(k);
    }
  }
  
  return build;
}

#pragma mark -

K placeholder_sql_insert(K target, K columns, K values, K conflict_keys)
{
  //This is a placeholder copied from change() (copy/pasta)
  //It stinks - see discussion at translate_sql_insert

  if(IS_CHARVEC(target)) //If it's a variable name, change it directly on the tree
  {
    work_push(strong(target));

    K1 k = denest_K1_from_start_and_key(The_Kerf_Tree, target, true, true); 
    
    K parent = k.u;
    if(!parent)
    {
      er(sql insert no parent);
      ERROR(ERROR_REFERENCE); 
    }
    I position = k.v.i;
    K0 o, o2;
    K key = LOOK_(kN(parent,KEYS),position, o); 
    
    K x = AT2(parent, key,o2);

    SW(xt)
    {
      CS(ATLAS, x = cow_atlas_insert(x, columns, values))
      CD:
      CS(TABLE, x = cow_table_insert(x, columns, values, conflict_keys))
    }

    update_ri_rd(parent, key, x, false, false);

    return work_pop();
  }
  else //Otherwise merely change a copy of the object
  {
    SW(target->t)
    {
      CR(ATLAS, cow_atlas_insert(strong(target), columns, values))
      CD:
      CR(TABLE, cow_table_insert(strong(target), columns, values, conflict_keys))
    }
  }
}

K cow_atlas_insert(K atlas, K columns, K values)
{
  if(columns) ERROR(ERROR_COLUMN);//no support for anything like this yet

  K x = atlas;
  K y = values;

  SW(yt)
  {
    CS(ATLAS, //POTENTIAL_OPTIMIZATION_POINT: merely join the SORT2s 
              //                              (relabeling REVERSE indices)
              //                              (and updating the INTVEC fence index) 
              DO(cy, K z = at(y, ki(i));
                     work_push(z);
                     x = cow_atlas_add(x, z);
                     work_pop_rd(true);
              )
    )
    CS(MAP, x = cow_atlas_add(x, y))
    CD: if(IS_ARRAY(y))
        {
          K z = new_atlas_from_K(y);
          work_push(z);
          x = cow_atlas_insert(x, columns, z);
          work_pop_rd(true);
          break;
        }
        ERROR(ERROR_ROW);
        break;
  }

  return x;
}

K cow_table_insert(K table, K columns, K values, K conflict_keys)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //retain ATTR_SORTED: check everywhere: cow_table_add, cow_join (remove OFF_ATTR) 
  //This is pretty tricky...for "table insert table" you get it if
  //table_row_compare the last row of the LHS is <= the first row of the RHS,
  //and if the RHS is a table with ATTR_SORTED. In other cases, trickier.

  //Does this factor with cow_table_add in some superior way... ???

  if(TABLE != table->t) ERROR(ERROR_TABLE); //could also table-ize() here...

  //No explicitly-specified columns given
  if((!columns || 0 == COUNT(columns)))
  {
    return cow_table_add(table, values);
  }

  //TODO pre resolve {|columns| = |values|, except if cols parens DNE } at compile time

  //table: cow table
  //columns: list of column names, or empty list
  //values: list of (lists of equal count of entries/cells)

  //Cases: 
  //A. no "(col1, ...)" exists: must match on values count (== |table.columns|)
  //B. some "(col1, ...)" exists: unspecified are defaults
  //(C. empty col list: all defaults)

  if(IS_MIXED_KEYED(values))
  {
    return cow_table_insert(table, columns, kN(values,VALUES), conflict_keys);
  }

  I values_count = COUNT(values);
  I table_key_count = COUNT(kN(table,KEYS));

  if(!columns) //no columns paren specified, not even empty list ()
  {
    if(table_key_count != values_count)
    {
      //TODO: error, count mismatch. can't insert b/c can't match
      //I think we actually catch this above now (!columns)
      //you could actually make this work anyway, with column-null values...
    }
  }

  I n = 1;

  //Count 1 items will resolve with any other arrays to be inserted
  //All non-size-1 arrays must be equal in size
  //Note: if a size-0 array is present, insert nothing
  //DEFAULT values will operate like size-1 items

  if(0 == values_count)
  {
    n = 1;//redundant for clarity
  }
  else
  {
    ENUM(values, I count = COUNT(v);
                  
                 //TODO?: probably better to build our own insert
                 //list with coerce_column. but this hacked check
                 //will do for now
                 if(IS_STRING(v) || !IS_ARRAY(v)) count = 1;

                 SW(count)
                 {
                  CS(1,)
                  CD: if(1==n)
                      {
                        n = count;
                      }
                      else if(count != n)
                      {
                        fprintf(stderr, "Error: inserting columns with mismatched counts.\n");
                        ERROR(ERROR_COLUMN); 
                      }
                      break;
                 }
    )
  }

  I insert_count = n;

  if(0==insert_count) goto done;

  table = cow(table);
  OFF_ATTR(table, ATTR_SORTED);
  K table_values =  cow(kN(table, VALUES));
  nestneu(table, VALUES, table_values);

  K keepem = NULL;
  K dropem = NULL;
  K hitem  = NULL;


  if(conflict_keys) //... ON CONFLICT (a,b) ... present
  {
    //ok, so now we are looking at the RHS. our columns are specified,
    //at least partially. 
    //we need to
    //1. find the matches on the LHS, INTVEC
    //2. reduce the RHS by the matches and proceed with insert
    //3. follow up with the remaining updates on the key-matches
    //   we could also do this before, I suppose
    //
    //Idea: put in ERRORs for the time being for everything we can get away with
    //

    assert(LIST == conflict_keys->t);
    assert(columns);

    if(COUNT(conflict_keys) >= ECOUNT(values))  ERROR(ERROR_COLUMN);// so we can use integer i indexing

    //TODO: dedupe the RHS inserting table (optional for now) {a: 1 1, b: 2 2} -> {a:1, b: 2}

    bool table_must_have_all_columns = false; 

    if(table_must_have_all_columns)
    {
      if(COUNT(kN(table,KEYS)) != ECOUNT(columns)) ERROR(ERROR_COLUMN);
    }
    else
    {
      if(0==table_columns(table))
      {
        rd(table);
        table = xtable(columns, NULL);
        table_values =  cow(kN(table, VALUES));
        nestneu(table, VALUES, table_values);
      }
    }

    bool column_counts_must_be_equal = true; 

    if(column_counts_must_be_equal)
    {
      if(COUNT(kN(table,KEYS)) != ECOUNT(values)) ERROR(ERROR_COLUMN);
    }
    else
    {
      ERROR(ERROR_COLUMN);//TODO: allow missing column specifications
    }

    //Select out the conflict columns

    K lhs_key_cols = at(table, conflict_keys);
    K rhs_key_cols = NULL;
    
    if(IS_MIXED_KEYED(values))
    {
      rhs_key_cols = at(values, conflict_keys);
    }
    else
    {
      K x = new_k(LIST,0);

      K0 o;
      ENUM(conflict_keys, x = cow_add(x, AT2(values, ki(lookupI(table,v)),o)))

      rhs_key_cols = x;
    }

    work_push(lhs_key_cols);
    work_push(rhs_key_cols);

    //Find the matches on the LHS

    //POTENTIAL_OPTIMIZATION_POINT
    //do the comparisons "all at once" (binning?)
    //instead of row at a time

    keepem = new_k(INTVEC, 0);
    dropem = new_k(INTVEC, 0);
    hitem  = new_k(INTVEC, 0);

    //Row at a time lookup
    DO(insert_count,

      I rhs_row_index = i;

      K carry = NULL;

      ENUM(rhs_key_cols, 

        I col_index = i;

        K0 o1,o2,o3;
        K left  = LOOK_(lhs_key_cols,  col_index, o1);
        K right = LOOK_(rhs_key_cols,  col_index, o2);

        K item = LOOK_(right, rhs_row_index, o3);

        K matches = NULL; 

        if(!carry)
        {
          carry = tri_compare_pre_whered(left, item, (I*)(I[3]){0,1,0});
          continue;
        }

        K more = new_k(INTVEC, 0);

        DO2(carry->n, K0 o; I k = kI(carry)[j]; if(EQUAL==KC_NUM(LOOK_(left,k,o), item)) more = cow_add(more, ki(k)); )   

        rd(carry);
        carry = more;
      )

      if(0 == carry->n > 0) //will INSERT (no conflict)
      {
        keepem = cow_add(keepem, ki(rhs_row_index));
      }
      else //UPDATE/REPLACE/IGNORE (found conflict)
      {
        I first = kI(carry)[0];
        I last = kI(carry)[carry->n-1];

        I lhs_row_index = first;

        dropem = cow_add(dropem, ki(rhs_row_index));
        hitem  = cow_add(hitem,  ki(lhs_row_index));
      }

      if(carry) rd(carry);
    )

    work_push(keepem);
    work_push(dropem);
    work_push(hitem);

    // IGNORE: no-op
    // UPDATE: update columns, only ones w/ provided values
    //REPLACE: update columns, use column-nulls for missing values
    //   FAIL: throw ERROR()
    // INSERT: keepem = cow_add(keepem, ki(rhs_row_index));
  }


  bool using_update = dropem && (COUNT(dropem) > 0);

  //INSERT
  ENUM(kN(table,KEYS), 
    K key = v;
    K value = NULL;

    if(columns)
    {
      //POTENTIAL_OPTIMIZATION_POINT
      //for tables with LOTS of columns we can do a hash iteration instead

      bool case_sensitive = true;
      case_sensitive = case_sensitive;

      LIST2(columns, values,
        if(matchC(key, u, false))
        {
          //POTENTIAL_OPTIMIZATION_POINT
          //get rid of allocation here and below
          value = strong(v);
          break;
        }
      )
    }
    else
    {
      K0 o;
      value = strong(AT2(values, ki(i),o));
    }
  
    if(!value)
    {
      bool continues_if_value_absent = true;

      if(continues_if_value_absent && using_update)
      {
        continue;
      }

      //POTENTIAL_OPTIMIZATION_POINT
      //lots to simplify here. can switch to K0 based-defaults, in-place, etc.
      //can get rid of `value` allocation and `rd(value)` at end
      value = table_column_default_value(table, key);
    }

    value = cow_coerce_column(value);
    //we depend on table columns being arrays only
    //otherwise you get something more complicated here than this simple nestneu

    if(using_update)
    {
      K replace = at(value, dropem);
      work_push(replace);

      K4 o;
      K diffed = change(kN(table_values, i), klist1(hitem, o), NULL, replace);

      nestset_ri_rd(table_values, i, diffed, false, true);

      work_pop_rd(true);
      
      //Continue on to INSERT reduced set...
      K fewer = at(value, keepem);

      rd(value);
      value = fewer;
    }


    if(COUNT(value) < insert_count && !using_update) //handle single entries conformable with lists
    {
      DO(insert_count, nestneu(table_values, i, cow_join(kN(table_values, i), value)))
    }
    else //regular
    {
      nestneu(table_values, i, cow_join(kN(table_values, i), value)); 
    }

    rd(value);
  )


  if(conflict_keys)
  {
    work_pop_n_rd(5, true);
  }

done:
  return table;
}


#pragma mark - Translate SQL to Bytecode

void translate_sql(K tree, K *bytecode, K *debug, K function, K parent)
{ 
  K0 o1,o2,o3,o4,o5;
  K separations = AT2(tree, ki(TOKEN_PAYLOAD),o1);

  K first_sep = AT2(separations, ki(0),o2);
  K first_sep_payload = AT2(first_sep, ki(TOKEN_PAYLOAD),o3);

  K sql_start = AT2(first_sep_payload, ki(0),o4);
  K start_word = AT2(sql_start, ki(TOKEN_PAYLOAD),o5);

  bool sensitivity = PARSE_RESERVED_CASE_INSENSITIVE;

  if(charvec_case_matchC(start_word, kcv("insert"), sensitivity))
  {
    translate_sql_insert(tree, bytecode, debug, function, parent);
    return;
  }
  else if(charvec_case_matchC(start_word, kcv("select"), sensitivity))
  {
    translate_sql_select(tree, bytecode, debug, function, parent);
    return;
  }
  else if(charvec_case_matchC(start_word, kcv("update"), sensitivity))
  {
    translate_sql_update(tree, bytecode, debug, function, parent);
    return;
  }
  else if(charvec_case_matchC(start_word, kcv("delete"), sensitivity))
  {
    translate_sql_delete(tree, bytecode, debug, function, parent);
    return;
  }

  return;
}

void translate_sql_insert(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[23] = {0};

  I insert_snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;

  K table_tree    = NULL;
  K columns_tree  = NULL;
  K values_tree   = NULL;
  K conflict_tree = NULL;

  K separations = AT2(tree, ki(TOKEN_PAYLOAD),o[1]);

  if(COUNT(separations) != 2 && COUNT(separations) != 3)
  {
    //TODO ERROR you didn't supply VALUES or supplied too many sql middles
    //           or did VALUES (123), (456)
  }

  K first_sep = AT2(separations, ki(0),o[2]);
  K first_sep_payload = AT2(first_sep, ki(TOKEN_PAYLOAD),o[3]);

  K x = first_sep_payload;

  if(cx < 3 || 4 < cx) 
  {
    //TODO error: INSERT t || INSERT INTO T (COLS) (JUNK THING)
  }

  //TODO verify x[1] is "INTO" or error (see "insert" in parent)

  table_tree = AT2(x, ki(2),o[4]);

  if(4 <= cx)
  {
    columns_tree = AT2(x, ki(3),o[5]);
  }

  K second_sep = AT2(separations, ki(1), o[6]);
  K second_sep_payload = AT2(second_sep, ki(TOKEN_PAYLOAD),o[7]);

  K y = second_sep_payload;

  if(cy != 2)
  {
    //INSERT INTO t VALUES || INSERT INTO t VALUES ()()
    ERROR(ERROR_PARSE_SQL_VALUES);
  }

  //TODO verify y[1] is "VALUES" or error

  values_tree = AT2(y, ki(1),o[8]);

  I values_tree_kind = AT2(values_tree, ki(TOKEN_KIND),o[9])->i;

  I table_tree_kind     = AT2(table_tree, ki(TOKEN_KIND),o[10])->i;
  I table_snippet_start = AT2(table_tree, ki(TOKEN_SNIPPET_START),o[11])->i;
  I table_snippet_end   = AT2(table_tree, ki(TOKEN_SNIPPET_END),o[12])->i;

  if(3 <= COUNT(separations)) //...ON CONFLICT (a,b) UPDATE
  {
    K third_sep = AT2(separations, ki(2),o[13]);
    K third_sep_payload = AT2(third_sep, ki(TOKEN_PAYLOAD),o[14]);

    K w = third_sep_payload;

    if(4 != COUNT(w))
    {
      ERROR(ERROR_PARSE_SQL_VALUES);
    }

    K on       = AT2(w,ki(0),o[15]); //ON
    K conflict = AT2(w,ki(1),o[16]); //CONFLICT
    K cols     = AT2(w,ki(2),o[17]); //(a,b)
    K action   = AT2(w,ki(3),o[18]); //UPDATE {UPDATE, IGNORE, REPLACE, ...}

    //Right now we're just passing `(a,b)` and hardcoding everything
    //as ... ON CONFLICT (a,b) UPDATE
    //but we could accept {UPDATE, IGNORE, REPLACE, ...}
    //Probably by pushing another item on the stack
    //If we, say, mark keys some other way, we could also make them optional

    conflict_tree = cols;
  }

  if(conflict_tree)
  {
    add_code_byte(bytecode, debug, EMPTY_PUSH, table_snippet_end);

    I conflict_tree_kind = AT2(conflict_tree, ki(TOKEN_KIND), o[19])->i;
    I conflict_snippet_start = AT2(conflict_tree, ki(TOKEN_SNIPPET_START),o[20])->i;

    SW(conflict_tree_kind)
    {
     CSF(TOKEN_GROUP_SQUARE_BRACKET,);   //noop, allowed
      CS(TOKEN_GROUP_ROUND_PAREN, );     //noop, allowed
      CD: ERROR(ERROR_PARSE_SQL_VALUES); //error, unsupported column type
          break;
    }

    translate_sql_columns(conflict_tree, bytecode, debug, function, parent);
  }
  else
  {
    add_code_byte(bytecode, debug, EMPTY_PUSH, table_snippet_end);
  }

  SW(values_tree_kind)
  {
#if SQL_INSERT_CLASSIC_PARENS 
    CS(TOKEN_GROUP_ROUND_PAREN, translate_sql_paren(values_tree, bytecode, debug, function, parent)) //special: list of enlist each
#endif
    CD: //really we want to accept tables, names, etc. but we can make this error if we want, too
   CSF(TOKEN_GROUP_CURLY_BRACE,) 
    CS(TOKEN_GROUP_SQUARE_BRACKET, translate(values_tree, bytecode, debug, function, parent, 0, 0))
  }

  if(columns_tree)
  {
    I columns_tree_kind = AT2(columns_tree, ki(TOKEN_KIND), o[21])->i;
    I columns_snippet_start = AT2(columns_tree, ki(TOKEN_SNIPPET_START), o[22])->i;

    SW(columns_tree_kind)
    {
     CSF(TOKEN_GROUP_SQUARE_BRACKET,);    //noop, allowed
      CS(TOKEN_GROUP_ROUND_PAREN, );      //noop, allowed
      CD: ERROR(ERROR_PARSE_SQL_VALUES);  //error, unsupported column type
          break;
    }

    translate_sql_columns(columns_tree, bytecode, debug, function, parent);
  }
  else
  {
    add_code_byte(bytecode, debug, EMPTY_PUSH, table_snippet_end);
  }

  //POTENTIAL_OPTIMIZATION_POINT
  //(see also similar areas in SQL DELETE, maybe)
  //we did 2, which is the fastest to develop but by no means the "best"
  //
  //mult directions here:
  //1. can develop multiple sql insert calls: as many as alter, plus non-alter memory version
  //   this sucks to build. but is probably the fastest. also most general.
  //   we'll want to do this at some point
  //2. can do a special cow_insert that's like cow_change. this is much simpler
  //3. can update cow_join to handle TABLE x TABLE or maybe TABLE x LIST, then
  //   we can piggyback on alter[mytable;();join:; fake_table]
  //   if column_tree is present we use a KEYING verb: KEY[columns; values]
  //   for name we use translate_string
  //
  //2 is shortest but uh pretty hacky / replicaty
  //3 is nicer, unlocks the joins for TABLE, but may be rabbit holey
  //  eg we're doing optimized cow_join, and for BTREE, HASHED
  //
  //a downside of {2, 3} is that you can't insert into local tables, only global
  //{1} does not have this downside
  //
  //Note: can do this to combine columns and values into table
  //  add_code_byte_op_compression(bytecode, debug, VERB_CALL, columns_snippet_start, VERB_ENTABLE_ID);

  SW(table_tree_kind)
  {
    CS(TOKENS_NAME, translate_string(table_tree, bytecode, debug, function, parent))
    CD: 
    add_code_byte(bytecode, debug, ASIDE_OPEN, table_snippet_start);
    translate(table_tree, bytecode, debug, function, parent, 0, 0);
    add_code_byte(bytecode, debug, ASIDE_CLOSE, table_snippet_start);
    break;
  }

  add_code_byte(bytecode, debug, SQL_INSERT2, insert_snippet_start);
}

void translate_sql_columns(K tree, K *bytecode, K *debug, K function, K parent)
{ 
  K0 o[8] = {0};
  //List of charvecs 
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);

  add_code_byte(bytecode, debug, LIST_OPEN, snippet_start);

  I location; 

  ENUM(payload, 
                K sep_payload = AT2(v, ki(TOKEN_PAYLOAD),o[3]);

                if(COUNT(sep_payload) != 1)
                {
                  continue;
                  //TODO error
                }

                K value = AT2(sep_payload, ki(0),o[4]);

                I value_kind  = AT2(value, ki(TOKEN_KIND),o[5])->i;
                I value_start = AT2(value, ki(TOKEN_SNIPPET_START),o[6])->i;
                I value_end   = AT2(value, ki(TOKEN_SNIPPET_END),o[7])->i;

                SW(value_kind)
                {
                 CSF(TOKENS_STRING,)//OK
                  CS(TOKENS_NAME,) //OK
                  CD: break;//TODO: error
                }

                add_code_byte(bytecode, debug, ASIDE_OPEN, value_start);
                translate_string(value, bytecode, debug, function, parent);
                add_code_byte(bytecode, debug, ASIDE_CLOSE, value_end - 1);
                add_list_separator_code(bytecode, debug, value_end - 1);
  )

  add_code_byte(bytecode, debug, LIST_CLOSE, snippet_end - 1);
}

void translate_sql_paren(K tree, K *bytecode, K *debug, K function, K parent)
{ 
  K0 o[5] = {0};
  //List of lists: As a list, adds a special enlist to each item
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);

  add_code_byte(bytecode, debug, LIST_OPEN, snippet_start);


  ENUM(payload, 
                I v_start = AT2(v, ki(TOKEN_SNIPPET_START),o[3])->i;
                I v_end   = AT2(v, ki(TOKEN_SNIPPET_END),o[4])->i;
                add_code_byte(bytecode, debug, LIST_OPEN, v_start);
                translate(v, bytecode, debug, function, parent, 0, 0);
                add_list_separator_code(bytecode, debug, v_end - 1);
                add_code_byte(bytecode, debug, LIST_CLOSE, v_end - 1);
                add_list_separator_code(bytecode, debug, v_end - 1);
  )

  add_code_byte(bytecode, debug, LIST_CLOSE, snippet_end - 1);
}

void translate_sql_select(K tree, K *bytecode, K *debug, K function, K parent)
{
  //I made a decision to postpone some heavier lifting to here instead of
  //solving everything perfectly in the parse pass because the parse pass was
  //fragile enough without trying to shoehorn intelligent sql separation into
  //the grouping recursion

  K0 o[15] = {0};
  I select_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;

  K parse_map = sql_parse_map_from_tree(tree);
  work_push(parse_map);

  //x symbolic translation
  //x from
  //x where
  //x group by
  //x select cols
  //  order by
  //  limit offset

  K from_list = AT2(parse_map, kcv("from"),o[1]);

  if(1 != COUNT(from_list)){ERROR(ERROR_TABLE);}

  K table_tree = AT2(from_list, ki(0),o[2]);

  I table_snippet_start = AT2(table_tree, ki(TOKEN_SNIPPET_START),o[3])->i;
  I table_snippet_end   = AT2(table_tree, ki(TOKEN_SNIPPET_END),o[4])->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  //////////////////////////////////////////////////////////////////
  //PUSH SELECT/COLUMN/AS CLAUSES - "SELECT"
  //
  //
  K0 o0;
  K selected = LOOKUP_(parse_map, kcv("select"), o0);

  if(selected)
  {
    I n = COUNT(selected);
    K select_clauses = new_k(LIST, n);
      select_clauses->n = 0;
    K as_clauses     = new_k(LIST, n);
      as_clauses->n  = 0;

    K args = enlist(ki(0));

    work_push(select_clauses);
    work_push(as_clauses);
    work_push(args);

    C attributes = (FUNC_ATTR_SQL_CLAUSE);
    I select_start = SENTINEL;

    ENUM(selected,
      I start = AT2(v, ki(TOKEN_SNIPPET_START),o[5])->i;
      I end   = AT2(v, ki(TOKEN_SNIPPET_END),o[6])->i;
      K payload = AT2(v, ki(TOKEN_PAYLOAD),o[7]);

      if(SENTINEL == select_start)
      {
        select_start = start;
      }

      //Handle `AS`
      bool has_as = false;
      K pen = NULL;
      K ultimate = NULL;
      I pen_start = -1;
      K pen_name = NULL;
      K ultimate_name = NULL;
      
      I cp = COUNT(payload);

      if(2 < cp)
      {
        pen = AT2(payload, ki(cp-2),o[8]);
        ultimate = AT2(payload, ki(cp-1),o[9]); 

        I pen_type = AT2(pen, ki(TOKEN_KIND),o[10])->i; 
        I ultimate_type = AT2(ultimate, ki(TOKEN_KIND),o[11])->i; 

        pen_start = AT2(pen, ki(TOKEN_SNIPPET_START),o[12])->i; 

        if(TOKENS_NAME == pen_type && TOKENS_NAME == ultimate_type)
        {
          pen_name =  AT2(pen, ki(TOKEN_PAYLOAD),o[13]);
          ultimate_name =  AT2(ultimate, ki(TOKEN_PAYLOAD),o[14]);
          if(charvec_case_matchC(pen_name, kcv("as"), true))
          {
            has_as = true;
          }
        }

      }

      if(has_as)
      {
        end = pen_start;//shorten substring
        as_clauses = cow_add(as_clauses, ultimate_name);
      }
      else //no AS clause present
      {
        as_clauses = cow_add(as_clauses, kn);
      }

      //trim, so we can skip over empty clauses
      while(start < end && isspace(kC(text)[start])) start++;
      while(start < end && isspace(kC(text)[end-1])) end--;

      if(end - start <= 0)
      {
        continue;
      }

      K substring = *work_push(new_subarray(text, start, end));

      K clause    = *work_push(compile_with_parent_args(substring, directory, function, args, FUNC_KIND_LAMBDA, attributes));

      select_clauses = cow_add(select_clauses, clause);

      work_pop_n_rd(2, true);
    )

    translate_constant(bytecode, debug, function, as_clauses, select_start);

    //Before we were just pushing the list (select_clauses) for the SQL instruction to see,
    //and that's wrong. (It doesn't handle closures so locals are missing)
    //There are a few ways to fix it. First what we're doing now:
    //1. Push the functions onto the stack (via LIST_CLOSE) as constants to trigger new_func_closed_from_stack() in CONSTANT_PUSH
    //2. Change the SQL emu instructions to revise passed functions using new_func_closed_from_stack() (kludgey)
    //3. Change the actual SQL methods called (placeholder_sql_select / table_select) to close the functions before executing (messy)
    //4. Change the CONSTANT_PUSH instructions to recursively hunt through MIXED_ARRAY, MAP, TABLE, etc. (any nested) constants
    //   for instances of FUNCTIONs and replace each by the relevant closure
    //   Depending on how we built the bytecode it may actually be necessary to do this at some point instead of #1.
    //   For instances, if pushing MAPs of FUNCTIONs does not already work correctly.
    //   grep keywords: CONSTANT_PUSH_RECURSIVE_NOTE
    //
    //See similar blocks elswhere in the SQL functions.

    //if we're doing it this way we could also skip this intermediate list...
    add_code_byte(bytecode, debug, LIST_OPEN, select_start);
    ENUM(select_clauses,  translate_constant(bytecode, debug, function, v, select_start); 
                          add_list_separator_code(bytecode, debug, select_start);
    )
    add_code_byte(bytecode, debug, LIST_CLOSE, select_start);

    work_pop_n_rd(3, true);
  }

  //////////////////////////////////////////////////////////////////
  //PUSH GROUP BY CLAUSES - "GROUP BY"
  translate_sql_groupbys(parse_map, bytecode, debug, function, parent);
  //////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////
  //PUSH WHERE CLAUSES - "WHERE"
  translate_sql_wheres(parse_map, bytecode, debug, function, parent);
  //////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////
  //PUSH TABLES - "FROM"
  add_code_byte(bytecode, debug, ASIDE_OPEN,  table_snippet_start);
  translate(table_tree, bytecode, debug, function, parent, 0, 0);
  add_code_byte(bytecode, debug, ASIDE_CLOSE, table_snippet_end - 1);
  //////////////////////////////////////////////////////////////////

  add_code_byte(bytecode, debug, SQL_SELECT2, select_start);

  work_pop();
  rd(parse_map);
}

K sql_parse_map_from_tree(K tree)
{
  K0 o[10] = {0};
  bool prepopulate = true;

  K separations = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);

  K parse_map = new_map();
  K parse_map_key = ex("'select'");

  if(prepopulate) //fill parse map with empty lists for potential keys
  {
    S *s = NULL;
    for(s=RESERVED_SQL_STARTS;    *s; s++) update(parse_map, kcv(*s), kk);
    for(s=RESERVED_SQL_MIDDLES;   *s; s++) update(parse_map, kcv(*s), kk);
  }

  ENUM(separations, 
  
    K separation = v;
    K payload = AT2(separation, ki(TOKEN_PAYLOAD),o[1]);
    I payload_n = COUNT(payload);

    I skip = 0;
    I skip_to_snippet_start = SENTINEL; //hacky. drop words like "group by" from substring

    K first = NULL;
    K second = NULL;

    if(0 < payload_n)
    {
      first = AT2(payload, ki(0),o[2]);
    }
 
    if(1 < payload_n)
    {
      second = AT2(payload, ki(1),o[3]);
    }
     
    if(first)
    {
      K first_word = AT2(first, ki(TOKEN_PAYLOAD),o[4]);
      I first_kind = AT2(first, ki(TOKEN_KIND),o[5])->i;

      SW(first_kind)
      {
       CSF(TOKENS_SQL_START, )
        CS(TOKENS_SQL_MIDDLE,
          rd(parse_map_key);
          parse_map_key = ex("tolower $1", first_word);
          skip += 1;

          skip_to_snippet_start = AT2(first, ki(TOKEN_SNIPPET_END),o[6])->i;

          bool check_for_by = false;

          if(charvec_case_matchC(first_word, kcv("group"), true))
          {
            check_for_by = true;
          }
          else if(charvec_case_matchC(first_word, kcv("order"), true))
          {
            check_for_by = true;
          }

          if(second && check_for_by)
          {
            K second_word = AT2(second, ki(TOKEN_PAYLOAD),o[7]);
            I second_kind = AT2(second, ki(TOKEN_KIND),o[8])->i;

            if(TOKENS_NAME == second_kind)
            {
              if(charvec_case_matchC(second_word, kcv("by"), true))
              {
                skip += 1;
                skip_to_snippet_start = AT2(second, ki(TOKEN_SNIPPET_END),o[9])->i;
              }

            }
          }
        )
      }
    }
    else //empty separation
    {

    }

    K x = copy(v);
    update_ri_rd(x, ki(TOKEN_PAYLOAD), drop(ki(skip), payload), false, true);

    if(SENTINEL != skip_to_snippet_start)
    {
      update_ri_rd(x, ki(TOKEN_SNIPPET_START), ki(skip_to_snippet_start), false, false);
    }

    K0 o1;
    K list = LOOKUP_(parse_map, parse_map_key, o1);

    if(!list) list = new_k(LIST, 0);

    list = cow_add(list, x);

    rd(x);

    update_ri_rd(parse_map, parse_map_key, list, false, false);
  )

  rd(parse_map_key);

  return parse_map;
}

void translate_sql_wheres(K parse_map, K *bytecode, K *debug, K function, K parent)
{
  //translate as one argument functions
  //so pass single arg to assemble
  //also, first we need to modify the NAME translator to check for 
  //GET_ALT_ATTR(f, FUNC_ATTR_SQL_CLAUSE);
  //and if so compile the funny thing in

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  K0 o[3] = {0};
  K wheres = LOOKUP_(parse_map, kcv("where"), o[0]);

  if(!wheres) return;

  K list = new_k(LIST, COUNT(wheres));
  list->n = 0;

  K args = enlist(ki(0));

  work_push(list);
  work_push(args);

  //POTENTIAL_OPTIMIZATION_POINT
  //We're re-parsing (making trees) where we don't need to here
  //(see also same thing in the "group by" section)
  //Instead we can do tree rewriting, the snippet starts and ends, basically.
  //There's a better/repeat discussion on this somewhere else 

  C attributes = (C)(FUNC_ATTR_SQL_CLAUSE | FUNC_ATTR_SQL_WHERE);

  I where_start = SENTINEL;

  ENUM(wheres,
    I start = AT2(v, ki(TOKEN_SNIPPET_START),o[1])->i;
    I end   = AT2(v, ki(TOKEN_SNIPPET_END),o[2])->i;

    if(SENTINEL == where_start)
    {
      where_start = start;
    }

    K substring = new_subarray(text, start, end);
    work_push(substring);
    K clause    = compile_with_parent_args(substring, directory, function, args, FUNC_KIND_LAMBDA, attributes);
    work_push(clause);

    list = cow_add(list, clause);

    work_pop_n_rd(2, true);
  )

  where_start = MAX(0, where_start);

  //if we're doing it this way we could also skip this intermediate list...
  add_code_byte(bytecode, debug, LIST_OPEN, where_start);
  ENUM(list,  translate_constant(bytecode, debug, function, v, where_start); 
                        add_list_separator_code(bytecode, debug, where_start);
  )
  add_code_byte(bytecode, debug, LIST_CLOSE, where_start);

  work_pop_n_rd(2, true);

}

void translate_sql_groupbys(K parse_map, K *bytecode, K *debug, K function, K parent)
{
  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  K0 o[3] = {0};
  K groupings = LOOKUP_(parse_map, kcv("group"), o[0]);

  if(!groupings) return;

  K list = new_k(LIST, COUNT(groupings));
  list->n = 0;

  K args = enlist(ki(0));

  work_push(list);
  work_push(args);

  C attributes = (FUNC_ATTR_SQL_CLAUSE);

  I group_start = SENTINEL;

  ENUM(groupings,
    I start = AT2(v, ki(TOKEN_SNIPPET_START),o[1])->i;
    I end   = AT2(v, ki(TOKEN_SNIPPET_END),o[2])->i;
    if(SENTINEL == group_start)
    {
      group_start = start;
    }

    K substring = *work_push(new_subarray(text, start, end));
    K clause    = *work_push(compile_with_parent_args(substring, directory, function, args, FUNC_KIND_LAMBDA, attributes));

    list = cow_add(list, clause);

    work_pop_n_rd(2, true);
  )

  group_start = MAX(0, group_start);

  //if we're doing it this way we could also skip this intermediate list...
  add_code_byte(bytecode, debug, LIST_OPEN, group_start);
  ENUM(list,  translate_constant(bytecode, debug, function, v, group_start); 
                        add_list_separator_code(bytecode, debug, group_start);
  )
  add_code_byte(bytecode, debug, LIST_CLOSE, group_start);
  //translate_constant(bytecode, debug, function, list, group_start);

  work_pop_n_rd(2, true);
}

void translate_sql_delete(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[9] = {0};
  //delete from t where a < 3 //regular
  //delete from t             //No "WHERE" clause: delete all rows
  //--------------------------------------------------------
  //delete c from t           //delete column
  //delete * from t           //?

  I delete_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;

  K parse_map = sql_parse_map_from_tree(tree);
  work_push(parse_map);

  //x symbolic translation
  //x cols
  //x from
  //x where
  //  order by
  //  limit offset

  K from_list = AT2(parse_map, kcv("from"),o[1]);

  if(1 != COUNT(from_list)){ERROR(ERROR_TABLE);}

  K table_tree = AT2(from_list, ki(0),o[2]);
  I table_snippet_start = AT2(table_tree, ki(TOKEN_SNIPPET_START),o[3])->i;
  I table_snippet_end   = AT2(table_tree, ki(TOKEN_SNIPPET_END),o[4])->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  //////////////////////////////////////////////////////////////////
  //PUSH DELETE/COLUMN CLAUSES - "DELETE"
  //

  K selected = LOOKUP_(parse_map, kcv("delete"), o[5]);

  if(selected)
  {
    //potential_feature_point
    //We'll keep this as a stub in case we want to later parse
    //delete a,b from {{a,b,c}}
    //to delete columns
    //see cow_table_drop_column for more notes

    I n = COUNT(selected);
    K select_clauses = new_k(LIST, n);
      select_clauses->n = 0;

    work_push(select_clauses);

    translate_constant(bytecode, debug, function, select_clauses, delete_start);

    work_pop_n_rd(1, true);
  }
  //////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////
  //PUSH WHERE CLAUSES - "WHERE"
  translate_sql_wheres(parse_map, bytecode, debug, function, parent);
  //////////////////////////////////////////////////////////////////
  
  //////////////////////////////////////////////////////////////////
  //PUSH TABLE

  K stringy = NULL;
  K tree_payload = AT2(table_tree, ki(TOKEN_PAYLOAD),o[6]);

  if(tree_payload && COUNT(tree_payload)==1)
  {
    K first = AT2(tree_payload, ki(0),o[7]);
    I first_kind  = AT2(first, ki(TOKEN_KIND),o[8])->i;

    if(TOKENS_NAME == first_kind) stringy = first;
    else stringy = NULL;
  }
  
  //POTENTIAL_OPTIMIZATION_POINT
  //see notes at INSERT's version of this

  if(stringy)//push string, will modify item in place
  {
    translate_string(stringy, bytecode, debug, function, parent);
  }
  else //will modify object, likely copying first
  {
    add_code_byte(bytecode, debug, ASIDE_OPEN, table_snippet_start);
    translate(table_tree, bytecode, debug, function, parent, 0, 0);
    add_code_byte(bytecode, debug, ASIDE_CLOSE, table_snippet_end - 1);
  }
  //////////////////////////////////////////////////////////////////

  add_code_byte(bytecode, debug, SQL_DELETE2, delete_start);

  work_pop();
  rd(parse_map);
}

void translate_sql_update(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[23] = {0};
  I update_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;

  K parse_map = sql_parse_map_from_tree(tree);
  work_push(parse_map);

  //x update (from)
  //x set
  //x select cols
  //x where
  //x group by
  //  order by
  //  limit offset

  K update_list = AT2(parse_map, kcv("update"),o[1]);

  if(1 != COUNT(update_list)){ERROR(ERROR_TABLE);}

  K table_tree = AT2(update_list, ki(0),o[2]);

  I table_tree_kind     = AT2(table_tree, ki(TOKEN_KIND),o[3])->i;
  I table_snippet_start = AT2(table_tree, ki(TOKEN_SNIPPET_START),o[4])->i;
  I table_snippet_end   = AT2(table_tree, ki(TOKEN_SNIPPET_END),o[5])->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  //////////////////////////////////////////////////////////////////
  //PUSH SET/AS CLAUSES - "SET"
  //
  //

  K setting = LOOKUP_(parse_map, kcv("set"), o[6]);

  if(setting)
  {
    I n = COUNT(setting);
    K setting_clauses = new_k(LIST, n);
      setting_clauses->n = 0;
    K as_clauses     =  new_k(LIST, n);
      as_clauses->n  = 0;

    K args = enlist(ki(0));

    work_push(setting_clauses);
    work_push(as_clauses);
    work_push(args);

    C attributes = (FUNC_ATTR_SQL_CLAUSE);
    I update_start = SENTINEL;

    //Generate the "as" ('col1=') clauses and the "settings" (col2+3) clauses
    ENUM(setting,
      I start = AT2(v, ki(TOKEN_SNIPPET_START),o[7])->i;
      I end   = AT2(v, ki(TOKEN_SNIPPET_END),o[8])->i;
      K payload = AT2(v, ki(TOKEN_PAYLOAD),o[9]);

      if(SENTINEL == update_start)
      {
        update_start = start;
      }

      //Handle `AS`
      bool has_as = false;

      K pre = NULL;
      I pre_start = -1;
      I pre_end   = -1;
      K pre_name = NULL;
      K first_payload  = NULL;

      I second_end = -1;
      
      I cp = COUNT(payload);

      //find 'a='
      if(cp >= 1)
      {
        K0 o0;
        pre = LOOK_(payload, 0, o0);

        I pre_type = AT2(pre, ki(TOKEN_KIND),o[10])->i; 
        pre_start = AT2(pre, ki(TOKEN_SNIPPET_START),o[11])->i; 
        pre_end   = AT2(pre, ki(TOKEN_SNIPPET_END),o[12])->i; 

        if(TOKEN_GROUP_VERBAL_NVA == pre_type || TOKEN_GROUP_ASSIGNMENT == pre_type)
        {
          K items  = AT2(pre, ki(TOKEN_PAYLOAD),o[13]);
          if(2==lenI(items))
          {
            K first  = AT2(items, ki(0),o[14]);
            K second = AT2(items, ki(1),o[15]); 

            I first_kind = AT2(first, ki(TOKEN_KIND),o[16])->i; 

            second_end =  AT2(second, ki(TOKEN_SNIPPET_END),o[17])->i;

            first_payload  = AT2(first, ki(TOKEN_PAYLOAD),o[18]);
            K second_payload = AT2(second, ki(TOKEN_PAYLOAD),o[19]);

            bool has_title    = (TOKENS_NAME == first_kind) || (TOKENS_STRING == first_kind);
            bool has_equal    = charvec_case_matchC(second_payload, kcv("="), true);
            bool has_colon    = charvec_case_matchC(second_payload, kcv(":"), true);
            bool has_assigner = has_equal || has_colon;

            if(has_title && has_assigner)
            {
              has_as = true;
            }
          }
        }
      }

      if(has_as)
      {
        start = second_end; //move beginning of executable/select clause to past the assignment part
        assert(second_end==pre_end);
      }

      //trim, so we can skip over empty clauses
      while(start < end && isspace(kC(text)[start])) start++;
      while(start < end && isspace(kC(text)[end-1])) end--;

      if(!has_as && end - start <= 0)
      {
        assert(!has_as); //we'll want empty assignments to use nulls...? i guess
        continue;
      }

      if(has_as)
      {
        K string = parse_string(first_payload);
        as_clauses = cow_add(as_clauses, string);
        rd(string);
      }
      else //no "a=" clause present so no name to give
      {
        as_clauses = cow_add(as_clauses, kn);
      }

      K substring = *work_push(new_subarray(text, start, end));

      K clause    = *work_push(compile_with_parent_args(substring, directory, function, args, FUNC_KIND_LAMBDA, attributes));

      setting_clauses = cow_add(setting_clauses, clause);

      work_pop_n_rd(2, true);
    )

    translate_constant(bytecode, debug, function, as_clauses, update_start);

    //if we're doing it this way we could also skip this intermediate list...
    add_code_byte(bytecode, debug, LIST_OPEN, update_start);
    ENUM(setting_clauses,  translate_constant(bytecode, debug, function, v, update_start); 
                          add_list_separator_code(bytecode, debug, update_start);
    )
    add_code_byte(bytecode, debug, LIST_CLOSE, update_start);

    work_pop_n_rd(3, true);
  }

  //////////////////////////////////////////////////////////////////
  //PUSH GROUP BY CLAUSES - "GROUP BY"
  translate_sql_groupbys(parse_map, bytecode, debug, function, parent);
  //////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////
  //PUSH WHERE CLAUSES - "WHERE"
  translate_sql_wheres(parse_map, bytecode, debug, function, parent);
  //////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////
  //PUSH TABLES - "FROM"

  K stringy = NULL;
  K tree_payload = AT2(table_tree, ki(TOKEN_PAYLOAD),o[20]);

  if(tree_payload && COUNT(tree_payload)==1)
  {
    K first = AT2(tree_payload, ki(0),o[21]);
    I first_kind  = AT2(first, ki(TOKEN_KIND),o[22])->i;

    if(TOKENS_NAME == first_kind) stringy = first;
    else stringy = NULL;
  }
  
  //POTENTIAL_OPTIMIZATION_POINT
  //see notes at INSERT's version of this

  if(stringy)//push string, will modify item in place
  {
    translate_string(stringy, bytecode, debug, function, parent);
  }
  else //will modify object, likely copying first
  {
    add_code_byte(bytecode, debug, ASIDE_OPEN, table_snippet_start);
    translate(table_tree, bytecode, debug, function, parent, 0, 0);
    add_code_byte(bytecode, debug, ASIDE_CLOSE, table_snippet_end - 1);
  }

  //////////////////////////////////////////////////////////////////

  add_code_byte(bytecode, debug, SQL_UPDATE2, update_start);

  work_pop();
  rd(parse_map);
}


