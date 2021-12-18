#include "kerf.h"

K new_from_json(K x)
{
  K z = interpret(x);//JSON is natively supported

  return z;
}

K new_json_from(K x)
{
  K z = new_k(CHARVEC, 0);

  z = cow_json_append(z, x);

  z = cow_ensure_null_terminated_chars(z);
 
  return z;
}

bool strict_json_compatible_key(K x)
{
  SW(xt)
  {
   CSF(CHAR,)
    CR(CHARVEC, true)
  }

  return false;
}

escapebuf escapebuf_for_char(C c)
{
  escapebuf buf = {0};

  SW(c)
  {
    CSF('\\',)
    CSF('"',)
     CS('/', snprintf((S)&buf, sizeof(buf), "\\%c", c))
    CS('\b', snprintf((S)&buf, sizeof(buf), "\\b"))
    CS('\f', snprintf((S)&buf, sizeof(buf), "\\f"))
    CS('\n', snprintf((S)&buf, sizeof(buf), "\\n"))
    CS('\r', snprintf((S)&buf, sizeof(buf), "\\r"))
    CS('\t', snprintf((S)&buf, sizeof(buf), "\\t"))
    CD:
      if(!isprint(c))
      {
             snprintf((S)&buf, sizeof(buf), "\\u00%02x", (UC)c);
      }
      else
      {
             snprintf((S)&buf, sizeof(buf), "%c", c);
      }
  }

  return buf;
}

K cow_json_append(K string, K object)
{
  K z = string;
  K x = object;

  bool pretty = false;

  char buf[256] = {0};

  SW(xt)
  {
   CSF(ZIP,)
   CSF(STAMPVEC, )
   CSF(FLOATVEC,)
   CSF(INTVEC, )
   CSF(BTREE,)
   CSF(HASH,)
    CS(LIST, 
      z = cow_add(z, kc('['));
      ENUM(x, if(i>0) z = cow_add(z, kc(','));
              z = cow_json_append(z, v))
      z = cow_add(z, kc(']'));
    )

   CSF(CHAR,) //Note: single CHAR lost in translation unless you do the same as with STAMP
    CS(CHARVEC, 
      z = cow_add(z, kc('"'));
      ENUM(x, escapebuf buf = escapebuf_for_char(vc); ENUM(kcv((S)&buf), z = cow_add(z, v)))
      z = cow_add(z, kc('"'));
    )

   CSF(TABLE,)
    CS(MAP,

      z = cow_add(z, kc('{'));
      ENUM(x, 
              if(i>0) z = cow_add(z, kc(','));

              if(PARSE_JSON_STRICT_KEYS)
              {
                if(!strict_json_compatible_key(u))
                {
                  work_push(z);
                  ERROR(ERROR_JSON_KEY);
                }
              }

              z = cow_json_append(z, u);
              z = cow_add(z, kc(':'));
              z = cow_json_append(z, v);
      )

      if(IS_TABLE(x))
      {
        if(table_columns(x) > 0) z = cow_add(z, kc(','));
        z = cow_json_append(z, kcv(PARSE_JSON_TABLE_KEY));
        z = cow_add(z, kc(':'));
        //attributes could go here
        z = cow_add(z, kc('{'));
        z = cow_add(z, kc('}'));
      }

      z = cow_add(z, kc('}'));
    )
    
    CS(NIL, ENUM(kcv("null"), z = cow_add(z, v)))
    CS(FUNC, K text = kN(x, FUNC_TEXT);
             if(text) z = cow_json_append(z, text);
             else z = cow_json_append(z, kcv("function"));
    )

    CS(FLOAT, if(isnan(xf))   snprintf(buf, sizeof(buf),  "NaN");
              else if(-FI==xf)snprintf(buf, sizeof(buf), "-Infinity");
              else if(FI==xf) snprintf(buf, sizeof(buf),  "Infinity");
              else            snprintf(buf, sizeof(buf), "%g", xf);

              I n = strlen(buf);
              DO(n, z = cow_add(z, kc(buf[i])))
    )

    CS(INT, snprintf(buf, sizeof(buf), "%lld", xi);  
            I n = strlen(buf);
            DO(n, z = cow_add(z, kc(buf[i])))
    )

    CS(STAMP, //as string example: (PARSE_JSON_STAMP_STRING_XTC "2015.01.01")

      //POTENTIAL_FEATURE_POINT
      //arguably there is an "accepted" non-JSON yes-JS timestamp method here 
      //http://stackoverflow.com/questions/10286204/the-right-json-date-format
      //http://stackoverflow.com/a/2316066/365478
      //http://stackoverflow.com/questions/206384/format-a-microsoft-json-date/2316066#2316066
      //one issue is nanosecond representation...

      if(PARSE_JSON_STAMP_STRING_HACK)
      {
        stampbuf buf = buf_from_stamp(x,false,10);
        S s = (S)&buf;
        I len = strlen(s);
        z = cow_add(z, kc('\"'));
        ENUM(kcv(PARSE_JSON_STAMP_STRING_XTC), z = cow_add(z, v))
        DO(len, z = cow_add(z, kc(s[i])))
        z = cow_add(z, kc('\"'));
      }
      else
      {
        z = cow_json_append(z, ki(xi));
      }
    )
  }

  return z;
}

int xml_go()
{
return 0;

//  K filename = kcv("stats19.csv");
//  C separator = ',';
//
//  K fields_k = kcv("IIIZ");
//  
//  I header_rows = 1;
//
//  bool header_titles = true;
//
//  K table = new_table();
//  table = cow_ build_table_from_lines(table, filename, separator, fields_k, header_rows, header_titles);
//
//  show(table);
//  rd(table);

  return 0;
}

I populated_fields(K x)
{
  I s = 0;
  DO(xn, if(!skipfield(xC[i])) s++)
  return s;
}

bool skipfield(C field)
{
  return (' '==field || '*'==field);
}

K new_empty_list_for_fixed_field(C field)
{
  SW(field) 
  {
     CS('r', return new_empty_list_for_delimited_field('e'))
     CS('R', return new_empty_list_for_delimited_field('E'))
  }

  return new_empty_list_for_delimited_field(field);
}

K new_empty_list_for_delimited_field(C field)
{
  K x = NULL;
  K y = NULL;

  SW(field) 
  {
     CS('9', x = new_zip_for_subtype(-STAMP   ))
     CS('V', x = new_zip_for_subtype(-FLOAT   ))
     CS('0', x = new_zip_for_subtype(-FLOAT4  ))
     CS('D', x = new_zip_for_subtype(-DEC4INT8))
     CS('4', x = new_zip_for_subtype(-INT4    ))
     CS('2', x = new_zip_for_subtype(-INT2    ))
     CS('1', x = new_zip_for_subtype(-INT1    ))
     CS('W', x = new_zip_for_subtype(-INT     ))
     CS('3', x = new_zip_for_subtype(-STR32   ))
     CS('e', y = new_intern(); x = new_btree_from_K(y); rd(y))
     CS('E', x = new_intern())
     CD:
         if(islower(field))
         {
           x = new_btree();
         }
         else
         {
           x = new_k(LIST, 0);
         }
  }

  return x;
}

ALWAYS_INLINE void sv_callback_column(S string, I length, SV_DATA *data)
{
  bool accounted_for = data->column < data->fields_count;
  C field = '\0';
  if(accounted_for)
  {
    field = data->fields[data->column];
  }

  I skipwise_index = data->column - data->fields_skipped;

  if(data->row < data->header_rows)        //processing header row
  {
    K y = charvec_from_cnstring(string, length);

    if(data->quoted) y = _sv_unescape_charvec(y);
    y = cow_ensure_null_terminated_chars(y);

    work_push(y);

    if(data->header_titles && 0==data->row)//capturing header titles
    {
      //I actual_rows = table_rows(data->table);
      //if(actual_rows > 0);//todo error?
      //fprintf(stderr, "col: '%s'\n", yC);

      bool unexisting = skipwise_index >= data->column_count;
      bool renamable  = skipwise_index <  data->column_count;
      bool skip = accounted_for && skipfield(field); 

      bool adding_column = unexisting && accounted_for && !skip;
      bool renaming_column = !adding_column && renamable && !skip;

      if(adding_column)
      {
        K original = alpha_uncollided_name(data->table, y);

        K vals = new_empty_list_for_delimited_field(field);
        
        data->table = cow_table_add_column(data->table, original, vals);

        data->column_count += 1;

        rd(vals);
        rd(original);
      } 
      else if(renaming_column)
      {
         bool collision = table_has_column(data->table, y);

         K old = kN(kN(data->table, KEYS), skipwise_index);

         if(!collision)
         {
           data->table = cow_table_rename_column(data->table, old, y);
         }

      }
      else
      {
        //skipped header field
        data->fields_skipped++;
      }
    }
    else //skipping non-title/non-first header row
    {
      //noop
    }

    work_pop_n_rd(1, true);
  }
  else if(data->column < data->fields_count) //processing regular row
  {
    bool is_skip = skipfield(field);

    if(skipwise_index >= data->column_count) //missed a header or an init
    {

      if(!is_skip)
      {
        K y = kcv("col");
        K original = alpha_uncollided_name(data->table, y);

        K vals = new_empty_list_for_delimited_field(field);
        
        data->table = cow_table_add_column(data->table, original, vals);
        data->column_count += 1;
        rd(vals);
        rd(original);
      }
    }

    if(is_skip)
    {
      data->fields_skipped++;
      goto finish;
    }

    //fprintf(stderr, "field(%2lld,%2lld): '%s'\n", data->row, data->column, yC);

    I column_index = skipwise_index;

    K column = kP(data->temp_column_pointers)[column_index]; 

    K x = column;

    //I max_count = max_possible_count_K(y);
    I max_count = POW2(xm) - sizeof(K0);//optimized for CHARVEC

    if(xn + length + 1 > max_count)
    {
      nestneu(data->temp_table, VALUES, cow(kN(data->temp_table,VALUES)));
      x = kP(data->temp_column_pointers)[column_index]; 

      x = expand(x, xm+1);
      kP(data->temp_column_pointers)[column_index] = x;//necessary for in-memory style, but not for on-disk
      nestset_ri_rd(kN(data->temp_table,VALUES), column_index, x, false, false);
    }

    I skipped = _sv_memcpy_skipping_escapes(xC + xn, string, length);
    //DO(length, xC[xn+i] = string[i]); //tested slightly faster than memcpy but not enough to matter
    xC[xn + length - skipped] = '\0'; //null delimit
    xn += length + 1 - skipped;
  }

finish:
  return;
}

ALWAYS_INLINE void sv_callback_row(SV_DATA *data)
{
  bool notify_on_extra_cols = true;
  if(notify_on_extra_cols)
  {
    if(0 == data->row)
    {
      if(data->column >= data->fields_count) 
      {
        I skipped = 1 + data->column - data->fields_count;
        fprintf(stderr, "Note: Skipping %lld extra column%s to the right of those explicitly accounted for in fields specifier.\n", skipped, 1==skipped?"":"s");
      }
    }
  }

  bool was_header = data->row < data->header_rows;
  bool had_some_fields = data->column > 0;
  bool had_too_few_fields = data->column < data->fields_count - 1;

  if(!was_header && had_some_fields && had_too_few_fields)
  {
    fprintf(stderr, "Data Error: Row had too few fields (or schema had too many columns).\n");
    ERROR(ERROR_ROW);
  }
}

ALWAYS_INLINE void sv_callback_column_pre_read(S string, I length, SV_DATA *data)
{

}

ALWAYS_INLINE void sv_callback_row_pre_read(SV_DATA *data)
{

}

K read_table_from_tsv(K tsv_filename, K fields_k, K header_rows)
{
  return read_table_from_delimited_file(kcv("\t"), tsv_filename, fields_k, header_rows);
}

K read_table_from_csv(K csv_filename, K fields_k, K header_rows)
{
  return read_table_from_delimited_file(kcv(","), csv_filename, fields_k, header_rows);
}

K create_table_from_csv(K table_filename, K csv_filename, K fields_k, K header_rows)
{
  return create_table_from_delimited_file(table_filename, kcv(","), csv_filename, fields_k, header_rows);
}

K create_table_from_tsv(K table_filename, K tsv_filename, K fields_k, K header_rows)
{
  return create_table_from_delimited_file(table_filename, kcv("\t"), tsv_filename, fields_k, header_rows);
}

K create_table_from_psv(K table_filename, K csv_filename, K fields_k, K header_rows)
{
  return create_table_from_delimited_file(table_filename, kcv("|"), csv_filename, fields_k, header_rows);
}

K append_table_from_csv(K table_filename, K csv_filename, K fields_k, K header_rows)
{
  return append_table_from_delimited_file(table_filename, kcv(","), csv_filename, fields_k, header_rows);
}

K append_table_from_tsv(K table_filename, K tsv_filename, K fields_k, K header_rows)
{
  return append_table_from_delimited_file(table_filename, kcv("\t"), tsv_filename, fields_k, header_rows);
}

K append_table_from_psv(K table_filename, K csv_filename, K fields_k, K header_rows)
{
  return append_table_from_delimited_file(table_filename, kcv("|"), csv_filename, fields_k, header_rows);
}

K create_table_from_delimited_file(K table_filename, K delimiter_k, K delimited_filename, K fields_k, K header_rows)
{
  if(!IS_STRING(table_filename)) ERROR(ERROR_STRING);
  recursive_delete_K(table_filename);
  return append_table_from_delimited_file(table_filename, delimiter_k, delimited_filename, fields_k, header_rows);
}

K append_table_from_delimited_file(K table_filename, K delimiter_k, K delimited_filename, K fields_k, K header_rows)
{
  //2005 standard: load & store at about 1G incoming per minute

  if(!IS_STRING(table_filename)) ERROR(ERROR_STRING);
  if(!IS_STRING(delimited_filename)) ERROR(ERROR_STRING);
  if(!IS_STRING(fields_k)) ERROR(ERROR_STRING);
  if(INT!=header_rows->t)  ERROR(ERROR_TYPE);
  if(!IS_STRING(delimiter_k) || COUNT(delimiter_k) != 1) ERROR(ERROR_STRING);

  C delimiter = kC(delimiter_k)[0];

  K table = open_table(table_filename, fields_k, FIELD_KIND_DELIMITED);

  bool header_titles = false;

  I hrows = header_rows->i;

  if(hrows > 0) header_titles = true;

  bool in_memory = false;

  table = cow_build_table_from_delimited_file(table, delimited_filename, delimiter, fields_k, hrows, header_titles, in_memory); 

  return table;
}

K read_table_from_delimited_file(K delimiter_k, K delimited_filename, K fields_k, K header_rows)
{
  K table = new_table();

  bool header_titles = false;

  if(INT!=header_rows->t)ERROR(ERROR_TYPE);
  if(!IS_STRING(delimiter_k) || COUNT(delimiter_k) != 1) ERROR(ERROR_STRING);
  C delimiter = kC(delimiter_k)[0];

  I hrows = header_rows->i;

  if(hrows > 0) header_titles = true;

  //this is a hack-ish thing we added so we could
  //have the CHARVEC slush columns be in memory instead of on disk (and so on...)
  //we should be able to switch it off if we need to
  bool in_memory = true;

  table = cow_build_table_from_delimited_file(table, delimited_filename, delimiter, fields_k, hrows, header_titles, in_memory); 

  return table;
}


//POTENTIAL_OPTIMIZATION_POINT
//We generated a lot of future possibilities for improving CSV (SV/delimited) reading:
//
// -1. does expanding width of DFA to read 2,3+ characters at a time instead of 1 help? I forget
//  0. Enable parallelization
//  1. chomp array of char* (instead of row at a time) - this avoids O(n) charvec stripe space but won't parallelize
//  2. in compressed charvec,you can iterate through strings w/ unmap (over a maphook)
//  3. punch hole for sparse file?
//  4. mapcores to parse chars to intermediate 1M row block
//  5. split out chunk sizes ->mk in ZIP (remove zip_chunk_size everywhere)
//  6. pre-count rows, pre-count chars-per-col (via DFA)
//  7. pre-count can also split into 1/n (bytewise) linebreaks on first run (for mapcores)
//  8. we can delete CSV as we read it
//  9. we can compress csv before we read it
// 10. doing 1/n tables allows n-1 merges or log(n) parallel merge rounds
// 11. if mmap-paging causes problems on bigger files, we may need to force unmap/msync
// 12. if we pre-count 1/n, the whole thing trivially parallelizes with mapcores (csv/cores) (mod headers)
// 13. we know rows no matter what by parsing time (under charvec-stripes method)
// 14. the log(n) parallel merge rounds thing is useful if you want to sort at the same time
// 15. fastest O(1) space model probably fan-in array of in-memory char buf queue per col,
//     CSV reader adds to them (bounded), col writers consume
// 16. temp file handles for columns would make it OK that we don't clear/release tmp table (also, avoids collisions if going in parallel)
// 17. (this also frees space as we go / keeps space use even/ok) ftruncate charvec after use could possibly prevent useless munmap flushing
// 18. zip algos need per-thread buffers. so either fix buffers or use fork method
// 19. when we map and unmap files we could ftruncate up and down (if centOS does not have sparse files?)
// 20. we can trivially split the CSV->charvec_fields producer piece into a function from csv_parse w/ variable-zC-modified + zn-modified 
// 21. if you reverse the csv in place, you can read it backwards, while deleting it (even backwards compressed). DFA reader backwards mode. (but actually see 25)
// 22. mapcores functions can also do their own dfa for ith field (even reverse, maybe even zipped/destructive) and this is probably OK
// 23. you can even give mapcores the GNU parser and it's OK
// 24. could be helpful to have DFA read from gzip stream and/or file descriptor (and/or socket)
// 25. if you read from the front fowards, you can still "ftruncate" by writing sparse holes (zeroes) as you go. (destructive). on linux madvise can accomplish this I think?
// 26. you can do a predictive guess for how wide a column is (min, avg), faulting back on exceptions
// 27. with a pre-read dfa column width counter you can detect CSVs that are actually fixed-width (faster)
// 28. Note: for in-memory reading (which has a hard time with forking, pre-allocation), we may be able
//     to pre-allocate by mmapping a "huge" [overcount] vector which we don't actually use: if it only
//     consumes memory when touched, then we can just not touch it. INDEXes and HASHes/ENUMs are slightly
//     trickier (and ZIPs and so on) but you can overcount those portions too, for the in-memory version,
//     theoretically.
// 29. putting MADV_SEQUENTIAL whenever a map was expanded sped up the CSV parser by 6s/G in serial mode
//     and 0s/G in parallel. this is for to-disk writing, not in-memory
// 30. see other speedups including quote-finding in "Instant Loading for Main Memory Databases" 
//     http://www.vldb.org/pvldb/vol6/p1702-muehlbauer.pdf

SV_DATA *global_sv_data = NULL;

K cow_build_table_from_delimited_file(K table, K filename, C separator, K fields_k, I header_rows, bool header_titles,
                                      bool in_memory)
{
  table = cow(table);
  nestneu(table, INDEX,  cow(kN(table,INDEX)));
  nestneu(table, KEYS,   cow(kN(table,KEYS)));
  nestneu(table, VALUES, cow(kN(table,VALUES)));

  //We could do processing on field itself to allow for groupings and such (eg remove whitespace)
  K fields2 = copy(fields_k);
  fields2 = cow_ensure_null_terminated_chars(fields2);
  work_push(fields2);

  I skipwise_fieldcount = populated_fields(fields2);

  S fields = kC(fields2);
  I fields_count = strlen(fields);

  K unskipped_fields = new_k(CHARVEC,0);
  DO(fields_count, if(skipfield(fields[i]))continue; unskipped_fields = cow_add(unskipped_fields, kc(fields[i])))
  work_push(unskipped_fields);

  K path = copy(filename);
  path = cow_ensure_null_terminated_chars(path);

  bool flat = true;

  K shared = disk_map_file_shared_maybe_flat(kC(path), NULL, flat);
  work_push(shared);

  rd(path);
 
  K z = ((V)shared) + PAGE_SIZE_BYTES - sizeof(K0);

  int madvise_eval = madvise(shared + PAGE_SIZE_BYTES, POW2(zm) - PAGE_SIZE_BYTES, MADV_SEQUENTIAL);

  //////////////////////////////
  //csv parser use
  SV_DATA data         = {0};
  global_sv_data       = &data;
  data.table           = table;
  data.column_count    = table_columns(data.table);
  data.temp_column_pointers = NULL;
  data.fields          = fields;
  data.fields_count    = fields_count;
  data.header_rows     = header_rows;
  data.header_titles   = header_titles;
  data.in_memory       = in_memory;

  const bool temp_table_strategy = true | true;//you can get this out, but you have to rip it out
  //Note: the temp_table is always on disk. You could have it go in memory (i.e., when it's an in-memory
  //only read), but it's probably better to just rip the whole thing out when that's no longer acceptable.
  //2016.09.15 otoh, it's also faster to make it in-memory for testing than to rip it out
  K temp_table = NULL;
  K temp_table_filename = charvec_from_cstring("_kerftemp");
  if(temp_table_strategy)
  {
    work_push(temp_table_filename);
    //TODO: better to make this a tmp-unique name
    
    K temp_fields_k = ex("$1 take 'S'", ki(skipwise_fieldcount));

    recursive_delete_K(temp_table_filename);

    temp_table = open_table(temp_table_filename, temp_fields_k, FIELD_KIND_DELIMITED);
    recursive_delete_K(temp_table_filename);//we can delete this here, or delete it right before we free up the resources for this stuff (work_pop_n_rd)

    bool in_memory_does_temp_vectors_in_memory = true;

    if(in_memory && in_memory_does_temp_vectors_in_memory)
    {
      K clone = copy_override(temp_table);
      rd(temp_table);
      temp_table = clone;
    }

    //TODO we can 
    //Alternatively, we could just use a vector of mapped tmpfilehandles() !
    //use disk_map_anonymous_shared and update it to accept pass a starting CHARVEC to disk_map_handle_shared_maybe_flat
 
    work_push(temp_table);
    rd(temp_fields_k);

    K y = new_k(LINKVEC, table_columns(temp_table)); 
    DO(yn, 
           K x =  kN(kN(temp_table, VALUES), i);
           if(LIST==xt)
           {
             xt=CHARVEC;
             OFF_ATTR(x,ATTR_SORTED);
           }
           yP[i]=x;
    )

    work_push(y);
    
    data.temp_table = temp_table;
    data.temp_column_pointers = y;
  }

  K format = NULL;
  
  format = ex(".Parse.strptime_format");
  if(IS_STRING(format))
  {
    I length = MIN(sizeof(data.strptime_format), 1+COUNT(format)); 
    snprintf(data.strptime_format, length, "%s", kC(format)); 
  }
  rd(format);

  format = ex(".Parse.strptime_format2");
  if(IS_STRING(format))
  {
    I length = MIN(sizeof(data.strptime_format2), 1+COUNT(format)); 
    snprintf(data.strptime_format2, length, "%s", kC(format)); 
  }
  rd(format);

  //////////////////////////////////////////////////
  sv_init_for_separator(separator);
  sv_read(zC, zn, &data);
  //////////////////////////////////////////////////

  if(temp_table_strategy)
  {
    K strips = kN(temp_table, VALUES);

    ENUM(strips, 
      if(IS_DISK(v))
      {
        int madvise_eval = madvise(v, POW2(vm), MADV_SEQUENTIAL);
      }
    )

    I cols = table_columns(temp_table);
    I batch_max_size = total_cores();

    I batches = cols / batch_max_size;
    if(batches * batch_max_size < cols) batches++;
 
    S pfields = fields;

    I field_i = 0;
    I temp_col_index = table_columns(temp_table) - 1;
    I result_col_index = table_columns(table) - 1;
  
    bool parallel = false;

    if(!parallel)
    {
      //We actually need to process the fields in REVERSE order (with skips!) to handle *additionally added* columns correctly
      for(field_i = fields_count - 1; field_i >= 0; field_i--)
      {
        if(skipfield(fields[field_i]))continue;

        C field = fields[field_i];

        K temp_col_right = kN(kN(temp_table, VALUES), temp_col_index);

        parser_converter_thing(temp_col_right, field, table, result_col_index, &data);

        temp_col_index--;
        result_col_index--;
      }
    }
    else
    {
      if(in_memory)
      {
        //XXX
        //This parallel threading strategy did not help at all. Any more threads made things worse
        //It didn't matter whether the text was in memory or not
        //XXX
        //
        //TODO also, if you want to turn this on, you need to make ZIPs resistant to multithreading (eliminate static buffers etc.)
        //     we can cheat this via __thread
        //     you can also put the buffers on the KVM VM for the thread
        //TODO you'll also need to disable signals to the threads, as we do with the socket hup thread
        //*We could do two passes: one for parallelizable columns, one for non
        //*We could free [in-memory] CHARVEC memory after done
 
        bool saved = The_Thread_Safe_Flag;
        The_Thread_Safe_Flag = true;

        field_i = fields_count - 1;

        DO(batches, 
          I lot = MIN(batch_max_size, cols - i*batch_max_size);
          
          pthread_t threads[lot];
          SV_DATA args[lot];

          DO2(lot,

             while(skipfield(fields[field_i])) field_i--;
             if(field_i < 0) break;

             C field = fields[field_i];

             K temp_col_right = kN(kN(temp_table, VALUES), temp_col_index);

             args[j].temp_column_pointers = temp_col_right;
             args[j].table = table;

             args[j].column_count = temp_col_index;
             args[j].header_titles = fields[field_i];
             args[j].fields = (V)&data;
 
             pthread_create(&threads[j], NULL, parser_thread, &args[j]);
             //pthread_join(threads[j],NULL);

             temp_col_index--;
             result_col_index--;
             field_i--;
          )
          DO2(lot, pthread_join(threads[j],NULL))
        )

        The_Thread_Safe_Flag = saved;
 
      }
      else
      {
        //TODO note, because you won't be able to track the files-mapped-in-virt-memory-page-in-growth across processes,
        //     you'll need to unmap and remap the striped table (easy) when you're done
        //TODO you'll also need to make sure the parallel version passes stripe-test
        K e = ex("{[i] temp:$1;real:$2; _debug_sv(temp,real,i,$4) } mapcores range $3", ki((I)temp_table), ki((I)table), ki(table_columns(table)), ki((I)unskipped_fields));
        rd(e);
      }
    }
    
  }

  //////////////////////////////

  if(temp_table_strategy)
  {
    work_pop_n_rd(3, true);
  }

  work_pop_n_rd(3, true);

  //TODO: check for ragged table

  return table;
}

void *parser_thread(void* arg)
{
  SV_DATA *a = (V)arg;
  K temp_col_right = a->temp_column_pointers;
  K table = a->table;
  I i =  a->column_count;
  C field = a->header_titles;
  SV_DATA* data = (V)a->fields;

  parser_converter_thing(temp_col_right, field, table, i, data);

  return NULL;
}




K _debug_sv(K temp_table_p, K table_p, K index_k, K fields_chars_k_p)
{
  K temp_table   = (K)temp_table_p->i;
  K table        = (K)table_p->i;
  K field_char_k = (K)fields_chars_k_p->i;

  I i = index_k->i;
  C field = kC(field_char_k)[i];

  K read_col  = kN(kN(temp_table, VALUES), i);

  //fprintf(stderr, "i:|%lld| field:|%c| \n", i, field);

  parser_converter_thing(read_col, field, table, i, global_sv_data);

  return Ki(i);
}

void parser_converter_thing(K null_delimited_charvec, C field, K table, I col_index, SV_DATA *data)
{
  I rows = data->row - data->header_rows;

  K x = null_delimited_charvec;

  K target_column = kN(kN(table,VALUES), col_index);

  bool sort_hack = true;
  bool was_sort = false;

  if(sort_hack && data->in_memory)
  {
    if(BTREE == target_column->t)
    {
      was_sort = true;
      K values = kN(table,VALUES);
      nestset_ri_rd(values, col_index, kN(target_column,KEYS), true, true);
      target_column = kN(kN(table,VALUES), col_index);
    }
  }

  C log_batch_max_size = 20;
  I batch_max_size = POW2(log_batch_max_size);

  I batches = rows / batch_max_size;
  if(batches * batch_max_size < rows) batches++;

  S string = xC;

  DO(batches, 
    I batch_size = MIN(batch_max_size, rows - batch_max_size * i);
    K z = NULL; //The joinable segment you'll pass at the end for the target_col

    //todo: U unix timestamp w/ decimal support
    C cap_field = toupper(field);
    SW(cap_field) //IFS, DT date time, E/H for enum, Z for custom datetime, N for IP, G for guid/uuid
    {
     CSF(' ',)
      CS('*',)//skip
     CSF('9',)
      CS('Z', z = new_k(STAMPVEC, batch_size);
         DO2(batch_size,  I nanos = nanos_xstrptime(string,data->strptime_format);
                              zI[j] = nanos;
                              I m  = strlen(string);
                              string += m + 1;
              )
      )
      CS('Y', z = new_k(STAMPVEC, batch_size);
              DO2(batch_size, I nanos = nanos_xstrptime(string,data->strptime_format);
                              zI[j] = nanos;
                              I m  = strlen(string);
                              string += m + 1;
              )
      )
      CS('A', //nanoseconds since epoch. copy-pasta with 'I', except STAMPVEC
              z = new_k(STAMPVEC, batch_size);
              DO2(batch_size,  
                               zI[j] = strtoll(string, 0, PARSE_ZERO_PREFIX_MEANS_OCTAL ? 0 : 10);
                               I m  = strlen(string);
                               string += m + 1;
              )
      )


      //todo: specials for I: INF, inf etc see parse_number
      CD:
     CSF('1',)
     CSF('2',)
     CSF('4',)
     CSF('W',)
      CS('I', 
              z = new_k(INTVEC, batch_size);
              DO2(batch_size,  
                               zI[j] = strtoll(string, 0, PARSE_ZERO_PREFIX_MEANS_OCTAL ? 0 : 10);
                               I m  = strlen(string);
                               string += m + 1;
              )
      )
     CSF('D',)
     CSF('0',)
     CSF('V',)
      CS('F', z = new_k(FLOATVEC, batch_size);
              DO2(batch_size,  zF[j] = strtod(string, 0);
                               I m  = strlen(string);
                               string += m + 1;
              )
      )
     CSF('E', )
      CS('S', 
              if('E'==cap_field)
              {
                z = new_intern();
              }
              else
              {
                z = new_k(LIST, batch_size);
                zn = 0;
              }
              DO2(batch_size,  K k = charvec_from_cstring(string);
                               z = cow_add(z,k);
                               rd(k);
                               I m = strlen(string); 
                               string += m + 1;
              )
      )
      CS('N', z = new_k(INTVEC, batch_size);
              DO2(batch_size,  struct in_addr ip = {0};
                               inet_pton(AF_INET, string, &ip); 
                               zI[j] = ip.s_addr;
                               I m  = strlen(string);
                               string += m + 1;
              )
      )
      CS('G', z = new_k(INTVEC, batch_size);
              DO2(batch_size,  uint64_t u = strtoll(17+string, 0, 16); //skipping 1 extra hex dig for now
                               zI[j] = u;
                               I m  = strlen(string);
                               string += m + 1;
              )
      )

      CS('3',
          z = new_k(LIST, batch_size);
          zn = 0;

          //1. you can either set the zip subtype ahead of time
          //2. or you can make ENUM(STR32) and more work

          DO2(batch_size,  
                           //if(0==j%10000)printf("string:|%s|\n", string);
                           K k = new_k(CHARVEC,32);
                           I l = strlen(string); 
                           memcpy(kC(k), string, l);
                           k->n = l;
                           if(k->n<32)kC(k)[k->n]='\0';
                           string += l + 1;
                           z = cow_add(z,k); //POTENTIAL_OPTIMIZATION_POINT: we can just set k directly
                           rd(k);
          )
      )

    }

    target_column = cow_join(target_column, z);

    K values = kN(table,VALUES);

    if(i == _i - 1 && sort_hack && was_sort)
    {
      K tmp = indexed(target_column);
      rd(target_column);
      target_column = tmp;
    }

    //in-memory vs. disk lists
    if(!IS_STRIPED(values) && !IS_DISK(values))
    {
      nestset_ri_rd(values, col_index, target_column, false, false);
    }
    else
    {
      msync(target_column, POW2(target_column->m), MS_SYNC);
    }

    rd(z);
  )
}

bool string_requires_delimiter_quoting_escaping(K x, C delimiter)
{
  if(!IS_STRING(x)) return false;
  DO(xn, if('"'==xC[i] || delimiter==xC[i]) return true; )
  return false;
}

K delimiter_string(K x, C delimiter)
{
  K z = NULL;

  SW(xt)
  {

    CSF(NIL,)
    CSF(STAMP,)
    CSF(CHAR,)
    CSF(INT,)
     CS(CHARVEC, z = string_cast(x))

     CS(FLOAT,   z = shout_float(xf, true))

    CD:
        //empty if not specified
        z = charvec_from_cstring("");
  }

  if(string_requires_delimiter_quoting_escaping(z,delimiter))
  {
    K y = shout(z, 0, true, true, true, true);
    rd(z);
    z = y;
  }

  return z;
}

K write_delimited_file_from_table(K delimiter_k, K delimited_filename, K table)
{
  if(!IS_TABLE(table)) ERROR(ERROR_TABLE);
  if(!IS_STRING(delimited_filename)) ERROR(ERROR_STRING);
  if(!IS_STRING(delimiter_k) || COUNT(delimiter_k) != 1) ERROR(ERROR_STRING);

  I rows = table_rows(table);
  I cols = table_columns(table);

  C delimiter = kC(delimiter_k)[0];
  C linesep = '\n';
  K linesep_k = kc(linesep);

  K x = copy(delimited_filename);
  x = cow_ensure_null_terminated_chars(x);
  work_push(x);

  I handle = open_file_handle(xC, true);

  handle_truncate(0, handle);

  //TODO: to avoid leaking handle on error/interrupt/longjmp, maybe map using disk_map_handle_shared_maybe_flat
  //      and push on stack but never touch it

  //POTENTIAL_OPTIMIZATION_POINT
  //This CSV writer has no optimizations yet

  bool writes_header = true;

  if(writes_header)
  {
    K build = new_k(CHARVEC,0);

    ENUM(table, 
      K y = delimiter_string(u, delimiter);
      DO2(yn, build = cow_add(build, kc(yC[j])))
      if(i < _i - 1) build = cow_add(build, kc(delimiter));
      rd(y);
    )

    if(rows>0) build = cow_add(build, kc(linesep));
    work_push(build);
    brute_write(kC(build), build->n, handle, false, NULL, NULL);
    work_pop_rd(true);
  }

  K lists = kN(table,VALUES);

  DO(rows,

    K build = new_k(CHARVEC,0);

    DO2(cols,
      K col = kN(lists, j);
      K0 o;
      K cell = AT2(col, ki(i),o);

      K y = delimiter_string(cell, delimiter);

      DO(yn, build = cow_add(build, kc(yC[i])))
      if(j < _j - 1) build = cow_add(build, kc(delimiter));

      rd(y);
    )
    if(i < _i - 1) build = cow_add(build, kc(linesep));
    work_push(build);
    brute_write(kC(build), build->n, handle, false, NULL, NULL);
    work_pop_n_rd(1, true);
  )

  I written = lseek(handle, 0, SEEK_CUR);

  close(handle);
  work_pop_n_rd(1, true);

  //return chars written? fseek peek?

  return Ki(written);
}

K write_csv_from_table(K csv_filename, K table)
{
  return write_delimited_file_from_table(kcv(","), csv_filename, table);
}

K lines(K filename, K limitation)
{
  K x = filename;

  I limit = II;

  if(limitation)
  {
    if(limitation->t == INT)
    {
      limit = limitation->i;
    }
    else
    {
      ERROR(ERROR_TYPE);
    }
  }

  K path = copy(x);
  path = cow_ensure_null_terminated_chars(path);

  bool flat = true;
  K shared = disk_map_file_shared_maybe_flat(kC(path), NULL, flat);
  work_push(shared);

  rd(path);
 
  K z = ((V)shared) + PAGE_SIZE_BYTES - sizeof(K0);

  //1h15i without copy
  //copying doesn't make much of a difference...
  //z = copy(z);
  //work_push(z);

  I i = 0;

  I line = 0;

  K y = new_k(LIST,0);
  work_push(y);

  K snippet = new_k(CHARVEC, 0);

  while(1)
  {
    if(line >= limit) break;
    C c = '\0';

    if(i<zn)
    {
      c = zC[i];
      i++;
    }

    if('\n'==c || i==zn)
    {
      if(i==zn && '\n'!=c && 0!=i) snippet = cow_add(snippet, kc(c));
      work_pop();//y
      y = cow_add(y, snippet);
      rd(snippet);
      work_push(y);
      snippet = new_k(CHARVEC, 0);
      line++;

      //if(line > 0) { K eval = apply(function, klist1(snippet)); rd(eval); }
    }
    else
    {
      snippet = cow_add(snippet, kc(c));
    }

    if(i >= zn) break;
  }

  rd(snippet);
  work_pop();
  work_pop_rd(true);

  return y;
}

K create_table_from_fixed_file(K table_filename, K fixed_filename, K maptributes)
{
  if(!IS_STRING(table_filename)) ERROR(ERROR_STRING);
  recursive_delete_K(table_filename);
  return append_table_from_fixed_file(table_filename, fixed_filename, maptributes);
}

K append_table_from_fixed_file(K table_filename, K fixed_filename, K maptributes)
{
  if(!IS_STRING(table_filename)) ERROR(ERROR_STRING);
  if(!IS_STRING(fixed_filename)) ERROR(ERROR_STRING);
  if(!IS_MAP(maptributes))       ERROR(ERROR_MAP);

  K0 o;
  K fields_k = LOOKUP_(maptributes, kcv("fields"), o);
  if(!fields_k || !IS_STRING(fields_k))
  {
    fprintf(stderr, "Attribute dictionary missing 'fields' string.\n");
    ERROR(ERROR_STRING);
  }

  K table = open_table(table_filename, fields_k, FIELD_KIND_FIXED);

  table = cow_build_table_from_fixed_file(table, fixed_filename, maptributes);

  return table;
}

K read_table_from_fixed_file(K fixed_filename, K maptributes)
{
  K table = new_table();

  table = cow_build_table_from_fixed_file(table, fixed_filename, maptributes);

  return table;
}

K cow_build_table_from_fixed_file(K table, K filename, K maptributes)
{
  //COW Table
  table = cow(table);
  nestneu(table, INDEX,  cow(kN(table,INDEX)));
  nestneu(table, KEYS,   cow(kN(table,KEYS)));
  nestneu(table, VALUES, cow(kN(table,VALUES)));

  //Type Checking
  if(!IS_TABLE(table))      ERROR(ERROR_TABLE);
  if(!IS_STRING(filename))  ERROR(ERROR_STRING);
  if(!IS_MAP(maptributes))  ERROR(ERROR_MAP);

  K0 o;
  K fields_k = LOOKUP_(maptributes, kcv("fields"), o);
  if(!fields_k || !IS_STRING(fields_k))
  {
    fprintf(stderr, "Attribute dictionary missing 'fields' string.\n");
    ERROR(ERROR_STRING);
  }

  K0 o1;
  K widths_k = LOOKUP_(maptributes, kcv("widths"), o1);
  if(!widths_k || widths_k->t != INTVEC)
  {
    fprintf(stderr, "Attribute dictionary missing 'widths' integer array.\n");
    ERROR(ERROR_ARRAY);
  }

  DO(widths_k->n , I w = kI(widths_k)[i]; 
                   if(w <= 0)
                   {
                     fprintf(stderr, "Attribute dictionary 'widths' has integer <= 0.\n");
                     ERROR(ERROR_SIZE);
                   }
  )

  I limit = II;
  K0 o2; 
  K limit_k = LOOKUP_(maptributes, kcv("line_limit"), o2);
  if(limit_k)
  {
    if(limit_k->t == INT)
    {
      limit = limit_k->i;
    }
    else
    {
      ERROR(ERROR_TYPE);
    }
  }

  if(COUNT(fields_k) != COUNT(widths_k)) ERROR(ERROR_CONFORMABLE);

  K titles_k = NULL;

  I precolumns = table_columns(table);

  bool empty_table = (precolumns == 0);

  //Handle Column Titles for Table
  //If the table already has columns, we don't care about titles (null or not)
  //--we do care that it matches up
  //if the table is empty (of columns) we do care
  if(!empty_table)
  {
    if(precolumns != COUNT(fields_k))
    {
      //technically we have the following cases:
      //cols  < |fields| -> if rows == 0, salvageable, consider titles
      //cols == |fields| -> OK
      //cols  > |fields| -> error
      ERROR(ERROR_COLUMN);
    }
  }
  else //table is empty
  {
    K0 o;
    titles_k = LOOKUP_(maptributes, kcv("titles"), o);
    if(titles_k && COUNT(titles_k) != COUNT(fields_k))
    {
      fprintf(stderr, "Attribute dictionary has wrong count for column 'titles' array.\n");
      ERROR(ERROR_COLUMN);
    }

    ENUM(fields_k,
      K y = kcv("col");

      K0 o;
      if(titles_k) y = LOOK_(titles_k, i,o);

      C field = vc;

      bool skip = skipfield(field);
      if(skip)continue;

      K original = alpha_uncollided_name(table, y);

      K vals = new_empty_list_for_fixed_field(field);
      
      table = cow_table_add_column(table, original, vals);
      assert(vals);
      rd(vals);
      rd(original);
    )
  }

  I header_rows = 0;
  bool newline_separated = true;
  bool trims = true;

  K0 o4;
  K header_rows_k = LOOKUP_(maptributes, kcv("header_rows"), o4);
  if(header_rows_k)
  {
    if(INT!=header_rows_k->t)ERROR(ERROR_TYPE);
    header_rows = header_rows_k->i;
  }

  K0 o5;
  K newline_separated_k = LOOKUP_(maptributes, kcv("newline_separated"), o5);
  if(newline_separated_k)
  {
    newline_separated = truthy(newline_separated_k);
  }

  //We could do processing on field itself to allow for groupings and such (eg remove whitespace)
  K fields2 = copy(fields_k);
  fields2 = cow_ensure_null_terminated_chars(fields2);
  work_push(fields2);

  S fields = kC(fields2);
  I fields_count = strlen(fields);

  K path = copy(filename);
  path = cow_ensure_null_terminated_chars(path);

  bool flat = true;
  K shared = disk_map_file_shared_maybe_flat(kC(path), NULL, flat);
  work_push(shared);
  rd(path);
 
  K z = ((V)shared) + PAGE_SIZE_BYTES - sizeof(K0);

/////////////////////////////////////////////
  //potential_feature_point we can add offset and length maptributes
  I start = 0;
  I length = z->n;
  I end = start + length;
  I i = start;

  K xsum = ex("sum $1", widths_k);
  I line_width = xsum->i; 
  rd(xsum);
  if(0 >= line_width) ERROR(ERROR_LENGTH);

  if(newline_separated) line_width += 1;
  I lines_read = 0;
  I line_count = (z->n + 1) / line_width; //plus 1 in case missing terminal newline

  K format = NULL;

  C strptime_format[256] = {0};
  format = ex(".Parse.strptime_format");
  if(IS_STRING(format))
  {
    I length = MIN(sizeof(strptime_format), 1+COUNT(format)); 
    snprintf(strptime_format, length, "%s", kC(format)); 
  }
  rd(format);

  C strptime_format2[256] = {0};
  format = ex(".Parse.strptime_format2");
  if(IS_STRING(format))
  {
    I length = MIN(sizeof(strptime_format2), 1+COUNT(format)); 
    snprintf(strptime_format2, length, "%s", kC(format)); 
  }
  rd(format);

  K y = new_k(CHARVEC, 0); //snippet

  for(; i < end && (lines_read - header_rows) < limit; lines_read++)
  {
    bool is_header = (lines_read < header_rows);

    I line_start = i;
    I line_end = line_start; 

    //Grab Line
    if(newline_separated)
    {
      //grab to newline & worry about length checking after
      while(line_end < end && zC[line_end] != '\n') line_end++;
      i = line_end + 1;
    }
    else
    {
      //grab line width
      line_end = line_start + line_width;
      i = line_end;

      if(line_end > end)
      {
        fprintf(stderr, "Warning: ragged line at end of fixed-width file, skipping.\n");
        continue;
      }
    }

    if(is_header) continue;

    //Grab Fields
    I field_read = 0;

    I column_index = 0;

    K sym = kcv("");

    DO(fields_count, 
      C field = fields[i]; 
      I width = kI(widths_k)[i];

      I field_start = line_start + field_read;
      I field_end = field_start + width;
      I field_start_untouched = field_start;

      if(trims)
      {
        while(field_start < field_end && isspace(zC[field_start])) field_start++;
        while(field_end > field_start && isspace(zC[field_end - 1])) field_end--;
      }

      I revised_width = field_end - field_start;

      field_read += width;

      if(skipfield(field)) continue;

      yn = 0;
      DO2(revised_width, y = cow_add(y, kc(zC[field_start + j])))
      y = cow_add(y, kc('\0'));
      yn = yn - 1;

      //Process Fields
      //copy-pasta with csv parser thing 
      K0 o;
      K column = AT2(kN(table,VALUES), ki(column_index),o);
      K longer = NULL;
      K thing = NULL;
      bool release = false;

      C cap_field = toupper(field);

      SW(cap_field) //IFS, DT date time, E/H for enum, Z for custom datetime, N for IP, G for guid/uuid
      {
        //TODO: specials for I: INF, inf etc see parse_number
        CS('A', I i = strtoll(yC, 0, PARSE_ZERO_PREFIX_MEANS_OCTAL ? 0 : 10); thing = ks(i);) //nanoseconds since epoch
       CSF('U',)//todo: unix timestamp w/ decimal support
        CD:
        CS('I', I i = strtoll(yC, 0, PARSE_ZERO_PREFIX_MEANS_OCTAL ? 0 : 10); thing = ki(i);)
        CS('F', F f = strtod(yC, 0); thing = kf(f);)

       CSF('E', )
        CS('S', thing = copy(y); release = true;) 
        CS('R', //ridiculous NYSE TAQ sym format

                if(width < 16)
                {
                  fprintf(stderr, "NYSE TAQ sym field must be >= 16 width\n");
                  ERROR(ERROR_LENGTH);
                } 
                S s = zC + field_start_untouched;

                sym->n = 0;
                DO(6, C c = s[i]; if(isspace(c))break; kC(sym)[sym->n++] = c)
                kC(sym)[sym->n]='\0';

                s = s + 6;

                if(!isspace(s[0])) //necessary check: do not append dot if no suffix field
                {
                  kC(sym)[sym->n++] = '.';
                  DO(10, C c = s[i]; if(isspace(c))break; kC(sym)[sym->n++] = c)
                  kC(sym)[sym->n]='\0';
                }

                thing = copy(sym);
                release = true;
        )
        CS('N', struct in_addr ip = {0}; inet_pton(AF_INET, yC, &ip); thing = ki(ip.s_addr);)
        CS('G', uint64_t u = strtoll(17+yC, 0, 16); thing = ki(u);)//skipping 1 extra hex dig for now
        CS('Z', struct tm t = TM_EPOCH;
                S stop = strptime(yC, strptime_format, &t);
                bool local = false;
                I nanos = stampI_from_tm(&t, local);
                thing = ks(nanos);
        )
        CS('Y', struct tm t = TM_EPOCH;
                S stop = strptime(yC, strptime_format2, &t);
                bool local = false;
                I nanos = stampI_from_tm(&t, local);
                thing = ks(nanos);
        )
        CS('Q', //ridiculous NYSE TAQ time format w/ millis HHMMSSXXX (can expand to micros/nanos if they expand. just check length)
                if(revised_width < 9) ERROR(ERROR_TIME);
                char hour_s[]    = {yC[0], yC[1], '\0'};
                char minute_s[]  = {yC[2], yC[3], '\0'};
                char seconds_s[] = {yC[4], yC[5], '\0'};
                char millis_s[]  = {yC[6], yC[7], yC[8], '\0'};
                I hour    = strtoll(hour_s,    0, 10); 
                I minute  = strtoll(minute_s,  0, 10); 
                I seconds = strtoll(seconds_s, 0, 10); 
                I millis  = strtoll(millis_s,  0, 10); 
                I pre = (60 * 60 * hour) + (60 * minute) + seconds;
                I nanos = (pre * BILLION) + (millis * MILLION);
                thing = ks(nanos);
        )
       CSF(' ',)
        CS('*', thing = NULL) //skip
      }

      if(thing)
      {
        longer = cow_add_funny(column, thing);

        K values = kN(table,VALUES);

        if(!IS_STRIPED(values) && !IS_DISK(values)) update_ri_rd(values, ki(column_index), longer, false, false);
        column_index++;

        if(release) rd(thing);
      }
    )


  }

  rd(y);

/////////////////////////////////////////////


  work_pop_n_rd(2, true);

  return table;
}

#pragma mark - Basic Disk IO

K read_from_path(K x)
{
  if(!IS_STRING(x)) ERROR(ERROR_TYPE);

  K z = copy(x);
  z = cow_ensure_null_terminated_chars(z);
  work_push(z);
  K a = read_k_from_path(zC);

  work_pop_rd(true);
  return a;
}

K write_to_path(K x, K y)
{
  if(!IS_STRING(x)) ERROR(ERROR_TYPE);
  K z = copy(x);
  z = cow_ensure_null_terminated_chars(z);
  work_push(z);

  if(TABLE==yt)
  {
    fprintf(stderr, "Warning: writing table the slow way, faster to use write_striped_to_path.\n");
  }

  bool c = write_k_to_path(y, zC);
  work_pop_rd(true);
  return Ki(c);
}

K verb_open_table(K x)
{
  return open_table(x, NULL, 0);
}

K open_table(K x, K fields, C fields_kind)
{
  if(!IS_STRING(x)) ERROR(ERROR_STRING);

  K file = copy(x);
  file = cow_ensure_null_terminated_chars(file);
  work_push(file);

  K z = NULL;

  I created_columns = -1;

  if(fields)
  {
    created_columns = populated_fields(fields);
  }

  if(file_path_is_directory(kC(file)))
  {
    z = read_striped_from_path(x); 
    work_push(z);

    if(created_columns >= 0)
    {
      if(zt != TABLE)
      {
        fprintf(stderr, "Existing object is not a table.\n");
        ERROR(ERROR_TABLE);
      }
      else if(created_columns != table_columns(z))
      {
        fprintf(stderr, "Existing table has differing number of columns.\n");
        ERROR(ERROR_TABLE);
      }
    }
    
    work_pop();

  }
  else if(file_exists_valid(kC(file)) || !fields)
  {
    z = disk_map_file_shared(kC(file));
  }
  else
  {
    K t = new_table();
    DO(fields->n, C field = kC(fields)[i]; 
                  K list = NULL;
                  if(skipfield(field)) continue; 

                  SW(fields_kind)
                  {
                    CS(FIELD_KIND_FIXED,     list = new_empty_list_for_fixed_field(field))
                    CD:
                    CS(FIELD_KIND_DELIMITED, list = new_empty_list_for_delimited_field(field))
                  }
                  t = cow_table_add_column_uncollided(t,kcv("col"),list);
                  rd(list);
    )
    work_push(t);
    z = write_striped_to_path(x,t);
    work_pop_n_rd(1,true);
  }

  work_pop_rd(true);

  //better signature: var_name_string, file_path_string, template_object (not necessarily table)

  return z;
}

#pragma mark - CSV & SV Stuff 

enum SV_CLASSES {SV_CLASS_REGULAR    = 0,
                 SV_CLASS_WHITESPACE = 1,
                 SV_CLASS_ROW_END    = 2,
                 SV_CLASS_SEPARATOR  = 3,
                 SV_CLASS_QUOTE      = 4,
                 SV_CLASS_BACKSLASH  = 5,
                 SV_CLASSES_SIZE};

enum SV_STATES  {SV_STATE_LEFT    = 0,
                 SV_STATE_REGULAR = 1,
                 SV_STATE_RIGHT   = 2,
                 SV_STATE_QUOTE   = 3,
                 SV_STATE_ESC     = 4,
                 SV_STATE_JUNK    = 5,
                 SV_STATES_SIZE};

C sv_char_classes[256] = {0};

C sv_char_class_for_char(C c)
{

  SW(c)
  {
   CSF('\t',)
   CSF('\v',)
   CSF('\f',)
    CR(' ',  SV_CLASS_WHITESPACE)

   CSF('\r',)
    CR('\n', SV_CLASS_ROW_END)

    CR('"',  SV_CLASS_QUOTE)

    CR('\\', SV_CLASS_BACKSLASH)
  }

  return SV_CLASS_REGULAR;
}

//STATE
S SV_STATE_STRING =
//  *sn,"b  
   "100031" // 0-LEFT
   "120051" // 1-REG
   "120051" // 2-RIGHT
   "333354" // 3-QUOTE
   "333333" // 4-ESC
   "550035" // 5-JUNK
   "";
C sv_transitions[MAX_SV_TRANSITIONS] = {0};

//ACTION
S SV_ACTION_STRING =
//  *sn,"b
   "104351" // LEFT
   "124351" // REG
   "624326" // RIGHT
   "111121" // QUOTE
   "111111" // ESC
   "224362" // JUNK
   "";
C sv_actions    [MAX_SV_TRANSITIONS] = {0};

enum SV_ACTIONS {SV_ACTION_IGNORE_LEFT  = 0,
                 SV_ACTION_READ_REGULAR = 1,
                 SV_ACTION_IGNORE_RIGHT = 2,
                 SV_ACTION_END_COLUMN   = 3,
                 SV_ACTION_END_BOTH     = 4,
                 SV_ACTION_QUOTE_START  = 5,
                 SV_ACTION_RIGHT_TO_REG = 6,
                 SV_ACTIONS_SIZE};

void do_sv()
{
  S s = 
  "1,2,3\n\r\r\r"
  " 4, 5, 6\n"
  "7 ,8 ,9 \n"
  " 10 , 11 , 12 \n"
  ",,\n"
  " \"13\"  , \"14\" , \"15\"1 \n\n"
  " best, in, the, west, \n"
  " i, mean, \" key \", west \n"
  " \"\\c\\t\", d , e\n"
  " , , \n"
  " \\ , \\ , \\ \n"
  " \"quotes\"\"\" , \" are \"\" fun \"  , \"to\" \"make\"  \n"
  "";

  //sv_init_for_separator(',');
  //SV_DATA data = {0};
  //sv_read(s, strlen(s), &data);
}

void sv_init_for_separator(C separator)
{
  bool newlines_allowed_in_quotes = true; //to disable: feasible, but avoid if possible
  bool backslash_escape_in_quotes = true; //to disable: make backslash class empty (no backslash char)
  bool exclude_whitespace         = true; //to disable: change whitespace class or change to regular read action
  bool exclude_quotes             = true; //to disable: feasible, but avoid if possible 
  bool two_quotes_is_literal      = true; //to disable: avoid, but change junk reads quote to STATE stay junk, ACTION right ignore (1,"ab "" cd",3\n)

  assert(MAX_SV_CLASSES == SV_CLASSES_SIZE);
  assert(MAX_SV_STATES  == SV_STATES_SIZE);
  assert(MAX_SV_TRANSITIONS == strlen(SV_STATE_STRING));
  assert(MAX_SV_TRANSITIONS == strlen(SV_ACTION_STRING));

  DO(sizeof(sv_char_classes), sv_char_classes[i] = sv_char_class_for_char(i))

  sv_char_classes[separator] = SV_CLASS_SEPARATOR;

  DO(MAX_SV_TRANSITIONS, sv_transitions[i] = SV_STATE_STRING[i]  - '0')
  DO(MAX_SV_TRANSITIONS, sv_actions[i]     = SV_ACTION_STRING[i] - '0')
}

K cow_sv_unescape_charvec(K x)
{
  x = cow(x);
  return _sv_unescape_charvec(x);
}

K _sv_unescape_charvec(K x)
{
  S d = xC;
  S s = d;

  I skipped = _sv_memcpy_skipping_escapes(d, s, xn);

  xn -= skipped;

  return x;
}

I _sv_memcpy_skipping_escapes(S d, S s, I n) //buffers must be big enough
{
  bool unescape_two_quotes  = true;
  bool unescape_backslashes = true;

  I skipped = 0;

  S end = s + n;

  while(s < end)
  {

    bool another = s < end-1;

    bool two_quotes = unescape_two_quotes &&   '"'==s[0] && another && '"'==s[1];
    bool backslash  = unescape_backslashes && '\\'==s[0] && another;

    bool is_escape = (two_quotes || backslash) && another;

    if(is_escape) //we could expand this to do "\b" and "\u0000" and so on...
    {
      s++;
      skipped++;
    }

    *d = *s; 

    d++;
    s++;
  }

  return skipped;
}

#define sv_read_macro(function_name, sv_callback_column_name, sv_callback_row_name)                \
void function_name(S source, I length, SV_DATA *data)                                              \
{                                                                                                  \
  /* optional improvements:                                       */                               \
  /* *can turn this whole thing into a macro: the swappable       */                               \
  /*   parts are the end-col/end-row actions, including in the    */                               \
  /*   finalizer. you may want this to do pre-counts for instance */                               \
  /* *can pre-read #rows, per-field char counts, establish 1/n    */                               \
  /*   markers for parallelization, etc.                          */                               \
  /* *can upgrade this to read from stream, save in buffer        */                               \
                                                                                                   \
  if(!data)                                                                                        \
  {                                                                                                \
    fprintf(stderr, "Null delimited reader callback data\n");                                      \
    return;                                                                                        \
  }                                                                                                \
                                                                                                   \
  data->source = source;                                                                           \
                                                                                                   \
  C action = 0;                                                                                    \
  C state = 0;                                                                                     \
                                                                                                   \
  bool skip_empty_rows = true;                                                                     \
                                                                                                   \
  I i = 0;                                                                                         \
  for(i = 0; i < length; i++)                                                                      \
  {                                                                                                \
    UC b = (UC)source[i];                                                                          \
    C c = sv_char_classes[b];                                                                      \
                                                                                                   \
    C prev_state = state;                                                                          \
    assert(127 >= (prev_state * MAX_SV_CLASSES) + c);                                              \
    C j = (prev_state * MAX_SV_CLASSES) + c;                                                       \
    action = sv_actions[j];                                                                        \
    state  = sv_transitions[j];                                                                    \
                                                                                                   \
    SW(action)                                                                                     \
    {                                                                                              \
                                                                                                   \
     CSF(SV_ACTION_QUOTE_START,  data->quoted = true) /*vvv fallthrough*/                          \
      CS(SV_ACTION_IGNORE_LEFT,  data->item_position++; data->left_trimmed++)                      \
                                                                                                   \
     CSF(SV_ACTION_RIGHT_TO_REG, data->item_width += data->right_trimmed;                          \
                                data->right_trimmed = 0)/* vvv fallthrough */                      \
      CS(SV_ACTION_READ_REGULAR, data->item_width++)                                               \
                                                                                                   \
      CS(SV_ACTION_IGNORE_RIGHT, data->right_trimmed++)                                            \
                                                                                                   \
      CS(SV_ACTION_END_COLUMN,                                                                     \
                                                                                                   \
        /*fprintf(stderr,"|%.*s|\n", (int)data->item_width, data->source + data->item_position);*/ \
        sv_callback_column_name(data->source + data->item_position, data->item_width, data);       \
                                                                                                   \
        data->column++;                                                                            \
                                                                                                   \
        data->item_position = 1 + data->item_position + data->item_width + data->right_trimmed;    \
        data->item_width    = 0;                                                                   \
        data->left_trimmed  = 0;                                                                   \
        data->right_trimmed = 0;                                                                   \
        data->quoted        = false;                                                               \
      )                                                                                            \
      CS(SV_ACTION_END_BOTH,                                                                       \
                                                                                                   \
        bool skip =    skip_empty_rows && (0 == data->column)                                      \
                    && (0 == data->item_width) && !data->quoted;                                   \
                                                                                                   \
        if(skip)                                                                                   \
        {                                                                                          \
          /* fprintf(stderr, "Note: skipping empty row in separated-value file.\n"); */            \
          data->row--; /*offset upcoming increment...*/                                            \
        }                                                                                          \
        else                                                                                       \
        {                                                                                          \
          /*fprintf(stderr,"|%.*s|\n",(int)data->item_width, data->source+data->item_position);*/  \
          sv_callback_column_name(data->source + data->item_position, data->item_width, data);     \
          sv_callback_row_name(data);                                                              \
        }                                                                                          \
                                                                                                   \
        data->column = 0;                                                                          \
        data->row++;                                                                               \
                                                                                                   \
        data->fields_skipped = 0;                                                                  \
                                                                                                   \
        data->item_position = 1 + data->item_position + data->item_width + data->right_trimmed;    \
        data->item_width    = 0;                                                                   \
        data->left_trimmed  = 0;                                                                   \
        data->right_trimmed = 0;                                                                   \
        data->quoted        = false;                                                               \
      )                                                                                            \
                                                                                                   \
      CD: assert(1==0); break;/*unreachable*/                                                      \
    }                                                                                              \
  }                                                                                                \
                                                                                                   \
  /*Tricky case: handles un-closed quotes at end of file*/                                         \
  bool finalize = !(action == SV_ACTION_END_BOTH);                                                 \
  if(finalize)                                                                                     \
  {                                                                                                \
    /*fprintf(stderr, "|%.*s|\n", (int)data->item_width, data->source + data->item_position);*/    \
    sv_callback_column_name(data->source + data->item_position, data->item_width, data);           \
    sv_callback_row_name(data);                                                                    \
    data->row++;                                                                                   \
  }                                                                                                \
}                                                                                                  \
/* this line intentionally blank */

void sv_read(S source, I length, SV_DATA *data);                                              
void sv_pre_read(S source, I length, SV_DATA *data);                                              

sv_read_macro(sv_read, sv_callback_column, sv_callback_row)

sv_read_macro(sv_pre_read, sv_callback_column_pre_read, sv_callback_row_pre_read)


