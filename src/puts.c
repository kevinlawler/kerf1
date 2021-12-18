#include "kerf.h"


int kerf_history_init() //you cannot call this "history_init" or you get a symbol collision with some libedit libs on linux
{
#ifdef CONSOLE
  struct passwd* pwd = getpwuid(getuid());
  char* home_dir = pwd->pw_dir;
  char* filename_only = ".kerf_history";
  I wanted = snprintf(The_History_File, sizeof(The_History_File), "%s/%s", home_dir, filename_only);
  if(wanted >= sizeof(The_History_File)) abort();

  read_history(The_History_File);
  stifle_history(100);

#endif
  return 0;
}

void show_blocks()
{
  DO(MAPPED_BLOCKS, O("%3d", virtual_memory_blocks[i]))
  O("\n");
}

void show_stack(VM m)
{
  I N = m->local_pointer;

  er(======frame top  =========)
  DO(N, 
        K0 o;
        K x = peeko(m,i,o);
        fprintf(stderr, "Stack %lld (t:%lld n:%lld): ", N-i-1, (I)xt, (I)xn); 
        show(x);
        if((N-i-1)==m->local_bottom && m->local_bottom!=0)er(======frame bottom=========)
  )
  er(======stack bottom=========)
  fprintf(stderr, "Stack: framept: %lld, framebot: %lld\n", m->local_pointer, m->local_bottom);

}

K show_hex(K k) {O("0x"); ENUM(k,) O("\n"); R k;}

K show_full(K x)
{
  if(!x)
  {
    O("NULL-POINTER");
  }
  else
  {
    K build = shout(x, 0, 1, 1, true, true);
    ENUM(build, O("%c", vc))
    rd(build);
  }
  O("\n");
  fflush(stdout);
  return x;
}

K show(K x)
{
  if(!x)O("NULL-POINTER");
  else easy_show(x);
  O("\n");
  fflush(stdout);
  R x;
}

K easy_show(K x)
{
  R easy_show2(x,1,1,true);
}

void show_float(F f, I last)
{
  if(isnan(f))O("NaN");
  else if(isinf(f))
  {
    if(f>0)O("Infinity");
    else O("-Infinity");
  }
  else if(last && 0==f-trunc(f)) O("%.1f",f);
  else O("%g",f);
}

void simple_show_error(int val)
{
  S string = error_string(val);

  fprintf(stderr, "%s\n", string);
}


K shout_error_map(K x)
{
  K0 o1,o2,o3,o4,o5,o6;

  K has_code        = LOOKUP_(x, kcv("has_error_code"), o1);
  K has_notice      = LOOKUP_(x, kcv("has_error_notice"), o2);
  K has_instruction = LOOKUP_(x, kcv("has_error_instruction"), o3);
  K has_position    = LOOKUP_(x, kcv("has_error_position"), o4);
  K has_snippet     = LOOKUP_(x, kcv("has_error_snippet"), o5);
  K has_caret       = LOOKUP_(x, kcv("has_error_caret"), o6);

  K build = charvec_from_cstring("");

  build = cow_join(build, kcv("\n"));

  S prefix = "  ";

  if(has_snippet && truthy(has_snippet))
  {
    K0 o;
    K snippet = LOOKUP_(x, kcv("error_snippet"), o);
    build = cow_join(build, kcv(prefix));
    K child = shout(snippet, 0, false, false, true, true); //escapes would harm caret position
    build = cow_join(build, child);
    rd(child);
    build = cow_join(build, kcv("\n"));
  }

  if(has_caret && truthy(has_caret))
  {
    K0 o;
    K caret = LOOKUP_(x, kcv("error_caret"), o);

    build = cow_join(build, kcv(prefix));
    DO(caret->i, build = cow_join(build, kcv(" ")))
    build = cow_join(build, kcv("^"));
    build = cow_join(build, kcv("\n"));
  }

  if(has_notice && truthy(has_notice))
  {
    K0 o;
    K notice = LOOKUP_(x, kcv("error_notice"), o);
    build = cow_join(build, kcv(" "));

    K child = shout(notice, 0, false, false, true, true);
    build = cow_join(build, child);
    rd(child);
  }

  build = cow_join(build, kcv("\n"));

  build = cow_ensure_null_terminated_chars(build);

  return build;
}


K easy_show_error_map(K x)
{
  K a = shout_error_map(x);
  printf("%s", kC(a));
  rd(a);
  return x;
}

K shout_dtbucket(K x, I depth)
{
  K build = charvec_from_cstring("");

  K key = charvec_from_cstring("1");
  work_push(key);

  I printed = 0;

  S blurb = "ymdhis";

  DO(strlen(blurb), 

    kC(key)[0] = blurb[i];
    K0 o;
    K k = LOOKUP_(x, key, o);

    if(k)
    {
      if(printed > 0)
      {
        build = cow_join(build, kcv("")); //want a separator later?
      }

      K child = NULL;

      child = shout(k, depth + 1, 0, true, false, true);
      build = cow_join(build, child);
      rd(child);

      child = shout(key, depth + 1, 0, true, false, true);
      build = cow_join(build, child);
      rd(child);
                   
      printed++;
    }
  )

  work_pop_rd(true);

  return build;
}

K easy_show_dtbucket(K x)
{
  K a = shout_dtbucket(x, 0);
  easy_show2(a, false, 0, false);
  rd(a);

  return x;
}

K shout_float(F f, I force_decimal)
{
  char b[64] = {0};
  I bn = sizeof(b);

  if(isnan(f))
  {
    return charvec_from_cstring("NaN");
  }
  else if(isinf(f))
  {
    if(f>0) return charvec_from_cstring( "Infinity");
    else    return charvec_from_cstring("-Infinity");
  }
  else if(force_decimal && 0==f-trunc(f))
  {
    snprintf(b, bn, "%.1f", f);
    return charvec_from_cstring(b);
  }
  else
  {
    snprintf(b, bn, "%g", f);
    return charvec_from_cstring(b);
  }
}

K pretty_table_add_border_topper(K build, K widths)
{
  //POTENTIAL_REFACTORING_POINT
  //can unify the three border methods by passing a quadruple of the four strings

  //┌─┬─────┬───┬────────┐
  build = cow_join(build, kcv("┌"));
  ENUM(widths, DO2(vi, build = cow_join(build, kcv("─"))) if(i < _i-1) build = cow_join(build, kcv("┬")))
  build = cow_join(build, kcv("┐"));
  return build;
}

K pretty_table_add_border_middle(K build, K widths)
{
  //├─┼─────┼───┼────────┤
  build = cow_join(build, kcv("├"));
  ENUM(widths, DO2(vi, build = cow_join(build, kcv("─"))) if(i < _i-1) build = cow_join(build, kcv("┼")))
  build = cow_join(build, kcv("┤"));
  return build;
}

K pretty_table_add_border_bottom(K build, K widths)
{
  //└─┴─────┴───┴────────┘
  build = cow_join(build, kcv("└"));
  ENUM(widths, DO2(vi, build = cow_join(build, kcv("─"))) if(i < _i-1) build = cow_join(build, kcv("┴")))
  build = cow_join(build, kcv("┘"));
  return build;
}

volatile K shout_pad(K string, I width, I align, I force_width)
{
  K build = NULL;

  I n = COUNT(string);

  if(force_width != -1) n = force_width;

  I missing = MAX(0, width - n);

  SW(align)
  {
    CD:
    CS(ALIGN_LEFT,   build = ex("$1 join $2 take ' '", string, ki(missing)))
    CS(ALIGN_RIGHT,  build = ex("($2 take ' ') join $1", string, ki(missing)))
    CS(ALIGN_CENTER, build = ex("((floor $2/2) take ' ') join $1 join (ceil $2/2) take ' '", string, ki(missing)))
  }

  return build;
}

K pretty_table_add_row(K build, K widths, K row, I align, I force_width, I depth, I tick, I escape, I full)
{
  build = cow_join(build, kcv("│"));

  ENUM(widths, 

    K0 o;
    K val = AT2(row, ki(i), o);
    K string = shout(val, depth + 1, false, false, full, false);

    K padded = shout_pad(string, vi, align, force_width);

    build = cow_join(build, padded);
    if(i < _i-1) build = cow_join(build, kcv("│"));

    rd(string);
    rd(padded);
  )
  build = cow_join(build, kcv("│"));

  return build; 
}


K shout_map(K x, I depth, bool tick, bool escaped, bool full, bool indent)
{
  I MAX_ITEMS = II;
  if(!full)
  {
    MAX_ITEMS = 40;
  }

  if(IS_ERROR(x)) return shout_error_map(x);
  if(IS_DTBUCKET(x))return shout_dtbucket(x, depth + 1); 
  K build = charvec_from_cstring("{");
  ENUM(x,  if(i>0)build = cow_join(build, kcv(", "));
           if(i>MAX_ITEMS){build = cow_join(build, kcv("..."));break;} 
           if(IS_TABLE(v)) build = cow_join(build, kcv("\n"));

           K child = NULL;
           
           child = shout(u, depth, false, escaped, full, indent);
           build = cow_join(build, child);
           rd(child);

           build = cow_join(build, kcv(":"));

           child = shout(v, depth, true, escaped, full, indent);
           build = cow_join(build, child);
           rd(child);
  )
  build = cow_join(build, kcv("}"));
  return build;
}

K shout(K x, I depth, bool tick, bool escaped, bool full, bool indent)
{
  //This function produces a charvec but would could change it to stream to a pipe
  //(and capture that output to a charvec)
  //This would allow us to write really large objects in pretty-printed form bypassing memory
  //though doubtful we'll ever need that

  if(!full && depth > SHOW_MAX_DEPTH)
  {
    return charvec_from_cstring("...");
  }

  I MAX_ITEMS = II;
  if(!full)
  {
    MAX_ITEMS = 40;
  }

  char b[64] = {0};
  I bn = sizeof(b);

  if(GET_ATTR(x,ATTR_BYTES))
  {
    K build = charvec_from_cstring("0x");
    ENUM(x, 
            if(MAX_ITEMS < II && i>MAX_ITEMS)
            {
              build = cow_join(build, kcv("..."));
              break;
            }
    
            snprintf(b, bn, "%02x", (UC)vc);
            build = cow_join(build, kcv(b));
    )
    return build;
  }

  if(IS_VECTOR(x) && 0==COUNT(x))
  {
    S s = NULL;

    SW(xt)
    {
      CS(CHARVEC,  s = tick ? "\"\"" : "")
      CS(INTVEC,   s = "INT[]")  
      CS(FLOATVEC, s = "FLOAT[]")
      CS(STAMPVEC, s = "STAMP[]")
    }

    if(s) return charvec_from_cstring(s);
  }

  SW(xt)
  {
     CS(LINK,  return charvec_from_cstring("LINK"))
     CS(-LINK, K build = charvec_from_cstring("LINK["); 
               ENUM(x, if(i>0) build = cow_join(build, kcv(", "));
                       snprintf(b, bn, "%lld", (I)sk);
                       build = cow_join(build, kcv((S)&b));
               )
               build = cow_join(build, kcv("]"));
               return build;
     )
    CSF(STAMPVEC,)
    CSF(FLOATVEC,)
     CS(INTVEC,  K build = charvec_from_cstring("["); 
                 ENUM(x, if(i>0) build = cow_join(build, kcv(", "));
                         if(i>MAX_ITEMS)
                         {
                           build = cow_join(build, kcv("..."));
                           break;
                         }

                         K child = NULL;
                         
                         bool last = (i>=cx-1) || (i >= MAX_ITEMS);
                         if(FLOATVEC == xt) child = shout_float(vf, last);
                         else child = shout(v, depth + 1, tick, escaped, full, indent);

                         build = cow_join(build, child);
                         rd(child);
                 )
                 build = cow_add(build, kc(']'));
                 return build;
     )
     CS(CHARVEC, K build = charvec_from_cstring("");

                 if(tick) build = cow_join(build, kcv("\""));
                 ENUM(x,  if(MAX_ITEMS < II && i>5*MAX_ITEMS)
                          {
                            build = cow_join(build, kcv("..."));
                            break;
                          }

                          if(escaped)
                          {
                            escapebuf buf = escapebuf_for_char(vc);
                            build = cow_join(build, kcv((S)&buf));
                          }
                          else
                          {
                            if(1 && vc=='\n')
                            {
                                 build = cow_add(build, kc(' '));
                            }
                            else build = cow_add(build, v);
                          }
                 )
                 if(tick) build = cow_join(build, kcv("\""));
                 return build;
     ) 
     CS(CHAR, escapebuf buf = escapebuf_for_char(xc); 
              snprintf(b, bn, "`\"%s\"", (S)&buf);
              return charvec_from_cstring(b);
     )
     CS(INT, SW(xi)
             {
               CR( II, charvec_from_cstring( "INFINITY"))
               CR(-II, charvec_from_cstring("-INFINITY"))
               CR( IN, charvec_from_cstring( "NAN"))
               CD: snprintf(b, bn, "%lld", xi);
                   return charvec_from_cstring(b);
             }
     )
     CS(FLOAT, return shout_float(xf, true))
     CS(STAMP, stampbuf buf = buf_from_stamp(x,false,3);
               return charvec_from_cstring((S)&buf)
     )
     CS(NIL,   return charvec_from_cstring("null"))

     CS(BTREE, K build = charvec_from_cstring("=");
               K child = shout(xKeys, depth+1, tick, escaped, full, indent);
               build = cow_join(build, child);
               rd(child);
               return build;
     )

    CSF(ZIP,  )
    CSF(HASH, )
     CS(LIST, K build = charvec_from_cstring(xt==ZIP?"COMPRESSED[":xt==HASH?"#[":"[");
              ENUM(x, 
                      if(i>0)
                      {
                        build = cow_join(build, kcv(", "));
                        if(i>MAX_ITEMS)
                        {
                          build = cow_join(build, kcv("..."));
                          break;
                        }

                        bool indents_lists = indent;
                        if(indents_lists && !IS_SATOM(v))
                        {
                          build = cow_join(build, kcv("\n"));
                          DO(depth, build = cow_join(build, kcv(" ")))
                        }
                      }
                      K child = shout(v, depth + 1 , tick, escaped, full, indent); 
                      build = cow_join(build, child);
                      rd(child);
              )
              build = cow_join(build, kcv("]"));
              return build;
     )
    CS(ATLAS, I mapcount = COUNT(x);

              if(0 == mapcount) return charvec_from_cstring("{[]}");
              K build = charvec_from_cstring("atlas[");
              
              DO(mapcount, 
                      if(i>0)
                      {
                        build = cow_join(build, kcv(", "));
                        if(i>MAX_ITEMS)
                        {
                          build = cow_join(build, kcv("..."));
                          break;
                        }
                        bool indents_atlases = true;
                        if(indents_atlases)
                        {
                          build = cow_join(build, kcv("\n"));
                          DO(depth, build = cow_join(build, kcv(" ")))
                        }
                      }
                      K v = simple_at(x, ki(i)); 
                      K child = shout(v, depth + 1 , tick, escaped, full, indent); 
                      build = cow_join(build, child);
                      rd(child);
                      rd(v);
              )
              build = cow_join(build, kcv("]"));
              return build;

     )
    CS(FUNC, SW(x->nk)
             {
               CS(FUNC_KIND_NULL,         return charvec_from_cstring("{NULL}"))
              CSF(FUNC_KIND_DYNAMIC_LIB, 
              
                 K build = charvec_from_cstring("{OBJECT:");

                 K child = shout(kN(x,FUNC_TEXT), depth, false, escaped, full, indent);
                 build = cow_join(build, child);
                 rd(child);

                 build = cow_join(build, kcv("}"));

                 return build;
              )
               CS(FUNC_KIND_DERIVED_VERB, return shout(kN(x,FUNC_TEXT), depth, false, escaped, full, indent))
               CS(FUNC_KIND_LAMBDA, 

                 K build = charvec_from_cstring("{[");
                 DO(kN(x,FUNC_TOTAL_ARGS)->i, 
                    if(i>0) build = cow_join(build, kcv(", ")); 
                    K0 o;
                    K child = shout(AT2(kN(kN(x,FUNC_ARGLOCALS),KEYS), ki(i),o), depth, false, escaped, full, indent); 
                    build = cow_join(build, child);
                    rd(child);
                 )
                 build = cow_join(build, kcv("]"));

                 K child = shout(kN(x,FUNC_TEXT), depth, false, false, true, indent);
                 if(0==child->n)
                 {
                   child = cow_join(child,kcv(" null")); 

                 }
                 build = cow_join(build, child);
                 rd(child);

                 //O("%lld; ",(I)x);
                 //easy_show(kN(x,FUNC_BYTECODE));
                 //O(" ; ");
                 //easy_show(kN(x,FUNC_CONSTANTS));
                 build = cow_join(build, kcv("}"));

                 if(kN(x, FUNC_UNFILLED_ARGS)->i != kN(x,FUNC_TOTAL_ARGS)->i )
                 {
                   build = cow_join(build, kcv("["));
                   K z = kN(kN(x,FUNC_ARGLOCALS),VALUES);
                   DO(kN(x,FUNC_TOTAL_ARGS)->i, 

                      K0 o;
                      K child = shout(LOOK_(z,i,o), depth + 1, tick, escaped, full, indent);
                      build = cow_join(build, child);
                      rd(child);

                      if(i< _i-1)build = cow_join(build, kcv(","));
                   )
                   build = cow_join(build, kcv("]"));
                 }
                 return build;
               )
               CD: return charvec_from_cstring("{nyi-print-function}");
             }
    )
    CS(PARTABLE,
      K first = ex("first $1['domain_values']", x);

      K domain = ex("$1['domain']", x);
      domain = cow_ensure_null_terminated_chars(domain);

      C query[256];
      snprintf(query, sizeof(query), "select from $1 where %s = $2", kC(domain));

      rd(domain);

      work_push(first);

      K s = ex(query, x, first);
      work_pop_rd(true);

      work_push(s);
      K build = shout(s, depth, tick, escaped, full, indent);
      work_pop_rd(true);
      return build;
    ) 
    CS(MAP, return shout_map(x, depth, tick, escaped, full, indent))
    CS(TABLE,

      I actual_columns = table_columns(x);

      if(!indent || 0 == actual_columns)
      {
        K maplike = shout_map(x, depth, tick, escaped, full, indent);
        K build = ex("'{' join $1 join '}'", maplike);
        rd(maplike);
        return build;
      }
      
      K build = charvec_from_cstring("");

      if(depth > 0) build = cow_join(build,kcv("\n"));

      I actual_rows = table_rows(x);
      I rows = MIN(actual_rows, The_Print_Table_Rows_Per_Page);

      bool incomplete = (rows < actual_rows);
      
      //don't kcv("...") this because -Os on clang can't handle it
      S ellipse = "⋮"; K continued = charvec_from_cstring(ellipse); I continued_display_width = 1; //fudge width
      work_push(continued);

      I base_width = 1; //pad to 1 to avoid 0 case
      if(incomplete) base_width = continued_display_width; //for ellipse
      K widths = new_k(INTVEC, actual_columns);
      work_push(widths);
      DO(widths->n, kI(widths)[i] = base_width) 

      ENUM(x, 
      
        //C a = table_column_attributes(x,u);
        //if(a&COLUMN_ATTR_KEY)O("*");
        //if(a&COLUMN_ATTR_FKEY)O("&");
      
        //Take maxes of widths and column header lengths
        K child = shout(u, depth + 1, false, false, full, false);  
        kI(widths)[i] = MAX(kI(widths)[i], child->n);
        rd(child);

        //Take maxes of widths and column values at each row
        K column = v;
        DO2(rows, K0 o; K val = AT2(column, ki(j),o);
          K child = shout(val, depth + 1, false, false, full, false);  
          kI(widths)[i] = MAX(kI(widths)[i], child->n);
          rd(child);
        )
      ) 

      //┌─┬─────┬───┬────────┐
      //│a│price│vol│name    │
      //├─┼─────┼───┼────────┤
      //│0│  0.2│110│    AAAA│
      //│1│  2.2│121│  EEEEEE│
      //│2│  4.2│132│ABCDEFGH│
      //└─┴─────┴───┴────────┘

      build = pretty_table_add_border_topper(build, widths);
      build = cow_join(build, kcv("\n"));
      build = pretty_table_add_row(build, widths, xKeys, ALIGN_LEFT, -1, depth, tick, escaped, full);
      build = cow_join(build, kcv("\n"));
      build = pretty_table_add_border_middle(build, widths);
      build = cow_join(build, kcv("\n"));

      DO(rows, K row = table_row_as_vlist(x,i); 
               work_push(row);
               build = pretty_table_add_row(build, widths, row, ALIGN_RIGHT, -1, depth, tick, escaped, full);
               work_pop_rd(true);
               build = cow_join(build, kcv("\n"));
      )

      if(incomplete)
      {
        //K continuers = repeat(ki(actual_columns), continued); 
        K4 o;
        K continuers = take(ki(actual_columns), klist1(continued, o)); 
        work_push(continuers);

        I funny_width = continued_display_width;
        build = pretty_table_add_row(build, widths, continuers, ALIGN_RIGHT, funny_width, depth, tick, escaped, full);
        build = cow_join(build, kcv("\n"));
        work_pop_rd(true);
      }
      build = pretty_table_add_border_bottom(build, widths);

      work_pop_n_rd(2, true);

      return build;
    )
  }

  return strong(kcv("nyi-printable"));
}



K easy_show2(K x, I tick, I depth, bool escaped)
{

//POTENTIAL_FEATURE_POINT
//1. we could inject into the libedit api code something to capture ESCAPE or TAB
//   or whatever key in order to show the next 'page' of a[n elided] table
//2. use a simple global int var or something to keep track of page of last
//   displayed table. and use whatever's set for #rows to show

#ifdef DEBUG
if(di(x, false))
{
  fprintf(stderr, "Debug Warning: printing-to-terminal object failed sanity check.\n");
}
#endif

K build = shout(x, depth, tick, escaped, false, true);
ENUM(build, O("%c", vc))
rd(build);
return x;

  //if(IS_VECTOR(x) && 1==cx && !IS_CHARVEC(x)) O(",");

  I MAX_ITEMS = 25;

  if(GET_ATTR(x,ATTR_BYTES))
  {
    O("0x");
    ENUM(x,O("%02x",(UC)vc))
    return x;
  }

  SW(xt)
  {
     CS(BTREE,O("="); easy_show2(xKeys, tick, depth+1, escaped))
    CSF(HASH, O("#"))
     CS(LIST, 
              O("["); //O(1==xn?"":"[");
              ENUM(x, 
                      if(i>0)
                      {
                        O(", ");
                        if(i>MAX_ITEMS){O("...");break;}
                        bool indents_lists = true;
                        if(indents_lists && !IS_SATOM(v))
                        {
                          O("\n");
                          DO(depth,O(" "))
                        }
                      }
                      easy_show2(v,tick,depth+1, escaped);)
              O("]"); //O(1==xn?"":"]");
       )
     CS(-STAMP, if(!cx)O("STAMP[]");else{ O("["); ENUM(x, if(i>0)O(", ");if(i>MAX_ITEMS){O("...");break;}easy_show(v))O("]"); })
     CS(-FLOAT, if(!cx)O("FLOAT[]");else{ O("["); ENUM(x, if(i>0)O(", ");if(i>MAX_ITEMS){O("...");break;}show_float(vf, i>=cx-1);)O("]"); })
     CS(-INT,   if(!cx)O("INT[]");  else{ O("["); ENUM(x, if(i>0)O(", ");if(i>MAX_ITEMS){O("...");break;}easy_show(v)) O("]"); })
     CS(-CHAR,  if(!cx && tick)O("\"\""); 
                else
                {
                      if(tick)O("\"");
                      ENUM(x, if(i>2*MAX_ITEMS){O("...");break;}
                        if(escaped)
                        {
                          escapebuf buf = escapebuf_for_char(vc);
                          O("%s", (S)&buf);
                        }
                        else
                        {
                          if(vc=='\n')O(" ");
                          else O("%c", vc);
                        }
                      )
                      if(tick)O("\"");
                }
     ) 
     CS( CHAR, escapebuf buf = escapebuf_for_char(xc); O("`\"%s\"", (S)&buf))
     CS( INT, SW(xi){CS(II,O("INFINITY"))CS(-II,O("-INFINITY"))CS(IN,O("NAN"))CD:O("%lld",xi);})
     CS( FLOAT, show_float(xf,true) )
     CS( STAMP, stampbuf buf = buf_from_stamp(x,false,3); O("%s",(S)&buf))
     CS( MAP, if(IS_ERROR(x)) return easy_show_error_map(x);
              if(IS_DTBUCKET(x))return easy_show_dtbucket(x); 
              O("{"); 
              ENUM(x,if(i>0)O(", ") ; 
                     if(i>MAX_ITEMS){O("...");break;} 
                     easy_show2(u,0,depth,escaped);
                     O(":");
                     easy_show(v);)
              O("}"))
     CS( NIL, O("null"))
     CS( FUNC, 
               SW(x->nk)
               {
                 CS(FUNC_KIND_NULL, O("{NULL}"))
                 CS(FUNC_KIND_DERIVED_VERB, easy_show2(kN(x,FUNC_TEXT),false,depth,escaped))
                 CD:
                   O("{[");
                   K0 o;
                   DO(kN(x,FUNC_TOTAL_ARGS)->i, if(i>0)O(", "); easy_show2(AT2(kN(kN(x,FUNC_ARGLOCALS),KEYS), ki(i), o), false, depth, escaped); )
                    
                   O("]");
                   easy_show2(kN(x,FUNC_TEXT),false,depth,escaped);
                   //O("%lld; ",(I)x);
                   //easy_show(kN(x,FUNC_BYTECODE));
                   //O(" ; ");
                   //easy_show(kN(x,FUNC_CONSTANTS));
                   O("}");
               }

              if(x->nk == FUNC_KIND_LAMBDA)
              {
                if(kN(x, FUNC_UNFILLED_ARGS)->i != kN(x,FUNC_TOTAL_ARGS)->i )
                {
                 O("[");
                 K z = kN(kN(x,FUNC_ARGLOCALS),VALUES);
                 DO(kN(x,FUNC_TOTAL_ARGS)->i, K0 o; easy_show(LOOK_(z,i,o)); if(i< _i-1)O(","))
                 O("]");
                }
              }
               
     )  
     CS(ATLAS, O("{[atlas_put]}");) 
     CS(TABLE, 
                I actual_columns = table_columns(x);
                if(0==actual_columns)
                {
                  O("{{}}");
                  break;
                }

                I actual_rows = table_rows(x);
                I rows = MIN(actual_rows,The_Print_Table_Rows_Per_Page);

      O("\n");
      O("|");
      ENUM(xKeys, C a = table_column_attributes(x,u);
                  if(a&COLUMN_ATTR_KEY)O("*");
                  if(a&COLUMN_ATTR_FKEY)O("&");
                  easy_show2(v,false,depth,escaped);if(i<_n)O("|")  ) 
      O("\n");
      DO(rows, K k = table_row_as_vlist(x,i); easy_show2(k,0,depth+1,escaped); rd(k);if(i< _i-1)O("\n");)
      //for table printing really what you want to do,
      //is select the print limit from each column, then flip(),
      //print, and free
      if(actual_rows > The_Print_Table_Rows_Per_Page) O("\n...");
     )
     CS(-LINK, O("LINK["); ENUM(x, if(i>0)O(", "); O("%lld", (I)sk) ) O("]");)
     CD:    O("nyi"); break;
  }

  if(TEST_MACROS)
  {
    //Print indicator for ATTR_SORTED arrays
    if(1 && (0||IS_ARRAY(x)) && SORTED(x) && ECOUNT(x)>1 && !IS_CHARVEC(x))
    {
      O("<");
    }
  }

  if(TEST_TRACK_SORT_ATTR)
  {
    test_incorrect_sort_attr(x);
  }

  R x;
}

void print_timing(F elapsed)
{
  fflush(stdout);
  fprintf(stderr, "\n");
  fprintf(stderr, "    ");

  I minutes = elapsed / 60;
  I modseconds = ((I)elapsed)%60; 

  if(elapsed < pow(10,-6)) 
  {
    //fprintf(stderr, "%.0f ns", elapsed * pow(10,9));
    fprintf(stderr, "0 ms");
  }
  else if(elapsed < pow(10,-3)) 
  {
    //fprintf(stderr, "%.0f µs", elapsed * pow(10,6));
    fprintf(stderr, "0 ms");
  }
  else if(elapsed < 1.0) 
  {
    fprintf(stderr, "%.0f ms", elapsed * pow(10,3));
  }
  else if (elapsed < 60.0)
  {
    fprintf(stderr, "%.1f s", elapsed);
  }
  else
  {
    fprintf(stderr, "%lld m %02lld s", minutes, modseconds);
  }

  fprintf(stderr, "\n");
}

void _play(S text, S out_prefix, bool timing)
{
  kerf_init();
    
  K snippet = charvec_from_cstring(text);

  work_push(snippet);

  K result = NULL;
  
  F elapsed = 0.0;

  if(USE_NS_CLOCK_FOR_TIMING)
  {
    NRTIME(elapsed, result = interpret(snippet))
  }
  else
  {
    RTIME(elapsed, result = interpret(snippet))
  }

  if(result)
  {
    work_pop(); rd(snippet);

    if(!IS_ERROR(result) && (!IS_TABLE(result) || table_columns(result) == 0) && !The_Clearing_Term_Flag ) O("%s", out_prefix);

    if(!IS_NIL(result))
    {
      if(TABLE == result->t)
      {
        populate_table_printing_feature(result);
      }

      if (PRINT_ASSIGNMENT_RESULTS || !The_Last_Expression_Was_Assignment_Flag)
      {
        show(result);
      }
    }

    if(timing && The_Timing && !The_Clearing_Term_Flag) print_timing(elapsed);

    rd(result);
  }
  else
  {
    if(IS_DEFINED(DEBUG))
    {
      er(Warning: play did not produce non-null result);
    }
  }
}

void play(S text)
{
  _play(text, "", false);
}

#pragma mark - 

K rep(K x)//ouput representation
{
  K build = shout(x, 0, true, false, true, true);
  return build;
}

K xout(K x)
{
  if(IS_STRING(x))
  {
    DO(xn, O("%c", xC[i]))
  }

  fflush(stdout);

  return Kn();
}

K xdisplay(K x)
{
  return ex("out (rep $1) # '\n'", x);
}


#pragma mark - Table Printing Feature
void try_table_printing_feature()
{

  I cap = MIN(PRINT_TABLE_MAX_PAGES_TO_SHOW, The_Print_Table_Available_Pages);

  if(The_Print_Table_Current_Page < cap)
  {
     K k = The_Print_Table_Pages[The_Print_Table_Current_Page];

     if(k) 
     {
       //VT100 terminal codes
       DO(1,O("\033[1A");)
       DO(1,O("\033[2K");)

       K build = shout(k, 0, true, true, false, true);
       DO(build->n, O("%c", kC(build)[i]))
       rd(build);
       printf("  Page: %lld/%lld  Total rows: %lld", 2 + The_Print_Table_Current_Page, 1 + The_Print_Table_Available_Pages, The_Print_Table_Sampled_Row_Count);
       O("\n");
       fflush(stdout);
     }
  }

  //if(The_Print_Table_Current_Page >= The_Print_Table_Available_Pages)
  //{
  // er(table debug)
  //}

  The_Print_Table_Current_Page++;
}

void reset_table_printing_feature()
{
  DO(PRINT_TABLE_MAX_PAGES_TO_SHOW,
     K k = The_Print_Table_Pages[i];
     if(k) rd(k);
     The_Print_Table_Pages[i] = NULL;
  )

  The_Print_Table_Available_Pages = 0;
  The_Print_Table_Current_Page  = 0;
}

void populate_table_printing_feature(K x)
{
  assert(TABLE==xt);
  assert(The_Print_Table_Rows_Per_Page > 0);
  The_Print_Table_Sampled_Row_Count = table_rows(x);
  The_Print_Table_Available_Pages = MAX(0, ceil(table_rows(x) / (F)The_Print_Table_Rows_Per_Page) - 1);
  DO(PRINT_TABLE_MAX_PAGES_TO_SHOW, The_Print_Table_Pages[i] = page_for_table_at_index(x, i))
}

K page_for_table_at_index(K x, I i)
{
  I total = table_rows(x);

  I start = (1+i) * The_Print_Table_Rows_Per_Page;
  I end   = start + The_Print_Table_Rows_Per_Page;

  if(end < total) end++; //force ellipse
  else if(end >= total) end = total;

  return ex("$1[range($2,$3)]", x, ki(start), ki(end));
}

C ensure_has_char(K x, I index)
{
  if (index >= xn)
  {
    fprintf(stderr, "reached the end of the format string while processing.\n");
    ERROR(ERROR_FORMAT_STRING);
  }
  return xg[index];
}

K ensure_has_arg(K x, I index)
{
  if (index >= lenI(x))
  {
    fprintf(stderr, "not enough arguments for this format string.\n");
    ERROR(ERROR_FORMAT_STRING);
  }
  return at(x, ki(index));
}

K format_chunk_string(K arg, int width)
{
  K x = cow_ensure_null_terminated_chars(IS_STRING(arg) ? strong(arg) : rep(arg));
  S s = NULL;
  asprintf(&s, "%*s", width, xg);
  K r = charvec_from_cstring(s);
  free(s);
  rd(x);
  return r;
}

K format_chunk_int(K arg, int width, int places)
{
  I a = INTIFY(arg);
  S s = NULL;
  asprintf(&s, "%*.*lld", width, places, a);
  K r = charvec_from_cstring(s);
  free(s);
  return r;
}

K format_chunk_float(K arg, int width, int places)
{
  F a = FLOATIFY(arg);
  S s = NULL;
  asprintf(&s, "%*.*f", width, places, a);
  K r = charvec_from_cstring(s);
  free(s);
  return r;
}

K format(K x, K y)
{
  // the first argument must be a format string.
  // the second argument must be a list of format parameters.
  if (!IS_STRING(x) || !IS_VLIST(y)) { ERROR(ERROR_TYPE); }

  K z = charvec_from_cstring("");
  I arg_index = 0;
  work_push(z);

  for(I index = 0; index < xn; index++)
  {
    C here = xg[index];
    if (here == '%')
    {
      // handle each format character
      C next = ensure_has_char(x, ++index);

      // TODO: handle padding specification?
      // ie, 0 versus spaces, right/left alignment, etc.

      // handle width specification (optional)
      I width = 0;
      while(isdigit(next))
      {
        width = (width * 10) + (next - '0');
        next = ensure_has_char(x, ++index);
      }

      // handle precision (optional)
      I places = -1;
      if (next == '.')
      {
        places = 0;
        while(1)
        {
          next = ensure_has_char(x, ++index);
          if (!isdigit(next)) { break; }
          places = (places * 10) + (next - '0');
        }
      }

      SW(next)
      {
        CS('%', z = cow_add(z, kc('%')))
        CS('s',
          K a = ensure_has_arg(y, arg_index++);
          K c = format_chunk_string(a, width);
          z = cow_join(z, c);
          rd(c);
          rd(a);
        )
        CS('d',
          K a = ensure_has_arg(y, arg_index++);
          K c = format_chunk_int(a, width, places);
          z = cow_join(z, c);
          rd(c);
          rd(a);
        )
        CS('f',
          K a = ensure_has_arg(y, arg_index++);
          K c = format_chunk_float(a, width, places);
          z = cow_join(z, c);
          rd(c);
          rd(a);
        )
        CD:
          fprintf(stderr, "invalid format character '%c'.\n", next);
          ERROR(ERROR_FORMAT_STRING);
      }
    }
    else
    {
      // copy the character literally to output
      z = cow_add(z, kc(here));
    }
  }

  work_pop_rd(false); // return value
  return z;
}
