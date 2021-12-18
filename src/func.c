#include "kerf.h"

__kerfthread I last_verb_bytecode_position;
__kerfthread K last_verb_function;

#pragma mark - Compile / Assemble

K compile(K snippet, K directory)
{
  return compile_with_parent_args(snippet, directory, NULL, NULL, FUNC_KIND_LAMBDA, 0);
}

K compile_with_parent_args(K snippet, K directory, K parent, K args, C function_kind, C alt_attributes)
{
  K tokens   = NULL;
  K tree     = NULL;
  K function = NULL;

  tokens = lex(snippet);

  if(IS_ERROR(tokens))
  {
    return tokens;
  }

  work_push(tokens);

  tree = parse(snippet, tokens);

  work_pop_rd(true);

  if(IS_ERROR(tree))
  {
    return tree;
  }

  work_push(tree);

  function = assemble(snippet, tree, directory, parent, args, function_kind, alt_attributes);

  work_pop_rd(true);

  //TODO can function be NULL? Yes, until you fix it
  return function;
}

K assemble(K snippet, K tree, K directory, K parent, K args, C function_kind, C alt_attributes)
{
  K function = new_k(FUNC,FUNC_SIZE);

  //0==function->{n,na,nn,...} is reserved for "LIST arg null/unspecified" since 0==FUNC
  //see IS_EMPTY_ATOM()
  function->n  = 0;
  function->na = alt_attributes;
  function->nn = FUNC_SIZE;
  function->nk = function_kind;
  function->nf = 1; //dv arg floor. derived verb minimum execution requirements 
                    //2 for some such as (+)
                    //1 for ambiguities like (plus over)
  //If we leave this as one always, (+) still "works" in the sense that it
  //throws a valence error. I haven't turned this on for those. You can
  //get around it by using {lambdas}. Also breaks (+) fold

  //We added this attribute to handle the case
  //where an in-progress function throws an error
  //b/c one symbol is undefined but another is defined,
  //and the defined symbol was processed first but
  //never compile referenced (at the end of function making).
  //This would cause the compiled-refereced-counts to be
  //out of whack when the in-progress function was released
  //because you'd decrement that processed variable incorrectly
  OFF_ALT_ATTR(function, FUNC_ATTR_CREFED);

  //DIR 
  //  Usually passing the current working directory.
  //  We need to store the dir on the function so we can
  //  compile reference decrement relative globals later.
  //  Note: dirs should be like "", ".k", ".q.r" but not "."
  //    The reason being it breaks append logic.
  if(!directory) directory = copy(KVM->KERFDIR);
  else directory = copy(directory);

  //BYTECODE
  K bytecode = new_k(-CHAR,0);
  SET_ATTR(bytecode, ATTR_BYTES);

  //DEBUG
  K debug = new_k(-INT,0);

  //TEXT
  K text = strong(snippet);

  //CONSTANTS
  K constant_set = new_hashset();//or we could just append them without deduping *shrug*

  //GLOBALS
  K globals = new_map(); //names -> pairs
  //--> globals proceed similar to constants
  //later, compile globals is called

  //ARGLOCALS
  //Kerf closures bind by value, not by reference. Bound values not shared.
  K arglocals = new_map();//variables -> values
  I total = 0;
  I unfilled = 0;

  if(args)
  {
    ENUM(args, update(arglocals, v, kn); 
               total++;
               unfilled++;//for show. these are init'd to the same value.
        )
  }

  //if(total > 126) //TODO ERROR too many args (127 is signed char arg inf)

  //PARENT ARGLOCALS / CLOSURES
  if(parent)
  {
    //Inherit parent's arguments/local variable names. 
    //Note: This functions's ARGS are golden. Parent arglocals
    //that collide with ARGS here are ignored/skipped.
    //"Closures" are populated later, at runtime during CONSTANT_PUSH.
    K inheritance = kN(parent, FUNC_ARGLOCALS);
    ENUM(inheritance, insert_replace(arglocals, u, kn, false, false))
  }

  k0(function)[FUNC_VERSION]       = kf0(KERF_FUNC_VERSION);
  k0(function)[FUNC_BYTECODE]      = kl0(bytecode); 
  k0(function)[FUNC_DEBUG]         = kl0(debug);
  k0(function)[FUNC_TEXT]          = kl0(text); 
  k0(function)[FUNC_CONSTANTS]     = kl0(constant_set);//replaced by array later
  k0(function)[FUNC_DIRECTORY]     = kl0(directory);
  k0(function)[FUNC_GLOBALS]       = kl0(globals);  
  k0(function)[FUNC_ARGLOCALS]     = kl0(arglocals);
  k0(function)[FUNC_TOTAL_ARGS]    = ki0(total); //Args only. This is *not* ECOUNT(arglocals)
  k0(function)[FUNC_UNFILLED_ARGS] = ki0(unfilled);//Also no locals. Careful w/ [non_]empty_count()
  k0(function)[FUNC_LOCAL_ASSIGNS]                = K00;
  k0(function)[FUNC_EXPANSION01]                  = K00;
  k0(function)[FUNC_EXPANSION02]                  = K00;
  k0(function)[FUNC_EXPANSION03_LAST_DITCH_MAP]   = K00;

#define SAFE_BYTECODE (k0i(function,FUNC_BYTECODE)->k)
#define SAFE_DEBUG    (k0i(function,FUNC_DEBUG)->k)

  work_push(function);

  translate(tree, &SAFE_BYTECODE, &SAFE_DEBUG, function, parent, 0, 0);

  bytecode = k0i(function,FUNC_BYTECODE)->k;

  //HACK: PRE-WHERED check: upgrade last verb instructions to WHERE-optimized
  bool is_where_clause = GET_ALT_ATTR(function, FUNC_ATTR_SQL_WHERE);

  if(is_where_clause)
  {
    I lp = last_verb_bytecode_position;
    bool was_me = (function == last_verb_function);
    bool sane_position = lp >= 0 && lp < COUNT(SAFE_BYTECODE);
    I back = 1 + sizeof(I); //1==sizeof(return_code) plus sizeof(int args to verb)  (was 1 + 2, for compressed verb limited to <127)
    bool correct_position = (lp == (COUNT(SAFE_BYTECODE) - back)); //HACK - whatever the distance should be
    bool upgrade_where = is_where_clause && was_me && sane_position && correct_position;

    if(upgrade_where)
    {
      //HACK - assumes uncompressed version of VERB_CALL (here, a few lines above, and in the replacement writing half below)
      VM0 n = {.code=kC(SAFE_BYTECODE), .instruction_pointer=last_verb_bytecode_position+1};
      VM m = &n;
      I original = read_uncompressed_integer(m);

      VERB verb = VERB_DISPATCH[original];

      S whered = verb.whered;

      if(whered)
      {
        I replacement = lookup_verb_id_by_name_or_sym(whered);
        assert(-1 != replacement);
        I data = htonll(replacement);
        memcpy(kC(SAFE_BYTECODE)+ (last_verb_bytecode_position + 1), (C*)&data, sizeof(I));
        SET_ALT_ATTR(function, FUNC_ATTR_PRE_WHERED);
      }
    }
  }
 
  last_verb_bytecode_position = -1;
  last_verb_function = NULL;

  //Add final function bytecode call: RETURN
  add_code_byte(&SAFE_BYTECODE, &SAFE_DEBUG, (C)RETURN_CODE, 0);

  //From the hash set keep only the entries
  K constants = strong(kN(constant_set,KEYS));
  rd(constant_set);
  k0(function)[FUNC_CONSTANTS]     = kl0(constants);

  arglocals = kN(function, FUNC_ARGLOCALS);
  zero_list_payload(kN(arglocals,VALUES));//will fail if for some reason you ever do on IS_DISK

  int compilation = func_compile_globals(function, directory);

  if(compilation) goto failed;

succeed:
  work_pop();
  return function;

failed:
  work_pop();
  if(function) rd(function);
  return NULL;

#undef SAFE_BYTECODE
#undef SAFE_DEBUG
}

#pragma mark - Dynamically Loaded Functions

K new_func_for_dynamically_loaded_library(K library_name, K library_function_name, C argc, C alt_attributes)
{
  K function = new_k(FUNC,FUNC_SIZE);

  //0==function->{n,na,nn,...} is reserved for "LIST arg null/unspecified" since 0==FUNC
  //see IS_EMPTY_ATOM()
  function->n  = 0;
  function->na = alt_attributes;
  function->nn = FUNC_SIZE;
  function->nk = FUNC_KIND_DYNAMIC_LIB;
  function->nf = MAX(1, argc);

  //Dynamically loaded functions need storage for:
  // argcount of ptr   - keep in ->nf, various
  // function pointer  - store in GLOBALS->VALUES->PAIR(/VALUES[0])->INT(/PAIR[0])
  // library filepath  - GLOBALS->KEY[0]
  // function name     - TEXT
  //Reusing GLOBALS is useful here because we already:
  //1. Refresh it on transfer from network/disk
  //2. Avoid reference decrementing LINKs in pairs

  K text = strong(library_function_name);

  K globals = new_map();

  K pair = new_k(LIST,2);
  k0(pair)[0] = ki0((UI)NULL);//no reference increment
  k0(pair)[1] = ki0(0);   //not used

  globals = update(globals, library_name, pair);
  rd(pair);

  K directory = charvec_from_cstring("");

  K bytecode = new_k(CHARVEC, 0);
  K debug    = new_k(INTVEC,  0);

  add_code_byte(&bytecode, &debug, GLOBAL_DYLIB_CALL, 0);
  add_code_byte(&bytecode, &debug, (C)RETURN_CODE,       0);
  SET_ATTR(bytecode, ATTR_BYTES);

  k0(function)[FUNC_VERSION]       = kf0(KERF_FUNC_VERSION);
  k0(function)[FUNC_BYTECODE]      = kl0(bytecode);
  k0(function)[FUNC_DEBUG]         = kl0(debug);
  k0(function)[FUNC_TEXT]          = kl0(text);
  k0(function)[FUNC_CONSTANTS]     = kl0(new_k(LIST,0)); 
  k0(function)[FUNC_DIRECTORY]     = kl0(directory);
  k0(function)[FUNC_GLOBALS]       = kl0(globals);
  k0(function)[FUNC_ARGLOCALS]     = kl0(new_map()); 
  k0(function)[FUNC_TOTAL_ARGS]    = ki0(argc); //Args only. This is *not* ECOUNT(arglocals)
  k0(function)[FUNC_UNFILLED_ARGS] = ki0(argc);//Also no locals. Careful w/ [non_]empty_count()
  k0(function)[FUNC_LOCAL_ASSIGNS]                = K00;
  k0(function)[FUNC_EXPANSION01]                  = K00;
  k0(function)[FUNC_EXPANSION02]                  = K00;
  k0(function)[FUNC_EXPANSION03_LAST_DITCH_MAP]   = K00;

  int compilation = func_compile_globals(function, directory);

  if(compilation) goto failed;

succeed:
  return function;

failed:
  if(function) rd(function);
  return NULL;
}

K dlload(K filename, K function_name, K arg_count)
{
  I argc = INTIFY(arg_count);

  if(argc < 0 || DYNAMIC_FUNCTION_MAX_ARGS < argc) goto failed;

  //Mac OS X: cc -m64 -flat_namespace -undefined suppress -dynamiclib file.c -o file.dylib
  //Linux:    cc -shared -o dyn.so -fPIC dyn.c
  //Doesn't seem to work on Linux: cc -rdynamic -o foo foo.c -ldl
  K z = new_func_for_dynamically_loaded_library(filename, function_name, argc, 0);

  if(!z) goto failed;

success:
  return z;

failed:
  ERROR(ERROR_DYLIB);
  return Kn();
}

int func_compile_globals_dyload(K func)
{
  func_wipe_compiled_globals_no_decrement(func);

  K globals = kN(func, FUNC_GLOBALS);
  K keys    = kN(globals, KEYS);
  K values  = kN(globals, VALUES);

  S error = NULL;

  K0 o;
  K library_name = LOOK_(keys, 0, o);    //dynamic library filename path
  K func_name = kN(func, FUNC_TEXT); //library function name

  K x = copy(library_name);          
  K y = copy(func_name);

  x = cow_ensure_null_terminated_chars(x);
  y = cow_ensure_null_terminated_chars(y);

  work_push(x);
  work_push(y);

  //|RTLD_DEEPBIND would be nice when universally supported
  V library = dlopen(xC, RTLD_LAZY);//don't use LOCAL: won't get subfuncs on OSX
  error = dlerror();

  if(!library)  goto failed;
  if(error) goto failed;

  V opaque = dlsym(library, yC);

  error = dlerror();

  if(error) goto failed;

  K pair = new_k(LIST,2);
  k0(pair)[0] = ki0((UI)opaque);//no reference increment
  k0(pair)[1] = ki0(0);     //not used

  work_push(pair);
  nestneu(func, FUNC_GLOBALS, globals = update(globals, library_name, pair));

  work_pop_n_rd(3, true);

success:
  return 0;

failed:
  if(!library)
  {
    fprintf(stderr, "Error loading dynamic library %s \n", xC);
  }

  if(error)
  {
    fprintf(stderr, "Error loading dynamic library '%s' function '%s', issue: %s\n", xC, yC, error);
  }

  return -1;
}

K kerf_look(K z, I i) //sic, no inline, for rdynamic dylib methods
{
  return at(z, ki(i));
}

K kerf_monad_ex(K x, K y)
{
  return MONAD_EX(x,y); 
}

#pragma mark - Function Specific Methods

bool is_global_name_assignment(K x, K function, K parent)
{
  K arglocals = kN(function, FUNC_ARGLOCALS);

  bool start_dot  = cx>0 && *xC=='.';
  bool has_parent = (NULL != parent);

  bool globalized = false;

  //partially hacky?
  //mostly we want functions to operate locally...except
  //eg as "actual console lines" or "api calls"
  if(!has_parent)
  {
    globalized = true;
  }

//Alternatively:
//    if(GET_ALT_ATTR(function, FUNC_ATTR_GLOBALIZE))
//    {
//      globalized = true;
//    }

  return (start_dot || globalized);
}

bool is_global_name_constant(K x, K function, K parent)
{
  K arglocals = kN(function, FUNC_ARGLOCALS);

  bool start_dot  = cx>0 && *xC=='.';
  //bool parentless = (NULL == parent); //I think parentless is redundant here
  K0 o;
  bool localized  = (NULL != LOOKUP_(arglocals, x, o));

  return start_dot || !localized;
}

void func_wipe_compiled_globals_no_decrement(K func)
{
  //zero the references to maps without reference decrementing the maps
  K x = kN(func,FUNC_GLOBALS);
  K z = xValues;
  ENUM(z, nestset(z,i,kn)) // no  check because vectors
}

int recursive_func_compile_globals_after_read(K x, K directory)
{
  if(!x) return 0;

  int e = 0;

  SW(xt)
  {
   CSF(FUNC, if(IS_EMPTY_ATOM(x)) return 0;
             if(!directory)directory = kN(x,FUNC_DIRECTORY);
              
             e = func_compile_globals(x, directory);

             if(e) return e;
      ) 
    CD:
        if(IS_NEST(x))
        {
          NEST(x, e = recursive_func_compile_globals_after_read(v,directory); if(e)return e;)
        }
        break;
  }

  return 0;
}

int func_compile_globals(K func, K directory)
{
  SW(func->nk)
  {
    CS(FUNC_KIND_DYNAMIC_LIB, return func_compile_globals_dyload(func))
  }

  //The idea is that it should be safe to call this
  //any number of times/whenever on a function
  //to reinitialize its globals

  //dir - probably, the current working dir
  //      which need not be transmitted (to disk or over inet)
  //      but should be appended to locals without dots
  //      note: dirs should be like "", ".k", ".q.r" but not "."


  K x = kN(func,FUNC_GLOBALS);
  K y = xKeys;
  K z = xValues;

  nestset(func, FUNC_DIRECTORY, directory);

  //The globals' map's Values consist of pairs: LISTs of size 2
  //The first element is a special INT which must not be reference decremented.
  //The second element is a tenant INT atom we can write over.
  //Zeroing the LIST gets rid of both
  //Then nestset() handle reference decrementing the LIST
  //and removing it from the globals' map Values.

  //Clear existing globals ////////////////
  //This process clears any junk in a read (disk or inet) function
  //or a copied one.
  //This method is different from what you need for free'ing a function
  //you've already compiled. In that case you also need to decrement the
  //compiled ref write counts. See "rd()"
  func_wipe_compiled_globals_no_decrement(func);
  /////////////////////////////////////////

  K absolutes = func_absolute_longform_globals(func);
  work_push(absolutes);

  ENUM(absolutes, 

    K path = explode(kc('.'),v);
    work_push(path);
    K1 tuple = compiled_var_path_increment(The_Kerf_Tree, path);

    I p = tuple.v.i;

    if(IS_HASH_NULL(p))
    {
      work_pop();
      rd(path);
      goto failed;
    }

    K pair = new_k(LIST,2);
    k0(pair)[0] = ki0((UI)tuple.u);//sic, no reference increment
    assert(k0(pair)[0].t != LINK); //don't actually use LINK b/c it creates a nightmare everywhere else. overload INT
    k0(pair)[1] = ki0(p);

    nestset(z,i,pair);

    rd(pair);

    work_pop();
    rd(path);
  )

  work_pop();
  rd(absolutes);

  SET_ALT_ATTR(func, FUNC_ATTR_CREFED); //we could do this on a per-variable basis somehow...

  return 0;

failed:
  work_pop();
  rd(absolutes);
  return -1;//compilation failed, likely because of incompatible directory
}

K func_absolute_longform_globals(K func)//globals names with dir prepended, as necessary
{
  K x = kN(func,FUNC_GLOBALS);
  K y = xKeys;

  K directory = kN(func, FUNC_DIRECTORY);

  K list = new_k(LIST,0);

  ENUM(xKeys, 
  
    K absolute = NULL;

    if (*vC == '.') absolute = strong(v); //".g.a" case: absolute already
    else
    {
      I n = directory->n + 1 + cv; //"g.a" case: ".dir.a" + "." + "g.a"
      absolute = new_k(-CHAR, n);

      //strncpy(kC(absolute), kC(directory), COUNT(directory));
      //strncpy(kC(absolute)+directory->n, ".", 1);
      //strncpy(kC(absolute)+directory->n+1, vC, cv);
      absolute->n = 0;
      ENUM(directory, absolute = cow_add(absolute, v))
      //ENUM(kc('.'), absolute = cow_add(absolute, v))
              absolute = cow_add(absolute, kc('.'));
      ENUM(v, absolute = cow_add(absolute, v))
    }

    list = cow_add(list, absolute);
    rd(absolute);
  )

  return list;
}

#pragma mark - Translate to Bytecode

void translate(K tree, K *bytecode, K *debug, K function, K parent, I apply, I argc)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);
  I kind     = AT2(tree, ki(TOKEN_KIND),o2)->i;

  SW(kind)
  {
    CS(TOKEN_GROUP_PLAIN,          translate_plain     (tree, bytecode, debug, function, parent))
    CS(TOKEN_GROUP_SEPARATION,     translate_separation(tree, bytecode, debug, function, parent))

    CS(TOKENS_RETURN,              translate_return    (tree, bytecode, debug, function, parent))

   CSF(TOKENS_VERB_SYM, )
    CS(TOKENS_VERB_WORD,           translate_verb      (tree, bytecode, debug, function, parent, apply, argc))

    CS(TOKEN_GROUP_VERBAL_NVA,     translate_verbal_nva(tree, bytecode, debug, function, parent, apply))
    CS(TOKEN_GROUP_VERBAL_NNA,     translate_verbal_nna(tree, bytecode, debug, function, parent, apply))
    CS(TOKEN_GROUP_VERBAL_VA,      translate_verbal_va (payload, tree, bytecode, debug, function, parent, apply, 0))
    CS(TOKEN_GROUP_VERBAL_NA,      translate_verbal_na (payload, tree, bytecode, debug, function, parent, apply, 0))

    CS(TOKEN_GROUP_ASSIGNMENT,     translate_assignment  (tree, bytecode, debug, function, parent, apply))

    CD:                            translate_noun      (tree, bytecode, debug, function, parent, apply); break;
  }
}

void translate_noun(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o1,o2;
  I kind     = AT2(tree, ki(TOKEN_KIND),o1)->i;
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o2)->i;

  SW(kind)
  {
    CS(TOKEN_GROUP_ROUND_PAREN,    translate_paren       (tree, bytecode, debug, function, parent))
    CS(TOKEN_GROUP_SQUARE_BRACKET, translate_list        (tree, bytecode, debug, function, parent))
    CS(TOKEN_GROUP_CURLY_BRACE,    translate_curly       (tree, bytecode, debug, function, parent))
    CS(TOKEN_GROUP_SQL,            translate_sql         (tree, bytecode, debug, function, parent))

    CS(TOKEN_GROUP_ALIKE,          translate_alike       (tree, bytecode, debug, function, parent))

    CS(TOKENS_REL_DATETIME,        translate_rel_datetime(tree, bytecode, debug, function, parent))
    CS(TOKENS_STRING,              translate_string      (tree, bytecode, debug, function, parent))
    CS(TOKENS_BACKTICK,            translate_backtick    (tree, bytecode, debug, function, parent))
    CS(TOKENS_NAME,                translate_name        (tree, bytecode, debug, function, parent))
    CS(TOKENS_RESERVED,            translate_reserved    (tree, bytecode, debug, function, parent))
    CS(TOKENS_SELF,                translate_self        (tree, bytecode, debug, function, parent))

   CSF(TOKEN_GROUP_BOUND_ROUND,)
    CS(TOKEN_GROUP_BOUND_SQUARE,   translate_bound       (tree, bytecode, debug, function, parent))

    CS(TOKEN_GROUP_CONTROL,        translate_control     (tree, bytecode, debug, function, parent, apply))

   CSF(TOKENS_ADVERB_WORD,) //could later do something (prob. have to move up outside of translate_noun)
    CD:  //Catch and throw error
         ERROR(ERROR_PARSE_UNKNOWN);
         break;
  }

  //Various cases where nouns need to be triggered, noun juxtaposition
  if(apply)
  {
    add_code_byte(bytecode, debug, apply, location); 
  }

}

void translate_control(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o1,o2,o3;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);
  K first = AT2(payload, ki(0),o2);
  I first_kind = AT2(first, ki(TOKEN_KIND),o3)->i;

  SW(first_kind)
  {
    CS(TOKENS_DEF,  translate_control_def   (tree, bytecode, debug, function, parent, apply))
    CS(TOKENS_DO,   translate_control_do    (tree, bytecode, debug, function, parent, apply))
    CS(TOKENS_WHILE,translate_control_while (tree, bytecode, debug, function, parent, apply))
    CS(TOKENS_FOR,  translate_control_for   (tree, bytecode, debug, function, parent, apply))
    CS(TOKENS_IF,   translate_control_if    (tree, bytecode, debug, function, parent, apply))
  }

}

void translate_control_def(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o[11] = {0};
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);

  K bound = AT2(payload, ki(1),o[3]);
  K curly = AT2(payload, ki(2),o[4]);

  I curly_start = AT2(curly, ki(TOKEN_SNIPPET_START),o[5])->i;

  K bound_items = AT2(bound, ki(TOKEN_PAYLOAD),o[6]);

  K name = AT2(bound_items, ki(0),o[7]);
  K arg_tree = AT2(bound_items, ki(1),o[8]);

  I name_kind = AT2(name, ki(TOKEN_KIND),o[9])->i;
  K name_payload = AT2(name, ki(TOKEN_PAYLOAD),o[10]);

  I debug_location = snippet_start;

  //if(TOKENS_NAME != name_kind);//TODO error (eg overwriting reserved verb name eg "plus" via bound round)

  //grep key other_end_14234234
  I other_end = snippet_end - 1; 
  while(other_end > curly_start + 1 && isspace(kC(text)[other_end - 1]))
  {
    other_end--;
  }

  K substring = *work_push(new_subarray(text, curly_start + 1, other_end));
  K string = *work_push(parse_string(name_payload));
  K argv = *work_push(parse_lambda_args(arg_tree));
  K derivation = *work_push(compile_with_parent_args(substring, directory, function, argv, FUNC_KIND_LAMBDA, 0));

  translate_constant(bytecode, debug, function, derivation, debug_location);
  add_code_byte(bytecode, debug, EMPTY_PUSH, snippet_end - 1);
  translate_constant(bytecode, debug, function, kk, debug_location);//push empty list

  //For now, just assign globally
  K globals = kN(function, FUNC_GLOBALS);
  I p = insert_replace(globals, string, kn, false, false);
  add_code_byte_op_compression(bytecode, debug, GLOBAL_ALTER, debug_location, p);

  work_pop_n_rd(4, true);
}

void translate_control_do(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o[6] = {0};
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;

  I location = snippet_start;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);

  K round = AT2(payload, ki(1), o[3]);
  K curly = AT2(payload, ki(2), o[4]);

  I curly_start = AT2(curly, ki(TOKEN_SNIPPET_START),o[5])->i;

  add_code_byte(bytecode, debug, ASIDE_OPEN,  snippet_start);
  add_code_byte(bytecode, debug, EMPTY_PUSH,  snippet_start);
  translate(round, bytecode, debug, function, parent, 0, 0);

//POTENTIAL_OPTIMIZATION_POINT
//Our DO() could be faster. One way may be to not
//reuse the ADVERB bytecode and so avoid overhead.
//(That is, write custom bytecode.)
//0. you can also redirect to the for() bytecode here, which is "optimized"
//1. one possibility is that our bytecode I is too slow, 
//   try to not copy byte by byte. read unaligned if possible.
//   or move I to constants outside of bytecode.
//2. Second possibility is that we need to dump all the adverb
//   bytecode and instead do something simpler with simple
//   JUMP IF and DECREMENT style instructions

  K list = new_k(LIST, 2);

  k0(list)[0] = kl0(new_k(CHARVEC,0));
  k0(list)[1] = kl0(new_k(INTVEC,0));

  work_push(list);

  K *local_bytecode = &kL(list, 0); 
  K *local_debug    = &kL(list, 1);
  SET_ATTR(*local_bytecode, ATTR_BYTES);

//add_code_byte(local_bytecode, local_debug, ASIDE_OPEN, snippet_start);
  add_code_byte(local_bytecode, local_debug, POP, snippet_start);
  translate_plain(curly, local_bytecode, local_debug, function, parent);
//add_code_byte(local_bytecode, local_debug, ASIDE_CLOSE, snippet_end - 1);

  K wrapper = *work_push(wrap_code_adverb(ADVERB_KIND_FOLD, local_bytecode, local_debug, function, parent, NILADIC, NULL, location));

  *bytecode = cow_join(*bytecode, kL(wrapper,0));
  *debug = cow_join(*debug, kL(wrapper,1));
  
  add_code_byte(bytecode, debug, ASIDE_CLOSE, snippet_end - 1);

  work_pop_n_rd(2, true);
}

void translate_control_while(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  //POTENTIAL_OPTIMIZATION_POINT
  //rewrite WHILE to use JUMP_IF style instructions
  //instead of adverb

  K0 o[8] = {0};

  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;

  I location = snippet_start;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);

  K round = AT2(payload, ki(1), o[3]);
  K curly = AT2(payload, ki(2), o[4]);

  I round_start = AT2(round, ki(TOKEN_SNIPPET_START),o[5])->i;
  I round_end =   AT2(round, ki(TOKEN_SNIPPET_END),o[6])->i;
  I curly_start = AT2(curly, ki(TOKEN_SNIPPET_START),o[7])->i;

  add_code_byte(bytecode, debug, ASIDE_OPEN,  snippet_start);
  add_code_byte(bytecode, debug, EMPTY_PUSH,  snippet_start);

  K substring  = *work_push(new_subarray(text, round_start + 1, round_end - 1));
  K derivation = *work_push(compile_with_parent_args(substring, directory, function, kk, FUNC_KIND_LAMBDA, 0));
  translate_constant(bytecode, debug, function, derivation, location);
  work_pop_n_rd(2,true);

  K list = new_k(LIST, 2);

  k0(list)[0] = kl0(new_k(CHARVEC,0));
  k0(list)[1] = kl0(new_k(INTVEC,0));

  work_push(list);

  K *local_bytecode = &kL(list, 0); 
  K *local_debug    = &kL(list, 1);
  SET_ATTR(*local_bytecode, ATTR_BYTES);

//add_code_byte(local_bytecode, local_debug, ASIDE_OPEN, snippet_start);
  add_code_byte(local_bytecode, local_debug, POP, snippet_start);
  translate_plain(curly, local_bytecode, local_debug, function, parent);

//add_code_byte(local_bytecode, local_debug, ASIDE_CLOSE, snippet_end - 1);

  K wrapper = *work_push(wrap_code_adverb(ADVERB_KIND_FOLD, local_bytecode, local_debug, function, parent, NILADIC, NULL, location));

  *bytecode = cow_join(*bytecode, kL(wrapper,0));
  *debug = cow_join(*debug, kL(wrapper,1));

  add_code_byte(bytecode, debug, ASIDE_CLOSE, snippet_end - 1);

  work_pop_n_rd(2, true);
}

void translate_control_for(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o[10] = {0};
  K payload = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[1])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[2])->i;

  K round  = AT2(payload, ki(1), o[3]);
  K curly  = AT2(payload, ki(2), o[4]);

  //if(COUNT(round)!=3) //TODO error. can catch earlier/higher if you want
  K round_payload = AT2(round, ki(TOKEN_PAYLOAD),o[5]);

  K initer =      AT2(round_payload, ki(0), o[6]);
  K whiler =      AT2(round_payload, ki(1), o[7]);
  K incrementer = AT2(round_payload, ki(2), o[8]);

  I debug_location = AT2(curly, ki(TOKEN_SNIPPET_START), o[9])->i;

  add_code_byte(bytecode, debug, ASIDE_OPEN, snippet_start);

  translate(initer, bytecode, debug, function, parent, 0, 0);

  I popper_begin = COUNT(*bytecode);
  add_code_byte(bytecode, debug, POP, snippet_start);
  I popper_end = COUNT(*bytecode);

  I whiler_begin = COUNT(*bytecode);
  translate(whiler, bytecode, debug, function, parent, 0, 0);
  I whiler_end = COUNT(*bytecode);

  I placeholder = 0;
  add_code_byte_op_flat(bytecode, debug, JMPRFIFN, debug_location, placeholder);
  
  I main_begin = COUNT(*bytecode);

  translate_plain(curly, bytecode, debug, function, parent);

  add_code_byte(bytecode, debug, POP, snippet_start);
  translate(incrementer, bytecode, debug, function, parent, 0, 0);

  I jmp_back = -1 + -( COUNT(*bytecode) - whiler_begin);

  add_code_byte_op_flat(bytecode, debug, JMPRB, debug_location, jmp_back);

  I end = COUNT(*bytecode);

  //Rewrite placeholder
  I instruction_width = 1;

  S target = kC(*bytecode) + whiler_end + instruction_width;
  I jmp_fwd_to_end = instruction_width + end - main_begin;
  I data = htonll(jmp_fwd_to_end);
  memcpy(target, &data, sizeof(data));

  //Replace this with whatever you wanted stored from earlier...
  add_code_byte(bytecode, debug, EMPTY_PUSH,  snippet_end - 1);

  add_code_byte(bytecode, debug, ASIDE_CLOSE, snippet_end - 1);
}

void translate_control_if(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o[5] = {0};
  K payload = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[1])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[2])->i;

  K rounds  = new_k(LIST, 0);
  K curlies = new_k(LIST, 0);

  //filter rounds and curlies
  ENUM(payload, 
    I kind = AT2(v, ki(TOKEN_KIND),o[3])->i; 
    SW(kind)
    {
      CS(TOKEN_GROUP_ROUND_PAREN, rounds  = cow_add(rounds,  v))
      CS(TOKEN_GROUP_CURLY_BRACE, curlies = cow_add(curlies, v))
    }
  )

  bool has_else_terminal = COUNT(curlies) > COUNT(rounds);
  //To simplify handling of ELSE, add a dummy IS_NIL
  if(has_else_terminal)
  {
    rounds = cow_add(rounds, kn);
  }

  add_code_byte(bytecode, debug, ASIDE_OPEN, snippet_start);

  K list = new_k(LIST, 3);

  k0(list)[0] = kl0(new_k(INTVEC,0));
  k0(list)[1] = kl0(new_k(INTVEC,0));
  k0(list)[2] = kl0(new_k(INTVEC,0));

  K *jump_nexts = &kL(list, 0);
  K *jump_ends  = &kL(list, 1);
  K *nexts      = &kL(list, 2);
  I end = 0;

  work_push(rounds);
  work_push(curlies);
  work_push(list);

  LIST2(rounds, curlies,

    if(!IS_NIL(u))
    {
      translate_plain(u, bytecode, debug, function, parent);
    }
    else
    {
      translate_constant(bytecode, debug, function, ki(true), snippet_start);
    }

    I curly_start = AT2(v, ki(TOKEN_SNIPPET_START),o[4])->i;

    *jump_nexts = cow_add(*jump_nexts, ki(COUNT(*bytecode)));
    I jmp_fwd = 0;
    add_code_byte_op_flat(bytecode, debug, JMPRBIFN, curly_start, jmp_fwd);

    translate_plain(v, bytecode, debug, function, parent);

    *jump_ends = cow_add(*jump_ends, ki(COUNT(*bytecode)));
    I jmp_end = 0;
    add_code_byte_op_flat(bytecode, debug, JMPRB, curly_start, jmp_end);

    I next  = COUNT(*bytecode);
    *nexts   = cow_add(*nexts, ki(next));
    end     = next;
  )

  LIST2(*jump_nexts, *nexts, 
    I next = vi;
    I forward = next - ui; 
    I data = htonll(forward);
    S target = kC(*bytecode)+ui+1;
    memcpy(target, &data, sizeof(data));
  )

  ENUM(*jump_ends, 
    I forward = end - ui; 
    I data = htonll(forward);
    S target = kC(*bytecode)+ui+1;
    memcpy(target, &data, sizeof(data));
  )

  add_code_byte(bytecode, debug, ASIDE_CLOSE, snippet_end - 1);

  work_pop_n_rd(3, true);
}

void translate_assignment(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o[14] = {0};
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);

  K x = payload;

  K target = AT2(payload, ki(0),o[3]);

  I target_kind = AT2(target, ki(TOKEN_KIND),o[4])->i;
  K target_charvec = AT2(target, ki(TOKEN_PAYLOAD),o[5]);

  bool is_name   = (TOKENS_NAME   == target_kind);
  bool is_string = (TOKENS_STRING == target_kind);

  K string = parse_string(target_charvec);
  work_push(string);

  K operator_tree = NULL;

  K colon = AT2(payload, ki(xn-1),o[6]);
  
  I debug_location = AT2(colon, ki(TOKEN_SNIPPET_START),o[7])->i;

  if(xn > 2)
  {
    K peek = AT2(payload, ki(xn-2),o[8]);
    I peek_kind = AT2(peek, ki(TOKEN_KIND),o[9])->i;

    if(TOKEN_GROUP_SQUARE_BRACKET != peek_kind)
    {
      operator_tree = peek;
    }
  }

  I bound_count = xn - 2 - (NULL!=operator_tree);

  //
  //Begin translation
  //

  if(operator_tree)
  {
    //This does, say, "negate:" and "minus:" but only dyadic -:
    //So... if you want monadic -: you'll have to build something that
    //tells derived verb how to do that. It seems like it's actually
    //pretty easy. Modify the signature below to accept the `apply` variable, 
    //(which we have here) then use it to fudge the dyad into a monad.
    //Maybe you can just tack a ":" or "\1" in front of the substring.
    //So that "+" becomes "+:" or "+\1". 
    I verb_code = -1;
    VERB verb = {0};
    bool has_adverb = false;
    I argc = 1;

    lookup_verb_data_via_token(operator_tree, &verb_code, &verb, has_adverb, !apply, argc);

    I min_args = verb.argc_range[0];
    I max_args = verb.argc_range[1];

    I start = AT2(operator_tree, ki(TOKEN_SNIPPET_START),o[10])->i;
    I end   = AT2(operator_tree, ki(TOKEN_SNIPPET_END),o[11])->i;
    translate_derived_verb(operator_tree, bytecode, debug, function, parent, min_args, max_args, start, end);
  }
  else
  {
    if(!apply)
    {
      //No following arg? Put a placeholder if we also don't have a verb
      //This just plays nice with cow_change later
      add_code_byte(bytecode, debug, EMPTY_PUSH, snippet_end - 1);
    }

    //Pass EMPTY_ATOM as Verb
    //This helps us in the emulator so we can pass NULL to cow_change
    add_code_byte(bytecode, debug, EMPTY_PUSH, snippet_end - 1);
  }

  add_code_byte(bytecode, debug, ARG_OPEN, snippet_start);
  DO(bound_count, K v = AT2(payload, ki(1+i),o[12]); translate(v, bytecode, debug, function, parent, 0, 0))
  add_code_byte(bytecode, debug, ARG_CLOSE, snippet_end - 1);

  K arglocals = kN(function, FUNC_ARGLOCALS);

  bool name_is_global = is_global_name_assignment(target_charvec, function, parent);
  bool localized = (NULL != LOOKUP_(arglocals, target_charvec, o[13]));
  bool is_global = (is_name && name_is_global) || (is_string && !parent) || (operator_tree && !localized) || (parent && !localized && bound_count > 0);
  bool is_local  = !is_global;

  if(is_global)
  {
    K globals = kN(function, FUNC_GLOBALS);
    I p = insert_replace(globals, string, kn, false, false);
    add_code_byte_op_compression(bytecode, debug, GLOBAL_ALTER, debug_location, p);
  }
  else if(is_local)
  {
    ENUM(string, if('.'==vc)goto fail)//TODO: Currently enforcing "flat" args/locals, 2 places
                                      // can't this just be an error?

    I p = insert_replace(arglocals, string, kn, false, false);
    add_code_byte_op_compression(bytecode, debug, ARGLOCAL_ALTER, debug_location, p);

    //This is a...expedient way to get this done now 2015.03.26
    //It's not the best way, which probably involves NOT using up an entire new
    //K0 on every function object This is fairly "separable" (func alt attr not
    //needed, the K0 not needed) in that you can rip it out (probably breaking
    //backwards compatibility), though the replacement methods are likely
    //slightly harder. You either need some functional way to track local
    //assigns, or you need some special bytecode (and potentially some
    //modification to the function frame such as a new object) in order to
    //reinstitute map/table specific constructors like {a:1,b:2}. You can't
    //merely use function locals because {[]f:2;{a:1,b:2}}0 is not right.
    //That's what prompted this fix in the first place.
    bool keyvaluey = GET_ALT_ATTR(function, FUNC_ATTR_keyvaluize);
    if(keyvaluey)
    {
      K assigns = kN(function, FUNC_LOCAL_ASSIGNS);

      if(IS_EMPTY_ATOM(assigns))
      {
        K hashset = new_hashset();
        k0(function)[FUNC_LOCAL_ASSIGNS] = kl0(hashset);
      }

      assigns = kN(function, FUNC_LOCAL_ASSIGNS);
      insert_replace_ri_rd(assigns, string, NULL, false, true, true, false);
    }

  }

  ///add_code_byte(bytecode, debug, SHOW, snippet_end - 1);

succeed:
  work_pop_n_rd(1, true);
  return;
fail:
  work_pop_n_rd(1, true);
  return;
}

void translate_name(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o;
  I start = AT2(tree, ki(TOKEN_SNIPPET_START),o)->i;

  //SQL clauses will cause an indirect, symbolic lookup via the 
  //actual string at runtime, into a provided table argument.
  //If that "fails" it falls back to the regular compiled reference
  bool sqly = GET_ALT_ATTR(function, FUNC_ATTR_SQL_CLAUSE);

  bool indirection = (0 || sqly || 0);

  I mark_jump = 0;
  I jump_forward_count = 0; //we'll rewrite this

  if(indirection)
  {
    add_code_byte(bytecode, debug, ASIDE_OPEN, start);
    I p = translate_string(tree, bytecode, debug, function, parent);
    I table_arg_index = 0;
    add_code_byte_op_compression(bytecode, debug, ARGLOCAL_PUSH, start, table_arg_index);

    if(ALPHA_DISK_REFCOUNT)
    {
      add_code_byte(bytecode, debug, SQL_TABLE_LOOKUP, start);
    }
    else
    {
      I verb_id = lookup_verb_id_by_name_or_sym("alter");
      assert(SENTINEL != verb_id);
      add_code_byte_op_flat(bytecode, debug, VERB_CALL, start, verb_id);
    }

    add_code_byte(bytecode, debug, ASIDE_CLOSE, start);

    add_code_byte_op_flat(bytecode, debug, SQL_PEEK_NAME, start, p); //A bit of a hack

    mark_jump = COUNT(*bytecode);
    add_code_byte_op_flat(bytecode, debug, JMPRBIFNOTNIL, start, jump_forward_count);
    add_code_byte(bytecode, debug, POP, start);//we skip/jump over this if the checked result was legit
  }

  bool creates = false;

  translate_direct_name(tree, bytecode, debug, function, parent, creates);

  if(indirection)
  {
    //go back and rewrite jump instruction
    I instruction_width = 1;
    jump_forward_count = COUNT(*bytecode) - mark_jump;

    I data = htonll(jump_forward_count);

    memcpy(kC(*bytecode) + mark_jump + instruction_width, &data, sizeof(data));
  }

}

void translate_direct_name(K tree, K *bytecode, K *debug, K function, K parent, bool creates)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o2)->i;

  //GLOBALS
  //1. If it starts with a dot it's a global
  //2. If it's outside a function (eg console, script) it's a global (NULL parent)
  //3. If it's inside a function, doesn't start with a dot, makes its first appearance
  //   as something other than total assignment (eg a push or other alter, say on sub-index), 
  //   and isn't yet defined in any ancestor function as a local, then it's a relative global
  //   The ancestor thing reduces to looking at parent's ARGLOCALS (parent arg counts)
  //   Later when it's assigned, the name becomes LOCAL; so we have to check for
  //   presence in the ARGLOCALS.
  //   We don't have to support this if we don't want. We can do a variety of things?
  //*. GLOBAL and LOCAL are mutually exclusive.
  //Sample strings:
  //".k.a", ".l.b", ".a.b.c", "d", "d.e", "d.e.f", "f.g" <-- note absent dot prefixes

  bool is_global = is_global_name_constant(payload, function, parent);

  if(is_global) //GLOBAL_PUSH
  {
    translate_global(bytecode, debug, function, parent, payload, location, creates);
  }
  else //ARGLOCAL_PUSH 
  {
    translate_arglocal(bytecode, debug, function, parent, payload, location, creates);
  }
}

void translate_plain(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);

  I location; 

  if(COUNT(payload) <= 0)
  {
    //you could always do the NIL_PUSH and then
    //unconditionally do the pop before translate in the ENUM below. shrug
    add_code_byte(bytecode, debug, NIL_PUSH, 0);
  }
  else
  {
    ENUM(payload, translate(v, bytecode, debug, function, parent, 0, 0);
                  if(i < _i - 1)
                  {
                    location = AT2(v, ki(TOKEN_SNIPPET_END),o2)->i;
                    add_separator_code(bytecode, debug, location);
                  }
    )
  }
}

void translate_bound_args(K tree, K *bytecode, K *debug, K function, K parent, bool reverse)
{
  K0 o[4] = {0};
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);

  if(COUNT(payload) <= 0)
  {
    add_code_byte(bytecode, debug, NIL_PUSH, 0);
  }
  else
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //if you don't like that reverse executes in reverse order in order to get args on the stack right
    //then what you can do is "wrap them", execute in forward order, and then expand to the stack
    //in the desired order (reversed), potentially using some specialized instruction

    if(!reverse)
    {
      ENUM(payload, I v_start = AT2(v, ki(TOKEN_SNIPPET_START),o[1])->i;
                    add_code_byte(bytecode, debug, ASIDE_OPEN, v_start);
                    translate(v, bytecode, debug, function, parent,0,0);
                    add_code_byte(bytecode, debug, ASIDE_CLOSE, v_start);
      )
    }
    else
    {
      I n = COUNT(payload);
      DO(n, K v = AT2(payload, ki(n-1-i),o[2]); 
            I v_start = AT2(v, ki(TOKEN_SNIPPET_START),o[3])->i;
            add_code_byte(bytecode, debug, ASIDE_OPEN, v_start);
            translate(v, bytecode, debug, function, parent,0,0);
            add_code_byte(bytecode, debug, ASIDE_CLOSE, v_start);
      )
    }
  }

}

void add_separator_code(K *bytecode, K *debug, I location)
{
  add_code_byte(bytecode, debug, POP, location);
}

I2 derived_verb_arity(K tree)
{
  K0 o[6] = {0};
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);
  I n = COUNT(payload);

  if(n <= 0)
  {
    goto not;
  }

  K last = AT2(payload, ki(n-1),o[1]);
  I last_kind = AT2(last,ki(TOKEN_KIND),o[2])->i;

  K list = AT2(last, ki(TOKEN_PAYLOAD),o[3]); 
  I verb_code = -1;
  VERB verb = {0};

  SW(last_kind)
  {
   CSF(TOKEN_GROUP_VERBAL_NNA, goto adverb_nv)
    CS(TOKEN_GROUP_VERBAL_NA, goto adverb_v)
    CS(TOKEN_GROUP_VERBAL_VA, 
      if(COUNT(list) > 1) goto adverb_v;
      K verb_token = AT2(list, ki(0),o[4]);
      lookup_verb_data_via_token(verb_token, &verb_code, &verb, false, 0, 2);
      I verb_min = verb.argc_range[0];
      I verb_max = verb.argc_range[1];
      return (I2){verb_min, verb_max};
    )
    CS(TOKEN_GROUP_VERBAL_NVA,
      if(COUNT(list) > 2) goto adverb_nv;
      K verb_token = AT2(list, ki(1),o[5]);
      lookup_verb_data_via_token(verb_token, &verb_code, &verb, false, 0, 2);
      I verb_min = verb.argc_range[0];
      I verb_max = verb.argc_range[1];
      return (I2){verb_min - 1, verb_max - 1};
    )
  }

not:
  return (I2){NOT_DERIVED, NOT_DERIVED};

adverb_nv: //is this instead {1,1} ?
adverb_v:
  return (I2){1,2};//Adverb has min arg 1. Max arg 2? Or 127?
}

void translate_separation(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[14] = {0};
  K text = kN(function, FUNC_TEXT);
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;

  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);
  I n = COUNT(payload);

  //Empty separation. Push nil
  if(n <= 0)
  {
    add_code_byte(bytecode, debug, NIL_PUSH, 0);
    return;
  }

  I2 dv_arity = derived_verb_arity(tree); 

  I min_args = dv_arity.x;
  I max_args = dv_arity.y;

  bool dv_verbal = min_args > NOT_DERIVED;
 
  I derived_width = 0;

  if(dv_verbal)
  {
    DO(n, K v = AT2(payload,ki(n-i-1),o[3]);
          I kind = AT2(v, ki(TOKEN_KIND),o[4])->i;
          if(TOKEN_GROUP_ASSIGNMENT == kind)
          {
            break;
          }
          derived_width++
    )
  }

  bool sqly = GET_ALT_ATTR(function, FUNC_ATTR_SQL_CLAUSE);

  I same_text = (snippet_end - snippet_start) == COUNT(text);
  I derived_recurse_ok = (!same_text || !parent || sqly);
  I do_derived_verb = dv_verbal && derived_recurse_ok;

  //Possibly slightly hacky. I didn't spend a lot of time perfecting this.

  if(do_derived_verb)
  {

    K beginner = AT2(payload,ki(n-derived_width),o[5]);
    I beginner_kind = AT2(beginner, ki(TOKEN_KIND),o[6])->i;
    K beginner_payload = AT2(beginner, ki(TOKEN_PAYLOAD),o[7]);
    I start = AT2(beginner, ki(TOKEN_SNIPPET_START),o[8])->i;
    I tree_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[9])->i;

  
    //hackiness level: low
    bool is_star = false;
    if(TOKEN_GROUP_VERBAL_VA == beginner_kind)
    {
      K x = beginner_payload;
      if(LIST == xt && COUNT(x) == 1)
      {
        K first = AT2(x, ki(0),o[10]);
        K first_payload = AT2(first, ki(TOKEN_PAYLOAD),o[11]);
        if(EQUAL == KC(kcv("*"),first_payload))
        {
          is_star = true;
        }
      }
    }

    if(sqly && is_star)//special case sql select *
    {
      SET_ALT_ATTR(function, FUNC_ATTR_HAS_STAR);
      I table_arg_index = 0;
      add_code_byte_op_compression(bytecode, debug, ARGLOCAL_PUSH, start, table_arg_index);
    }
    else //regular
    {
      translate_derived_verb(tree, bytecode, debug, function, parent, min_args, max_args, start, tree_end);
    }
    
    DO(n - derived_width, I apply = APPLY;
          K v = AT2(payload,ki(n-i-1-derived_width),o[12]);
          translate(v, bytecode, debug, function, parent, apply, 0))
  }
  else
  {
    //Regular
    DO(n, I apply = 0;
          if(i>0) apply = APPLY;
          K v = AT2(payload,ki(n-i-1),o[13]);
          translate(v, bytecode, debug, function, parent, apply, 0))
  }

}

void translate_paren(K tree, K *bytecode, K *debug, K function, K parent)
{ 
  //POTENTIAL_OPTIMIZATION_POINT
  //detect if a paren contains only a single paren, eg  " ((1)) "
  //then ignore it entirely and go to its child

  K0 o1,o2;
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START), o1)->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END), o2)->i;

  add_code_byte(bytecode, debug, ASIDE_OPEN,  snippet_start);
  translate_plain(tree, bytecode, debug, function, parent);
  add_code_byte(bytecode, debug, ASIDE_CLOSE, snippet_end - 1);
}

void translate_list(K tree, K *bytecode, K *debug, K function, K parent)
{ 
  K0 o1,o2,o3,o4;
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o1)->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o2)->i;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o3);

  add_code_byte(bytecode, debug, LIST_OPEN, snippet_start);

  I location; 

  ENUM(payload, 
                add_code_byte(bytecode, debug, ASIDE_OPEN, snippet_start);
                translate(v, bytecode, debug, function, parent, 0, 0);
                add_code_byte(bytecode, debug, ASIDE_CLOSE, snippet_end - 1);

                location = AT2(v, ki(TOKEN_SNIPPET_END),o4)->i;
                add_list_separator_code(bytecode, debug, location);
  )

  add_code_byte(bytecode, debug, LIST_CLOSE, snippet_end - 1);
}

void add_list_separator_code(K *bytecode, K *debug, I location)
{
  add_code_byte(bytecode, debug, LIST_STORE, location);
}

void translate_curly(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[5] = {0};
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);

  //Uses of Curly Braces "{}"
  //  1. Maps       {a:1}
  //  2. Tables     {{prices: 10.5 10.75 11}} 
  //  3. Atlases    {[{},{},{}]}
  //  4. Functions  {[a]1+a}

  I ns = COUNT(payload);
  I ng = -1;

  bool has_args_first  = false;
  bool has_curly_first = false;

  if(ns > 0)
  {
    K separation = AT2(payload, ki(0),o[1]);

    K groupings = AT2(separation, ki(TOKEN_PAYLOAD),o[2]);
    ng = COUNT(groupings);

    if(ng > 0)
    {
      K first = AT2(groupings, ki(0),o[3]);
      I kind = AT2(first, ki(TOKEN_KIND),o[4])->i;

      SW(kind)
      {
        CS(TOKEN_GROUP_CURLY_BRACE, has_curly_first = true;)
        CS(TOKEN_GROUP_LAMBDA_ARGS, has_args_first  = true;)
      }
    }
  }

  bool is_atlas    = (1 == ns) && (1 == ng) && has_args_first; //we could do different things, like not parse as LAMBDA_ARGS in this case
  bool is_function = (1 <= ns) && has_args_first; 
  bool is_table    = (1 == ns) && (1 == ng) && has_curly_first;

  if(is_atlas) //check before function since we're stealing the {[]} case
  {
    translate_atlas(tree, bytecode, debug, function, parent);
    return;
  }

  if(is_function)
  {
    translate_function(tree, bytecode, debug, function, parent);
    return;
  }

  if(is_table)
  {
    translate_table(tree, bytecode, debug, function, parent);
    return;
  }

  translate_map(tree, bytecode, debug, function, parent);
  return;
}

void translate_derived_verb(K tree, K *bytecode, K *debug, K function, K parent, I min_args, I max_args, I start, I end)
{
  if(!PARSE_ALLOW_DERIVED_VERBS)
  {
    ERROR(ERROR_PARSE_DERIVED);
  }

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  K substring = *work_push(new_subarray(text, start, end));

  K args = *work_push(til(ki(max_args)));
  K derivation = *work_push(compile_with_parent_args(substring, directory, function, args, FUNC_KIND_DERIVED_VERB, 0));

  derivation->nf = min_args;
  kN(derivation, FUNC_UNFILLED_ARGS)->i = min_args;
  kN(derivation, FUNC_TOTAL_ARGS)->i = max_args;

  translate_constant(bytecode, debug, function, derivation, start);
  
  work_pop_n_rd(3, true);
  
  return;
}

K parse_lambda_args(K tree)
{
  K0 o[5] = {0};
  K separations = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);
  K list = *work_push(new_k(LIST, 0));

  ENUM(separations,
    K pieces = AT2(v, ki(TOKEN_PAYLOAD),o[1]);
    if(COUNT(pieces) != 1) goto fail; //error. we could add type checking here
    K first = AT2(pieces, ki(0),o[2]);
    if(TOKENS_NAME != AT2(first, ki(TOKEN_KIND),o[3])->i) goto fail;//error, non-name

    K name = AT2(first, ki(TOKEN_PAYLOAD),o[4]);
    ENUM(name, if('.'==vc)goto fail)//error. Currently enforcing "flat" args/locals

    work_pop();
    list = cow_add(list, name);
    work_push(list);
  )

succeed:
  return work_pop();

fail:
  ERROR(ERROR_PARSE_LAMBDA_ARGS);
  return NULL;
}

void translate_function(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[7] = {0};
  K payload = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[1])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[2])->i;

  K first_separation = AT2(payload, ki(0),o[3]);
  K groupings = AT2(first_separation, ki(TOKEN_PAYLOAD),o[4]);
  K arg_tree = AT2(groupings, ki(0),o[5]);

  I arg_end = AT2(arg_tree, ki(TOKEN_SNIPPET_END),o[6])->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  I other_end = snippet_end - 1; 

  //Trim trailing whitespace/newlines at end of function/lambda definition
  //so that {[x]1+1\n\n\n} returns 2
  //**probably better to handle this in the lexer to catch COMMENTS and such
  //see also translate_control_def where this is copy/pasta
  //grep key other_end_14234234
  while(other_end > arg_end && isspace(kC(text)[other_end - 1]))
  {
    other_end--;
  }

  K substring = *work_push(new_subarray(text, arg_end, other_end));

  //POTENTIAL_OPTIMIZATION_POINT
  //Good chance you'll never need this, but:
  //Occurs in other places too:
  //grep: compile_with_parent_args
  //Note: O(n^2) compilation is OK here because of depth bounds. You can do
  //100^2 = 10,000 which is OK. If for some reason you need to optimize, what
  //you can do is pass the subTREE here to ASSEMBLE, instead of COMPILING.
  //Probably though you'll need to normalize the tree snippet indices, which
  //is not that hard. Otherwise the tree nodes will have the wrong indices into
  //the new snippet substring. Copying the tree should be fine.
  //K lambda = assemble(text, tree, directory, function ...
  K argv = parse_lambda_args(arg_tree);
  work_push(argv);

  K lambda = *work_push(compile_with_parent_args(substring, directory, function, argv, FUNC_KIND_LAMBDA, 0));

  translate_constant(bytecode, debug, function, lambda, snippet_start);

  work_pop_n_rd(3, true);

  return;
}

void translate_table(K tree, K *bytecode, K *debug, K function, K parent)
{
  //TODO: OK, but what about parsing column keyed, fkey, BTREE, ENUM'D, etc. ?
  //      also, maybe, types?

  K0 o[10] = {0};


  //get the {} out of {{}}
  tree = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);
  tree = AT2(tree, ki(0),o[1]);
  tree = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);
  tree = AT2(tree, ki(0),o[3]);

  K payload = AT2(tree, ki(TOKEN_PAYLOAD),o[4]);

  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[5])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[6])->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  I start = snippet_start + 1;
  I end = snippet_end - 1;

  K substring = *work_push(new_subarray(text, start, end));

  C attr = FUNC_ATTR_keyvaluize;

  //POTENTIAL_OPTIMIZATION_POINT
  //See translate_function  
  K lambda = compile_with_parent_args(substring, directory, function, kk, FUNC_KIND_LAMBDA, attr);
  work_push(lambda);

  //Hack
  K x = kN(lambda, FUNC_BYTECODE);
  xC[xn-1] = (C)TABLE_RETURN;//overwrite RETURN

  //handle a special case like "{{a,b,c}}"
  bool simple = true;
  K args = new_k(LIST, 0);
  ENUM(payload, K parts = AT2(v, ki(TOKEN_PAYLOAD),o[7]);
                if(COUNT(parts) != 1){simple = false;} 
                ENUM(parts, 
                            if(AT2(v,ki(TOKEN_KIND),o[8])->i != TOKENS_NAME) simple = false; 
                            args = cow_add(args, AT2(v,ki(TOKEN_PAYLOAD),o[9]))))
  if(simple)
  {
    ENUM(args, K arglocals = kN(lambda, FUNC_ARGLOCALS); I p = insert_replace(arglocals, v, kk, false, false);)
      
      bool keyvaluey = GET_ALT_ATTR(lambda, FUNC_ATTR_keyvaluize);
      if(keyvaluey)
      {
          K assigns = kN(lambda, FUNC_LOCAL_ASSIGNS);
          
          if(IS_EMPTY_ATOM(assigns))
          {
              K hashset = new_hashset();
              k0(lambda)[FUNC_LOCAL_ASSIGNS] = kl0(hashset);
          }
          
          assigns = kN(lambda, FUNC_LOCAL_ASSIGNS);
          ENUM(args, insert_replace_ri_rd(assigns, v, NULL, false, true, true, false))
      }
  }
  rd(args);


  add_code_byte(bytecode, debug, ASIDE_OPEN, snippet_start);
  translate_constant(bytecode, debug, function, lambda, snippet_start);
  add_code_byte(bytecode, debug, APPLY, snippet_start);
  add_code_byte(bytecode, debug, ASIDE_CLOSE, end);

  work_pop_n_rd(2, true);

  return;


}

void translate_atlas(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1,o2;
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o1)->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o2)->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  I start = snippet_start + 1;
  I end = snippet_end - 1;

  K substring = new_subarray(text, start, end);
  work_push(substring);

  C attr = FUNC_ATTR_keyvaluize;

  //POTENTIAL_OPTIMIZATION_POINT
  //See translate_function  
  K lambda = compile_with_parent_args(substring, directory, function, kk, FUNC_KIND_LAMBDA, attr);
  work_push(lambda);

  //Hack
  K x = kN(lambda, FUNC_BYTECODE);
  xC[xn-1] = (C)ATLAS_RETURN;//overwrite RETURN

  add_code_byte(bytecode, debug, ASIDE_OPEN, snippet_start);
  translate_constant(bytecode, debug, function, lambda, snippet_start);
  add_code_byte(bytecode, debug, APPLY, snippet_start);
  add_code_byte(bytecode, debug, ASIDE_CLOSE, end);

  work_pop_n_rd(2, true);

  return;
}

void translate_map(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1,o2;
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o1)->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o2)->i;

  K text       = kN(function, FUNC_TEXT);
  K directory  = kN(function, FUNC_DIRECTORY);

  I start = snippet_start + 1;
  I end = snippet_end - 1;


  K substring = new_subarray(text, start, end);
  work_push(substring);

  C attr = FUNC_ATTR_keyvaluize;

  //POTENTIAL_OPTIMIZATION_POINT
  //See translate_function  
  K lambda = compile_with_parent_args(substring, directory, function, kk, FUNC_KIND_LAMBDA, attr);
  work_push(lambda);

  //Hack
  K x = kN(lambda, FUNC_BYTECODE);
  xC[xn-1] = (C)MAP_RETURN;//overwrite RETURN

  add_code_byte(bytecode, debug, ASIDE_OPEN, snippet_start);
  translate_constant(bytecode, debug, function, lambda, snippet_start);
  add_code_byte(bytecode, debug, APPLY, snippet_start);
  add_code_byte(bytecode, debug, ASIDE_CLOSE, end);

  work_pop_n_rd(2, true);

  return;
}

void translate_rel_datetime(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o2)->i;

  bool local = false;
  K relative = parse_relative_datetime(payload, local);

  translate_constant(bytecode, debug, function, relative, location);

  rd(relative);
}

void translate_verb(K tree, K *bytecode, K *debug, K function, K parent, I apply, I argc)
{
  K0 o1;
  I debug_location = AT2(tree, ki(TOKEN_SNIPPET_START),o1)->i;

  I verb_code = -1;
  VERB verb = {0};

  bool has_adverb = false;

  lookup_verb_data_via_token(tree, &verb_code, &verb, has_adverb, apply, argc);

  //TODO: EITHER parse-time error if you don't like argc ??? 
  //          OR fix the bug that causes plus(1,2,3) to leak on execution :)
  //may require comment in verb.c -> lookup_verb_...
  //assert(-1 != verb_code);

  last_verb_bytecode_position = COUNT(*bytecode);
  last_verb_function = function;

  add_code_byte_op_flat(bytecode, debug, VERB_CALL, debug_location, verb_code);
}

void translate_bound(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[7];
  I snippet_start = AT2(tree, ki(TOKEN_SNIPPET_START),o[0])->i;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[1])->i;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[2]);

  K left  = AT2(payload, ki(0),o[3]);
  I left_type = AT2(left, ki(TOKEN_KIND),o[4])->i;
  I n = COUNT(payload);

  add_code_byte(bytecode, debug, ASIDE_OPEN, snippet_start);

  I argc = 0; 

  bool reverse = true;  //Kludge: reverse order of arguments on stack
                        //Possibly, this is something I messed up?
                        //Functions (Lambda/Derived) want first argument on bottom
                        //Verbs want first argument on top
                        //Verbs is because (1-) makes more sense.
                        //Functions was because....?
                        //Maybe it would be awkward to reverse the order
                        //of the keys in the arg map of the function
                        //or for projections
                        //Maybe that's where I messed up?
                        //It may be this impedance mismatch was unavoidable as well
                        //based on other assumptions built in
                        //Thinking about it now, I think the reason is because
                        //I wanted arguments to execute left to right.
                        //this is more awkward if the first isn't on the bottom
                        //So that (desirable) constraint is likely responsible
  bool moreverso = true;

  SW(left_type)
  {
    CS(TOKENS_VERB_WORD, reverse = true)
  }


  DO(n-1, I k = 1+i;
          if(moreverso) k = n-1-i;
          K right = AT2(payload, ki(k),o[5]);
          argc += COUNT(AT2(right, ki(TOKEN_PAYLOAD),o[6]));
          translate_bound_args(right, bytecode, debug, function, parent, reverse);
  )

  translate(left, bytecode, debug, function, parent, APPLY, argc);

  add_code_byte(bytecode, debug, ASIDE_CLOSE, snippet_end - 1);
}

void translate_verbal_nna(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);

  translate(AT2(payload, ki(0),o2), bytecode, debug, function, parent, 0, 0);

  K chopped = drop(ki(1), payload);
  translate_verbal_na(chopped, tree, bytecode, debug, function, parent, apply, 1);
  rd(chopped);
}

void translate_verbal_nva(K tree, K *bytecode, K *debug, K function, K parent, I apply)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);

  translate(AT2(payload, ki(0),o2), bytecode, debug, function, parent, 0, 0);

  K chopped = drop(ki(1), payload);
  translate_verbal_va(chopped, tree, bytecode, debug, function, parent, apply, 1);
  rd(chopped);
}

void translate_verbal_na(K payload, K tree, K *bytecode, K *debug, K function, K parent, I apply, I additional_arity)
{
  //=============================================================================
  //POTENTIAL_OPTIMIZATION_POINT
  //So in the future this might be a worthwhile optimization.
  //The way we're doing it now, "f[1;2] fold y" recomputes
  //"f[1;2]". To get around it do "a:f[1;2]; a fold y".
  //There's also an extra "pre-push" of the noun to init
  //the adverb.
  //This is OK for now. In the future we might want to do a
  //fairly heavy (or medium) duty fix: precompute the noun
  //arg, then pass it *through each adverb* in the adverb stack.
  //This is slightly tricky, and may need the adverb extended,
  //or the bytecode changed, or something. I've put in a placeholder
  //now in case. See ADVERB_NOUN_OPERATOR.
  //-------------
  //Option 1:
  //It looks like the way to go forward here is to 
  //  i) push the noun prior to the ADVERB_OPEN call
  // ii) signal that we affected the stack via a sentinel argument to open
  //iii) take it off the stack but save it for when we open the adverbial frame
  // iv) keep it in the adverb data for the adverb frame
  //  v) push it using a special adverb call instead of a verb call
  // vi) the noun must execute with another instruction or as part of say the adverb_push
  //
  //Use APPLY_NO_PROJECT (because function f) Does this work for nested adverbs ... ?
  //Maybe if you always go fishing in the parent adverb for the same thing ???
  //
  //Option 2:
  //You could make a separate KVM->adverb_noun_stack like work_push() & pop
  //=============================================================================

  //See translate_verbal_va 

  K0 o1,o2;
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o1)->i;
 
  K noun_token = AT2(payload, ki(0),o2);

  I min_args = SENTINEL;
  I max_args = SENTINEL;

  K list = new_k(LIST, 2);

  k0(list)[0] = kl0(new_k(CHARVEC,0));
  k0(list)[1] = kl0(new_k(INTVEC,0));

  work_push(list);

  K *local_bytecode = &kL(list, 0); 
  K *local_debug    = &kL(list, 1);
  SET_ATTR(*local_bytecode, ATTR_BYTES);

  translate(noun_token, local_bytecode, local_debug, function, parent, APPLY_NOPROJECT, additional_arity);

  ENUM(payload, if(0==i)continue;
    K pass_noun = NULL;
    if(1==i) pass_noun = noun_token;
    translate_adverb(v, local_bytecode, local_debug, function, parent, apply, min_args, pass_noun);
    min_args = 1;//adverbials are monadic by default
    max_args = 2;//another adverbial thing...but is it right? may want 127...
  )

  *bytecode = cow_join(*bytecode, *local_bytecode);
  *debug    = cow_join(*debug,    *local_debug);

  work_pop_n_rd(1, true);

  return; 

}

void translate_verbal_va(K payload, K tree, K *bytecode, K *debug, K function, K parent, I apply, I additional_arity)
{
  K0 o[5];
  I snippet_end   = AT2(tree, ki(TOKEN_SNIPPET_END),o[0])->i;
 
  K verb_token = AT2(payload, ki(0),o[1]);

  I debug_location = AT2(verb_token, ki(TOKEN_SNIPPET_START),o[2])->i;

  I verb_code = -1;
  VERB verb = {0};

  bool has_adverb = COUNT(payload) > 1;
  I argc = 1 + additional_arity;

  I last_verbal;

  //TODO 1. {no additional_arity arg, no adverb, fixed dyad} ("plus" not "+"). Can ERROR here or in parsing or execution. Or can PROJECT
  //     2. {yes additional arity, no adverb, fixed monad} should juxtapose maybe. Can error here or in parse or in exec.
  //     0. wait to see if execution will handle these nicely for us...? by either erroring or projecting
  //     3. also for nna na
  //     potentially also you could split into [n v2 a* | n v1 a +] and [v1 a* | v2 a+]
  //     Another way to satisfy both of them is to turn on name_arity_matches in the lookup & error here on -1 verb_code
  //     but be careful about that because turning on that error checking might break other stuff?

  //Sym verbs always work. Named verbs can error here if we enable it

  lookup_verb_data_via_token(verb_token, &verb_code, &verb, has_adverb, apply, argc);

  bool derived =   (snippet_end == COUNT(kN(tree,FUNC_TEXT))) && (function->nk == FUNC_KIND_DERIVED_VERB);
  if(!apply && derived)
  {
    argc = kN(function, FUNC_UNFILLED_ARGS)->i;
    lookup_verb_data_via_token(verb_token, &verb_code, &verb, has_adverb, true, argc);
  }

  assert(-1 != verb_code);

  I min_args = verb.argc_range[0];
  I max_args = verb.argc_range[1];

  K list = new_k(LIST, 2);

  k0(list)[0] = kl0(new_k(CHARVEC,0));
  k0(list)[1] = kl0(new_k(INTVEC,0));

  work_push(list);

  K *local_bytecode = &kL(list, 0); 
  K *local_debug    = &kL(list, 1);
  SET_ATTR(*local_bytecode, ATTR_BYTES);

  I optimized = SENTINEL;
  I first_adverb_kind = 0;
  bool is_optimizable = false;

  if(COUNT(payload)>1) //Check for optimized availabilty
  {
    K first_adverb = AT2(payload,ki(1),o[3]);
    K first_adverb_name = AT2(first_adverb, ki(TOKEN_PAYLOAD),o[4]);
    first_adverb_kind = lookup_adverb(first_adverb_name);

    //Find optimized verb id here
    S string = verb.optimized[first_adverb_kind];

    if(string)
    {
      optimized = lookup_verb_id_by_name_or_sym(string);
      if(SENTINEL != optimized)
      {
        is_optimizable = true;
      }
    }
  }

  I start_index = 1;

  if(is_optimizable)
  {
    //er(this whole optimize thing is broken because 1 +/ 1 2 doesnt work)
    //er(actually maybe not all is lost. yum seems to handle it correctly)
    add_code_byte_op_flat(local_bytecode, local_debug, VERB_CALL, debug_location, optimized);
    start_index = 2; //skip "plus fold"
    min_args = 1;//adverbials are monadic by default
    max_args = 2;//another adverbial thing...but is it right? may want 127...

    last_verb_bytecode_position = COUNT(*bytecode); //note that this is bytecode not local_bytecode
    last_verb_function = function;
  }
  else
  {
    add_code_byte_op_flat(local_bytecode, local_debug, VERB_CALL, debug_location, verb_code);
    start_index = 1;//skip "plus"

    last_verb_bytecode_position = COUNT(*bytecode);
    last_verb_function = function;
  }

  ENUM(payload, 
      if(i<start_index)continue;
      translate_adverb(v, local_bytecode, local_debug, function, parent, apply, min_args, NULL);
      min_args = 1;//adverbials are monadic by default
      max_args = 2;//another adverbial thing...but is it right? may want 127...

      last_verb_bytecode_position = -1;
      last_verb_function = NULL;
   )


  *bytecode = cow_join(*bytecode, *local_bytecode);
  *debug    = cow_join(*debug,    *local_debug);

  work_pop_n_rd(1, true);
}

void translate_adverb(K token, K *local_bytecode, K *local_debug, K function, K parent, I apply, I min_args, K noun_token)
{
  K0 o1,o2;
  K payload  = AT2(token, ki(TOKEN_PAYLOAD),o1);
  I location = AT2(token, ki(TOKEN_SNIPPET_START),o2)->i;

  I adverb_kind = lookup_adverb(payload);

  K wrapper = wrap_code_adverb(adverb_kind, local_bytecode, local_debug, function, parent, min_args, noun_token, location);

  rd(*local_bytecode);
  rd(*local_debug);

  *local_bytecode = strong(kL(wrapper,0));
  *local_debug = strong(kL(wrapper,1));
  
  rd(wrapper);
}

K wrap_code_adverb(I adverb_kind, K *bytecode, K *debug, K function, K parent, I min_args, K noun_token, I location)
{
  C AWHILE = '\0';
  C APUSH  = '\0';

  SW(adverb_kind)
  {
    CS(ADVERB_KIND_FOLD,     AWHILE = ADVERB_FOLD_WHILE;     APUSH = ADVERB_FOLD_PUSH)
    CS(ADVERB_KIND_UNFOLD,   AWHILE = ADVERB_UNFOLD_WHILE;   APUSH = ADVERB_UNFOLD_PUSH)
    CS(ADVERB_KIND_MAPDOWN,  AWHILE = ADVERB_MAPDOWN_WHILE;  APUSH = ADVERB_MAPDOWN_PUSH)
    CS(ADVERB_KIND_MAPRIGHT, AWHILE = ADVERB_MAPRIGHT_WHILE; APUSH = ADVERB_MAPRIGHT_PUSH)
    CS(ADVERB_KIND_MAPLEFT,  AWHILE = ADVERB_MAPLEFT_WHILE;  APUSH = ADVERB_MAPLEFT_PUSH)
    CS(ADVERB_KIND_MAPBACK,  AWHILE = ADVERB_MAPBACK_WHILE;  APUSH = ADVERB_MAPBACK_PUSH)
  }

  //HOW ADVERBS LOOK IN THE BYTECODE
  //  ,ADVERB_OPEN, 0x00, 0x01
  //  //OPTIONAL: push f before jmp to initialize args
  //  ,ADVERB_FOLD_WHILE, 0x00, 0x08 //jmp fwd to close if done 
  //  ,ADVERB_FOLD_PUSH
  //  //CODE PAYLOAD EXECUTION
  //  //regular code execution A
  //  ,VERB_CALL, 0x00, 0x07
  //  //
  //  //regular code execution B
  //  //,PUSH F  
  //  //,APPLY_NO_PROJECT F (probably don't project, throw error instead. but let variadic verbs work)
  //  //
  //  ,ADVERB_STORE
  //  ,JMPRB, 0x00, 0xF8 //jmp back to while 
  //  //      NOTE this is TRICKY as variable-width integer with WHILE
  //  //      reversing the order of these instructions doesn't really help
  //  //      the fastest fix is to use either absolute positions or 
  //  //      just lock the integer size to 64-bit so that the calculations
  //  //      are not dependent on the # of intervening instructions
  //  //      causing the variable width integers to unexpectedly expand
  //  //
  //  //      Best is to make the plain old jump at the end fixed size (8bytes)
  //  //      then later if you want to do the logic you can
  //  //      begin producing smaller bytecode. Everything is compatible
  //  ,ADVERB_CLOSE


  K list = new_k(LIST, 2);

  k0(list)[0] = kl0(new_k(CHARVEC,0));
  k0(list)[1] = kl0(new_k(INTVEC,0));

  work_push(list);

  K *wrapcode = &kL(list,0);
  K *wrapbug = &kL(list,1);

  SET_ATTR(*wrapcode, ATTR_BYTES);

  I existing = (*bytecode)->n;

  I flat = 1 + sizeof(I);
  I uncompressed = 1 + 1 + sizeof(I);
  I jump_forward_count  =                  1 +  existing +  1 + flat;
  I jump_backward_count = -uncompressed + -1 + -existing + -1;

  add_code_byte_op_compression(wrapcode, wrapbug, ADVERB_OPEN, location, min_args);

  if(noun_token)
  {
    I DONT_APPLY = 0;
    translate(noun_token, wrapcode, wrapbug, function, parent, DONT_APPLY, 0);
  }

  add_code_byte_op_compressed_3(wrapcode, wrapbug, AWHILE, location, jump_forward_count);
  add_code_byte(wrapcode, wrapbug, APUSH, location);

  *wrapcode = cow_join(*wrapcode, *bytecode); //VERB_CALL or noun call
  *wrapbug  = cow_join(*wrapbug,   *debug); 

  add_code_byte(wrapcode, wrapbug, ADVERB_STORE, location);
  add_code_byte_op_flat(wrapcode, wrapbug, JMPRB, location, jump_backward_count);
  add_code_byte(wrapcode, wrapbug, ADVERB_CLOSE, location);

  return work_pop();
}

void translate_alike(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o[5];
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o[0]);
  I location = -1;

  K list = new_k(LIST, 0);

  ENUM(payload, 
    I kind    = AT2(v, ki(TOKEN_KIND),o[1])->i;

    K q = NULL;

    K text = AT2(v,ki(TOKEN_PAYLOAD),o[2]);

    bool local = false;

    SW(kind)
    {
      CS(TOKENS_NUMBER,       q = parse_number(text))
      CS(TOKENS_ABS_TIME,     q = parse_absolute_time(text, local))
      CS(TOKENS_ABS_DATE,     q = parse_absolute_date(text, local))
      CS(TOKENS_ABS_DATETIME, q = parse_absolute_datetime(text, local))
      CD:                     q = Kn(); break;
    }

    if(-1==location) location = AT2(v, ki(TOKEN_SNIPPET_START),o[3])->i;

    list = cow_add_funny(list, q);
    rd(q);
  )

  K constant = NULL;

  if(1 == COUNT(list))
  {
    constant = strong(AT2(list,ki(0),o[4]));
  }
  else
  {
    constant = strong(list);
  }

  rd(list);
  translate_constant(bytecode, debug, function, constant, location);
  rd(constant);
}

void translate_reserved(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1,o2;
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o1)->i;
  K payload = AT2(tree, ki(TOKEN_PAYLOAD),o2);

  bool sensitivity = PARSE_RESERVED_CASE_INSENSITIVE;
  bool is_root  = charvec_case_matchC(payload, kcv("root"), sensitivity);
  bool is_true  = charvec_case_matchC(payload, kcv("true"), sensitivity);
  bool is_false = charvec_case_matchC(payload, kcv("false"), sensitivity);

  if(is_root)
  {
    add_code_byte(bytecode, debug, ROOT_PUSH, location);
    return;
  }
  else if(is_true)
  {
    translate_constant(bytecode, debug, function, ki(1), location);
    return;
  }
  else if(is_false)
  {
    translate_constant(bytecode, debug, function, ki(0), location);
    return;
  }
  else //nil, null, etc.
  {
    add_code_byte(bytecode, debug, NIL_PUSH, location);
    return;
  }

}

void translate_self(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1;
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o1)->i;
  add_code_byte(bytecode, debug, SELF_PUSH, location);
}

K parse_string(K x)//HACK - overloaded to do `a "a" 'a' `"a" `'a'
{
  I n = COUNT(x);
  I start = 0;
  I end = n;

  K z = new_k(CHARVEC, 0);

  if(start >= end) return z;
  if('`'== xC[start]) start++; //Hack
  if(start >= end) return z;
  if('\''== xC[start] || '"' == xC[start])
  {
    start++;
    end--;
  }

  if(PARSE_JSON_STAMP_STRING_HACK)
  {
    K xtc = kcv(PARSE_JSON_STAMP_STRING_XTC);
    if(end - start > COUNT(xtc))
    {
      K sub = new_subarray(x, start, start + COUNT(xtc));

      bool prefixed = false;

      if(matchC(xtc, sub, 0)) prefixed = true;
      
      rd(sub);

      if(prefixed)
      {
        K pay = new_subarray(x, start + COUNT(xtc), end);

        work_push(pay);

        K stamp = interpret(pay);

        work_pop_rd(true);

        rd(z);
        return stamp;
      }
    }
  }

  I k = 0;
  for(k = start; k < end; k++)
  {
    C c = xC[k];

    C a[5] = {0};

    if('\\' == c)
    {
      c = xC[++k];

      SW(c)
      { //  "\/bfnrtu
        CS('"',  c = '"')
        CS('/',  c = '/')
        CS('\\', c = '\\')
        CS('b',  c = '\b')
        CS('f',  c = '\f')
        CS('n',  c = '\n')
        CS('r',  c = '\r')
        CS('t',  c = '\t')
        CS('u',
          DO(4, a[i] = xC[++k])
          //HACK.
          //if this is an issue you probably want to just rewrite this method entirely
          //the ECMA standard also has a bunch of newly added stuff like \xhh and octal
          //that you could put in too
          a[0]='0'; a[1]='x'; c = (UC)strtoll(a, 0, 0);
        )
        CD: c = c; break;
      }
    }

    z = cow_add(z, kc(c));
  }

  return z;
}

I translate_string(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o2)->i;

  //Hack
  if(*kC(payload)=='`') 
  {
    return translate_backtick(tree, bytecode, debug, function, parent);
  }

  K constant = parse_string(payload);

  I p = translate_constant(bytecode, debug, function, constant, location);

  rd(constant);

  return p;//constant's position
}

I translate_backtick(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o1,o2;
  K payload  = AT2(tree, ki(TOKEN_PAYLOAD),o1);
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o2)->i;

  K x = parse_string(payload);
  C character = '\0';

  //Hack
  if(COUNT(payload) == 2)
  {
    character = kC(payload)[1];
  }
  else 
  {
    if(COUNT(x) > 0) character = xC[0];
  }

  K constant = Kc(character);

  I p = translate_constant(bytecode, debug, function, constant, location);

  rd(x);
  rd(constant);

  return p;
}

void translate_return(K tree, K *bytecode, K *debug, K function, K parent)
{
  K0 o;
  I location = AT2(tree, ki(TOKEN_SNIPPET_START),o)->i;
  add_code_byte(bytecode, debug, (C)RETURN_CODE, location);
}

I translate_constant(K *bytecode, K *debug, K function, K found_constant, I debug_location)
{
  K constants = kN(function, FUNC_CONSTANTS);
  I p = insert_replace(constants, found_constant, 0, false, true);
  add_code_byte_op_compression(bytecode, debug, CONSTANT_PUSH, debug_location, p);
  return p;
}

void translate_global(K *bytecode, K *debug, K function, K parent, K found_global, I debug_location, bool creates)
{

  if(DYNAMIC_DOT_NAMES)
  {
    //ers(found_global);
    //add_code_byte_op_compression(bytecode, debug, GLOBAL_PUSH, debug_location, p);
    //
    //There's two portions to this thing, right?
    //There's the regular lookup:  1 + tmp.c
    //And there's *assignment*:   tmp.c: 2
    //This only does the lookup?
    //
    //You have to be careful about .user.a  
    //
    //This method only accounts for globals, so the methods
    //that handle locals will need to be updated too.
    //actually, maybe we don't since arglocals treat
    //dot names as a single string?
    //
    //modifying the parser is tough b/c the
    //groupings look a lot different there.
  }

  K globals   = kN(function, FUNC_GLOBALS);
  K directory = kN(function, FUNC_DIRECTORY);
  bool sqly   = GET_ALT_ATTR(function, FUNC_ATTR_SQL_CLAUSE);

  I p = HASH_NULL;

  if(!creates && !parent && !sqly)
  {
    //Look in a subtree of the Kerf tree
    K1 k = denest_K1_from_start_and_key(The_Kerf_Tree, found_global, false, true);
    //Look in our function's list of globals
    p = lookupI(globals, found_global);

    bool absent_in_global_tree = !k.u;
    bool absent_in_global_list = IS_HASH_NULL(p);

    bool forthcoming = false || parent; //with a parent, any dir could get prepended

    //Can we reconstruct .user.a from a?
    K4 o; 
    ENUM(globals, K x = implode(kc('.'), klist2(directory, u, o)); if(matchC(x, found_global, 0)){forthcoming = true;} rd(x);)

    bool absent = absent_in_global_tree && absent_in_global_list && !forthcoming;

    if(absent)
    {
      //er(global)
      //show(function);
      //show(globals);
      //show(found_global);
      //show(The_Kerf_Tree);
      ERROR(ERROR_VARIABLE);
    }
  }
  
  p = insert_replace(globals, found_global, kn, false, false);

  add_code_byte_op_compression(bytecode, debug, GLOBAL_PUSH, debug_location, p);
}

void translate_arglocal(K *bytecode, K *debug, K function, K parent, K found_arglocal, I debug_location, bool creates)
{
  K arglocals = kN(function, FUNC_ARGLOCALS);

  I p;

  if(!creates)
  {
    p = lookupI(arglocals, found_arglocal);
    if(IS_HASH_NULL(p)) ERROR(ERROR_VARIABLE);
  }
  else
  {
    p = insert_replace(arglocals, found_arglocal, kn, false, false);
  }

  add_code_byte_op_compression(bytecode, debug, ARGLOCAL_PUSH, debug_location, p);
}

#pragma mark - Actual Byte Code Adding Methods
void add_code_byte_op_compression(K *bytecode, K *debug, C op, I debug_location, I uncompressed)
{
  //say CONSTANT_PUSH, 0x00, 0x02 --> contant_lookup_bytecode_op on POW2(q.h) bytes from q.buf
  K0 q = log_2_n_bytes_from_integer(uncompressed);
  add_code_byte(bytecode, debug, op, debug_location);
  add_code_byte(bytecode, debug, q.h,           debug_location);
  DO(POW2(q.h), add_code_byte(bytecode, debug, q.buf[i], debug_location))
}

void add_code_byte_op_compressed_3(K *bytecode, K *debug, C op, I debug_location, I uncompressed)
{ 
  I s = sizeof(I);
  C h = floor_log_2(s);
  K net = ki(htonll(uncompressed));

  add_code_byte(bytecode, debug, op, debug_location);
  add_code_byte(bytecode, debug, h,  debug_location);
  DO(s, add_code_byte(bytecode, debug, net->buf[i], debug_location))
}

void add_code_byte_op_flat(K *bytecode, K *debug, C op, I debug_location, I uncompressed)
{ 
  I s = sizeof(I);
  C h = floor_log_2(s);
  K net = ki(htonll(uncompressed));

  add_code_byte(bytecode, debug, op, debug_location);
  DO(s, add_code_byte(bytecode, debug, net->buf[i], debug_location))
}

void add_code_byte(K *bytecode, K *debug, C code, I debug_location)
{
  //fprintf(stderr, "%02x:", (UC)code);
  *bytecode = cow_add(*bytecode, kc(code));
  *debug    = cow_add(*debug, ki(htonll(debug_location)));
}


#pragma mark -

int func_go()
{
  return 0;
}


