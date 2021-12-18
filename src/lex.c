#include "kerf.h"

__kerfthread int lex_errno = 0;
__kerfthread int lex_errno_location = SENTINEL;
__kerfthread int parse_errno = 0;
__kerfthread int parse_errno_location = SENTINEL;

//S RESERVED_CONTROL_NAMES[] = {"return", "def", "function", "if", "do", "while", "for", 0};
  S RESERVED_NUMBER_NAMES[]  = {"inf", "nan", "infinity", 0};
  S RESERVED_NAME_NAMES[]    = {"nil", "null", "root", "true", "false", 0};
  S RESERVED_SQL_STARTS[]    = {"select", "update", "insert", "upsert", "delete", 0};
  S RESERVED_SQL_MIDDLES[]   = {"from", "group", "where", "order", "limit", "values", "set", "on", 0};//TODO: allow as map key, etc, eg {limit:1, set:22}
//S RESERVED_SQL_OTHER[]      = {"as", "into", "by", 0};

S token_names[] = 
{
  [TOKENS_NONE]         = "none",
  [TOKENS_VERB_SYM]     = "verb_sym",
  [TOKENS_VERB_WORD]    = "verb_word",
  [TOKENS_ADVERB_SYM]   = "adverb_sym",
  [TOKENS_ADVERB_WORD]  = "adverb_word",
  [TOKENS_NAME]         = "name",
  [TOKENS_NUMBER]       = "number",
  [TOKENS_BYTES]        = "bytes",
  [TOKENS_SEPARATOR]    = "separator",
  [TOKENS_ABS_DATE]     = "abs_date",
  [TOKENS_ABS_TIME]     = "abs_time",
  [TOKENS_ABS_DATETIME] = "abs_datetime",
  [TOKENS_REL_DATETIME] = "rel_datetime",
  [TOKENS_STRING]       = "string",
  [TOKENS_SINGLE_QUOTE] = "single_quote",
  [TOKENS_COMMENT]      = "comment",
  [TOKENS_WHITESPACE]   = "whitespace",
  [TOKENS_BACKTICK]     = "backtick",
  [TOKENS_BACKSLASH]    = "backslash",
  [TOKENS_LEFT]         = "left",
  [TOKENS_RIGHT]        = "right",
  [TOKENS_BIGGER]       = "bigger",

  [TOKENS_RESERVED]     = "reserved",

  [TOKENS_DEF]          = "ctrl_def",
  [TOKENS_IF]           = "ctrl_if",
  [TOKENS_DO]           = "ctrl_do",
  [TOKENS_WHILE]        = "ctrl_while",
  [TOKENS_FOR]          = "ctrl_for",
  [TOKENS_ELSE]         = "ctrl_else",
  [TOKENS_RETURN]       = "ctrl_return",
  [TOKENS_COLON]        = "colon",

  [TOKENS_SQL_START]    = "sql_start",
  [TOKENS_SQL_MIDDLE]   = "sql_middle",

  [TOKEN_GROUP_ALIKE]           = "group_alike",

  [TOKEN_GROUP_SQL]             = "group_sql",
  [TOKEN_GROUP_PLAIN]           = "group_plain",
  [TOKEN_GROUP_CURLY_BRACE]     = "group_curly",
  [TOKEN_GROUP_SQUARE_BRACKET]  = "group_square",
  [TOKEN_GROUP_ROUND_PAREN]     = "group_round",
  [TOKEN_GROUP_SEPARATION]      = "group_separation",

  [TOKEN_GROUP_LAMBDA_ARGS]     = "group_lambda_args",
  [TOKEN_GROUP_ASSIGNMENT]      = "group_assignment",
  [TOKEN_GROUP_BOUND_SQUARE]    = "group_bound_square",
  [TOKEN_GROUP_BOUND_ROUND]     = "group_bound_round",
  [TOKEN_GROUP_VERBAL_NNA]      = "group_verbal_nna",
  [TOKEN_GROUP_VERBAL_NVA]      = "group_verbal_nva",
  [TOKEN_GROUP_VERBAL_NA]       = "group_verbal_na",
  [TOKEN_GROUP_VERBAL_VA]       = "group_verbal_va",
  [TOKEN_GROUP_CONTROL]         = "group_control",

  [TOKEN_GROUP_REJECT_NUM_VAR]  = "reject_number_variable", ///eg 0a or 0_a
  
  [TOKEN_GROUP_SIZE] = NULL,
};

#pragma mark - Lexer DFAs and Character Classes

//Our strategy is to use hand-rolled DFAs instead of regex DFA generators
//There's a few reasons for this:
//1. It's more expedient right now 2014.11.19
//2. The fine grained state control we want probably eliminates regexes
//3. It's roughly the same difficulty to add new token classes
//4. We don't expect to add many token classes
//
//You could munge all these DFAs into a single DFA, but what's the point?
//Liable to be micro-faster, harder to read, harder to change.
//Even automatic DFA minimization is likely to be a bad move.

C lex_transition_grids[TOKENS_SIZE][MAX_DFA_STATES][256];
C parse_transition_grids[TOKEN_GROUP_SIZE][MAX_DFA_STATES][256];

//We could change this to eliminate about 20 lines that initalize chars
//to themselves-as-strings. Let's not since it's easy to clobber a required
//implicit class later if it doesn't have a listing here.
//
//We use this char array structure instead of the superior string because
//this lets us factor easily with the parse DFAs, where strings would
//be very awkward.
//
//Left is the class name (single char), right is the members
//This is easy, don't get scurred
C lex_classes[][MAX_DFA_CLASS_SIZE] = 
{
  ['v']  = "~!@#$%^&*_-+=:|<>.?/\\",
  ['A']  = "/\\=<>~", //add '1' if you want to deal with that & force monadic
  ['a']  = "$._ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
  ['b']  = "._ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
  ['d']  = "0123456789",
  ['e']  = "eE",
  ['.']  = ".",
  ['-']  = "-",
  ['+']  = "+",
  ['0']  = "0",
  ['x']  = "xX",
  ['h']  = "0123456789ABCDEFabcdef",
  ['s']  = ",;\n", //separators
  [':']  = ":",
  ['T']  = "T",
  ['r']  = "YMDHISymdhis",
  ['"']  = "\"", //double quote
  ['\\'] = "\\", //backslash
  ['u']  = "u",
  ['\''] = "'",  //single quote
  ['/']  = "/",
  ['\n'] = "\n", //newline
  ['w']  = " \x20\t\v\f\r", //whitespace; \x20 is a dupe of ' ' for redundancy
  ['`']  = "`",
  ['R']  = "'\"\n",//stops backtick- Reject
  ['g']  = "([{",
  ['G']  = ")]}",
  ['=']  = "=",
  ['<']  = "<",
  ['>']  = ">",
  ['!']  = "!",
  ['o']  = '*',
  //Special
  ['*']  = "*",  //special that handles all ascii chars,
};

//Unless you don't know DFAs, in which case, yeah, you need to do your homework
DFA LEX_DFAS[] =
{
  //Order here doesn't matter. Order in enum in the .h matters
  //Process ascii classes in reverse order: earlier appearing take precedence in ties
  //Repeating the enum has certain nice properties, among others: repeat mappings
  //Tie-breaking inter-DFA is based on order in TOKENS_MEMBERS
  [TOKENS_COLON]        = {TOKENS_COLON, ":", "01", "RA", "10"},
  [TOKENS_VERB_SYM]     = {TOKENS_VERB_SYM, "v", "01", "RA", "10"},

  [TOKENS_NUMBER]       = {TOKENS_NUMBER, "de.-+", "0123456", "RIIIAAA", 
                                          "40100"
                                          "50000"
                                          "60000"
                                          "60022"
                                          "43500"
                                          "53000"
                                          "60000"},

  [TOKENS_NAME]         = {TOKENS_NAME, "ab", "01", "RA", 
                                        "10"
                                        "11"},


//Intentionally disabled for now to save time.
//To turn on you'll need to complete a parse_bytes path mirroring parse_number
//Use strtoll on pairs but beware ragged half-pair 0x123
//ETA: <30m 
//  [TOKENS_BYTES]        = {TOKENS_BYTES, "0xh", "0123", "RIIA", 
//                                         "100"
//                                         "020"    
//                                         "303"    
//                                         "303"},

  [TOKENS_SEPARATOR]    = {TOKENS_SEPARATOR, "s", "01", "RA", "10"},

  [TOKENS_ABS_DATETIME] = {TOKENS_ABS_DATETIME, "d.-T:", "0123456789ABC", "RHHHHHIIIAAAA", 
                                                "10000"
                                                "12200"
                                                "30000"
                                                "34400"
                                                "50000"
                                                "50060"
                                                "70000" 
                                                "70008" 
                                                "90000" 
                                                "9000A" 
                                                "B0000" 
                                                "BC000" 
                                                "C0000"},
 
  [TOKENS_ABS_DATE]     = {TOKENS_ABS_DATE, "d.-", "012345", "RRRRIA", 
                                            "100"
                                            "120"
                                            "300"
                                            "340"
                                            "500"
                                            "500"},

  //Arguably we could reject "1:1:" and "1:1:1."
  [TOKENS_ABS_TIME]     = {TOKENS_ABS_TIME, "d:.", "0123456", "RIIAAAA",
                                            "100"
                                            "120"
                                            "300"
                                            "340"
                                            "500"
                                            "506"
                                            "600"},


  [TOKENS_REL_DATETIME] = {TOKENS_REL_DATETIME, "dr.-", "0123", "RIIA", 
                                                "1000"
                                                "1300"
                                                "1000"
                                                "1022"},

#if DATES_ALLOW_DASHED
  [TOKENS_ABS_DATE_ALT]     = {TOKENS_ABS_DATE, "d-", "012345", "RHHHIA", 
                                                "10"
                                                "12"
                                                "30"
                                                "34"
                                                "50"
                                                "50"},
//                                                                       2016-02-03
//  [TOKENS_ABS_DATE_ALT]     = {TOKENS_ABS_DATE, "d-", "0123456789A", "RRRRHHHHHIA", 
//                                                "10"
//                                                "20"
//                                                "30"
//                                                "40"
//                                                "05"
//                                                "60"
//                                                "70"
//                                                "08"
//                                                "90"
//                                                "A0"
//                                                "00"},
 
#else
  [TOKENS_ABS_DATE_ALT]     = {0},
#endif
 
   
  //Double and single quoted strings. We could allow \uX\b and such if we want
  [TOKENS_STRING]       = {TOKENS_STRING, "\"\\uh*", "01234567", "RIIIIIIA", "10000"
                                                                             "72111"
                                                                             "11311"
                                                                             "02141"
                                                                             "02151"
                                                                             "02161"
                                                                             "02111"
                                                                             "10000"},

  [TOKENS_SINGLE_QUOTE] = {TOKENS_STRING, "'\\uh*", "01234567", "RIIIIIIA",  "10000"
                                                                             "72111"
                                                                             "11311"
                                                                             "02141"
                                                                             "02151"
                                                                             "02161"
                                                                             "02111"
                                                                             "10000"},
  [TOKENS_WHITESPACE]   = {TOKENS_WHITESPACE, "w", "01", "RA", "11"},

  [TOKENS_COMMENT]      = {TOKENS_COMMENT, "/\n*", "012", "RIA", "100"
                                                                 "200"
                                                                 "202"},

  [TOKENS_BACKTICK]     = {TOKENS_BACKTICK, "R`bv", "012", "RIA", 
                                            "0100"
                                            "0222"
                                            "0000"},

  [TOKENS_ADVERB_SYM]   = {TOKENS_ADVERB_SYM, "\\A", "012", "RIA",
                                               "10"
                                               "22"
                                               "00"},

  [TOKENS_BACKSLASH]    = {TOKENS_BACKSLASH, "\\A*", "012", "RIR", //off. "RIA" to turn on
                                              "120"
                                              "222"
                                              "020"},

  [TOKENS_BIGGER]       = {TOKENS_VERB_SYM, "=><!o", "01234", "RIIIA", 
                                            "11213"
                                            "40000"    
                                            "44000"    
                                            "00004"
                                            "00000"},

  [TOKENS_LEFT]         = {TOKENS_LEFT,  "g", "01", "RA", "10"},
  [TOKENS_RIGHT]        = {TOKENS_RIGHT, "G", "01", "RA", "10"},

  [TOKENS_SIZE]         = {0}, //required if some enum members have no DFA
};

#pragma mark - Parser DFAs and Classes

C parse_classes[][MAX_DFA_CLASS_SIZE] =
{
  ['A'] = {TOKENS_NAME, TOKENS_STRING, 0},
  ['N'] = {TOKENS_NAME,0},
  ['0'] = {TOKENS_NUMBER,0},
  ['d'] = {TOKENS_ABS_DATE, TOKENS_ABS_TIME, TOKENS_ABS_DATETIME,0},
  ['s'] = {TOKENS_WHITESPACE, TOKENS_COMMENT, 0},

  ['C'] = {TOKENS_DO, TOKENS_WHILE, TOKENS_FOR,0},
  ['D'] = {TOKENS_DEF,0},
  ['I'] = {TOKENS_IF,0},
  ['E'] = {TOKENS_ELSE,0},
  
  ['('] = {TOKEN_GROUP_ROUND_PAREN,0},
  ['['] = {TOKEN_GROUP_SQUARE_BRACKET,0},
  ['{'] = {TOKEN_GROUP_CURLY_BRACE,0},
  ['v'] = {TOKENS_VERB_SYM, TOKENS_VERB_WORD,0},
  ['a'] = {TOKENS_ADVERB_SYM, TOKENS_ADVERB_WORD,0},
  ['b'] = {TOKEN_GROUP_BOUND_ROUND,0},
  ['w'] = {TOKENS_VERB_WORD,0},
  ['W'] = {TOKENS_ADVERB_WORD,0},
  [':'] = {TOKENS_COLON,0},
  ['p'] = {TOKENS_NAME, TOKENS_VERB_WORD, TOKEN_GROUP_CURLY_BRACE, TOKENS_SELF, 0},  //p( binds. Adverbs disallowed for now: see notes.

  //^ whitespace comment adverb verb_sym verb_word ?  might be smarter
  ['n'] = {TOKENS_NUMBER,TOKENS_NAME,TOKENS_BYTES, TOKENS_ABS_DATE, TOKENS_ABS_TIME, TOKENS_ABS_DATETIME,
           TOKENS_REL_DATETIME, TOKENS_STRING, TOKENS_SINGLE_QUOTE, TOKENS_BACKTICK, TOKENS_BACKSLASH, TOKENS_BIGGER,
           TOKENS_RESERVED, TOKEN_GROUP_ALIKE, TOKEN_GROUP_SQL, TOKEN_GROUP_PLAIN, TOKEN_GROUP_CURLY_BRACE,
           TOKEN_GROUP_SQUARE_BRACKET, TOKEN_GROUP_ROUND_PAREN, TOKEN_GROUP_BOUND_SQUARE, TOKEN_GROUP_BOUND_ROUND,
           TOKEN_GROUP_ALIKE, TOKEN_GROUP_CONTROL, 0},

  //Special
  ['*'] = {0},//special ALL
};

DFA PARSE_DFAS[] =
{
  [TOKENS_NUMBER]       = {TOKEN_GROUP_ALIKE, "0s", "01", "RA", 
                                              "10"
                                              "11"},

  [TOKENS_ABS_DATETIME] = {TOKEN_GROUP_ALIKE, "ds", "01", "RA", 
                                              "10"
                                              "11"},

  [TOKEN_GROUP_ASSIGNMENT] = {TOKEN_GROUP_ASSIGNMENT, "A[sv:", "01234", "RIIIA", 
                                                      "10000"
                                                      "01234"
                                                      "00234"
                                                      "00004"
                                                      "00000"}, 

  [TOKEN_GROUP_BOUND_ROUND] = {TOKEN_GROUP_BOUND_ROUND, "p(", "012", "RIA", 
                                                        "10"
                                                        "02"
                                                        "00"},

  [TOKEN_GROUP_BOUND_SQUARE] = {TOKEN_GROUP_BOUND_SQUARE, "[n", "012", "RIA",  //backwards because [ ⊂ n
                                                          "11"
                                                          "20" //n[][] same as n[;] in fwd order
                                                          "20"},

  //DO WHILE FOR
  [TOKEN_GROUP_CONTROL] = {TOKEN_GROUP_CONTROL, "C({", "0123", "REEA", 
                                                "100"
                                                "020"
                                                "003"
                                                "000"},

//  [TOKENS_DEF] = {TOKEN_GROUP_CONTROL, "DN({", "01234", "REEEA", 
//                                       "1000"
//                                       "0200"
//                                       "0030"
//                                       "0004"
//                                       "0000"},

  [TOKENS_DEF] = {TOKEN_GROUP_CONTROL, "Db{", "0123", "REEA", //'b' b/c Name gets captured w/ "()" as bound round
                                       "100"
                                       "020"
                                       "003"
                                       "000"},

  [TOKENS_IF] = {TOKEN_GROUP_CONTROL, "I({E", "012345", "REEAEA", //  I({[EI({]*[E{]
                                      "1000"
                                      "0200"
                                      "0030"
                                      "0004"
                                      "1050"
                                      "0000"},

//If munged, we just have to separate it again later, and the separation is complicated
//  [TOKEN_GROUP_VERBAL] = {TOKEN_GROUP_VERBAL, "nva", "0123", "RIIA", // X: [[n]na+ | [n]va*]
//                                              "130"
//                                              "233"
//                                              "003"
//                                              "003"},

  [TOKEN_GROUP_VERBAL_NNA] = {TOKEN_GROUP_VERBAL_NNA, "na", "0123", "RRRA", //nna+
                                                      "10"
                                                      "20"
                                                      "03"
                                                      "03"},

  [TOKEN_GROUP_VERBAL_NVA] = {TOKEN_GROUP_VERBAL_NVA, "nva", "012", "RRA", //nva*
                                                      "100"
                                                      "020"
                                                      "002"},

  [TOKEN_GROUP_VERBAL_NA] = {TOKEN_GROUP_VERBAL_NA, "na", "012", "RRA", //na+
                                                    "10"
                                                    "02"
                                                    "02"},

  [TOKEN_GROUP_VERBAL_VA] = {TOKEN_GROUP_VERBAL_VA, "va", "01", "RA", //va*
                                                    "10"
                                                    "01"},

  [TOKEN_GROUP_REJECT_NUM_VAR] = {TOKEN_GROUP_REJECT_NUM_VAR, "0N", "012", "RRE", 
                                                              "10"
                                                              "02"
                                                              "22"},

  [TOKEN_GROUP_SIZE]         = {0}, //required if some enum members have no DFA
};


#pragma mark - Init

void reserved_init()
{
  The_Reserved = new_map();

  S *s = NULL;

//for(s=RESERVED_CONTROL_NAMES; *s; s++) update(The_Reserved, kcv(*s), ki(TOKENS_CONTROL));
  for(s=RESERVED_NUMBER_NAMES;  *s; s++) update(The_Reserved, kcv(*s), ki(TOKENS_NUMBER));
  for(s=RESERVED_NAME_NAMES;    *s; s++) update(The_Reserved, kcv(*s), ki(TOKENS_RESERVED));
  for(s=RESERVED_SQL_STARTS;    *s; s++) update(The_Reserved, kcv(*s), ki(TOKENS_SQL_START));
  for(s=RESERVED_SQL_MIDDLES;   *s; s++) update(The_Reserved, kcv(*s), ki(TOKENS_SQL_MIDDLE));

  reserved_verbs_init();
  reserved_adverbs_init();

  update(The_Reserved, kcv("self"),     ki(TOKENS_SELF));
  update(The_Reserved, kcv("this"),     ki(TOKENS_SELF));
  update(The_Reserved, kcv("def"),      ki(TOKENS_DEF));
  update(The_Reserved, kcv("function"), ki(TOKENS_DEF));
  update(The_Reserved, kcv("if"),       ki(TOKENS_IF));
  update(The_Reserved, kcv("do"),       ki(TOKENS_DO));
  update(The_Reserved, kcv("while"),    ki(TOKENS_WHILE));
  update(The_Reserved, kcv("for"),      ki(TOKENS_FOR));
  update(The_Reserved, kcv("else"),     ki(TOKENS_ELSE));
  update(The_Reserved, kcv("return"),   ki(TOKENS_RETURN));
}

void dfa_populate(DFA dfa_array[], I array_len, C class_maps[][MAX_DFA_CLASS_SIZE])
{
  //Initialize the transition grids
  //We expand the shorthand used in the DFA structs to real transition matrices
  //of width 256 (all ascii/char values), one row for each state, cells are
  //non-negative integers of type char corresponding to a state.
  DO(array_len, DFA dfa = dfa_array[i]; 
                I token_kind = i;
                C *grid   = dfa.grid;
                S states  = dfa.state_names;
                S classes = dfa.ascii_classes;
                S chart   = dfa.init_string;

                if(!states || !classes) continue;//unitialized DFA in array
                I N_states  = strlen(states);
                I N_classes = strlen(classes);

                assert(strlen(chart) == N_states * N_classes);
                assert(strlen(dfa.state_kinds) == N_states);

                DO(N_states,
                  DO2(N_classes, I back = (N_classes - 1) - j;//step backwards
                                 C class = classes[back];
                                 C destination = chart[i*N_classes + back];
                                 UC state_index = strchr(states, destination) - states;

                                 assert(state_index >= 0);
                                 assert(state_index < MAX_DFA_STATES);

                                 S map = class_maps[class];
                                 I N_map = strlen(map);
                                 if('*' == class)//special: all
                                   DO3(256,   grid[i*256 +     k ] = state_index)
                                 else 
                                   DO3(N_map, grid[i*256 + map[k]] = state_index;)
                  )

                //DO3(127, O("%d", grid[i*256 + k]))O("\n"); //print
                )
                //O("\n");
  )
}

void dfa_init()
{
  I max_states = 0;
  I max_class_size = 0;

  DO(ARRAY_LEN(  LEX_DFAS), S s =   LEX_DFAS[i].state_names; if(s) max_states = MAX(max_states, strlen(s)))
  DO(ARRAY_LEN(PARSE_DFAS), S s = PARSE_DFAS[i].state_names; if(s) max_states = MAX(max_states, strlen(s)))
  assert(max_states == MAX_DFA_STATES);//enforce grids not ridiculously large in .bss

  DO(ARRAY_LEN(lex_classes),   S s =   lex_classes[i]; if(s) max_class_size = MAX(max_class_size, strlen(s)))
  DO(ARRAY_LEN(parse_classes), S s = parse_classes[i]; if(s) max_class_size = MAX(max_class_size, strlen(s)))
  assert(max_class_size == MAX_DFA_CLASS_SIZE);//enforce grids not ridiculously large in .bss

  assert(TOKEN_GROUP_SIZE <= 256); //so the parser can use the 256 char thing from the lexer

  //For each token class, assign it the space for its transition grid (.bss again)
  DO(ARRAY_LEN(LEX_DFAS), LEX_DFAS[i].grid = &lex_transition_grids[i])
  DO(ARRAY_LEN(PARSE_DFAS), PARSE_DFAS[i].grid = &parse_transition_grids[i])

  dfa_populate(LEX_DFAS, ARRAY_LEN(LEX_DFAS), lex_classes);
  dfa_populate(PARSE_DFAS, ARRAY_LEN(PARSE_DFAS), parse_classes);
}

void lex_init()
{
  reserved_init();
  dfa_init();
  reset_lex_parse_globals();
}

#pragma mark - Tools

K lower(K x)
{
  K y = new_k(-CHAR, 0);

  if(IS_CHARVEC(x))
  {
    ENUM(x, y = cow_add(y, kc(tolower(vc))))
  }

  return y;
}

I reserved_lookup(K x, bool case_insensitive)
{
  if(IS_CHARVEC(x))
  {
    K y = case_insensitive ? lower(x) : strong(x); 
    K0 o;
    K z = LOOKUP_(The_Reserved, y, o);
    rd(y);
    if(z) return zi;
  }

  return TOKENS_NONE;
}

K parse_number(K x)
{
  //optional: you could also allow 1E6 to be INT not FLOAT

  K z = NULL;

  I n = cx;
  K y = new_k(-CHAR, n+1);
  yC[n] = '\0';
  yn = n;
  ENUM(x, yC[i] = vc)

  bool has_decimal = !!strchr(yC,'.');
  bool has_exponent = !!strpbrk(yC, "eE");  

  bool has_special = false;

  //Float's strtod handles -NaN NaN Nan nan ...
  //Int's strtoll needs special help
  S special_strings[] = {"-NAN", "NAN", "-INF", "INF", "-INFINITY","INFINITY"};
  I special_ints[] = {IN, IN, -II, II, -II, II};
  I special_value = 0; 

  DO(ARRAY_LEN(special_strings), if(!strcmp(yC, special_strings[i])){
                                  has_special = true;
                                  special_value = special_ints[i];
                                 }
  )

  //whoops, hack fix
  bool insensitive_special = false;
  DO(ARRAY_LEN(special_strings), if(!strcasecmp(yC, special_strings[i])){
                                  insensitive_special = true;
                                 }
  )

  bool is_float = has_decimal || has_exponent || insensitive_special;
  bool is_int = has_special || !is_float;

  if(is_int)
  {
    z = new_k(INT, 0);

    if(has_special)
    {
      zi = special_value;
    }
    else
    {
      zi = strtoll(yC, 0, PARSE_ZERO_PREFIX_MEANS_OCTAL ? 0 : 10);
    }
  }
  else
  {
    z = new_k(FLOAT, 0);
    zf = strtod(yC, 0);
  }

  rd(y);

  return z;
}

#pragma mark - Lexer / Tokenizer

K lex(K snippet)
{
  reset_lex_parse_globals();

  K error = NULL;

  I current_start = 0;

  K token_list = new_k(LIST, 0);
  work_push(token_list);

  K u = NULL;
  
  while(current_start < COUNT(snippet))
  {
    I travelled = 0;

    u = toke(snippet, current_start, &travelled);

    if(!u)
    {
      goto fail;
    }

    work_pop();
    token_list = cow_add(token_list, u);
    work_push(token_list);

    rd(u);
    current_start += travelled;
  }

  if(lex_errno)
  {
    goto fail;
  }
  
succeed:
  work_pop();
  return token_list;

fail:
  error = new_error_map_lexing_parsing(lex_errno, lex_errno_location, snippet);
  reset_lex_parse_globals();

  work_pop_rd(true);
  return error;
}

K toke(K snippet, I start, I *travelled)
{
  I position = start;
  *travelled = 0;

  C prev_states[TOKENS_SIZE] = {0};
  C      states[TOKENS_SIZE] = {0};
  I run_lengths[TOKENS_SIZE] = {0}; 

  I max_run = 0;
  I max_run_index = 0;
  I max_run_state = 0;

  I legit_position  = position;

  I legit_max       = 0;
  I legit_max_index = 0;
  I legit_max_state = 0;

  while(position < COUNT(snippet))
  {
    C ascii = kC(snippet)[position];

    I current_max = 0;
    I current_max_index = 0;
    I current_max_state = 0;
    C current_max_kind  = '\0';

    //step
    DO(TOKENS_SIZE, DFA dfa = LEX_DFAS[i]; 
                    C *grid = dfa.grid;
                    C *state_kinds = dfa.state_kinds;
                    if(!state_kinds) continue;
                    I before = states[i];
                    prev_states[i] = before;
                    states[i] = grid[256*before + ascii];
                    run_lengths[i]++;
                    C kind = state_kinds[states[i]];
                    if(0==states[i]) run_lengths[i] = 0;

                    bool special_capture = false;

                    //special case `"..." and `'...'
                    bool backtick_string = (TOKENS_STRING==i || TOKENS_SINGLE_QUOTE==i)
                    && (TOKENS_BACKTICK==max_run_index) && (1==max_run) && (1==run_lengths[i]);
                    if(backtick_string)
                    {
                      special_capture = true;
                    }

                    if(special_capture)
                    {
                      run_lengths[i] = 1 + max_run;
                    }

                    SW(kind)
                    {
                      CS('R', )

                      CSF('H',) //H for Hopeful
                      CSF('I',) 
                      CSF('A',)
                      CD:     
                              if(current_max < run_lengths[i])
                              {
                                // "<" ensures we take the first token kind in the event of a tie
                                current_max       = run_lengths[i];
                                current_max_index = i;
                                current_max_state = states[i];
                                current_max_kind  = kind;
                             }
                    }


    )

    if(current_max <= max_run)
    {
      break;
    }

    //keep stepping
    max_run       = current_max;
    max_run_index = current_max_index;
    max_run_state = current_max_state;

    SW(current_max_kind)
    {
      CS('H',)
      CD:
          legit_max       = max_run;
          legit_max_index = max_run_index;
          legit_max_state = max_run_state;
          legit_position  = position;
    }

    position++;
  }

  max_run       = legit_max;
  max_run_index = legit_max_index;
  max_run_state = legit_max_state;

  *travelled = max_run;

  if(max_run <= 0)
  {
    lex_errno = ERROR_LEX_UNKNOWN;
    lex_errno_location = position; 
    return NULL;
  }

  I i     = max_run_index;
  I state = max_run_state;
  C kind  = LEX_DFAS[i].state_kinds[state];

  SW(kind)
  {
    CS('I', lex_errno=ERROR_LEX_INCOMPLETE; lex_errno_location = legit_position; R NULL)
    CS('R', lex_errno=ERROR_LEX_UNKNOWN;    lex_errno_location = legit_position; R NULL)
  }

  I token_index = max_run_index;
  I mark = LEX_DFAS[token_index].bundle_mark;

  K payload = new_k(-CHAR,0);
  DO(max_run, K item = kc(kC(snippet)[start+i]); payload = cow_add(payload, item));

  I end = start + max_run;

  //Relabel reserved
  if(TOKENS_NAME == mark)
  {
    bool insensitive = PARSE_RESERVED_CASE_INSENSITIVE;    
      
    I reservation = reserved_lookup(payload, insensitive);

    if(TOKENS_NONE != reservation)
    {
      assert(0 <= reservation);
      assert(reservation < TOKENS_SIZE); 

      mark = reservation; 
    }
  }

  assert(0 <= mark);
  assert(mark < TOKENS_SIZE);

  //Put all the validated token information into a LIST
  K bundle = token_bundle(mark, start, end, payload);

  return bundle;
}

K token_bundle(I mark, I start, I end, K payload)
{
  K bundle = new_k(LIST, TOKEN_SIZE);

  S name = token_names[mark];
  if(!name) name = "not_listed";
  K kname = new_k(-CHAR,0);
  DO(strlen(name), kname = cow_add(kname, kc(name[i])))

  if(IS_LIST(payload))
  {
    I snippet_start = 0;
    I snippet_end = 0;
    I p = COUNT(payload);

    if(p > 0)
    {
      K0 o1,o2,o3,o4;
      K first = AT2(payload, ki(0),o1); 
      K last = AT2(payload, ki(p-1),o2); 

      snippet_start = AT2(first, ki(TOKEN_SNIPPET_START),o3)->i;
      snippet_end = AT2(last, ki(TOKEN_SNIPPET_END),o4)->i;
    }

    if(-1==start) start = snippet_start;
    if(-1==end)   end   = snippet_end;
  }

  nestset_ri_rd(bundle, TOKEN_KIND,           ki(mark),  false, false);
  nestset_ri_rd(bundle, TOKEN_KIND_STRING,    kname,     false, false);//optional
  nestset_ri_rd(bundle, TOKEN_SNIPPET_START,  ki(start), false, false);
  nestset_ri_rd(bundle, TOKEN_SNIPPET_END,    ki(end),   false, false);
  nestset_ri_rd(bundle, TOKEN_PAYLOAD,        payload,   false, false);

  return bundle;
}

#pragma mark - Parser 

I check_parenthetical_groupings(K tokens)
{
  I bad_location = 0;
  //Splitting this out as a pass simplifies
  //other parsing code later

  S left = lex_classes['g'];
  S right = lex_classes['G'];

  assert(!strcmp(left, "([{"));
  assert(!strcmp(right,")]}"));

  I depth = 0;
  C stack[PARSE_MAX_DEPTH+1] = {0};
//  bool sql_started[PARSE_MAX_DEPTH+1] = {false};
  
  ENUM(tokens,
    K peek    = v;

    K0 o1,o2,o3,o4;
    I kind    = AT2(peek, ki(TOKEN_KIND),o1)->i;
    K payload = AT2(peek, ki(TOKEN_PAYLOAD),o2);
    I token_start = AT2(peek, ki(TOKEN_SNIPPET_START),o3)->i;
    I token_end   = AT2(peek, ki(TOKEN_SNIPPET_END),o4)->i;

    bad_location = token_start;

//    //SQL is a "parenthetical" grouping.
//    if((TOKENS_SQL_START == kind))
//    {
//      if(sql_started[depth])
//      {
//        //Strictly, I don't think we need to reject on SQL
//        //nested raw outside of "([{" groups; still, 
//        //having trouble thinking of a use case and this
//        //is more expedient for now 2014.12.02
//        //Full support is tricky.
//        parse_errno = ERROR_PARSE_NESTED_SQL;
//        //goto fail;
//      }
//
//      sql_started[depth] = true;
//    }

    if(TOKENS_LEFT != kind && TOKENS_RIGHT != kind)
    {
      continue;//ignore most everything
    }

    C found = kC(payload)[0];

    if(TOKENS_LEFT == kind)
    {
      stack[depth++] = found;

      if(depth > PARSE_MAX_DEPTH)
      {
        parse_errno = ERROR_PARSE_DEPTH;
        goto fail;
      }

//      sql_started[depth] = false;
      assert(*strchr(left, found));
    }

    if(TOKENS_RIGHT == kind)
    {
      if(depth <= 0)
      {
        parse_errno = ERROR_PARSE_OVERMATCH;
        goto fail;
      }

      //The left item we read in earlier
      C stored = stack[--depth];

      //What the left item should be based on
      //the right item we just read
      assert(*strchr(right, found));
      I index = strchr(right, found) - right;
      C expectation = left[index]; 

      if(stored != expectation)
      {
        parse_errno = ERROR_PARSE_MISMATCH;
        goto fail;
      }
    }
  )

  if(depth > 0)
  {
    parse_errno = ERROR_PARSE_UNMATCH;
    goto fail;
  }

succeed:
  parse_errno = 0;
  parse_errno_location = 0;
  return 0;

fail:
  parse_errno_location = bad_location;
  return -1;
}

K parse(K snippet, K tokens)
{
  reset_lex_parse_globals();

  K error = NULL;

  //If you eliminate this check then you need to track vs.
  //PARSE_MAX_DEPTH in the recursive scooping functions
  I check = check_parenthetical_groupings(tokens);

  if(check)
  {
    goto fail;
  }

  I ran = 0;
  K scooped = scoop_top(tokens, TOKEN_GROUP_PLAIN, 0, COUNT(tokens), &ran);

  if(!scooped) goto fail;
 
succeed:
  return scooped;

fail:
  error = new_error_map_lexing_parsing(parse_errno, parse_errno_location, snippet);
  reset_lex_parse_globals();
  return error;
}

K scoop_top(K tokens, I top_type, I start, I length, I *top_ran)
{
  //Indirect/Mutually recursive function pair, with scoop_separator
  //scoop_top -> scoop_separator -> scoop_top -> ...
  //Like a recursive descent parser.
  //While the lexer is pretty close to ideal, I have no idea
  //how close this is to ideal. I can see a fine solution
  //and don't have time to keep looking. 2014.12.10

  K list = new_k(LIST, 0);

  I p = start;
  I end = start + length;

  I bstart = -1;
  I bend   = -1;

  while(p < end)
  {
    K0 o1,o2,o3;
    K peek    = AT2(tokens, ki(p),o1); 
    I kind    = AT2(peek, ki(TOKEN_KIND),o2)->i;
    K payload = AT2(peek, ki(TOKEN_PAYLOAD),o3);

    I continue_flag = false;
    I break_flag = false;

    SW(top_type)
    {
      CS(TOKEN_GROUP_SQL,
        SW(kind)
        {
          CS(TOKENS_RIGHT, break_flag = true)
        }
      )
     CSF(TOKEN_GROUP_CURLY_BRACE,)
     CSF(TOKEN_GROUP_SQUARE_BRACKET,)
      CS(TOKEN_GROUP_ROUND_PAREN,
        SW(kind)
        {
          CS(TOKENS_LEFT,
            if(start == p)
            {
              K0 o;
              bstart = AT2(peek, ki(TOKEN_SNIPPET_START),o)->i;
              p += 1;
              continue_flag = true;
            }
          )
          CS(TOKENS_RIGHT, 
              K0 o;
              bend = AT2(peek, ki(TOKEN_SNIPPET_END),o)->i;
              p += 1; 
              break_flag = true
          )
        }
      )
    }

    if(TOKENS_SEPARATOR == kind)
    {
      //SQL groups end at the ';' separator, others continue [1;2] (3;4;)
      if(TOKEN_GROUP_SQL == top_type && ';'==kC(payload)[0])
      {
        break_flag = true;
      }
      else
      {
        p += 1;
        //continue_flag = true; //not commenting this suppresses empty separations?
      }
    }

    if(continue_flag) continue;
    if(break_flag) break;

    I scooped_ran = 0;

    K scooped = scoop_separator(tokens, top_type, p, end - p, &scooped_ran); 

    if(!scooped) goto fail;

    list = cow_add(list, scooped);

    rd(scooped);

    p += scooped_ran;

  }

  *top_ran = p - start;
  K bundle = token_bundle(top_type, bstart, bend, list);
succeed:
  return bundle;
fail:
  rd(list);
  return NULL;
}

K scoop_separator(K tokens, I scoop_type, I start, I length, I *middle_ran)
{

  K bundle = NULL;
  K list = new_k(LIST, 0);

  I p = start;
  I end = start + length;

  //Pass 1: Group parentheses, brackets, braces (and SQL)
  //If this thing encounters a LEFT_([{ or SQL_START it will
  //call scoop_top with corresponding arg before continuing.
  //The point is to get a "flat" version for Pass 2.

  while(p < end)
  {
    K0 o1,o2,o3;
    K peek    = AT2(tokens, ki(p),o1); 
    I kind    = AT2(peek, ki(TOKEN_KIND),o2)->i;
    K payload = AT2(peek, ki(TOKEN_PAYLOAD),o3);

    I ran = 0;

    I break_flag = false;

    SW(kind)
    {
      CS(TOKENS_SQL_START,
        if(TOKEN_GROUP_SQL == scoop_type)
        {
          //if(p == start)
          //{
            K0 o;
            list = cow_add(list, AT2(tokens,ki(p),o));
            ran = 1;
          //}
        }
        else
        {
          K scooped = scoop_top(tokens, TOKEN_GROUP_SQL, p, end - p, &ran); 
          if(!scooped) goto fail;
          list = cow_add(list, scooped);
          rd(scooped);
        }
      )
      CS(TOKENS_SQL_MIDDLE, 
        
        if(p == start)
        {
          K0 o;
          list = cow_add(list, AT2(tokens,ki(p),o));
          ran = 1;
        }
        else
        {
          break_flag = true;
          break;
        }
      )
      CS(TOKENS_LEFT,
        C ascii = *kC(payload);

        S left = lex_classes['g'];
        assert(!strcmp(left, "([{"));

        I index = strchr(left, ascii) - left;
        assert(index < strlen(left));

        I types[] = {TOKEN_GROUP_ROUND_PAREN, TOKEN_GROUP_SQUARE_BRACKET, TOKEN_GROUP_CURLY_BRACE};
        I top_type = types[index];

        K scooped = scoop_top(tokens, top_type, p, end - p, &ran); 
        if(!scooped) goto fail;
        list = cow_add(list, scooped);
        rd(scooped);
      )
     CSF(TOKENS_RIGHT,)
      CS(TOKENS_SEPARATOR, break_flag = true;)
      CD:
      {
        K0 o;
        list = cow_add(list,AT2(tokens,ki(p),o));
        ran = 1;
      }
    }

    p += ran;

    if(break_flag) break;
  }

  //Pass 2: With everything grouped (flat), resolve as DFA thingies
  //        This lets us avoid descending into a built tree later
  //        We can modify the flat list without any overhead.
  list = flat_pass(list, scoop_type);

  if(!list) goto fail;

no_mo:
  *middle_ran = p - start; 

  bundle = token_bundle(TOKEN_GROUP_SEPARATION, -1, -1, list);

succeed:
  return bundle;
fail:
  return NULL;
}

K flat_pass(K list, I scoop_type)
{
  //-----------------
  //RE-MARK LAMBDA ARGS      -dependencies: almost everything? probably negate. VERBAL would capture
  //NEGATE NUMBERS           -dependencies: before drop spacing, before ALIKE
  //-----------------
  //TOKEN_GROUP_ALIKE        -dependencies: after negate numbers, hacked to work before drop spaces
  //TOKEN_GROUP_ASSIGNMENT   -dependencies: before SQUARE; so hacked to work before DROP SPACES
  //-----------------
  //TOKEN_GROUP_BOUND_SQUARE -dependencies: after alike, after assign, before drop spacing
  //TOKEN_GROUP_BOUND_ROUND  -dependencies: before drop spacing
  //----------------- 
  //DROP WHITESPACE
  //-----------------
  //TOKEN_GROUP_CONTROL      -dependencies: after drop spacing
  //TOKEN_GROUP_VERBAL       -dependencies: after alike, bound, drop spacing
  //-----------------

  I batch_alike[]           = {TOKENS_NUMBER, TOKENS_ABS_DATETIME, 0};
  I batch_assignment[]      = {TOKEN_GROUP_ASSIGNMENT, 0};
  I batch_bound_round[]     = {TOKEN_GROUP_BOUND_ROUND, 0};
  I batch_bound_square[]    = {TOKEN_GROUP_BOUND_SQUARE, 0};
  I batch_reject_num_var[]  = {TOKEN_GROUP_REJECT_NUM_VAR, 0};
  I batch_control[]         = {TOKEN_GROUP_CONTROL, TOKENS_DEF, TOKENS_IF, 0};
  I batch_verbiage[]        = {TOKEN_GROUP_VERBAL_NNA, TOKEN_GROUP_VERBAL_NVA, TOKEN_GROUP_VERBAL_NA, TOKEN_GROUP_VERBAL_VA, 0};

 
  ///////////////////////////////////////////////////

  if(TOKEN_GROUP_CURLY_BRACE == scoop_type)
  {
    if(COUNT(list) > 0)
    {
      K0 o1,o2;
      K first = AT2(list, ki(0),o1);
      K v = first;
      I kind = AT2(v, ki(TOKEN_KIND),o2)->i;
      if(TOKEN_GROUP_SQUARE_BRACKET == kind)
      {
        K c = copy(v);
        update(c, ki(TOKEN_KIND), ki(TOKEN_GROUP_LAMBDA_ARGS));
        update(c, ki(TOKEN_KIND_STRING), kcv("lambda args rename"));
        update_ri_rd(list, ki(0), c, false, true);
      }
    }
  }

  list = token_filter_negate_numbers(list);

  list = token_filter_group_dfas(list, batch_reject_num_var);

  list = token_filter_group_dfas(list, batch_alike);
  list = token_filter_group_dfas(list, batch_assignment);

  list = token_filter_group_dfas(list, batch_bound_round);
  list = token_filter_group_dfas(list, batch_bound_square);


  /// DROP WHITESPACE ////
  list = token_filter_drop_whitespace_comments(list);
  ////////////////////////

  list = token_filter_group_dfas(list, batch_control);
  list = token_filter_group_dfas(list, batch_verbiage);

  ///////////////////////////////////////////////////
  
  return list;
}

#pragma mark - Parser Filters

K token_filter_group_dfas(K input_tokens, I dfas[])
{
  if(!input_tokens) return NULL;

  I current_start = 0;

  K list = new_k(LIST, 0);

  I travelled;

  for(; current_start < COUNT(input_tokens); current_start += travelled)
  {
    travelled = 0;
    I mark = 0;
    I error = 0;
    I drop = 0;
    I bundle = 0;

    mine(input_tokens, dfas, current_start, &travelled, &mark, &error, &drop, &bundle);

    travelled = MAX(1, travelled); //at least 1

    if(error)
    {
      //this madness has some useful info in it
      //extract before deleting
      //?????????????????????????????????????????????????
      //     instead of erroring deep in here, we're going to handle it later on
      //      higher up, via rejecting unexpected tokens
      //     actually, wait, we can't do that right? if we want to handle
      //     incomplete "if" statements and such by waiting on the console
      //     how do we deal with that?
      // well actually we can deal with that higher up. if you see the token "if" etc
      // instead of the group
      //we can also add E an error state to RIA for RIAE, E will give us early
      //     warning about broken if clauses and such
      //?????????????????????????????????????????????????

      goto fail;
    }

    if(drop)
    {
      continue;
    }

    if(bundle)
    {
      //Bundle
      K grabbed = wrangle(input_tokens, current_start, travelled, true);
      K bundle = token_bundle(mark, -1, -1, grabbed);

      list = cow_add(list, bundle);
      rd(bundle);

      continue;
    }

    //POTENTIAL_OPTIMIZATION_POINT///////////////////////////
    //uh, this may be unnecessary
    //without it you can skip ahead in bigger increments in parsing
    //i didn't prove that I needed it or didn't need it
    //but you need it if there are any parse patterns who
    //are right substrings of themselves but don't accept the first? maybe.
    //getting rid of it may not speed up any timings at all anyway
    //it won't break anything to have it
    travelled = 1;
    /////////////////////////////////////////////////////////

    //Unbundled: add as is without grouping
    K0 o;
    DO(travelled, list = cow_add(list, AT2(input_tokens, ki(current_start + i),o)))
    continue;
  }

succeed:
  rd(input_tokens);
  return list;
fail:
  rd(input_tokens);
  rd(list);
  return NULL;
}

void mine(K tokens, I dfas[], I start, I *travelled, I *mark, I *error, I *drop, I *bundle)
{
  //This is copied from/modeled on LEXing's toke()
  //Factoring them would be possible but counterproductive

  I dfa_count = 0;

  I *p = dfas;
  while(*p++) dfa_count++;

  I position = start;
  *travelled = 0;
  *mark = TOKENS_NONE;
  *bundle = false;

  I textual_location = SENTINEL;

  C prev_states[TOKEN_GROUP_SIZE] = {0};
  C      states[TOKEN_GROUP_SIZE] = {0};
  I run_lengths[TOKEN_GROUP_SIZE] = {0}; 

  I max_run = 0;
  I max_run_index = 0;
  I max_run_state = 0;

  while(position < COUNT(tokens))
  {
    K0 o1,o2,o3;
    K peek = AT2(tokens, ki(position),o1);
    I kind = AT2(peek, ki(TOKEN_KIND),o2)->i;

    I start = AT2(peek, ki(TOKEN_SNIPPET_START),o3)->i;
    textual_location = start;

    I current_max = 0;
    I current_max_index = 0;
    I current_max_state = 0;

    //step
    DO(dfa_count, DFA dfa = PARSE_DFAS[dfas[i]]; 
                    C *grid = dfa.grid;
                    C *state_kinds = dfa.state_kinds;
                    if(!state_kinds) continue;
                    I before = states[i];
                    prev_states[i] = before;

                    states[i] = grid[256*before + kind];
                    run_lengths[i]++;
                    C kind = state_kinds[states[i]];
                    if(0==states[i]) run_lengths[i] = 0;

                    //NB: If this causes problems, split into
                    //current_max for INCOMPLETE and ACCEPT states
                    //Then you can always fall back to old accept if exists
                    if(current_max < run_lengths[i]) 
                    {
                      // "<" ensures we take the first token kind in the event of a tie
                      current_max = run_lengths[i];
                      current_max_index = i;
                      current_max_state = states[i];
                    }
    )

    if(current_max <= max_run)
    {
      break;
    }

    //keep stepping
    max_run = current_max;
    max_run_index = current_max_index;
    max_run_state = current_max_state;

    position++;
  }

  *travelled = max_run;

  if(max_run <= 0)
  {
    return;
  }

  I dfa_index = dfas[max_run_index];
  I state = max_run_state;
  C kind = PARSE_DFAS[dfa_index].state_kinds[state];

  *bundle = false;//early returns -> no bundling

  SW(kind) 
  {
   CSF('R',)
    CS('I', return) //no error
    CS('E', *error=ERROR_PARSE_INCOMPLETE; 

            SW(dfas[0]) //slightly hacky
            {
              CS(TOKEN_GROUP_REJECT_NUM_VAR, *error = ERROR_PARSE_NUM_VAR)
            }

            parse_errno = *error;
            parse_errno_location = textual_location;
            return)
  }


  *mark = PARSE_DFAS[dfa_index].bundle_mark;
  *drop = PARSE_DFAS[dfa_index].bundle_drop;
  *bundle = true;

  return;
}

K wrangle(K parent, I start, I length, I drop_inert)
{
  K list = new_k(LIST, 0);

  I i = 0;

  for(i = start; i < start + length && i < COUNT(parent); i++)
  {
    K0 o1,o2;
    K peek = AT2(parent, ki(i),o1);
    I kind = AT2(peek, ki(TOKEN_KIND),o2)->i;
    bool is_inert = (kind == TOKENS_WHITESPACE || kind == TOKENS_COMMENT); 

    if(drop_inert && is_inert) continue;

    list = cow_add(list, peek);
  }

  return list;
}

K token_filter_negate_numbers(K tokens)
{
  if(!tokens) return NULL;
  //probably could refactor this as a modified parse DFA
  //but it's already written

  K list = new_k(LIST, 0);

  K dash = kcv("-");

  K0 o1,o2,o3,o4,o5;
  ENUM(tokens,  if(matchC(dash, AT2(v, ki(TOKEN_PAYLOAD),o1), 0))
                {
                  if(i < _i - 1)
                  {
                    K peek = AT2(tokens, ki(i+1),o2);
                    if(matchC(ki(TOKENS_NUMBER), AT2(peek, ki(TOKEN_KIND),o3), 0))
                    {
                      bool grab = false;

                      if(i==0)
                      {
                        grab = true;
                      }
                      else
                      {
                        K before = AT2(tokens, ki(i-1),o4);
                        I kind = AT2(before, ki(TOKEN_KIND),o5)->i;

                        SW(kind)
                        {
                         CSF(TOKEN_GROUP_CURLY_BRACE,)
                         CSF(TOKEN_GROUP_PLAIN,)
                         CSF(TOKEN_GROUP_SEPARATION,)
                         CSF(TOKENS_VERB_SYM,)
                         CSF(TOKENS_VERB_WORD,)
                         CSF(TOKENS_COLON,)
                         CSF(TOKENS_COMMENT,)//?
                         CSF(TOKENS_LEFT,)
                         CSF(TOKENS_SEPARATOR,)
                          CS(TOKENS_WHITESPACE, grab = true)
                        }

                        if(grab)
                        {
                          assert(kind != TOKEN_GROUP_ROUND_PAREN);    // (2+3)-1 is minus
                          assert(kind != TOKEN_GROUP_SQUARE_BRACKET); // ar[0]-1 is minus
                        }
                      }

                      if(grab)
                      {

                        K0 o1,o2,o3;
                        I start = AT2(v, ki(TOKEN_SNIPPET_START),o1)->i;
                        I end = AT2(peek, ki(TOKEN_SNIPPET_END),o2)->i;
                        K payload = cow_join(dash, AT2(peek,ki(TOKEN_PAYLOAD),o3));

                        K bundle = token_bundle(TOKENS_NUMBER, start, end, payload);
                        list=cow_add(list, bundle);
                        rd(bundle);
                        i+=1;
                        continue;
                      }
                    }
                  }
                }
  
               list=cow_add(list,v);
  )

  rd(tokens);

  return list;
}

K token_filter_drop_whitespace_comments(K tokens)
{
  if(!tokens) return NULL;
  //could refactor this as a modified parse DFA (but why I guess)

  K list = new_k(LIST, 0);

  K0 o;
  ENUM(tokens, I k = AT2(v, ki(TOKEN_KIND),o)->i; if(TOKENS_WHITESPACE==k || TOKENS_COMMENT ==k)continue; list=cow_add(list,v))

  rd(tokens);

  return list;
}

