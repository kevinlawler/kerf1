typedef void* V;
typedef char C;
typedef C* S;
typedef int64_t I;
typedef uint64_t UI;
typedef double F;
typedef unsigned char UC;
typedef struct s_membuf{C m;C a;UC h;C t;}membuf;
typedef struct s_8buf {char buf[ 8];} escapebuf; //"\b" or "\u0000"
typedef struct s_64buf{char buf[64];} stampbuf;  //"2014.07.03T23:25:43.877\0"
typedef struct k0{C m;C a;UC h;C t;int32_t r;union{I i;F f;C c;S s;int32_t i4;int16_t i2;int8_t i1;float f4;struct k0*k;struct{union{int32_t w;struct{C ma,mn,mk,mf;};};C na,nn,nk,nf;};C buf[sizeof(I)];struct{int64_t n;C g[];};};}*K,K0;
typedef struct k1{K u; K0 v;}K1;
typedef struct k2{K0 u; K0 v;}K2;
typedef struct k4{K0 k0s[4];}K4;
typedef struct i2{I x; I y;}I2;

//this is for lz4 and we should get rid of it...
  typedef  uint8_t BYTE;
  typedef uint16_t U16;
  typedef uint32_t U32;
  typedef  int32_t S32;
  typedef uint64_t U64;
///////////////

//Python for instance has its own constructs for JSON NaN Infinity -Infinity 
//JSON execution is 4==execution_type, so we can either
//A. Make a new execution_type [or alter this one] which emits "Inifinity" etc.
//B. We can set this as a "packing_type" or "channel_type"
typedef struct inet_message0{char endianness, 
                                  channel_type,
                                  zip_type,
                                  packing_type,
                                  execution_type,
                                  response_type,
                                  display_type,
                                  logging_type;//if adding more, probably keep this aligned
                                  I nested_k_wire_size;} MESSAGE0; 
//Because we wipe MESSAGE1 with zeroes, items like "authenticated" should be phrased in such a sense that the zero case is safe.
typedef struct inet_message1{MESSAGE0 m0; I read_so_far; K nested_k; C m_saver_memo; C authenticated; C eventing; V eventing_callback; C http_connection;} MESSAGE1;

typedef union landis_node0{struct{I left, right;};I c[2];} LANDIS_NODE;

#define NULL_NODE ((LANDIS_NODE){LANDIS_NULL, LANDIS_NULL})

enum MESSAGE_ZIP_TYPES {MESSAGE_ZIP_NONE=0, MESSAGE_ZIP_COMPRESSED_PAYLOAD=1, MESSAGE_ZIP_PACKETS=2, MESSAGE_ZIP_TWO_PASS=3, MESSAGE_ZIP_INTERLEAVED=4, MESSAGE_ZIP_MAX};
enum MESSAGE_EXECUTION_TYPES {MESSAGE_EXECUTION_NONE=0, MESSAGE_EXECUTION_STRING_EVAL=1, MESSAGE_EXECUTION_STRING_CALL=2, MESSAGE_EXECUTION_APPLY=3, MESSAGE_EXECUTION_JSON=4, MESSAGE_EXECUTION_MAX};
enum MESSAGE_RESPONSE_TYPES  {MESSAGE_RESPONSE_NO_ACK=0, MESSAGE_RESPONSE_FULL=1, MESSAGE_RESPONSE_ABBREVIATED=2, MESSAGE_RESPONSE_TMPFILENAME=3, MESSAGE_RESPONSE_MAX};
enum MESSAGE_DISPLAY_TYPES   {MESSAGE_DISPLAY_NONE=0, MESSAGE_DISPLAY_SHOW_MESSAGE=1, MESSAGE_DISPLAY_SHOW_EX=2, MESSAGE_DISPLAY_SHOW_MSG_EX=3, MESSAGE_DISPLAY_MAX};
enum ALIGNMENT_TYPES {ALIGN_NONE=0, ALIGN_LEFT=1, ALIGN_RIGHT=2, ALIGN_CENTER=3};

enum ZIP_TYPES {ZIP_TYPE_IDENTITY, ZIP_TYPE_WKDM, ZIP_TYPE_GZIP_16K, ZIP_TYPE_LZ4, ZIP_TYPES_MAX};

enum TREE_NODE_TRAVERSAL {LEFT=0, RIGHT=1};
enum SORTING {ASCENDING=-1, EQUAL=0, DESCENDING=1};
enum ARITY {SENTINEL=-1, NILADIC=0, MONADIC=1, DYADIC=2, TRIADIC=3, TETRADIC=4, PENTADIC=5};
enum ERROR_MEMBERS { ERROR_DISK=1, //TODO: fix items in place as errors can be saved/looked at
  ERROR_FILE, ERROR_VMEM, ERROR_DEPTH, ERROR_CTRL_C, ERROR_SIZE, ERROR_SIGN,
  ERROR_LENGTH, ERROR_REFERENCE, ERROR_VARIABLE, ERROR_RANK, ERROR_INDEX, ERROR_ARITY,
  ERROR_VALENCE, ERROR_REPEAT, ERROR_ARGS, ERROR_CONFORMABLE, ERROR_TYPE, ERROR_STRING,
  ERROR_ARRAY, ERROR_MAP, ERROR_TABLE, ERROR_KEYS, ERROR_COLUMN, ERROR_ROW, ERROR_RAGGED,
  ERROR_LEX_UNKNOWN, ERROR_LEX_INCOMPLETE, ERROR_PARSE_UNKNOWN,
  ERROR_PARSE_UNMATCH, ERROR_PARSE_INCOMPLETE, ERROR_PARSE_DEPTH,
  ERROR_PARSE_OVERMATCH, ERROR_PARSE_MISMATCH, ERROR_PARSE_NESTED_SQL,
  ERROR_PARSE_DERIVED, ERROR_PARSE_LAMBDA_ARGS, ERROR_PARSE_SQL_VALUES,
  ERROR_PARSE_NUM_VAR, 
  ERROR_SUBERROR, ERROR_NET, ERROR_HOST, ERROR_DYLIB, ERROR_PARALLEL, 
  ERROR_ATLAS_MAP, ERROR_TIME, ERROR_JSON_KEY, ERROR_RADIX, ERROR_FORMAT_STRING,
  ERROR_ZIP, ERROR_REMOTE_HUP, ERROR_FORKED_VERB, ERROR_MISSING,
};

enum REGISTRY_MEMBERS {REGISTRY_NEXT, REGISTRY_FILE_HANDLE, REGISTRY_FILE_SIZE, REGISTRY_MAP_ORIGIN, REGISTRY_MAP_SIZE, REGISTRY_PRIVATE, REGISTRY_SIZE};
enum TREE_EQUAL_MEMBERS {TREE_EQUAL_TOP, TREE_EQUAL_LEFT, TREE_EQUAL_RIGHT, TREE_EQUAL_INDEX, TREE_EQUAL_SIZE};
enum TYPE_MEMBERS {
                   LINKVEC   =-101,
                   ///////////////
                   STAMPVEC  =  -4,
                   FLOATVEC  =  -3,
                   INTVEC    =  -2,
                   CHARVEC   =  -1,
                   ////////////////
                   FUNC      =   0,
                   CHAR      =   1, 
                   INT       =   2, 
                   FLOAT     =   3,
                   STAMP     =   4, 
                   NIL       =   5, 
                   LIST      =   6, 
                   MAP       =   7, 
                   HASH      =   8, 
                   BTREE     =   9, 
                   TABLE     =  10,
                   ATLAS     =  11,
                   DATABASE  =  12, //probably best saved as a directory of tables (minimize whitespace)
                   ZIP       =  13,
                   INT4      =  14,
                   INT2      =  15,
                   INT1      =  16,
                   FLOAT4    =  17,
                   DEC4INT8  =  18, 
                   STR32     =  19,
                   PARTABLE  =  20,
                   /////////////////
                   PUBLIC_COUNT,
                   /////////////////
                   PRIVATE   = 101,
                   /////////////////
                   LINK      = 101,
                   JUMP      = 102,
                   /////////////////
                   EMU       = 103,
                   DEQUE     = 104,
                   /////////////////
                   TYPE_SIZE
                   };
typedef struct type_s {union{bool t[PUBLIC_COUNT];struct{bool FUNC, CHAR, INT, FLOAT, STAMP, NIL, LIST, MAP, HASH, BTREE, TABLE, ATLAS, DATABASE, ZIP, PARTABLE;};};}*TYPE,TYPE0;//or t[TYPE_SIZE]

C size_of_type_element[TYPE_SIZE], log_size_of_type_element[TYPE_SIZE];

enum MAP_MEMBERS {INDEX, KEYS, VALUES, HASH_SIZE=VALUES, SORT_SIZE=VALUES, DEQUE_SIZE=VALUES, MAP_SIZE, ATLAS_SIZE=MAP_SIZE, TRAITS=MAP_SIZE, TABLE_SIZE, DB_SIZE=TABLE_SIZE};

#define NOT_DERIVED (0)
enum FUNC_MEMBERS {
                   FUNC_VERSION,
                   FUNC_BYTECODE,
                   FUNC_TEXT,
                   FUNC_DEBUG,
                   FUNC_CONSTANTS,
                   FUNC_DIRECTORY,
                   FUNC_GLOBALS,
                   FUNC_ARGLOCALS,
                   FUNC_TOTAL_ARGS,
                   FUNC_UNFILLED_ARGS,
                   FUNC_LOCAL_ASSIGNS,//not truly necessary, see FUNC_ATTR_keyvaluize
                   FUNC_EXPANSION01,  //we could add...e.g. type checking
                   FUNC_EXPANSION02,  //must initialize all these to not have true garbage
                   FUNC_EXPANSION03_LAST_DITCH_MAP,
                   FUNC_SIZE};

enum FUNC_KINDS {FUNC_KIND_NULL=0, FUNC_KIND_LAMBDA, FUNC_KIND_DERIVED_VERB, FUNC_KIND_DYNAMIC_LIB};

enum TABLE_KINDS {TABLE_KIND_NULL=0, TABLE_KIND_REGULAR=1, TABLE_KIND_MAYBE_PARALLEL_DUPES=2, TABLE_KIND_MAYBE_PARALLEL_SPLITS=3};
enum ATLAS_KINDS {ATLAS_KIND_NULL=0, ATLAS_KIND_REGULAR=1, };

enum ADVERB_KINDS {ADVERB_KIND_NULL=0, ADVERB_KIND_FOLD, ADVERB_KIND_UNFOLD, ADVERB_KIND_MAPDOWN, 
                                      ADVERB_KIND_MAPRIGHT, ADVERB_KIND_MAPLEFT, ADVERB_KIND_MAPBACK, ADVERB_KIND_SIZE};
enum ADVERB_MEMBERS {ADVERB_SUPPLIED_ARGS, ADVERB_NOUN_OPERATOR, ADVERB_PRESET_VALENCE, ADVERB_WORKING_VALENCE, ADVERB_KIND, ADVERB_SUBKIND, ADVERB_i, ADVERB_n, ADVERB_FIRST, ADVERB_PREVIOUS, ADVERB_LATEST, ADVERB_ACCUMULATOR, ADVERB_SIZE};
enum ADVERB_SUBKINDS {SUBKIND_REGULAR=0, SUBKIND_DO_N, SUBKIND_WHILE_B};
typedef struct adverb_vars0{ I position[ADVERB_SIZE]; K object[ADVERB_SIZE];} ADVERB_VARS0; 

typedef struct vm0{
                       K0 fake_header;//so we could pass VM as K, to say print()
                       K  KERFDIR;
                       I  local_bottom;//frame bottom
                       I  local_pointer;//frame pointer
                       I  instruction_pointer;
                       I  instruction_pointer_prev;//for unwinding errors
                       I  function_workspace_height;
                       I  function_index;//index in stack of opened function
                       I  function_bottom;//easy offset for function vars
                       K  function;//current executing function
                       C* code;//easier reference inside ->function
                       I  execution_depth;//for depth errors
                       K  sql_peek;
                       K  registers_K[8];
                       K0 registers_K0[8];
                       I  registers_I[8];
                       K  workspace;
                       K  stack;//an allocated object needing reference management
}*VM,VM0;

#define MAX_SIMPLE_VERB_ARGS (4) //changing this will require other changes. example: ply()

typedef struct atomize_s{
                      C HASHES;  //maintains HASH status. roughly HASH x Y -> HASH
                      C BTREES;    //maintains BTREE status. roughly BTREE x Y -> BTREE
                      C VECTOR[MAX_SIMPLE_VERB_ARGS]; //vector atomic
                      C MIXED_ARRAY[MAX_SIMPLE_VERB_ARGS]; //"atomic" mixed array (LIST+HASH+BTREE, no VECTORS) corresponds to stack position
                      C MIXEDxVECTOR[MAX_SIMPLE_VERB_ARGS]; //Left/Right: Mixed arrays X vectors
                      C MAP[MAX_SIMPLE_VERB_ARGS];    //map value atomic
                      C DTBUCKET[MAX_SIMPLE_VERB_ARGS];
                      C TABLE[MAX_SIMPLE_VERB_ARGS];
                      C TABLExMAP[MAX_SIMPLE_VERB_ARGS];
                      C TABLExARRAY[MAX_SIMPLE_VERB_ARGS];
}*ATOMIZE, ATOMIZE0;

typedef struct verb_funcs_s { 
                              K (*func)(); //pointer to function returning K, e.g., K myfunc(K x, K y)
                              K (*aggregate)();

}VERB_FUNCS;

typedef struct verb_s{
                      I id;
                      S name;
                      S sym;
                      VERB_FUNCS funcs;
                      C argc_range[2];
                      ATOMIZE atomize;
                      TYPE types[MAX_SIMPLE_VERB_ARGS];
                      S optimized[ADVERB_KIND_SIZE];
                      S whered;
                      S cow;
                      C write_restricted;
}VERB;

typedef struct adverb_s{
                      S name;
                      S sym;
                      C kind; 
}ADVERB;

typedef struct tm_nanos_s{ 
  I tm_nanos;
  I tm_sec; 
  I tm_min;
  I tm_hour;
  I tm_mday;
  I tm_mon;
  I tm_year;
  I tm_wday;
  I tm_yday;
  I tm_isdst;
}TM_NANOS;

typedef struct SV_DATA0{

  ///////////////////
  //Items core to the CSV (SV) parser
  S source;
  I column; //column in the CSV file (not in the table)
  I row;

  I item_position;
  I item_width;

  I left_trimmed;
  I right_trimmed;

  bool quoted; //end quote present only if right_trimmed > 0

  ///////////////////
  //Items for passing extraneous info

  K table;
  K temp_table;
  I column_count;
  K temp_column_pointers;
  S fields;
  I fields_count;
  I fields_skipped;
  I header_rows;
  C header_titles;
  C strptime_format[256];
  C strptime_format2[256];
  C in_memory;
} SV_DATA;

enum FIELD_KIND {FIELD_KIND_NONE=0,
                 FIELD_KIND_DELIMITED,
                 FIELD_KIND_FIXED,
                 FIELD_KIND_SIZE};



#define MAX_DFA_STATES (13)
#define MAX_DFA_CLASS_SIZE (64)

#define MAX_SV_CLASSES 6
#define MAX_SV_STATES 6
#define MAX_SV_TRANSITIONS (MAX_SV_CLASSES * MAX_SV_STATES)


//Kinds of tokens
enum TOKENS_KINDS {TOKENS_NONE=0,

                   TOKENS_COLON,//COLON needs to precede VERB
                   TOKENS_VERB_SYM,//VERB needs to precede NUMBER b/c of '-', NAME b/c of '.'
                   TOKENS_VERB_WORD,//VERB needs to precede NUMBER b/c of '-', NAME b/c of '.'
                   TOKENS_ADVERB_SYM,
                   TOKENS_ADVERB_WORD,
                   TOKENS_NUMBER,//NUMBER must precede NAME to capture ".1"
                   TOKENS_NAME,
                   TOKENS_BYTES,
                   TOKENS_SEPARATOR,//comma,semicolon,newline in/out of [()]
                   TOKENS_ABS_DATE,
                   TOKENS_ABS_DATE_ALT,
                   TOKENS_ABS_TIME,
                   TOKENS_ABS_DATETIME,
                   TOKENS_ABS_DATETIME_ALT,
                   TOKENS_REL_DATETIME,
                   TOKENS_STRING,
                   TOKENS_SINGLE_QUOTE,
                   TOKENS_WHITESPACE,
                   TOKENS_COMMENT,
                   TOKENS_BACKTICK,
                   TOKENS_BACKSLASH,
                   TOKENS_BIGGER,
                   TOKENS_LEFT,
                   TOKENS_RIGHT,
                   TOKENS_SQL_START,
                   TOKENS_SQL_MIDDLE,
                   TOKENS_RESERVED,
                   TOKENS_SELF,
                   TOKENS_RETURN,
                   TOKENS_DEF,
                   TOKENS_WHILE,
                   TOKENS_DO,
                   TOKENS_FOR,
                   TOKENS_IF,
                   TOKENS_ELSE,

                   TOKENS_SIZE};

enum TOKEN_GROUP_KINDS {TOKEN_GROUP_START=TOKENS_SIZE,

                        //Can do in advance
                        TOKEN_GROUP_ALIKE,//vectors: ints, floats, stamps, ...

                        //Must happen at same time because recursive
                        TOKEN_GROUP_PLAIN,
                        TOKEN_GROUP_SQL,
                        TOKEN_GROUP_CURLY_BRACE,
                        TOKEN_GROUP_SQUARE_BRACKET,
                        TOKEN_GROUP_ROUND_PAREN,
                        TOKEN_GROUP_SEPARATION,
 
                        TOKEN_GROUP_LAMBDA_ARGS,
                        TOKEN_GROUP_ASSIGNMENT,
                        TOKEN_GROUP_BOUND_SQUARE,
                        TOKEN_GROUP_BOUND_ROUND,
                        TOKEN_GROUP_VERBAL_NNA,
                        TOKEN_GROUP_VERBAL_NVA,
                        TOKEN_GROUP_VERBAL_NA,
                        TOKEN_GROUP_VERBAL_VA,
                        TOKEN_GROUP_CONTROL,

                        TOKEN_GROUP_REJECT_NUM_VAR,

                        TOKEN_GROUP_SIZE};

typedef struct DFA_s{
                     char bundle_mark;
                     S ascii_classes;
                     S state_names;
                     S state_kinds;//Accept-continue, accept-Break, Incomplete/In-progress, Ready/Reject
                     S init_string;
                     V grid;
                     char bundle_drop;
}DFA;

//Parts of a token
enum TOKEN_MEMBERS {
                    TOKEN_KIND,
                    TOKEN_KIND_STRING, //optional
                    TOKEN_SNIPPET_START,
                    TOKEN_SNIPPET_END,
                    TOKEN_PAYLOAD,
                    TOKEN_SIZE};


enum OPCODES { //APPEND-ONLY. DO NOT CHANGE/REORDER/INSERT/REMOVE/ETC.
               //// BASIC TOOLS
               NOOP                   =0x00
              ,POP                    =0x01
              ,JMPA                   =0x02 //Jump Absolute
              ,JMPRB                  =0x03 //Jump Relative [Back)  (of half-open JMP instruction: offset arg width varies)
              ,JMPRF                  =0x04 //Jump Relative [Front) (of half-open JMP instruction)
              ,JMPRBIFN               =0x05
              ,JMPRFIFN               =0x06
              ,JMPRBIFNOTNIL          =0x07
              ,JMPRFIFNOTNIL          =0x08
              ,JUMP_RESERVED00        =0x09
              ,JUMP_RESERVED01        =0x0a
              ,JUMP_RESERVED02        =0x0b
              ,JUMP_RESERVED03        =0x0c
              ,JUMP_RESERVED04        =0x0d
              ,JUMP_RESERVED05        =0x0e
              ,JUMP_RESERVED06        =0x0f
              ,JUMP_RESERVED07        =0x10
              //// PUSHES, MISC.
              ,EMPTY_PUSH             =0x12
              ,NIL_PUSH               =0x13
              ,CONSTANT_PUSH          =0x14
              ,SELF_PUSH              =0x15
              ,ROOT_PUSH              =0x16
              ,PUSH_RESERVED00        =0x17
              ,PUSH_RESERVED01        =0x18
              ,PUSH_RESERVED02        =0x19
              ,PUSH_RESERVED03        =0x1a
              //// GLOBALS
              ,GLOBAL_ALTER           =0x1c
              ,GLOBAL_PUSH            =0x1d
              ,GLOBAL_DYLIB_CALL      =0x1e
              //// ARGS+LOCALS
              ,ARGLOCAL_ALTER         =0x1f
              ,ARGLOCAL_PUSH          =0x20
              //// NESTING
              ,ASIDE_OPEN             =0x22
              ,ASIDE_CLOSE            =0x23
              ,ARG_OPEN               =0x24
              ,ARG_CLOSE              =0x25
              ,LIST_OPEN              =0x26
              ,LIST_CLOSE             =0x27
              ,LIST_STORE             =0x28
              //// VERBS
              ,VERB_CALL              =0x2a
              //// ADVERBS
              ,ADVERB_OPEN            =0x2c //preset_arg_valence
              ,ADVERB_CLOSE           =0x2d
              ,ADVERB_STORE           =0x2e
              ,ADVERB_FOLD_WHILE      =0x2f
              ,ADVERB_FOLD_PUSH       =0x30
              ,ADVERB_UNFOLD_WHILE    =0x31
              ,ADVERB_UNFOLD_PUSH     =0x32
              ,ADVERB_MAPDOWN_WHILE   =0x33
              ,ADVERB_MAPDOWN_PUSH    =0x34
              ,ADVERB_MAPRIGHT_WHILE  =0x35
              ,ADVERB_MAPRIGHT_PUSH   =0x36
              ,ADVERB_MAPLEFT_WHILE   =0x37
              ,ADVERB_MAPLEFT_PUSH    =0x38
              ,ADVERB_MAPBACK_WHILE   =0x39
              ,ADVERB_MAPBACK_PUSH    =0x3a
              //// SQL
              ,SQL_INSERT2            =0x3c
              ,SQL_SELECT2            =0x3d
              ,SQL_PEEK_NAME          =0x3e
              ,SQL_TABLE_LOOKUP       =0x3f
              ,SQL_DELETE2            =0x40
              ,SQL_UPDATE2            =0x41
              ,SQL_RESERVED02         =0x42
              ,SQL_RESERVED03         =0x43
              // PENDING 
              ,APPLY                  =0x45 //? projection, f[x]
              ,APPLY_NOPROJECT        =0x46 //? no projection, f x.  could also differentiate based on function-setup or not
              //HELPERS TESTING
              ,ATOM_I_PUSH            =0x48
              ,SHOW                   =0x49
              ,JMPRFIF                =0x4a

              ////////////////////// /*...*/
              /*...*/
              ,ATLAS_RETURN           =0xfa
              ,TABLE_RETURN           =0xfb
              ,MAP_RETURN             =0xfc
              ,RETURN_CODE            =0xfd
              ,STOP                   =0xfe
              ,ESCAPE                 =0xff};


#define __kerfthread //only informational
#define ALWAYS_INLINE __attribute__((always_inline))

//General Attributes [->r reference count gives endianness, so not needed as attr]
//Another interesting attribute might be "marked for death" or "marked for preservation"
#define ATTR_flat            ((C)(1 << 0))
#define ATTR_okremap         ((C)(1 << 1))
#define ATTR_sort_descending ((C)(1 << 2))
#define ATTR_STRIPED         ((C)(1 << 3))
#define ATTR_SORTED          ((C)(1 << 4))
#define ATTR_BYTES           ((C)(1 << 5))//CHAR type display as 0xbytes (?int type display as bool? ?LINK/JUMP as something?)
#define ATTR_FILLED          ((C)(1 << 6))//Type fills entire reserved portion (all ->m despite ->n)
#define ATTR_DISK            ((C)(1 << 7))//Disk-backed storage, mmapped on disk

//Func Bytes: ma mn mk mf - unspecified
//Func Bytes: na nn nk nf - attributes, nest_n, func_kind, arg_floor
//Func Attributes
#define FUNC_ATTR_globalize  ((C)(1 << 0))
#define FUNC_ATTR_keyvaluize ((C)(1 << 1))
#define FUNC_ATTR_2          ((C)(1 << 2))
#define FUNC_ATTR_HAS_STAR   ((C)(1 << 3))
#define FUNC_ATTR_CREFED     ((C)(1 << 4))
#define FUNC_ATTR_PRE_WHERED ((C)(1 << 5))
#define FUNC_ATTR_SQL_WHERE  ((C)(1 << 6))
#define FUNC_ATTR_SQL_CLAUSE ((C)(1 << 7))

//Sort Attributes
#define SORT_ATTR_0          (1 << 0)
#define SORT_ATTR_1          (1 << 1)
#define SORT_ATTR_2          (1 << 2)
#define SORT_ATTR_3          (1 << 3)
#define SORT_ATTR_4          (1 << 4)
#define SORT_ATTR_5          (1 << 5)
#define SORT_ATTR_NONNULL    (1 << 6)
#define SORT_ATTR_UNIQUE     (1 << 7)

//Map Attributes
#define MAP_ATTR_0           (1 << 0)
#define MAP_ATTR_1           (1 << 1)
#define MAP_ATTR_2           (1 << 2)
#define MAP_ATTR_3           (1 << 3)
#define MAP_ATTR_4           (1 << 4)
#define MAP_ATTR_HASHSET     (1 << 5)
#define MAP_ATTR_DTBUCKET    (1 << 6)
#define MAP_ATTR_ERROR       (1 << 7)

//Table Attributes: na nn nk nf - table_attributes, nest_n, table_kind, unspecified
#define TABLE_ATTR_0         (1 << 0)
#define TABLE_ATTR_1         (1 << 1)
#define TABLE_ATTR_2         (1 << 2)
#define TABLE_ATTR_3         (1 << 3)
#define TABLE_ATTR_4         (1 << 4)
#define TABLE_ATTR_5         (1 << 5)
#define TABLE_ATTR_6         (1 << 6)
#define TABLE_ATTR_7         (1 << 7)

//Column Attributes
#define COLUMN_ATTR_KEY      (1 << 0)//'key' does not mean INDEX(/BTREE)
#define COLUMN_ATTR_FKEY     (1 << 1)
#define COLUMN_ATTR_PRIMARY  (1 << 2)
#define COLUMN_ATTR_NONNULL  (1 << 3) 
#define COLUMN_ATTR_AUTOINCR (1 << 4)
#define COLUMN_ATTR_5        (1 << 5)
#define COLUMN_ATTR_6        (1 << 6)
#define COLUMN_ATTR_defaultv (1 << 7)

#define TRAIT_COLUMNS    ("columns")
#define TRAIT_ATTRIBUTES ("attributes")

//Zip Attributes
#define ZIP_ATTR_BYTE_TRANSFORM (1 << 0)
#define ZIP_ATTR_BIT_TRANFORM   (1 << 1)
#define ZIP_ATTR_XOR_TRANSFORM  (1 << 2) //if you try this/add this, do itemwidth xors before BYTE_TRANSFORM, and then de-xors after de-transform
#define ZIP_ATTR_DELTA_DELTA_I  (1 << 3) //timestamps
#define ZIP_ATTR_4              (1 << 4)
#define ZIP_ATTR_5              (1 << 5)
#define ZIP_ATTR_6              (1 << 6)
#define ZIP_ATTR_7              (1 << 7)

//POTENTIAL_OPTIMIZATION_POINT: "assign" attribute a value might skip `if -> SET` statements
#define ATTR_NONE       ((C)0)
#define SET_ATTR(x,y)   (((K)x)->a |= (y))
#define GET_ATTR(x,y)   (!!(((K)x)->a & (y)))
#define OFF_ATTR(x,y)   (((K)x)->a &= (~(y)))

#define SET_ALT_ATTR(x,y) (((K)x)->na |= (y))
#define GET_ALT_ATTR(x,y) (!!(((K)x)->na & (y)))
#define OFF_ALT_ATTR(x,y) (((K)x)->na &= (~(y)))

#define CAN_WRITE(x) (1==(x)->r || IS_DISK(x))
#define TENANT_REF_SIGNAL (-1) //note: must cause CAN_WRITE->false for in-memory ("i don't own my slab")
#define IS_TENANT(x) (TENANT_REF_SIGNAL==(x)->r)
//#define USES_CREF(x)  (IS_NEST(x) && !IS_LIST(x))
#define USES_CREF(x)  (IS_STRICT_MAP(x) || IS_TABLE(x)) //don't need table here unless compiling into it
//#define IS_CREFED(x) (MAP==(x)->t && 0<(x)->w) //compiled-reference
#define IS_CREFED(x) (USES_CREF(x) && 0<(x)->w) //compiled-reference

#define II (INT64_MAX) //I Infinity (Use -II for I Negative Infinity)
#define IN (INT64_MIN) //I Null (one less than -II)
#define FI (1/0.)      //IEEE should work everywhere 
#define FN (0/0.)     
#define SN (0)         //stamp nan...could be maybe changed to 0 or IN or i dunno...

//ERROR can/should SIGINT if it's a mapcores child process, but not if it's a forked server
#define ERROR(x) ({if(TEST_MACROS)er(<-- err loc); if(The_Process_is_Child_Flag && !The_Process_is_Child_Server_Flag) raise(SIGINT); siglongjmp(*soft_jmp_env, x); NULL;})
#define WERROR(x) ({ERROR(x); -1;})

#define RTIME(d,...) {d=clock();{__VA_ARGS__;}d=(clock()-d)/CLOCKS_PER_SEC;} //Note: sleep() does not count against clock()
#define NRTIME(d,...) {I e=NOW_STAMP();{__VA_ARGS__;}d=(NOW_STAMP()-e)/((F)BILLION);}
#define TIME(...) {F _d; RTIME(_d,__VA_ARGS__); fprintf(stderr,"[DEBUG] Elapsed:%.7f\n",_d);}
#define dump(fmt, x) {fprintf(stderr, "[DEBUG] %s:%u: %s=" fmt "\n", __FILE__, __LINE__, #x, x);}
#define dd(x) dump("%lld",((I)(x)))
#define er(...) {fprintf(stderr, "[DEBUG] %s:%u: %s\n",__FILE__, __LINE__, #__VA_ARGS__);}
#define ers(...) {er(__VA_ARGS__); if(__VA_ARGS__) show(__VA_ARGS__); else er(is NULL and would crash)}

#define O printf
#define R return
#define SW switch
#define CD default
#define CS(n,...)   case n:{__VA_ARGS__;break;}
#define CSF(n,...)  case n:{__VA_ARGS__;}
#define CR(n,...)   case n:{R ({__VA_ARGS__;});}
#define DO(n,...) {I i=0,_i=(n);for(;i<_i;++i){__VA_ARGS__;}}
#define DO2(n,...){I j=0,_j=(n);for(;j<_j;++j){__VA_ARGS__;}}
#define DO3(n,...){I k=0,_k=(n);for(;k<_k;++k){__VA_ARGS__;}}
#define ARRAY_LEN(x) (sizeof(x)/sizeof(x[0]))

//POTENTIAL_OPTIMIZATION_POINT
//almost certainly doesn't matter, but Hacker's Delight has
//the bytewise ops for ABS, etc.
#ifndef ABS
#define ABS(x)     ((x) < 0 ? -(x) : (x))
#endif
#ifndef SIGN
#define SIGN(x)    ((x) < 0 ? -(1) : (1))
#endif
#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

#if __has_builtin(__builtin_isnan)
#undef  isnan                        //see: https://github.com/kevinlawler/kerf-source/issues/34
#define isnan(x) __builtin_isnan(x)  //this gives a 4x speedup for eg `sum()` on Linux. 
#endif

#if __has_builtin(__builtin_isinf)
#undef  isinf                        
#define isinf(x) __builtin_isinf(x)  
#endif

#ifndef ntohll
#define ntohll(x) ( ((uint64_t)(ntohl((uint32_t)((x << 32) >> 32) )) << 32) | ntohl(((uint32_t)(x >> 32))) )
#endif
#ifndef htonll
#define htonll(x) ntohll(x)
#endif

#define BILLION (1000000000LL)
#define MILLION (1000000LL)

#define TM_EPOCH ((struct tm){.tm_year =(1970 - 1900), .tm_mon = 0, .tm_mday = 1})
#define TM_NANOS_ZEROES ((TM_NANOS){0})
#define TM_NANOS_NANS   ((TM_NANOS){.tm_nanos=IN, .tm_sec=IN, .tm_min=IN, .tm_hour=IN, \
.tm_mday=IN, .tm_mon=IN, .tm_year=IN, .tm_wday=IN, .tm_yday=IN, .tm_isdst=IN, })

#define LOG2_SIZEOF_K0  (4) //Clang not optimizing log2() of a sizeof()

#define  K00       ((K0){0})

#define  kg(x)     ((x)->g)
#define  kI(x)     ((I*)kg(x))
#define  kF(x)     ((F*)kg(x))
#define  kC(x)     ((C*)kg(x))//Chars/Char-strings (+3/-3) may contain '\0'
#define  kS(x)     ((S*)kg(x))
#define  kP(x)     ((V*)kg(x))
#define  kA(x)     ((LANDIS_NODE*)kg(x))
#define  k0(x)     ((K )kg(x))
#define  k0i(x,y)  (k0(x)+(y))
#define  kL(x,y)   (k0i(x,y)->k)  //map: link, not for inline map items
#define  kEND(x,y) (((V)x)+POW2((x)->y)) 
#define  kENDM(x)  kEND(x,m)
#define  kENDH(x)  kEND(x,h)
#define  kJ(x,y)   ((K)(kENDH(x)+k0i(x,y)->n))//jump
//#define  kN(x,y)   (k0i(x,y)->t==LINK?kL(x,y):k0i(x,y)->t==JUMP?kJ(x,y):k0i(x,y))//nest
#define  kN(x,y)   ({K _v=k0i(x,y);SW(_v->t){CS(LINK,_v=_v->k)CS(JUMP,_v=kENDH(x)+_v->n; SW(_v->t){CS(LINK,_v=_v->k)})};_v;})//nest

#define kt0(_t,...) ((K0){.m=LOG2_SIZEOF_K0,.a=ATTR_SORTED,.t=_t,.r=TENANT_REF_SIGNAL,__VA_ARGS__})
#define kc0(x)      (kt0(CHAR,.c=x))
#define ki0(x)      (kt0(INT,.i=x))
#define kf0(x)      (kt0(FLOAT,.f=x))
#define ks0(x)      (kt0(STAMP,.i=x))
#define kn0         (kt0(NIL,.k=0))
#define kkn0(x)     (kt0(LIST,.n=x))
#define kk0         (kkn0(0))
#define kl0(x)      (kt0(LINK,.k=(V)x))
#define kj0(x)      ({K0 a = kt0(JUMP,.i=x); a.a |= ATTR_DISK; a;})  
#define kb0(x)      ({K0 a = kt0(CHAR,.c=x); a.a |= ATTR_BYTES; a;})

#define kt(_t,...) (&kt0(_t,__VA_ARGS__))
#define kc(x)      (kt(CHAR,.c=x))
#define ki(x)      (kt(INT,.i=x))
#define kf(x)      (kt(FLOAT,.f=x))
#define ks(x)      (kt(STAMP,.i=x))
#define kn         (kt(NIL,.k=0))
#define kk         (kt(LIST,.n=0))//be careful: you don't want this inline as an atom (you probably could, but it's not built that way)
#define kl(x)      (kt(LINK,.k=(V)x))
#define kj(x)      (&kj0(x))
#define kb(x)      (&kb0(x))
#define SHORT_STRLEN 64

#define kcv(x)     ({static C f[sizeof(K0)+1+SHORT_STRLEN]; K k = (K)&f; *k=kt0(-CHAR); k->m = 6; OFF_ATTR(k,ATTR_SORTED); I slx = strlen(x); I top = sizeof(f) - sizeof(K0); if(slx > top){er(kcv string must be small)} k->n=MIN(slx,top); strncpy(k->g,(x),k->n); kC(k)[k->n]='\0'; k;})

#define klist01(x)     ({ K0 k = kkn0(1); (K4){k,kl0(x)};}) 
#define klist02(x,y)   ({ K0 k = kkn0(2); OFF_ATTR(&k,ATTR_SORTED); (K4){k,kl0(x),kl0(y)};}) 
#define klist03(x,y,z) ({ K0 k = kkn0(3); OFF_ATTR(&k,ATTR_SORTED); (K4){k,kl0(x),kl0(y),kl0(z)};})
#define klist1(x,k)      ({k = klist01(x);     K r = (V)&k; r;})
#define klist2(x,y,k)    ({k = klist02(x,y);   K r = (V)&k; r;})
#define klist3(x,y,z,k)  ({k = klist03(x,y,z); K r = (V)&k; r;})

#define kdoublet(k)      ({ k = (K4){kt0(INTVEC,.n=2),0,0,0}; K r = (V)&k; r;})
//#define ktriplet(k)       ({k = (K4){kt0(INTVEC,.n=3),0,0,0}; K r = (V)&k; r;})

#define rm      r.m
#define ra      r.a
#define rh      r.h
#define rt      r.t
#define rr      r.r
#define rn      r.n
#define ri      r.i
#define rf      r.f
#define rc      r.c
#define rs      r.s
#define rk      r.k
#define rw      r.w

#define sm      s.m
#define sa      s.a
#define sh      s.h
#define st      s.t
#define sr      s.r
#define sn      s.n
#define si      s.i
#define sf      s.f
#define sc      s.c
#define ss      s.s
#define sk      s.k
#define sw      s.w

#define um      u->m
#define ua      u->a
#define uh      u->h
#define ut      u->t
#define ur      u->r
#define un      u->n
#define ui      u->i
#define uf      u->f
#define uc      u->c
#define us      u->s
#define uk      u->k
#define ug      u->g
#define uw      u->w
#define uG      ug
#define u0 ((K )uG)
#define uI ((I*)uG)
#define uF ((F*)uG)
#define uC ((C*)uG)
#define uS ((S*)uG)
#define uP ((V*)uG)
#define uA ((LANDIS_NODE*)uG)
#define uIndex  kN(u,INDEX)
#define uKeys   kN(u,KEYS)
#define uValues kN(u,VALUES)
#define uTraits kN(u,TRAITS)

#define vm      v->m
#define va      v->a
#define vh      v->h
#define vt      v->t
#define vr      v->r
#define vn      v->n
#define vi      v->i
#define vf      v->f
#define vc      v->c
#define vs      v->s
#define vk      v->k
#define vg      v->g
#define vw      v->w
#define vG      vg
#define v0 ((K )vG)
#define vI ((I*)vG)
#define vF ((F*)vG)
#define vC ((C*)vG)
#define vS ((S*)vG)
#define vP ((V*)vG)
#define vA ((LANDIS_NODE*)vG)
#define vIndex  kN(v,INDEX)
#define vKeys   kN(v,KEYS)
#define vValues kN(v,VALUES)
#define vTraits kN(v,TRAITS)

#define wm      w->m
#define wa      w->a
#define wh      w->h
#define wt      w->t
#define wr      w->r
#define wn      w->n
#define wi      w->i
#define wf      w->f
#define wc      w->c
#define ws      w->s
#define wk      w->k
#define wg      w->g
#define ww      w->w
#define wG      wg
#define w0 ((K )wG)
#define wI ((I*)wG)
#define wF ((F*)wG)
#define wC ((C*)wG)
#define wS ((S*)wG)
#define wP ((V*)wG)
#define wA ((LANDIS_NODE*)wG)
#define wIndex  kN(w,INDEX)
#define wKeys   kN(w,KEYS)
#define wValues kN(w,VALUES)
#define wTraits kN(w,TRAITS)

#define xm      x->m
#define xa      x->a
#define xh      x->h
#define xt      x->t
#define xr      x->r
#define xn      x->n
#define xi      x->i
#define xf      x->f
#define xc      x->c
#define xs      x->s
#define xk      x->k
#define xg      x->g
#define xw      x->w
#define xG      xg
#define x0 ((K )xG)
#define xI ((I*)xG)
#define xF ((F*)xG)
#define xC ((C*)xG)
#define xS ((S*)xG)
#define xP ((V*)xG)
#define xA ((LANDIS_NODE*)xG)
#define xIndex  kN(x,INDEX)
#define xKeys   kN(x,KEYS)
#define xValues kN(x,VALUES)
#define xTraits kN(x,TRAITS)

#define ym      y->m
#define ya      y->a
#define yh      y->h
#define yt      y->t
#define yr      y->r
#define yn      y->n
#define yi      y->i
#define yf      y->f
#define yc      y->c
#define ys      y->s
#define yk      y->k
#define yg      y->g
#define yw      y->w
#define yG      yg
#define y0 ((K )yG)
#define yI ((I*)yG)
#define yF ((F*)yG)
#define yC ((C*)yG)
#define yS ((S*)yG)
#define yP ((V*)yG)
#define yA ((LANDIS_NODE*)yG)
#define yIndex  kN(y,INDEX)
#define yKeys   kN(y,KEYS)
#define yValues kN(y,VALUES)
#define yTraits kN(y,TRAITS)


#define zm      z->m
#define za      z->a
#define zh      z->h
#define zt      z->t
#define zr      z->r
#define zn      z->n
#define zi      z->i
#define zf      z->f
#define zc      z->c
#define zs      z->s
#define zk      z->k
#define zg      z->g
#define zw      z->w
#define zG      zg
#define z0 ((K )zG)
#define zI ((I*)zG)
#define zF ((F*)zG)
#define zC ((C*)zG)
#define zS ((S*)zG)
#define zP ((V*)zG)
#define zA ((LANDIS_NODE*)zG)
#define zIndex  kN(z,INDEX)
#define zKeys   kN(z,KEYS)
#define zValues kN(z,VALUES)
#define zTraits kN(z,TRAITS)

#define IS_I_NAN(x)    (IN == x) 
#define IS_CHAR(x)     (CHAR==((x)->t))
#define IS_STAMP(x)    (STAMP==((x)->t))
#define IS_LIST(x)     (LIST==((x)->t))
#define IS_NIL(x)      (NIL==((x)->t))
#define IS_MAP(x)      (MAP==((x)->t))
#define IS_HASH(x)     (HASH==((x)->t))
#define IS_BTREE(x)    (BTREE==((x)->t))
#define IS_TABLE(x)    (TABLE==((x)->t))
#define IS_PARTABLE(x) (PARTABLE==((x)->t))
#define IS_ATLAS(x)    (ATLAS==((x)->t))
#define IS_ZIP(x)      (ZIP==((x)->t))
#define IS_LINK(x)     (LINK==((x)->t))
#define IS_JUMP(x)     (JUMP==((x)->t))
#define IS_CHARVEC(x)  (CHARVEC==((x)->t))
#define IS_FUNC(x)     (FUNC==((x)->t))

#define SORTED(x)     GET_ATTR(x,ATTR_SORTED)
#define BYTEY(x)      GET_ATTR(x,ATTR_BYTES)
#define IS_STRING(x)  (IS_CHARVEC(x) && !BYTEY(x))

#define WANTS_LEXICOGRAPHIC(x) (IS_STRING(x))

#define IS_VECTOR(x)       ((x)->t < 0)
#define IS_VLIST(x)        (IS_VECTOR(x) || IS_LIST(x))  
#define IS_TWIN_ARRAY(x)   (IS_HASH(x)   || IS_BTREE(x))
#define IS_ARRAY(x)        (IS_VLIST(x)  || IS_TWIN_ARRAY(x) || IS_ZIP(x))
#define IS_MIXED_ARRAY(x)  (IS_LIST(x)   || IS_TWIN_ARRAY(x) || IS_ZIP(x))
#define IS_MIXED_KEYED(x)  (IS_MAP(x)    || IS_TABLE(x))
#define IS_INDEXER(x)      (IS_MIXED_KEYED(x) || IS_TWIN_ARRAY(x))
#define IS_DTBUCKET(x)     (IS_MAP(x) && GET_ALT_ATTR(x,MAP_ATTR_DTBUCKET)) //need IS_MAP or crashes: raw attr check
#define IS_ERROR(x)        (IS_MAP(x) && GET_ALT_ATTR(x,MAP_ATTR_ERROR))
#define IS_STRICT_MAP(x)   (IS_MAP(x) && !IS_DTBUCKET(x) && !IS_ERROR(x))

#define IS_ATOM(x)    (!IS_ARRAY(x))
#define IS_SATOM(x)   (IS_STRING(x) || IS_ATOM(x))

//has subelements, (==FUNC || >=LIST) && (< PRIVATE || ==ZIP )
#define IS_NEST(x)    ((x)->t == FUNC       ||\
                       (x)->t == LIST       ||\
                       (x)->t == MAP        ||\
                       (x)->t == HASH       ||\
                       (x)->t == BTREE       ||\
                       (x)->t == TABLE      ||\
                       (x)->t == ATLAS      ||\
                       (x)->t == DATABASE   ||\
                       (x)->t == ZIP        ||\
                       (x)->t == PARTABLE ||\
                       0)

#define IS_EMPTY_ATOM(x) (0==(x)->t && 0==(x)->nn && 0==(x)->nk)
#define IS_EMPTY_CHARVEC(x) (-CHAR==(x)->t && 0==COUNT(x))

//Varieties of Count. Probably COUNT should be replaced with ACOUNT (array count?)
#define _COUNT(x)    (IS_ZIP(x)?lenI(x):IS_ATOM(x)?1:IS_FILLED(x)?max_possible_count_K(x):((x)->n))
//#define COUNT(x)     (IS_TWIN_ARRAY(x)?_COUNT(kN(x,KEYS)):_COUNT(x))
#define COUNT(x)     (IS_ATLAS(x)||IS_TWIN_ARRAY(x)?lenI(x):_COUNT(x))
#define ECOUNT(x)    (IS_INDEXER(x)||IS_TWIN_ARRAY(x)?lenI(kN(x,KEYS)):_COUNT(x))

#define IS_STRIPED(x)       GET_ATTR((x),ATTR_STRIPED)
#define IS_DISK(x)          GET_ATTR((x),ATTR_DISK)
#define IS_FILLED(x)        GET_ATTR((x),ATTR_FILLED)

#define au IS_ATOM(u)
#define av IS_ATOM(v)
#define aw IS_ATOM(w)
#define ax IS_ATOM(x)
#define ay IS_ATOM(y)
#define az IS_ATOM(z)

#define cu COUNT(u)
#define cv COUNT(v)
#define cw COUNT(w)
#define cx COUNT(x)
#define cy COUNT(y) 
#define cz COUNT(z) 

#define aut abs(ut)
#define avt abs(vt)
#define awt abs(wt)
#define axt abs(xt)
#define ayt abs(yt)
#define azt abs(zt)

//POTENTIAL_OPTIMIZATION_POINT: put ranges here
#define VECTOR_ATOM(t) (CHAR==(t) || INT==(t) || FLOAT==(t) || STAMP==(t) || INT4==(t) || INT2==(t) || INT1==(t) || FLOAT4==(t)|| DEC4INT8==(t) || STR32==(t) || LINK==(t) ) //vectorizable atom type
#define VECTOR_TYPE(t) ((t)<0) //vector type

#define PAYLOAD_START(x) (IS_ATOM(x)?&((x)->i):kI(x)) //IS_ATOM(x)? x-atom-integer-address : x-vector-integer-payload-address
#define CONFORM_TRY(x, y)  ({ (1==cy||cx==cy)?cx:1==cx?cy:WERROR(ERROR_LENGTH);})

#pragma mark - Stack Execution Methods

//#define peekn(m,n)    LOOK((m)->stack, (m)->local_pointer-((n)+1))
#define peeko(m,n,o) LOOK_((m)->stack, (m)->local_pointer-((n)+1), o)
#define scrub(m) nestset_ri_rd(m->stack, m->local_pointer - 1, kn, false, false)

//Macro version: 50% faster
#define _NILAD_EX(always_heap_alloc, _func)       ({K0 o; frame_open(KVM); local_ex(KVM, _func); K k = peeko(KVM,0,o); scrub(KVM); frame_close(KVM); if(always_heap_alloc){K result = strong(k); rd(k); k = result; }  k;})
#define _MONAD_EX(always_heap_alloc, _func, arg0) ({K0 o; frame_open(KVM); push_ri(KVM, arg0, true); local_ex(KVM, _func); K k = peeko(KVM,0,o); scrub(KVM); frame_close(KVM);  if(always_heap_alloc){K result = strong(k); rd(k); k = result; }k;})
#define _DYAD_EX(always_heap_alloc,  _func, arg0, arg1) ({K0 o; frame_open(KVM); push_ri(KVM, arg1, true); push_ri(KVM, arg0, true); local_ex(KVM, _func); K k = peeko(KVM,0,o); scrub(KVM); frame_close(KVM); if(always_heap_alloc){K result = strong(k); rd(k); k = result; } k;})

#define NILAD_EX(_func) _NILAD_EX(true, _func)
#define MONAD_EX(_func, arg0) _MONAD_EX(true, _func, arg0)
#define DYAD_EX(_func, arg0, arg1) _DYAD_EX(true, _func, arg0, arg1)

#define _AT(x,y,creates_maps,throws_errors, _s) ({K _v=&_s; _s = kat(x,y,creates_maps,throws_errors); SW((_s).t){CS(LINK, _v=(_s).k)} _v;})
#define AT2(x,y,o) _AT(x,y,0,0,o)

#define ex(...) ({_ex(0, __VA_ARGS__, (K)SENTINEL);})

#define SWAP(x,y)  ({V _swap_temp = x; x = y; y = _swap_temp;})//you can get a hygienic macro with __COUNTER__
#define SWAPI(x,y) ({I _swap_temp = x; x = y; y = _swap_temp;})//you can get a hygienic macro with __COUNTER__

#pragma mark -

//this is necessary or linux crashes. _GNU_SOURCE also works but is a stronger change
char *strptime(const char*, const char *, struct tm*);

#pragma mark - 

static bool falsy(K x)
{
  SW(xt)
  {
    //we can make this whatever. not a big deal
    CS(FLOAT, if(0.0==xf)return true;)
    CS(INT,   if(  0==xi)return true;)
   CSF((C)NULL, if(!IS_EMPTY_ATOM(x))break;)//necessary?
    CS(NIL,   return !TRUTHTABLE_VALUE_FOR_NIL;)
  }
  
  return false;
}

static bool truthy(K x)
{
  return !falsy(x);
}

static inline bool isnull(K x)
{
  SW(xt)
  {
    CS( CHAR,  if(0==xc)     R true)
    CS( INT,   if(IN==xi)    R true)
    CS( FLOAT, if(isnan(xf)) R true)
    CS( STAMP, if(0==xi)     R true)
    CS( NIL,                 R true)
  }

  return false;
}

//We don't actually have to fool with this since the stack will handle it for us
//static inline bool accepts_valence(K x, I argc)
//{
//  SW(xt)
//  {
//    CS(FUNC, 
//        I valence_max = valence(x);
//        SW(x->nk)
//        {
//          CS(FUNC_KIND_DERIVED_VERB, return ( ))
//          CD: return argc == valence_max;
//        }
//    )
//  }
//
//  return (argc == 0 || argc == 1);//acceptable valence for nouns
//}


static inline ALWAYS_INLINE K0 klook(K z, I i);
static inline ALWAYS_INLINE K0 klook_intern(K z, I i) 
{
  I j = kI(zKeys)[i];
  return klook(kN(zIndex,KEYS),j);
}

extern C *compression_decompressed_chunk_by_index(K x,I p,bool skip_cache);
extern K cow_expand(K x,I m);
extern K nestneu(K x, I i, K y);
static inline ALWAYS_INLINE K0 klook_zip(K z, I i) 
{
  //POTENTIAL_OPTIMIZATION_POINT
  //1. mlock the page piece for klook when mapped on disk (this should go with an madvise tool maybe)
  //2. you may need to update ENUM and so on to have an extra k4 in case we ever nest these
  //   (and still perhaps this klook should be a gnu ({}) macro so we can have auto space for it?)



  C t = ABS(z->nk); //atom type the zip is storing

  C log_2_chunk_size = z->mk;
  I chunk_size = POW2(log_2_chunk_size);

  I width = size_of_type_element[t];
  I element_position = i * width;
  I chunk_index = element_position >> log_2_chunk_size;

  I position_mod_chunk = element_position - (chunk_index * chunk_size);  
  
  C *chunk = compression_decompressed_chunk_by_index(z, chunk_index, false);

  K0 k = kt0(t,.k=0);

  K x = NULL;

  SW(t)
  {
    CS(STR32, //too big to fit in K0
              I n = 0;
              x = zValues;
              assert(xm>=6);//this will hold for now. when it doesn't, use below. you can also play with ATTR_FILLED but be careful it prints weird
              //if(xm<6)
              //{
              //  nestneu(z,VALUES,x=cow_expand(x,6));//copy_overrides of ZIP 
              //}
              while(n<width && chunk[position_mod_chunk + n]) n++;//if you zero the payload and use a strictly larger zValues you can strlen instead, shrug
              xt = CHARVEC;
              xn = n;
              //Don't [re-]set xr=TENANT_REF_SIGNAL as this isn't always true
              memcpy(xC, chunk + position_mod_chunk, n);
    )
    CD: memcpy(&(k.k), chunk + position_mod_chunk, width);
        break;
  }

  //this atom conversion method probably generalizes to a function
  I j;
  F f;
  SW(t)
  {
    CS(INT4,     j=k.i4; k.i=j; k.t=INT)
    CS(INT2,     j=k.i2; k.i=j; k.t=INT)
    CS(INT1,     j=k.i1; k.i=j; k.t=INT)
    CS(FLOAT4,   f=k.f4; k.f=f; k.t=FLOAT)
    CS(DEC4INT8, f=k.i/(F)10000.0; k.f=f; k.t=FLOAT)
    CS(STR32,    k.t=LINK; k.k=x)
  }

  return k;
}

//If this function is not inline loops are 6x slower
static inline ALWAYS_INLINE K0 klook(K z, I i) 
{
  K0 r;
  SW(zt)
  {
   CSF(-STAMP,)
   CSF(-FLOAT,)
    CS(-INT,  r=kt0(-zt,.i=zI[i]))
    CS(-CHAR, r=kt0(-zt,.c=zC[i]))
    CS(-LINK, r=kt0(-zt,.k=zP[i]))\

   CSF( CHAR,)
   CSF( INT,)
   CSF( FLOAT,)
   CSF( STAMP,)
    CS( NIL, r=kt0(zt,.n=zn)) 

    CS( LIST, r = k0(z)[i]; SW(rt){CS((C)NULL,r=kn0)CS(JUMP,r=kl0(kENDH(z)+rn);SW(rk->t){CS(LINK,r=*rk)})})
    CS( BTREE, return klook(zKeys,i))
   CSF( TABLE,)
    CS( MAP,  return klook(zValues,i))
    CS( HASH, return klook_intern(z,i))
    CS( ZIP,  return klook_zip(z,i)) 

   CSF( FUNC, )
   CSF( ATLAS,)
   CSF( PARTABLE,)
   CSF( DATABASE,)
    CD: r=kl0(z);
  }

  return r;
} 
//#define LOOKUP(_map,_key) ({I _p = lookupI(_map, _key); volatile K _k = IS_HASH_NULL(_p)?NULL:LOOK(kN(_map,VALUES),_p); _k;})
#define LOOKUP_(_map,_key, _o) ({I _p = lookupI(_map, _key); K _k = IS_HASH_NULL(_p)?NULL:LOOK_(kN(_map,VALUES),_p, _o); _k;})

#define LOOKAB(c,d,x,i,f) c = f(x,i); SW((c).t){CS(LINK, d=(c).k)}
//#define LOOKF(x,i,f) ({K0 _s; K _v=&_s; LOOKAB(_s,_v,x,i,f) _v;})
//#define LOOKF(x,i,f) ({K _v=alloca(16);LOOKAB(*_v,_v,x,i,f) _v;})
//#define LOOK(x,i) LOOKF(x,i,klook)
#define LOOKG(x,i,f,_s) ({K _v=&_s; LOOKAB(_s,_v,x,i,f) _v;})
#define LOOK_(x,i,_s) LOOKG(x,i,klook,_s)

//#define denest_other(x) ({K1 k = denest_K1_from_start_and_key(The_Kerf_Tree, x, true, true); if(k.u)LOOK(k.u,k.v.i)?NULL;})
#define denest_create(x,y,o,k) ({k = denest_K1_from_start_and_key(The_Kerf_Tree,x,y, true); if(!k.u){er(null parent in denest); ERROR(ERROR_REFERENCE);} LOOK_(k.u,k.v.i, o);})
#define denest_of(x,o,k)      denest_create(x,false,o,k)

//for some reason factoring the switches as macros makes it slower
//using the real _a->t and not a stored value lets us mutate list (amend)
#define REFK2(_a, _b, _i2, _j2, ...)\
{\
  K0 _s, _t;\
  SW(_a->t)\
  {\
    CSF(-STAMP,)\
    CSF(-FLOAT,)\
     CS(-INT,  r.i = kI(_a)[(_i2)])\
     CS(-CHAR, r.c = kC(_a)[(_i2)])\
     CS(-LINK, r.k = kP(_a)[(_i2)])\
     CS( LIST, r = k0(_a)[(_i2)];SW(r.t){CS((C)NULL,r=kn0)CS(LINK,u=r.k)CS(JUMP,u=kENDH(_a)+rn; SW(ut){CS(LINK,u=uk)})CD:u=_AR;})\
     CS( HASH, u = LOOKG(_a,_i2,klook_intern,_s)) /*makes the code way bigger & not needed everywhere */\
     CS(  ZIP, u = LOOKG(_a,_i2,klook_zip   ,_s)) /*makes the code way bigger & not needed everywhere */\
  }\
  SW(_b->t)\
  {\
    CSF(-STAMP,)\
    CSF(-FLOAT,)\
     CS(-INT,  s.i = kI(_b)[(_j2)])\
     CS(-CHAR, s.c = kC(_b)[(_j2)])\
     CS(-LINK, s.k = kP(_b)[(_j2)])\
     CS( LIST, s = k0(_b)[(_j2)];SW(s.t){CS((C)NULL,r=kn0)CS(LINK,v=s.k)CS(JUMP,v=kENDH(_b)+sn;SW(vt){CS(LINK,v=vk)})CD:v=_AS;})\
     CS( HASH, v = LOOKG(_b,_j2,klook_intern,_t)) /*makes the code way bigger & not needed everywhere */\
     CS(  ZIP, v = LOOKG(_b,_j2,klook_zip   ,_t)) /*makes the code way bigger & not needed everywhere */\
  }\
  {__VA_ARGS__;}\
}

#define LIST2(_x2, _y2, ...)\
{\
  K _tmp0 = (_x2);\
  K _tmp1 = (_y2);\
  K _a = _tmp0;\
  K _b = _tmp1;\
  if(IS_BTREE(_a))_a = kN(_a,KEYS);\
  if(IS_BTREE(_b))_b = kN(_b,KEYS);\
  K0 r = kt0(-_a->t,.n=0);\
  r.a = _a->a;\
  K0 s = kt0(-_b->t,.n=0);\
  s.a = _b->a;\
  K u;\
  K v;\
  K _AR = &r;\
  K _AS = &s;\
  if(IS_ATOM(_a)){r= *_a; u = _a;}else u = _AR;\
  if(IS_ATOM(_b)){s= *_b; v = _b;}else v = _AS;\
  /* increment method same speed as ANDing method same speed as "flag?0:i" method */\
  I _CA = ECOUNT(_a);\
  I _CB = ECOUNT(_b);\
  I _n = _CA;\
  I _flagA = (1==_CA?1:0);\
  I _flagB = (1==_CB?1:0);\
  /* Length_error if both lists and n!=1 and not n=n */\
  if(_flagB) _n = _CA;\
  else if (_flagA) _n = _CB;\
  else if (_CA!=_CB)ERROR(ERROR_LENGTH);\
  /* these are in progress & we may get rid of them */\
  K4 astr32k4 = {kt0(CHARVEC,),0,0,0};\
  K4 bstr32k4 = {kt0(CHARVEC,),0,0,0};\
  K _astr32 = (K)&astr32k4;\
  K _bstr32 = (K)&bstr32k4;\
  if(MACRO_FASTER_BUT_BIGGER_BINARY)\
  {\
    SW(_flagA){ CS(0,SW(_flagB){CS(0, DO(_n, REFK2((_a), (_b), i, i, __VA_ARGS__)))\
                                CS(1, DO(_n, REFK2((_a), (_b), i, 0, __VA_ARGS__)))})\
                CS(1,                 DO(_n, REFK2((_a), (_b), 0, i, __VA_ARGS__)))\
    }\
  }\
  else\
  {\
    I _iA = 0;\
    I _iB = 0;\
    I _incA = _flagA?0:1;\
    I _incB = _flagB?0:1;\
    DO(_n, REFK2((_a), (_b), _iA, _iB, __VA_ARGS__);  _iA+=_incA; _iB+=_incB)\
  }\
} 

#define ENUM(_k, ...)\
{\
  K _x=_k;\
  K _y=_k;\
  SW((_k)->t)\
  {\
   CSF(PARTABLE,)\
   CSF(TABLE,)\
    CS(MAP,  _x=kN(_k,KEYS);  _y=kN(_k,VALUES))\
    CS(BTREE, _x=kN(_k,KEYS);  _y=kN(_k,KEYS))\
    /*BTREE: sic, No sensible way to use INDEX,VALUES */\
  }\
  LIST2(_x,_y,__VA_ARGS__);\
}

#define SATOM(_k, ...)\
{\
  K _z = _k;\
  K4 _o;\
  if(-CHAR==(_z)->t) _z = klist1(_z,_o);\
  ENUM(_z, __VA_ARGS__);\
}

#define SATOM2(_j, _k,  ...)\
{\
  K _y = _j;\
  K4 _o1,_o2;\
  if(-CHAR==(_y)->t) _y = klist1(_y,_o1);\
  K _z = _k;\
  if(-CHAR==(_z)->t) _z = klist1(_z,_o2);\
  LIST2(_y, _z, __VA_ARGS__);\
}

#define NEST(_x4, ...)\
{\
  K _tmp0 = (_x4);\
  K _a = _tmp0;\
  DO(box_count_K(_a), K v = kN(_a,i); __VA_ARGS__ )\
}

//#define ROWENUM(_x5,_x6, ...)\
//{\
//  K _table = _x5;\
//  K *_row = _x6;\
//  I _rows_count = lenI(_table);\
//  if(!*_row) *_row=sample_map_from_table(_table);\
//  work_push(*_row);\
//  K _table_values = kN(_table,VALUES);\
//  K _map_values = kN(*_row,VALUES);\
//  I _key_count = COUNT(kN(*_row,KEYS));\
//  DO(_rows_count,\
//    DO2(_key_count, K col = kN(_table_values,j); nestset_ri_rd(_map_values,j,LOOK_(col,i),false,false))\
//    K v = *_row;\
//    __VA_ARGS__)\
//  work_pop();\
//}
