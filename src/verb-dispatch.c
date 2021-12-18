#include "kerf.h"
//Verb calls increase by +1 the overall number of references,
//that is, they generally allocate new objects.
//Should the need arise, it's possible to do something like 
//say, have a verb return a pointer to a K0 register on the VM.
//In that case the pointer should gracefully handle rd().

#pragma mark - Verb Dispatch Table
ATOMIZE0 NONE_D   = {.HASHES=0, .BTREES=0, .VECTOR={0,0}, .MIXED_ARRAY={0,0}, .MIXEDxVECTOR={0,0}, .MAP={0,0}, .DTBUCKET={0,0}, .TABLE={0,0},};
ATOMIZE0 ATOMICA  = {.HASHES=1, .BTREES=1, .VECTOR={0,0}, .MIXED_ARRAY={1,1}, .MIXEDxVECTOR={1,1}, .MAP={1,1}, .DTBUCKET={1,1}, .TABLE={1,1},};
ATOMIZE0 ATOMICB  = {.HASHES=0, .BTREES=0, .VECTOR={0,0}, .MIXED_ARRAY={1,1}, .MIXEDxVECTOR={1,1}, .MAP={1,1}, .DTBUCKET={0,0}, .TABLE={1,1},};
ATOMIZE0 AGGR_A   = {.HASHES=0, .BTREES=0, .VECTOR={0,0}, .MIXED_ARRAY={0,0}, .MIXEDxVECTOR={0,0}, .MAP={1,1}, .DTBUCKET={0,0}, .TABLE={1,1},};
ATOMIZE0 LAZYDEV  = {.HASHES=1, .BTREES=1, .VECTOR={1,1}, .MIXED_ARRAY={1,1}, .MIXEDxVECTOR={1,1}, .MAP={1,1}, .DTBUCKET={1,1}, .TABLE={1,1},};

TYPE0 NO_T     = {0};
TYPE0 ALL_T    = {0};//initialized to all-ones later
TYPE0 CHAR_T   = {.CHAR=1, };
TYPE0 INT_T    = {         .INT=1, };
TYPE0 NUMERIC  = {         .INT=1, .FLOAT=1};
TYPE0 NUMERIC2 = {.CHAR=1, .INT=1, .FLOAT=1};
TYPE0 NUM_TIME = {         .INT=1, .FLOAT=1, .STAMP=1, .MAP=1};
TYPE0 NUMTIME2 = {.CHAR=1, .INT=1, .FLOAT=1, .STAMP=1, .MAP=1};
TYPE0 MAP_T    = {.MAP=1};
TYPE0 TABLE_T  = {.TABLE=1};

//TODO: fix items in place with [0]=... [1]=....
//TODO: when we allow shadowing / context overwriting of built-ins, create a toggle to block shadowing in the "global" context (usu. bad)
VERB VERB_DISPATCH[] = { //APPEND-ONLY. DO NOT REORDER/INSERT/REMOVE/ETC. 
 {.name="not",       .sym="!",  .funcs={xnot},          .argc_range={1,1}, .atomize=&ATOMICB, .types={&ALL_T,    &ALL_T   }}
,{.name="hash",      .sym="#",  .funcs={hashed},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="000NYI",    .sym="$",  .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}//saved for sql api, "$1, $2"
,{.name="distinct",  .sym="%",  .funcs={distinct},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="part",      .sym="&",  .funcs={part},          .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}//must not be "group" (SQL)
,{.name="car",       .sym="*",  .funcs={xcar},          .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="transpose", .sym="+",  .funcs={transpose},     .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="negate",    .sym="-",  .funcs={negate},        .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &NUMERIC }}
,{.name="eval",      .sym=".",  .funcs={monad_eval},    .argc_range={1,1}, .atomize=&ATOMICB, .types={&ALL_T,    &ALL_T   }}
,{.name="reverse",   .sym="/",  .funcs={reverse},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="ident",     .sym=":",  .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="ascend",    .sym="<",  .funcs={grade_up},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="index",     .sym="=",  .funcs={indexed},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="descend",   .sym=">",  .funcs={grade_down},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="which",     .sym="?",  .funcs={which},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&NUMERIC2, &ALL_T   }}
,{.name="001NYI",    .sym="@",  .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="002NYI",    .sym="\\", .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="enumerate", .sym="^",  .funcs={enumer},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="floor",     .sym="_",  .funcs={xfloor},        .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC2, &ALL_T   }}
,{.name="len",       .sym="|",  .funcs={len},           .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="atom",      .sym="~",  .funcs={atom},          .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="map",       .sym="!",  .funcs={xmap},          .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="join",      .sym="#",  .funcs={join},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .cow=0, .optimized={[ADVERB_KIND_FOLD]="flatten"}}
,{.name="004NYI",    .sym="$",  .funcs={ident},         .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}//saved for sql api, "$1, $2"
,{.name="mod",       .sym="%",  .funcs={xmod},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}//ex handles atomize
,{.name="mins",      .sym="&",  .funcs={min_and},       .argc_range={2,2}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T   }, .optimized={[ADVERB_KIND_FOLD]="min", [ADVERB_KIND_MAPBACK]="0minback"}}
,{.name="times",     .sym="*",  .funcs={xtimes},        .argc_range={2,2}, .atomize=&ATOMICA, .types={&NUMERIC,  &NUMERIC }}
,{.name="plus",      .sym="+",  .funcs={plus},          .argc_range={2,2}, .atomize=&ATOMICA, .types={&NUM_TIME, &NUM_TIME}, .optimized={[ADVERB_KIND_FOLD]="sum", [ADVERB_KIND_UNFOLD]="rsum"}}
,{.name="minus",     .sym="-",  .funcs={minus},         .argc_range={2,2}, .atomize=&ATOMICA, .types={&NUMTIME2, &NUMTIME2}}
,{.name="0alter",    .sym=".",  .funcs={ident},         .argc_range={2,4}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }} //todo: alter({[x,y] x+y}, enlist 1 2) or just 1 2
,{.name="divide",    .sym="/",  .funcs={divide},        .argc_range={2,2}, .atomize=&ATOMICA, .types={&NUMERIC,  &NUMERIC }}
,{.name="000right",  .sym=":",  .funcs={right},         .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="less",      .sym="<",  .funcs={less},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0less_pre"}
,{.name="equals",    .sym="=",  .funcs={equals},        .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0equals_pre"}
,{.name="greater",   .sym=">",  .funcs={greater},       .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0greater_pre"}
,{.name="rand",      .sym="?",  .funcs={verb_rand},     .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="005NYI",    .sym="@",  .funcs={ident},         .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="lsq",       .sym="\\", .funcs={lsq},           .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="take",      .sym="^",  .funcs={take},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&NUMERIC,  &ALL_T   }}
,{.name="drop",      .sym="_",  .funcs={drop},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&NUMERIC,  &ALL_T   }}
,{.name="maxes",     .sym="|",  .funcs={max_or},        .argc_range={2,2}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T   }, .optimized={[ADVERB_KIND_FOLD]="max", [ADVERB_KIND_MAPBACK]="0maxback"}}
,{.name="match",     .sym="~",  .funcs={match},         .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="lesseq",    .sym="<=", .funcs={lesseq},        .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0lesseq_pre"}
,{.name="greatereq", .sym=">=", .funcs={greatereq},     .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0greatereq_pre"}
,{.name="equal",     .sym="==", .funcs={equals},        .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0equals_pre"}
,{.name="noteq",     .sym="!=", .funcs={noteq},         .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0noteq_pre"}
,{.name="noteq",     .sym="<>", .funcs={noteq},         .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0noteq_pre"}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="exp",       .sym="**", .funcs={xexp},          .argc_range={1,2}, .atomize=&ATOMICA, .types={&NUMERIC,  &NUMERIC }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="unique",               .funcs={distinct},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="count",                .funcs={len},           .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="first",                .funcs={xcar},          .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="last",                 .funcs={last},          .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="avg",                  .funcs={xavg},          .argc_range={1,1}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="std",                  .funcs={xstd},          .argc_range={1,1}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="var",                  .funcs={xvar},          .argc_range={1,1}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="min",                  .funcs={xmin},          .argc_range={1,2}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="max",                  .funcs={xmax},          .argc_range={1,2}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="sum",                  .funcs={xsum},          .argc_range={1,2}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="median",               .funcs={verb_median},   .argc_range={1,1}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="rsum",                 .funcs={rsum},          .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="0less_pre",            .funcs={less_pre},      .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="0equals_pre",          .funcs={equals_pre},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="0greater_pre",         .funcs={greater_pre},   .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="0lesseq_pre",          .funcs={lesseq_pre},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="0greatereq_pre",       .funcs={greatereq_pre}, .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="0noteq_pre",           .funcs={noteq_pre},     .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="0maxback",             .funcs={maxback},       .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="0minback",             .funcs={minback},       .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="enlist",               .funcs={enlist},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="or",                   .funcs={max_or},        .argc_range={2,2}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T },   .optimized={[ADVERB_KIND_FOLD]="max", [ADVERB_KIND_MAPBACK]="0maxback"}}
,{.name="and",                  .funcs={min_and},       .argc_range={2,2}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T },   .optimized={[ADVERB_KIND_FOLD]="min", [ADVERB_KIND_MAPBACK]="0minback"}}
,{.name="explode",              .funcs={explode},       .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="implode",              .funcs={implode},       .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="add",                  .funcs={plus},          .argc_range={2,2}, .atomize=&ATOMICA, .types={&NUM_TIME, &NUM_TIME}, .optimized={[ADVERB_KIND_FOLD]="sum", [ADVERB_KIND_UNFOLD]="rsum"}}
,{.name="subtract",             .funcs={minus},         .argc_range={2,2}, .atomize=&ATOMICA, .types={&NUMTIME2, &NUMTIME2}}
,{.name="negative",             .funcs={negate},        .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &NUMERIC }}
,{.name="flatten",              .funcs={flatten},       .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="string",               .funcs={string_cast},   .argc_range={1,1}, .atomize=&ATOMICB, .types={&ALL_T,    &ALL_T   }}
,{.name="int",                  .funcs={int_cast},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="float",                .funcs={float_cast},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="char",                 .funcs={char_cast},     .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="stamp",                .funcs={stamp_cast},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="hashed",               .funcs={hashed},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="enum",                 .funcs={hashed},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="btree",                .funcs={indexed},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="indexed",              .funcs={indexed},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="KEY",                  .funcs={indexed},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="PRIMARY",              .funcs={indexed},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}//to do: PRIMARY should trigger uniq & nonnull flags
,{.name="NONNULL",              .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="UNIQUE",               .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="global",               .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="globals",              .funcs={ident},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="range",                .funcs={py_range},      .argc_range={1,3}, .atomize=&NONE_D,  .types={&NUMERIC2, &NUMERIC2, &NUMERIC2}}
,{.name="repeat",               .funcs={repeat},        .argc_range={2,2}, .atomize=&NONE_D,  .types={&NUMERIC2, &ALL_T   }}
,{.name="tolower",              .funcs={xfloor},        .argc_range={1,1}, .atomize=&ATOMICB, .types={&NUMERIC2, &ALL_T   }}
,{.name="toupper",              .funcs={xceil},         .argc_range={1,1}, .atomize=&ATOMICB, .types={&NUMERIC2, &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="pow",                  .funcs={xexp},          .argc_range={1,2}, .atomize=&ATOMICA, .types={&NUMERIC,  &NUMERIC }}
,{.name="abs",                  .funcs={xabs},          .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="ceil",                 .funcs={xceil},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC2, &ALL_T   }}
,{.name="sqrt",                 .funcs={xsqrt},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC2, &ALL_T   }}
,{.name="ln",                   .funcs={xln},           .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="log",                  .funcs={xlog},          .argc_range={1,2}, .atomize=&ATOMICA, .types={&NUMERIC,  &NUMERIC }}
,{.name="lg",                   .funcs={xlg},           .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="sin",                  .funcs={xsin},          .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="cos",                  .funcs={xcos},          .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="tan",                  .funcs={xtan},          .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="asin",                 .funcs={xasin},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="acos",                 .funcs={xacos},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="atan",                 .funcs={xatan},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="sinh",                 .funcs={xsinh},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="cosh",                 .funcs={xcosh},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="tanh",                 .funcs={xtanh},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="erf",                  .funcs={xerf},          .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
,{.name="erfc",                 .funcs={xerfc},         .argc_range={1,1}, .atomize=&ATOMICA, .types={&NUMERIC,  &ALL_T   }}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
,{.name="timing",               .funcs={timing},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="now",                  .funcs={now},           .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="now_date",             .funcs={now_date},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="now_time",             .funcs={now_time},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="kerf_from_json",       .funcs={new_from_json}, .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="json_from_kerf",       .funcs={new_json_from}, .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="uneval",               .funcs={new_json_from}, .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="reserved",             .funcs={reserved_verb}, .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="sleep",                .funcs={xsleep},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="lines",                .funcs={lines},         .argc_range={1,2}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="trim",                 .funcs={trim},          .argc_range={1,1}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T   }}
,{.name="load",                 .funcs={xload},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="run",                  .funcs={xload},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="exit",                 .funcs={xexit},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="open_socket",          .funcs={open_socket},   .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="close_socket",         .funcs={close_socket},  .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="send_async",           .funcs={send_async},    .argc_range={2,3}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="send_sync",            .funcs={send_sync},     .argc_range={2,3}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="open_table",           .funcs={verb_open_table},.argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,  &ALL_T   }}
,{.name="read_from_path",       .funcs={read_from_path},.argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="write_to_path",        .funcs={write_to_path}, .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }} // not fork restricting this
,{.name="write_text",           .funcs={write_text},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}  // not fork restricting this
,{.name="read_table_from_csv",  .funcs={read_table_from_csv},  .argc_range={3,3}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &INT_T}}
,{.name="read_table_from_tsv",  .funcs={read_table_from_tsv},  .argc_range={3,3}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &INT_T}}
,{.name="read_table_from_delimited_file", .funcs={read_table_from_delimited_file}, .argc_range={4,4}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &CHAR_T, &INT_T}}
 ,{.name="create_table_from_csv", .funcs={create_table_from_csv}, .argc_range={4,4}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &CHAR_T, &INT_T}, .write_restricted=1}
,{.name="create_table_from_tsv", .funcs={create_table_from_tsv}, .argc_range={4,4}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &CHAR_T, &INT_T}, .write_restricted=1}
,{.name="create_table_from_psv", .funcs={create_table_from_psv}, .argc_range={4,4}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &CHAR_T, &INT_T}, .write_restricted=1}
,{.name="append_table_from_csv", .funcs={append_table_from_csv}, .argc_range={4,4}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &CHAR_T, &INT_T}, .write_restricted=1}
,{.name="append_table_from_tsv", .funcs={append_table_from_tsv}, .argc_range={4,4}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &CHAR_T, &INT_T}, .write_restricted=1}
,{.name="append_table_from_psv", .funcs={append_table_from_psv}, .argc_range={4,4}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &CHAR_T, &INT_T}, .write_restricted=1}
,{.name="create_table_from_fixed_file", .funcs={create_table_from_fixed_file},     .argc_range={3,3}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &MAP_T}, .write_restricted=1}
,{.name="append_table_from_fixed_file", .funcs={append_table_from_fixed_file},     .argc_range={3,3}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &MAP_T}, .write_restricted=1}
,{.name="read_table_from_fixed_file",  .funcs={read_table_from_fixed_file},     .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T, &MAP_T}}
,{.name="write_csv_from_table", .funcs={write_csv_from_table}, .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T, &TABLE_T }, .write_restricted=1}
,{.name="write_delimited_file_from_table", .funcs={write_delimited_file_from_table}, .argc_range={3,3}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &TABLE_T }, .write_restricted=1}
,{.name="read_striped_from_path",.funcs={read_striped_from_path},.argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="write_striped_to_path",.funcs={write_striped_to_path}, .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }, .write_restricted=1}
,{.name="rep",                  .funcs={rep},           .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="out",                  .funcs={xout},          .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="display",              .funcs={xdisplay},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="stamp_diff",           .funcs={stamp_diff},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="dlload",               .funcs={dlload},        .argc_range={3,3}, .atomize=&NONE_D,  .types={&CHAR_T, &CHAR_T, &NUMERIC}}
,{.name="atlas",                .funcs={atlased},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="bars",                 .funcs={bars},          .argc_range={2,3}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="xkeys",                .funcs={xobj_keys},     .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="xvals",                .funcs={xobj_values},   .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}//NOT "values" - SQL insert uses
,{.name="shift",                .funcs={xshift},        .argc_range={2,3}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="msum",                 .funcs={msum},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="mcount",               .funcs={mcount},        .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="mavg",                 .funcs={mavg},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&NUMERIC,  &ALL_T   }}
,{.name="mmax",                 .funcs={mmax},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&NUMERIC,  &ALL_T   }}
,{.name="mmin",                 .funcs={mmin},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&NUMERIC,  &ALL_T   }}
,{.name="sort",                 .funcs={xsort},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="shuffle",              .funcs={xshuffle},      .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="left_join",            .funcs={join_left},     .argc_range={3,3}, .atomize=&NONE_D,  .types={&TABLE_T,  &TABLE_T, &ALL_T}}
,{.name="asof_join",            .funcs={join_asof},     .argc_range={4,4}, .atomize=&NONE_D,  .types={&TABLE_T,  &TABLE_T, &ALL_T, &ALL_T}}
,{.name="kerf_type",            .funcs={xkerf_type},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="meta_table",           .funcs={meta_table},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="kerf_type_name",       .funcs={xkerf_type_name},.argc_range={1,1},.atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="tables",               .funcs={xtables},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="delete_keys",          .funcs={xdelete},       .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="bucketed",             .funcs={xrank},         .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="in",                   .funcs={xin},           .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="except",               .funcs={xexcept},       .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="setminus",             .funcs={xexcept},       .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="between",              .funcs={between},       .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }, .whered="0between_pre"}
,{.name="union",                .funcs={xunion},        .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="intersect",            .funcs={xintersect},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="powerset",             .funcs={verb_powerset}, .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="has_column",           .funcs={xhas_column},   .argc_range={2,2}, .atomize=&NONE_D,  .types={&TABLE_T,  &ALL_T   }}
,{.name="has_key",              .funcs={xhas_key},      .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="system",               .funcs={xsystem},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="shell",                .funcs={xshell},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="dir_ls",               .funcs={dir_list_k},    .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="sort_debug",           .funcs={sort_debug},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="order",                .funcs={order},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="isnull",               .funcs={xisnull},       .argc_range={1,2}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T   }}
,{.name="ifnull",               .funcs={xisnull},       .argc_range={1,2}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T   }}
,{.name="cdr",                  .funcs={xcdr},          .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="type_null",            .funcs={null_type},     .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="count_null",           .funcs={count_null},    .argc_range={1,1}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="count_nonnull",        .funcs={count_nonnull}, .argc_range={1,1}, .atomize=&AGGR_A,  .types={&ALL_T,    &ALL_T   }}
,{.name="dotp",                 .funcs={dotp},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="mmul",                 .funcs={mmul},          .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="minv",                 .funcs={minv},          .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="seed_prng",            .funcs={verb_seed_prng},.argc_range={1,1}, .atomize=&NONE_D,  .types={&INT_T,    &ALL_T   }}
,{.name="split",                .funcs={verb_split},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="ngram",                .funcs={verb_ngram},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="search",               .funcs={verb_search},   .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="deal",                 .funcs={verb_deal},     .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="cross",                .funcs={verb_cross},    .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="permutations",         .funcs={verb_perms},    .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="combinations",         .funcs={verb_combs},    .argc_range={2,3}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="filter",               .funcs={verb_filter},   .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="parse_int",            .funcs={parse_int},     .argc_range={1,2}, .atomize=&ATOMICB, .types={&CHAR_T,   &NUMERIC }}
,{.name="parse_float",          .funcs={parse_float},   .argc_range={1,1}, .atomize=&ATOMICB, .types={&CHAR_T,   &ALL_T   }}
,{.name="reset",                .funcs={verb_reset},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="table",                .funcs={xtable},        .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="mkdir",                .funcs={xmkdir},        .argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="zip",                  .funcs={new_zipped_charvec_temp},  .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,   &ALL_T   }}
,{.name="unzip",                .funcs={new_unzipped_charvec_temp},.argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,   &ALL_T   }}
,{.name="checksum",             .funcs={xhash},         .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,   &ALL_T   }}
,{.name="help",                 .funcs={verb_help},     .argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="emu_debug_mode",       .funcs={emu_debug_mode},.argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="format",               .funcs={format},        .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="format_stamp",         .funcs={format_stamp},  .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="parse_stamp",          .funcs={parse_stamp},   .argc_range={2,2}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="compressed",           .funcs={compressed},    .argc_range={1,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="_debug_sv",            .funcs={_debug_sv},     .argc_range={4,4}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="extract",              .funcs={extract},       .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="xor",                  .funcs={xor},           .argc_range={2,2}, .atomize=&ATOMICA, .types={&ALL_T,    &ALL_T   }}
,{.name="0between_pre",         .funcs={between_pre},   .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="_debug_gby",           .funcs={_debug_gby},    .argc_range={1,1}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}
,{.name="read_parceled_from_path",.funcs={read_parceled_from_path},.argc_range={1,1}, .atomize=&NONE_D,  .types={&CHAR_T,   &ALL_T   }}
,{.name="_debug_partable_select", .funcs={_debug_partable_select}, .argc_range={2,2}, .atomize=&NONE_D,  .types={&ALL_T,    &ALL_T   }}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
};

ADVERB ADVERB_DISPATCH[] = {
   {.name="fold",       .sym="\\/",  .kind=ADVERB_KIND_FOLD}
  ,{.name="refold",     .sym="\\\\", .kind=ADVERB_KIND_UNFOLD}
  ,{.name="mapdown",    .sym="\\=",  .kind=ADVERB_KIND_MAPDOWN}
  ,{.name="mapright",   .sym="\\>",  .kind=ADVERB_KIND_MAPRIGHT}
  ,{.name="mapleft",    .sym="\\<",  .kind=ADVERB_KIND_MAPLEFT}
  ,{.name="mapback",    .sym="\\~",  .kind=ADVERB_KIND_MAPBACK}

  ,{.name="reduce",     .sym="0",    .kind=ADVERB_KIND_FOLD}
  ,{.name="rereduce",   .sym="0",    .kind=ADVERB_KIND_UNFOLD}
  ,{.name="converge",   .sym="0",    .kind=ADVERB_KIND_FOLD}
  ,{.name="reconverge", .sym="0",    .kind=ADVERB_KIND_UNFOLD}
  
  ,{.name="unfold",     .sym="0",    .kind=ADVERB_KIND_UNFOLD}
  ,{.name="deconverge", .sym="0",    .kind=ADVERB_KIND_UNFOLD}

  ,{.name="mapcores",   .sym="0",    .kind=ADVERB_KIND_MAPBACK}

  //POTENTIAL_FEATURE_POINT
  //A useful adverb addition is probably "mapcross" or "mapouter" w/ abbreviation "\x" or similar
  //this replaces the common: "flatten n1 op mapleft mapright n2"
};

#pragma mark - Verb Init

int verb_init()
{
  DO(sizeof(TYPE0), ALL_T.t[i] = 1);

  ////////////
  //ALL_T.t[ZIP] = false; //disable until we're confident zips will work
  ////////////

  DO(ARRAY_LEN(VERB_DISPATCH), VERB_DISPATCH[i].id = i)

  return 0;
}

I VERB_PLUS_ID;
VERB VERB_PLUS;

I VERB_NEGATE_ID;
VERB VERB_NEGATE;

I VERB_OR_ID;
VERB VERB_OR;

I VERB_AND_ID;
VERB VERB_AND;

void reserved_verbs_init()
{
  DO(ARRAY_LEN(VERB_DISPATCH), S s=VERB_DISPATCH[i].name; update(The_Reserved, kcv(s), ki(TOKENS_VERB_WORD)))
  VERB_PLUS_ID = lookup_verb_id_by_name_or_sym("plus");
  assert(-1 != VERB_PLUS_ID);
  VERB_PLUS = VERB_DISPATCH[VERB_PLUS_ID];

  VERB_NEGATE_ID = lookup_verb_id_by_name_or_sym("negate");
  assert(-1 != VERB_NEGATE_ID);
  VERB_NEGATE = VERB_DISPATCH[VERB_NEGATE_ID];

  VERB_OR_ID = lookup_verb_id_by_name_or_sym("or");
  assert(-1 != VERB_OR_ID);
  VERB_OR = VERB_DISPATCH[VERB_OR_ID];

  VERB_AND_ID = lookup_verb_id_by_name_or_sym("and");
  assert(-1 != VERB_AND_ID);
  VERB_AND = VERB_DISPATCH[VERB_AND_ID];
}

void reserved_adverbs_init()
{
  DO(ARRAY_LEN(ADVERB_DISPATCH), S s=ADVERB_DISPATCH[i].name; update(The_Reserved, kcv(s), ki(TOKENS_ADVERB_WORD)))
}

#pragma mark - Verb Tools

void lookup_verb_data_via_token(K token, I *verb_code, VERB *verb, bool has_adverb, I apply, I argc)
{
  int (*cmp)(const char*, const char*, unsigned long);

  if(PARSE_RESERVED_CASE_INSENSITIVE)
  {
    cmp = strncasecmp;
  }
  else
  {
    cmp = strncmp;
  }


  K0 o1,o2;
  K payload = AT2(token, ki(TOKEN_PAYLOAD),o1);
  I kind = AT2(token, ki(TOKEN_KIND), o2)->i; 

  *verb_code = -1;
 
  I sym_arity_request = 2;
  if(!has_adverb && 1>=argc && apply) sym_arity_request = 1; //(!apply) -> solitary verb like +

  DO(ARRAY_LEN(VERB_DISPATCH),
    VERB v = VERB_DISPATCH[i];
    K x = payload;
    S s = v.name; 
    S t = v.sym; 

    bool matches = false;
    bool name_matches = false;
    bool sym_matches = false;
    bool sym_arity_matches = false;
    bool name_arity_matches = false;

    if(s) name_matches |= (strlen(s)==xn && !cmp(s, kC(x), xn));
    if(t) sym_matches  |= (strlen(t)==xn && !cmp(t, kC(x), xn));
    sym_arity_matches = ((v.argc_range[0] <= sym_arity_request ) && (sym_arity_request <= v.argc_range[1]));
    name_arity_matches = ((v.argc_range[0] <= argc) && (argc <= v.argc_range[1]));

    //////////////////////////
    name_arity_matches = true; //disable to enforce error checking here for name verbs (may want as toggle in signature)
    //////////////////////////

    //This lets us use placeholders in the verb table, but otherwise isn't strictly necessary
    if(v.funcs.func == NULL && v.funcs.aggregate == NULL) continue;

    SW(kind)
    {
      CS(TOKENS_VERB_SYM,  matches = sym_matches && sym_arity_matches)
      CS(TOKENS_VERB_WORD, matches = name_matches && (has_adverb || name_arity_matches))
    }

    if(matches)
    {
      *verb_code = i;
      *verb = v;
      return;
    } 
  )
}

I lookup_verb_id_by_name_or_sym(S x)
{
  int (*cmp)(const char*, const char*, unsigned long);

  if(PARSE_RESERVED_CASE_INSENSITIVE)
  {
    cmp = strncasecmp;
  }
  else
  {
    cmp = strncmp;
  }


  DO(ARRAY_LEN(VERB_DISPATCH),
    VERB v = VERB_DISPATCH[i];
    S s = v.name; 
    S t = v.sym; 
    I n = strlen(x);

    bool name_matches = (s && strlen(s)==n && !cmp(s, x, n));
    bool sym_matches  = (t && strlen(t)==n && !cmp(t, x, n));
    
    if(name_matches || sym_matches) return i;
  )

  return SENTINEL;
}

I lookup_adverb(K payload)
{
  int (*cmp)(const char*, const char*, unsigned long);

  if(PARSE_RESERVED_CASE_INSENSITIVE)
  {
    cmp = strncasecmp;
  }
  else
  {
    cmp = strncmp;
  }


  DO(ARRAY_LEN(ADVERB_DISPATCH),
    ADVERB a = ADVERB_DISPATCH[i];
    K x = payload;
    S s = a.name; 
    S t = a.sym; 

    bool name_matches = (strlen(s)==xn && !cmp(s, kC(x), xn));
    bool sym_matches  = (strlen(t)==xn && !cmp(t, kC(x), xn));

    if(name_matches || sym_matches)
    { 
      return a.kind;
    }
  )

  return ADVERB_KIND_NULL;
}

#pragma mark - Verb wrapper for VERB_CALL / atomization
 
K ply(K w, K x, K y, K z, I argc, VERB op)
{
  //Summing VECTORs directly is 10x faster than using atomize: 0.05s vs 0.44s, 10^8 pairwise -INTs

  //Before calling the VERB operation op->func, check some if-branches to see
  //if we recurse on any form of atomize or divisibility.

  //POTENTIAL_OPTIMIZATION_POINT
  //1. We can replace these "IS_OBJECTKIND(x)" tests with a
  //   boolean array lookup on x->t
  //2. We could also consolidate all such outcomes
  //   into a single if(){recurse;} test, using OR "||"
  //3. Could also turn this into a SWITCH statement on bits


  if (0 && (op.write_restricted==1)&&The_Process_is_Child_Flag) ERROR(ERROR_FORKED_VERB);

  K args[] = {w,x,y,z};

  //Table atomize
  bool first_table  = argc >= 1 && op.atomize->TABLE[0] && IS_TABLE(w);
  bool second_table = argc >= 2 && op.atomize->TABLE[1] && IS_TABLE(x);

  if(first_table || second_table)
  {
    //Note: The method of a TABLE being a thinly disguised MAP has its ups and
    //downs. (One alternative is say, a LIST of MAPS each with TRAITS.) The
    //biggest downside so far is that table-trait-column attributes do not
    //transfer for free. This is minor at the moment. Maybe later it will
    //affect verbs more. If this MAP method turns into a problem we can go back
    //and switch to another method. The problem should look pretty bad first,
    //as I suspect dealing with the LIST method is harder.
    //
    //The other worry is that this will be the wrong table format given that we
    //expect to eventually split tables (by rows) across
    //threads/instances/servers. Inevitably this will mean some collection of
    //multiple tables that will appear to work as one. Additionally, it is
    //necessary that the split tables possess some understanding of order, so
    //that methods like FIRST and LAST are possible. It is possible to
    //accomplish this with either a MAP or a LIST. It is not clear yet whether
    //the choice matters.

    //POTENTIAL_OPTIMIZATION_POINT
    //We can make the work table either of the provided tables
    //(given the correct refcount, etc.) and modify the columns
    //in place. This may use dmend or something similar.

    K *table = work_push(new_table());

    if(first_table && !second_table)
    {
      nestset(*table, TRAITS, wTraits);//Table operations should retain key attributes, etc.
      ENUM(w, if(table_column_is_key_or_fkey(w,u)){cow_table_add_column(*table, u, v); continue;}//Leave keys alone
              K *result = work_push(ply(v,x,y,z,argc,op)); cow_table_add_column(*table, u, *result); rd(work_pop()))
    }
    else if(!first_table && second_table )
    {
      nestset(*table, TRAITS, xTraits);
      ENUM(x, if(table_column_is_key_or_fkey(x,u)){cow_table_add_column(*table, u, v); continue;}
              K *result = work_push(ply(w,v,y,z,argc,op)); cow_table_add_column(*table, u, *result); rd(work_pop()))
    }
    else //both tables
    {
      if(!tables_match_on_keys(w,x)) ERROR(ERROR_KEYS);

      I key_count = table_count_keys(w);

      nestset_ri_rd(*table, TRAITS, wTraits, true, true);

      //Two cases: unkeyed and keyed
      SW(key_count)
      {
        //Unkeyed - match by row number (i), union unshared columns
        CS(0,

          ENUM(w, if(!table_has_column(x,u) || table_column_is_key_or_fkey(w,u))
                  {
                    cow_table_add_column(*table, u, v);
                    continue;
                  }
                  K col = v;
                  K0 o;
                  K other_col = LOOKUP_(x,u, o);
                  K *result = work_push(ply(col,other_col,y,z,argc,op));
                  cow_table_add_column(*table, u, *result);
                  rd(work_pop());
          ) 
          ENUM(x, if(!table_has_column(w,u))
                  {
                    cow_table_receive_column_from_table(*table, x, u, false);//add column + column attributes (fkey)
                    continue;
                  }
          )
        )
        //One or more keys - match by keyed column values;
        //                   reject extraneous columns (in right table);
        //                   append unshared rows (from right to left)
        //                   fill in left columns missing from right
        CD:
        CS(1,

          K list = table_keyed_columns_list(w);
          work_push(list);
          
          //TODO: probably just do a til() list that we amend for each key
          //      0I or 0N can be "not present" and 1,2,3 can be matches
          //      at 1,2,3 in the left.
          //      Can probably factor with above method
          //TODO: KEYS AND FKEYS ARE SKIPPED
          //cow_table_receive_column_from_table(*table, x, u);
          //TODO: maybe, do we want to grade the rhs so that
          //      we can bin search really fast for matching with
          //      keys on the lhs?
          //TODO: can maybe be factored with cow_join?
          //
          //TODO in the WHERE iterator, leverage both ATTR_SORT and .t=BTREE 

          work_pop_rd(true);
        )
      }
    }

    return work_pop();
  }

  //Map Key-Value atomize
  bool first_map  = argc >= 1 && op.atomize->MAP[0] && IS_STRICT_MAP(w);//no datetime buckets
  bool second_map = argc >= 2 && op.atomize->MAP[1] && IS_STRICT_MAP(x);//no datetime buckets

  bool w_stamp = argc >= 1 && IS_STAMP(w);
  bool x_stamp = argc >= 2 && IS_STAMP(x);

  bool first_dtbucket  = argc >= 1 && op.atomize->DTBUCKET[0] && IS_DTBUCKET(w) && !x_stamp;
  bool second_dtbucket = argc >= 2 && op.atomize->DTBUCKET[1] && IS_DTBUCKET(x) && !w_stamp;

  bool first_kv  = first_map  || first_dtbucket;
  bool second_kv = second_map || second_dtbucket;

  bool dt_bucket = first_dtbucket || second_dtbucket;

  if(first_kv || second_kv)
  {
    //POTENTIAL_OPTIMIZATION_POINT
    //Instead of adding to an empty dictionary, you can reuse the old index,
    //reuse the old key lists, and merely generate a new value list (which can
    //even rely on pre-allocated space). The existing map construction methods
    //are insufficient for this purpose. A new one would be needed.
    //The two-map case may differ.

    K map = *work_push(new_map());

    if(dt_bucket)
    {
      SET_ALT_ATTR(map, MAP_ATTR_DTBUCKET);
    }

    if(first_kv && !second_kv)
    {
      ENUM(w, K result = *work_push(ply(v,x,y,z,argc,op)); insert_replace(map,u,result,false,false); work_pop_rd(true))
    }
    else if(!first_kv && second_kv)
    {
      ENUM(x, K result = *work_push(ply(w,v,y,z,argc,op)); insert_replace(map,u,result,false,false); work_pop_rd(true))
    }
    else//both maps
    {
      ENUM(w, K key = u, valueW = v; I p = lookupI(x, key); 
              if(IS_HASH_NULL(p))
              {
                if(ATOMIC_MAP_PRESERVES_RAGGED_KEYS)
                {
                  insert_replace(map,key,valueW,false,false);
                }
                else
                {
                  continue;//skip unshared keys
                }
              }
              K0 o;
              K valueX = LOOK_(xValues,p,o);
              K result = ply(valueW,valueX,y,z,argc,op); 
              insert_replace(map,key,result,false,false);
              rd(result);
      )

      if(ATOMIC_MAP_PRESERVES_RAGGED_KEYS)
      {
        //Make a second trip through, skipping already
        //handled keys, this time from the other map
        ENUM(x, K key = u, valueX = v; I p = lookupI(w, key); 
                if(!IS_HASH_NULL(p))
                {
                  continue;
                }
                insert_replace(map,key,valueX,false,false);
        )
      }
    }

    //TODO if(dt_bucket) -> either force via floor to INT, or reject on non INT/FLOAT
    //                      alternatively, handle ranges & such. so like y=>[2015, 2016]
    //                      can be packed in one, and work with IN/BETWEEN ???

    return work_pop();
  }


  //"Regular" atomize
  bool first_atomic   = argc >= 1 && op.atomize->MIXED_ARRAY[0]  && IS_MIXED_ARRAY(w);
       first_atomic  |= argc >= 1 && op.atomize->VECTOR[0]       && IS_VECTOR(w);

  bool second_atomic  = argc >= 2 && op.atomize->MIXED_ARRAY[1]  && IS_MIXED_ARRAY(x);
       second_atomic |= argc >= 2 && op.atomize->VECTOR[1]       && IS_VECTOR(x);

  if(first_atomic || second_atomic)
  {
      if(argc >= 2)//redundant for clarity. hiding branching
      {
        first_atomic  |= argc >= 2 && op.atomize->MIXEDxVECTOR[1] && IS_VECTOR(w)      && IS_MIXED_ARRAY(x);
        second_atomic |= argc >= 2 && op.atomize->MIXEDxVECTOR[0] && IS_MIXED_ARRAY(w) && IS_VECTOR(x);
      }

      //POTENTIAL_OPTIMIZATION_POINT
      //uh, if you have a BTREE with a vector payload<
      //as is common, then you're going to badly use
      //an ENUM/LIST2/LOOK iteration here, instead of doing what you should
      //do, which is run the operation on the BTREE's payload vector,
      //and then potentially re-sort/index it (or not depending, see plus)
      //So...I guess maybe this needs another type of atomize?
      //or another case in this big ply function?

      //POTENTIAL_OPTIMIZATION_POINT
      //1. This pre-alloc is  overkill for CHAR/BYTE vectors
      //2. This pre-alloc is underkill for LISTs
      //3. HASH currently has no pre-alloc
      //Same goes for all similar instances around this section of the code.
      //If this causes problem maybe best to just eat it at pre-n = 0 which is O(2n)
      //You could try to solve this by changing divisor to 8/2/1 based on MIN type
      //but that won't work for verbs like "=" equals

    //BTREE is indexed afterwards in batch. HASH is indexed on the fly
    //BTREE indexed for more than just atomic types? Regular verb application?
    bool interns = false;
    interns |= (argc >= 1 && IS_HASH(w));
    interns |= (argc >= 2 && (HASH == MAX(wt,xt)));
    interns &= op.atomize->HASHES;
    
    bool sorts = false;
    sorts |= (argc >= 1 && IS_BTREE(w));
    sorts |= (argc >= 2 && (BTREE == MAX(wt,xt)));
    sorts &= op.atomize->BTREES;

    //TODO
    bool zips = false;
    //Figure out under what conditions we should zip the output
    //Maybe if *any* arg is ZIP, maybe if *all* are
    //Potentially also there should be op.atomize-ZIPS (as ->HASHES above)


    I pre = 0;//pre-alloc k size (c/2 because vectors half size)
    K m, *k;//work_push: don't leak k on error
 
    if(first_atomic && !second_atomic)
    {
      pre = COUNT(w)/2;
      m = NULL;
      if(interns)
      {
        m = new_intern();
      }
      else
      {
        m = new_k(LIST,pre);
        m->n = 0;
      }
      work_push(m); 
      //LIST2(w, x, *k = cow_add(*k,ply(u,x,y,z,argc,op)))//commented because ![0,1,`c] was crash bug
      ENUM(w, K p = ply(v,x,y,z,argc,op); work_pop(); m = cow_add(m,p); work_push(m); rd(p))  
    }
    else if(!first_atomic && second_atomic)
    {
      pre = COUNT(x)/2;
      m = NULL;
      if(interns)
      {
        m = new_intern();
      }
      else
      {
        m = new_k(LIST,pre);
        m->n = 0;
      }
      work_push(m);
      ENUM(x, K p = ply(w,v,y,z,argc,op); work_pop(); m = cow_add(m,p); work_push(m); rd(p))  
    }
    else //1 && 1
    {
      pre = MAX(COUNT(w),COUNT(x))/2;
      m = NULL;
      if(interns)
      {
        m = new_intern();
      }
      else
      {
        m = new_k(LIST,pre);
        m->n = 0;
      }
      work_push(m);
      LIST2(w, x, K p = ply(u,v,y,z,argc,op); work_pop(); m = cow_add(m,p); work_push(m); rd(p))  
    }

    if(sorts)
    {
      K a = *k;
      K b = new_btree_from_K(*k);//TODO: preserve BTREE specific attributes?
      *k = b;
      rd(a);
    }

    return work_pop();
  }

  //Type check occurs after atomization check: ATOMIZE overrides TYPE
  SW(argc)
  {
    CSF(4,)
     CS(3, return op.funcs.func(w, x, y, z))
     CS(2, if(!op.types[0]->t[awt] || !op.types[1]->t[axt])ERROR(ERROR_TYPE))
     CS(1, if(!op.types[0]->t[awt]) ERROR(ERROR_TYPE))
  }

  //Plain Old Verb Call
  return op.funcs.func(w, x, y, z);
  //You could do return op.funcs.func(w, x, y, z, argc, op.id); (entire "VERB op" is slow)
}

#pragma mark - Verb Functions

K ident(K w, K x, K y, K z){R strong(w);}
K right(K w, K x, K y, K z){R strong(x);}

#pragma mark - 

