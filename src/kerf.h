//Kerf Copyright ©2014-2016 Kerf Software - Not for Distribution
#pragma once

static char *kerf = 
" ╔═════════════════════════════════════════════════════════════════════════╗\n"
" ║                                                                         ║\n"
" ║                                                                         ║\n"
" ║                                                                         ║\n"
" ║                     ██╗  ██╗ █████╗ ██████╗  ██████╗                    ║\n"
" ║                     ██║ ██╔╝██╔══██╗██╔══██╗███╔═══╝                    ║\n"
" ║                     █████╔╝ ██████╔╝██████╔╝██████╗                     ║\n"
" ║                     ██╔═██╗ ██╔═══╝ ██╔══██╗██╔═══╝                     ║\n"
" ║                     ██║  ██╗╚██████╗██║  ██║██║                         ║\n"
" ║                     ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚═╝     ©                   ║\n"
" ║                                                                         ║\n"
" ║                                                                         ║\n"
" ║"                                                                  "%72s ║\n"
" ╚═════════════════════════════════════════════════════════════════════════╝\n";

//////////////////////////////////////////////////////////////
//Testing Settings////////////////////////////////////////////
#ifdef DEBUG
#define TEST_MACROS (true)
#else
#define TEST_MACROS (false)
#endif

#define TEST_TRACK_ALLOCATIONS              (true  && TEST_MACROS)
#define TEST_TRACK_DEALLOC_IN_WORK_STACK    (true  && TEST_MACROS)
#define TEST_TRACK_SORT_ATTR                (true  && TEST_MACROS)

#define TEST_TRY_POOL_PREZEROES_ALLOC       (false && TEST_MACROS)
#define TEST_TRY_POOL_SIDESTEP_VIA_MALLOC   (false && TEST_MACROS)
#define TEST_TRY_NO_REFERENCE_DECREMENT     (false && TEST_MACROS)
//Licensing///////////////////////////////////////////////////
#define EXPIRES                             (false)
//Version/////////////////////////////////////////////////////
#define KERF_FUNC_VERSION                   (0.1)
//Compile Time Settings///////////////////////////////////////
#define PRNG_USES_RANDOMIZED_SEED_ON_INIT   (true)

#define HASH_USES_RANDOMIZED_SEED_ON_INIT   (false)
#define HASH_ROBIN_HOODS                    (true) //true: slower inserts, lower sigma lookups
#define HASH_LOAD_FACTOR                    (0.50) //high load factors (~0.9) demand robin hooding
#define HASH_NULL                           (IN)
#define HASH_DELETED                        (-II)  //because no index can have this we can add later
#define IS_HASH_NULL(x)                     ((x)==HASH_NULL)
#define HASH_MODS_OUT_DICTIONARY_ORDER      (true)

#define LANDIS_NULL                         (0LL)
#define IS_LANDIS_NULL(x)                   ((x)==LANDIS_NULL)
#define SORT_INDEX_YIELDS_SORT_NOT_LIST     (false) 
#define SORT_ORDER_STRINGS_LEXICOGRAPHIC    (true) //necessary for SQL LIKE % matching

#define FINDER_FAIL                         (IN)

#define ATLAS_USES_REVERSE_KEYING           (true)
#define ATLAS_REVERSE_KEY                   ("\034\035\036\037DUAL_KEY")
 
#define MAPPED_COLUMNS_GROW_DENSE_SIBLINGS  (true)

#define DISK_ALLOCATE_FORCES_ENOSPC         (false)
#define MAPPED_HEURISTIC_HALVES_WIDEST      (true)
#define MAPPED_INIT_EXPECTS_FUTURE_GROWTH   (true) //try to pre-reserve extra to guarantee expansion
#define MAPPED_FILES_DOUBLE_IN_SIZE         (false)//probably obviated
#define MAPPED_POSIX_STICKLER_REMAP         (false)//technically standard says you're supposed to map again when a file grows
#define MAPPED_LOG_2_BLOCKS                 (14)
#define MAPPED_BLOCKS                       POW2(MAPPED_LOG_2_BLOCKS)
#define MAPPED_DISK_FORCES_PATH             (false)//may want /tmp or /var/tmp or not-tmpfs
#define MAPPED_DISK_FORCED_DIR              ("/var/tmp/")
#define MAPPED_DISK_FORCED_TEMPLATE         ("kerfswap-XXXXXX")
#define MAPPED_DISK_FORCED_SUFFIX           (".tmp")
#define MAPPED_REDIRECT_REOPEN_TO_EXISTING  (true)

#define POOL_SANE_MEMORY_BOUND              (POW2(50))
#define POOL_CHECKS_NONZERO_ALLOC_RETURN    (true)
#define POOL_CHECKS_SANE_ALLOC_BOUNDS       (true)
#define POOL_CHECKS_SANE_LIST_SIZE          (true) //if this is off, range(INF) will crash
#define POOL_DYNAMIC_MAX_LANE_OVERRIDE      (false)
#define POOL_MAX_LANE_RETAINED_IN_POOL      (25)   //ScottL says 26 too aggressive (200M retained)
#define POOL_SUBDIVIDES_PAGES               (true)
#define POOL_WARMS_CACHES_ON_INIT           (false)
#define POOL_BIG_ATOMS_PAGE_SIZE            (false)
#define POOL_BIG_ATOMS_CACHE_LINE           (true)
#define POOL_LANE_BUF                       (sizeof(void*)*8+1)
#define POOL_MIN_ALLOC_WIDTH                (PAGE_SIZE_BYTES << 0)

#define GENERIC_DEPTH_LIMIT                 (100)

#define PARSE_RESERVED_CASE_INSENSITIVE     (true)
#define PARSE_ZERO_PREFIX_MEANS_OCTAL       (false) //Check that this doesn't break "08m12d"
#define PARSE_ALLOW_DERIVED_VERBS           (true)
#define PARSE_MAX_DEPTH                     (GENERIC_DEPTH_LIMIT)
#define PARSE_JSON_STAMP_STRING_HACK        (true)
#define PARSE_JSON_STAMP_STRING_XTC         ("KERFSTAMP_")
#define PARSE_JSON_TABLE_KEY                ("is_json_table") //presence indicates table
#define PARSE_JSON_STRICT_KEYS              (false) //false for now for json inet

#define DYNAMIC_DOT_NAMES                   (true) //a.b.c resolved at assemble or execution time?

#define FLOAT_NANS_COMPARE_AS_SMALLEST      (true)
#define FLOAT_INF_IS_FAR_FROM_FLT_MAX       (false)

#define INET_SERVER_LIVES_WHEN_STDIN_CLOSED (false) //false: CTRL+D kills server, so does `</dev/null`
#define INET_SERVER_RESPECTS_CTRL_C         (true)
#define INET_SOCKET_HUP_ENDS_EXECUTION      (true)

#define PRINT_TABLE_MAX_PAGES_TO_SHOW       (10)
#define PRINT_ASSIGNMENT_RESULTS            (true)
#define PARALLEL_EXECUTION_DEPTH_LIMIT      (2+1+1) //+1 because we use mapcores internally, +1 we fork child servers
#define CTRL_C_EXIT_CODE                    (130) //bash ctrl+c script exit code
#define DYNAMIC_FUNCTION_MAX_ARGS           (8)
#define SHOW_MAX_DEPTH                      (15)
#define VM_STACK_EXECUTION_MAX_DEPTH        (GENERIC_DEPTH_LIMIT)
#define IO_MAX_DEPTH                        (GENERIC_DEPTH_LIMIT)
#define TRUTHTABLE_VALUE_FOR_NIL            (0)
#define TIME_DATE_CUTOFF                    (2*24*60*61*BILLION)
#define PUTS_TIMING_ON_BY_DEFAULT           (false)
#define USE_NS_CLOCK_FOR_TIMING             (true)
#define VERB_MONADIC_GROUP_RETURNS_MAP      (true)
#define ATOMIC_MAP_PRESERVES_RAGGED_KEYS    (true)
#define MACRO_FASTER_BUT_BIGGER_BINARY      (true)
#define SECURITY_ALLOW_LONGJMP_FROM_SIGNAL  (true) //Technically, CTRL+C is a risk
#define KERF_TREE_STARTING_DIRECTORY        (".user")
#define KERF_CACHE_MAX_SIZE                 (12345)
#define LONGJMP_NESTS                       (false)
#define DATES_ALLOW_DASHED                  (false)
#define SQL_INSERT_CLASSIC_PARENS           (false)

#define MAKE_TABLE_USES_LAST_BINDING        (true)

//------------/////////////////////////////////////////////////////////////////
//if this goes false you'll need to denest and manually assign tables &such
//this is probably OK...except...you need to make sure DISK TENANT columns
//are not being modified during special the sql accesses. this could take on a 
//variety of forms. maybe it means regular tenant pushes DO increment to stack.
//maybe it means no nested SQL-UPDATES. maybe it means finding and disabling
//table alters during sql calls. maybe it means making sure join/optimized
//join do not attempt to reuse tenants. maybe we just tell people: "don't
//do that"
#define ALPHA_DISK_REFCOUNT                 (true)
//----------///////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////

#ifndef DEBUG
#define NDEBUG
#endif

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#define IS_DEFINED(macro) IS_DEFINED_(macro)
#define MACROTEST_
#define MACROTEST_1 ,
#define IS_DEFINED_(value) IS_DEFINED__(MACROTEST_##value)
#define IS_DEFINED__(comma) IS_DEFINED___(comma 1, 0)
#define IS_DEFINED___(_, v, ...) v
//Includes ///////////////////////////////////////////////////
#include <arpa/inet.h>
#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <fts.h>
#include <limits.h>
#include <math.h>
#include <netdb.h>
#include <poll.h>
#include <pthread.h>
#include <pwd.h>
#include <setjmp.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sysexits.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <zlib.h>

#ifdef __linux
#include <sys/eventfd.h>
#endif

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__)
#include <netinet/in.h>
#endif

#ifdef __MACH__ //Apple OSX
#include <mach/clock.h>
#include <mach/mach.h>
#include <netinet/tcp.h>
#include <sys/sysctl.h>

#include <Accelerate/Accelerate.h>
#undef vA
#undef vs
#undef si

#endif

#define POW2(x) (1LL<<(x))

#ifdef _WIN64 //Windows (64-bit only)
    #define CONSOLE 1
#elif  _WIN32 //Windows (32-bit and 64-bit, where not already eliminated)
    #define CONSOLE 1
#elif  __APPLE__
    #include "TargetConditionals.h" //*defines* following as 0 or 1

    #define MAPPED_MORE_VMEM_SLOWER_EXIT (false)//On OSX at least munmapping 2^46 is ~1second

    #if   TARGET_IPHONE_SIMULATOR // no console on iOS Simulator
    #elif TARGET_OS_IPHONE // no console on iOS device
        #ifdef _LP64 //64-bit pointers
            //2015.04.03 iPhone 6+: 2^30 works, 2^31 fails
            //you could try mapping smaller parts to make the whole, doubtful it will work
            #define MAPPED_VIRTUAL_RESERVED_BYTES  (POW2(29))
        #else
            //2^{29} ~ 536M - same story as the old carmack post
            #define MAPPED_VIRTUAL_RESERVED_BYTES  (POW2(29))
        #endif
    #else
      #define CONSOLE 1
    #endif
#elif  __linux
    #define CONSOLE 1
#elif  __unix
    #define CONSOLE 1
#elif  __posix
    #define CONSOLE 1
#endif

#ifdef NOCONSOLE
#undef CONSOLE
#endif

#ifndef MAPPED_MORE_VMEM_SLOWER_EXIT
#define MAPPED_MORE_VMEM_SLOWER_EXIT (true) //On OSX at least munmapping 2^46 is ~1second
#endif

#ifndef MAPPED_VIRTUAL_RESERVED_BYTES
  #ifdef _LP64 //64-bit pointers
    #if MAPPED_MORE_VMEM_SLOWER_EXIT
      #define MAPPED_VIRTUAL_RESERVED_BYTES     (POW2(46) + POW2(45))//assumes 47-bit virtual address space
    #else
      #define MAPPED_VIRTUAL_RESERVED_BYTES     (POW2(42))
    #endif
  #else
    #define MAPPED_VIRTUAL_RESERVED_BYTES       (POW2(29))
  #endif
#endif

static int64_t The_Mapped_Virtual_Reserved_Bytes = MAPPED_VIRTUAL_RESERVED_BYTES;

//#ifdef CONSOLE
//#warning console defined
//#else
//#warning console NOT defined
//#endif

#ifdef CONSOLE
#ifdef __linux
#else
#include <editline/readline.h>
#endif
#endif 

#include "dry.h"
#include "help.h"
#include "lz4.h"
#include "yikes.h"
//////////////////////////////////////////////////////////////

