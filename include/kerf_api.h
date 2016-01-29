#pragma once
#include <stdint.h>

// All Kerf objects are instances of the KERF struct:

typedef struct kerf0 {
	char m;     // log_2 of memory slab size (internal use only)
	char a;     // attribute flags
	char h;     // log_2 of header subslice size (internal use only)
	char t;     // typecode, as in kerf_type
	int32_t r;  // reference count
	
	union {
		int64_t       i; // value of a Kerf integer or stamp
		double        f; // value of a Kerf float
		char          c; // value of a Kerf char
		char*         s;
		struct kerf0* k;
		
		struct {
			int64_t n;   // in a vector or list, the number of elements
			char g[];    // in a vector or list, the elements themselves
		};
	};
} *KERF, KERF0;

// Constants

enum KERF_TYPES {
   KERF_STAMPVEC =  -4,
   KERF_FLOATVEC =  -3,
   KERF_INTVEC   =  -2,
   KERF_CHARVEC  =  -1,
   KERF_FUNC     =   0,
   KERF_CHAR     =   1, 
   KERF_INT      =   2, 
   KERF_FLOAT    =   3,
   KERF_STAMP    =   4, 
   KERF_NIL      =   5, 
   KERF_LIST     =   6, 
   KERF_MAP      =   7, 
   KERF_HASH     =   8, 
   KERF_SORT     =   9, 
   KERF_TABLE    =  10,
   KERF_ATLAS    =  11,
};

#define KERF_ATTR_SORTED (1 << 4) // This collection is in ascending order
#define KERF_ATTR_BYTES  (1 << 5) // CHAR type display as 0xbytes
#define KERF_ATTR_DISK   (1 << 7) // Disk-backed storage, mmapped on disk

// Public API exposed by the Kerf executable

// objects/lifecycle
extern void kerf_api_release(KERF x);
extern KERF kerf_api_retain(KERF x);
extern KERF kerf_api_copy_on_write(KERF x);
extern KERF kerf_api_new_kerf(char type, int64_t length);
extern KERF kerf_api_new_int(int64_t n);
extern KERF kerf_api_new_float(double n);
extern KERF kerf_api_new_stamp(int64_t nanoseconds);
extern KERF kerf_api_new_charvec(char* cstring);
extern KERF kerf_api_new_map();
extern KERF kerf_api_new_list();
extern KERF kerf_api_nil();

// accessors/mutators
extern KERF kerf_api_show(KERF x);
extern int64_t kerf_api_len(KERF x);
extern int64_t  kerf_api_nanos_from_stamp(void * x);  // needs documentation
extern KERF kerf_api_get(KERF x, KERF index);
extern KERF kerf_api_set(KERF x, KERF index, KERF replacement);
extern KERF kerf_api_append(KERF x, KERF y);

// execution
extern KERF kerf_api_interpret(KERF charvec);
extern KERF kerf_api_call_nilad(KERF func);
extern KERF kerf_api_call_monad(KERF func, KERF x);
extern KERF kerf_api_call_dyad(KERF func, KERF x, KERF y);


// event handling
extern void kerf_api_register_for_eventing(void** p_kerf_hook, int (*p_func)(void *) );
extern void kerf_api_deregister_for_eventing(void* kerf_hook);
extern void kerf_api_notify_event(void *kerf_hook);
extern void kerf_api_denotify_event(void *kerf_hook);
