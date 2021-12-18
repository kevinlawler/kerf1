#include "kerf.h"

// publicly exposed API for dylibs

int kerf_api_init()                          { return kerf_init();}

// objects/lifecycle
void kerf_api_release(K x)                   { rd(x); }
K kerf_api_retain(K x)                       { return strong(x); }
K kerf_api_copy_on_write(K x)                { return cow(x); }
K kerf_api_new_kerf(C t, I n)                { return new_k(t, n); }
K kerf_api_new_int(I n)                      { return Ki(n); }
K kerf_api_new_float(F n)                    { return Kf(n); }
K kerf_api_new_stamp(I n)                    { return Ks(n); }
K kerf_api_new_charvec(char* str)            { return charvec_from_cstring(str); }
K kerf_api_new_map()                         { return new_map(); }
K kerf_api_new_list()                        { return Kk(); }
K kerf_api_nil()                             { return Kn(); }

// accessors/mutators
K kerf_api_show(K x)                         { return show(x); }
I kerf_api_len(K x)                          { return lenI(x); }
I kerf_api_nanos_from_stamp(void * x)        { return stampI_from_tm(x, false); }
K kerf_api_get(K x, K index)                 { return at(x, index); }
K kerf_api_set(K x, K index, K replacement)  { return update(x, index, replacement); }
K kerf_api_append(K x, K y)                  { return cow_add(x, y); }

// execution
K kerf_api_interpret(K str)                  { return interpret(str); }
K kerf_api_call_nilad(K func)                { return NILAD_EX(func); }
K kerf_api_call_monad(K func, K x)           { return MONAD_EX(func, x); }
K kerf_api_call_dyad(K func, K x, K y)       { return DYAD_EX(func, x, y); }

// eventing
void kerf_api_register_for_eventing(void** p_kerf_hook, int (*p_func)(void *) );
void kerf_api_deregister_for_eventing(void* kerf_hook);
void kerf_api_notify_event(void *kerf_hook);
void kerf_api_denotify_event(void *kerf_hook);
