#ifndef __LIBLFDS611_H

  /***** library header *****/
  #define LFDS611_RELEASE_NUMBER_STRING  "6.1.1"




  /***** lfds611_abstraction *****/

  /***** defines *****/
  #if (defined _WIN64 && defined _MSC_VER && !defined WIN_KERNEL_BUILD)
    // TRD : 64-bit Windows user-mode with the Microsoft C compiler, any CPU
    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <windows.h>
    #include <intrin.h>
    typedef unsigned __int64                 lfds611_atom_t;
    #define LFDS611_INLINE                   __forceinline
    #define LFDS611_ALIGN(alignment)         __declspec( align(alignment) )
    #define LFDS611_ALIGN_SINGLE_POINTER     8
    #define LFDS611_ALIGN_DOUBLE_POINTER     16
    #define LFDS611_BARRIER_COMPILER_LOAD    _ReadBarrier()
    #define LFDS611_BARRIER_COMPILER_STORE   _WriteBarrier()
    #define LFDS611_BARRIER_COMPILER_FULL    _ReadWriteBarrier()
    #define LFDS611_BARRIER_PROCESSOR_LOAD   _mm_lfence()
    #define LFDS611_BARRIER_PROCESSOR_STORE  _mm_sfence()
    #define LFDS611_BARRIER_PROCESSOR_FULL   _mm_mfence()
  #endif

  #if (!defined _WIN64 && defined _WIN32 && defined _MSC_VER && !defined WIN_KERNEL_BUILD)
    // TRD : 32-bit Windows user-mode with the Microsoft C compiler, any CPU
    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <windows.h>
    #include <intrin.h>
    typedef unsigned long int                lfds611_atom_t;
    #define LFDS611_INLINE                   __forceinline
    #define LFDS611_ALIGN(alignment)         __declspec( align(alignment) )
    #define LFDS611_ALIGN_SINGLE_POINTER     4
    #define LFDS611_ALIGN_DOUBLE_POINTER     8
    #define LFDS611_BARRIER_COMPILER_LOAD    _ReadBarrier()
    #define LFDS611_BARRIER_COMPILER_STORE   _WriteBarrier()
    #define LFDS611_BARRIER_COMPILER_FULL    _ReadWriteBarrier()
    #define LFDS611_BARRIER_PROCESSOR_LOAD   _mm_lfence()
    #define LFDS611_BARRIER_PROCESSOR_STORE  _mm_sfence()
    #define LFDS611_BARRIER_PROCESSOR_FULL   _mm_mfence()

    // TRD : this define is documented but missing in Microsoft Platform SDK v7.0
    #define _InterlockedCompareExchangePointer(destination, exchange, compare) _InterlockedCompareExchange((volatile long *) destination, (long) exchange, (long) compare)
  #endif

  #if (defined _WIN64 && defined _MSC_VER && defined WIN_KERNEL_BUILD)
    // TRD : 64-bit Windows kernel with the Microsoft C compiler, any CPU
    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <wdm.h>
    typedef unsigned __int64                 lfds611_atom_t;
    #define LFDS611_INLINE                   __forceinline
    #define LFDS611_ALIGN(alignment)         __declspec( align(alignment) )
    #define LFDS611_ALIGN_SINGLE_POINTER     8
    #define LFDS611_ALIGN_DOUBLE_POINTER     16
    #define LFDS611_BARRIER_COMPILER_LOAD    _ReadBarrier()
    #define LFDS611_BARRIER_COMPILER_STORE   _WriteBarrier()
    #define LFDS611_BARRIER_COMPILER_FULL    _ReadWriteBarrier()
    #define LFDS611_BARRIER_PROCESSOR_LOAD   _mm_lfence()
    #define LFDS611_BARRIER_PROCESSOR_STORE  _mm_sfence()
    #define LFDS611_BARRIER_PROCESSOR_FULL   _mm_mfence()
  #endif

  #if (!defined _WIN64 && defined _WIN32 && defined _MSC_VER && defined WIN_KERNEL_BUILD)
    // TRD : 32-bit Windows kernel with the Microsoft C compiler, any CPU
    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <wdm.h>
    typedef unsigned long int                lfds611_atom_t;
    #define LFDS611_INLINE                   __forceinline
    #define LFDS611_ALIGN(alignment)         __declspec( align(alignment) )
    #define LFDS611_ALIGN_SINGLE_POINTER     4
    #define LFDS611_ALIGN_DOUBLE_POINTER     8
    #define LFDS611_BARRIER_COMPILER_LOAD    _ReadBarrier()
    #define LFDS611_BARRIER_COMPILER_STORE   _WriteBarrier()
    #define LFDS611_BARRIER_COMPILER_FULL    _ReadWriteBarrier()
    #define LFDS611_BARRIER_PROCESSOR_LOAD   _mm_lfence()
    #define LFDS611_BARRIER_PROCESSOR_STORE  _mm_sfence()
    #define LFDS611_BARRIER_PROCESSOR_FULL   _mm_mfence()

    // TRD : this define is documented but missing in Microsoft Platform SDK v7.0
    #define _InterlockedCompareExchangePointer(destination, exchange, compare) _InterlockedCompareExchange((volatile long *) destination, (long) exchange, (long) compare)
  #endif

  #if (defined __unix__ && defined __x86_64__ && __GNUC__)
    // TRD : any UNIX with GCC on x64
    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>
    typedef unsigned long long int           lfds611_atom_t;
    #define LFDS611_INLINE                   inline
    #define LFDS611_ALIGN(alignment)         __attribute__( (aligned(alignment)) )
    #define LFDS611_ALIGN_SINGLE_POINTER     8
    #define LFDS611_ALIGN_DOUBLE_POINTER     16
    #define LFDS611_BARRIER_COMPILER_LOAD    __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_COMPILER_STORE   __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_COMPILER_FULL    __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_PROCESSOR_LOAD   __sync_synchronize()
    #define LFDS611_BARRIER_PROCESSOR_STORE  __sync_synchronize()
    #define LFDS611_BARRIER_PROCESSOR_FULL   __sync_synchronize()
  #endif

  #if (defined __unix__ && defined __i686__ && __GNUC__)
    // TRD : any UNIX with GCC on x86
    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>
    typedef unsigned long int                lfds611_atom_t;
    #define LFDS611_INLINE                   inline
    #define LFDS611_ALIGN(alignment)         __attribute__( (aligned(alignment)) )
    #define LFDS611_ALIGN_SINGLE_POINTER     4
    #define LFDS611_ALIGN_DOUBLE_POINTER     8
    #define LFDS611_BARRIER_COMPILER_LOAD    __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_COMPILER_STORE   __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_COMPILER_FULL    __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_PROCESSOR_LOAD   __sync_synchronize()
    #define LFDS611_BARRIER_PROCESSOR_STORE  __sync_synchronize()
    #define LFDS611_BARRIER_PROCESSOR_FULL   __sync_synchronize()
  #endif

  #if (defined __unix__ && defined __arm__ && __GNUC__)
    // TRD : any UNIX with GCC on ARM
    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>
    typedef unsigned long int                lfds611_atom_t;
    #define LFDS611_INLINE                   inline
    #define LFDS611_ALIGN(alignment)         __attribute__( (aligned(alignment)) )
    #define LFDS611_ALIGN_SINGLE_POINTER     4
    #define LFDS611_ALIGN_DOUBLE_POINTER     8
    #define LFDS611_BARRIER_COMPILER_LOAD    __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_COMPILER_STORE   __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_COMPILER_FULL    __asm__ __volatile__ ( "" : : : "memory" )
    #define LFDS611_BARRIER_PROCESSOR_LOAD   __sync_synchronize()
    #define LFDS611_BARRIER_PROCESSOR_STORE  __sync_synchronize()
    #define LFDS611_BARRIER_PROCESSOR_FULL   __sync_synchronize()
  #endif

  #define LFDS611_BARRIER_LOAD   LFDS611_BARRIER_COMPILER_LOAD; LFDS611_BARRIER_PROCESSOR_LOAD; LFDS611_BARRIER_COMPILER_LOAD
  #define LFDS611_BARRIER_STORE  LFDS611_BARRIER_COMPILER_STORE; LFDS611_BARRIER_PROCESSOR_STORE; LFDS611_BARRIER_COMPILER_STORE
  #define LFDS611_BARRIER_FULL   LFDS611_BARRIER_COMPILER_FULL; LFDS611_BARRIER_PROCESSOR_FULL; LFDS611_BARRIER_COMPILER_FULL

  /***** enums *****/
  enum lfds611_data_structure_validity
  {
    LFDS611_VALIDITY_VALID,
    LFDS611_VALIDITY_INVALID_LOOP,
    LFDS611_VALIDITY_INVALID_MISSING_ELEMENTS,
    LFDS611_VALIDITY_INVALID_ADDITIONAL_ELEMENTS,
    LFDS611_VALIDITY_INVALID_TEST_DATA
  };

  /***** structs *****/
  struct lfds611_validation_info
  {
    lfds611_atom_t
      min_elements,
      max_elements;
  };

  /***** public prototypes *****/
  void *lfds611_abstraction_malloc( size_t size );
  void lfds611_abstraction_free( void *memory );





  /***** lfds611_freelist *****/

  /***** enums *****/
  enum lfds611_freelist_query_type
  {
    LFDS611_FREELIST_QUERY_ELEMENT_COUNT,
    LFDS611_FREELIST_QUERY_VALIDATE
  };

  /***** incomplete types *****/
  struct lfds611_freelist_state;
  struct lfds611_freelist_element;

  /***** public prototypes *****/
  int lfds611_freelist_new( struct lfds611_freelist_state **fs, lfds611_atom_t number_elements, int (*user_data_init_function)(void **user_data, void *user_state), void *user_state );
  void lfds611_freelist_use( struct lfds611_freelist_state *fs );
  void lfds611_freelist_delete( struct lfds611_freelist_state *fs, void (*user_data_delete_function)(void *user_data, void *user_state), void *user_state );

  lfds611_atom_t lfds611_freelist_new_elements( struct lfds611_freelist_state *fs, lfds611_atom_t number_elements );

  struct lfds611_freelist_element *lfds611_freelist_pop( struct lfds611_freelist_state *fs, struct lfds611_freelist_element **fe );
  struct lfds611_freelist_element *lfds611_freelist_guaranteed_pop( struct lfds611_freelist_state *fs, struct lfds611_freelist_element **fe );
  void lfds611_freelist_push( struct lfds611_freelist_state *fs, struct lfds611_freelist_element *fe );

  void *lfds611_freelist_get_user_data_from_element( struct lfds611_freelist_element *fe, void **user_data );
  void lfds611_freelist_set_user_data_in_element( struct lfds611_freelist_element *fe, void *user_data );

  void lfds611_freelist_query( struct lfds611_freelist_state *fs, enum lfds611_freelist_query_type query_type, void *query_input, void *query_output );





  /***** lfds611_liblfds *****/

  /***** public prototypes *****/
  void lfds611_liblfds_abstraction_test_helper_increment_non_atomic( lfds611_atom_t *shared_counter );
  void lfds611_liblfds_abstraction_test_helper_increment_atomic( volatile lfds611_atom_t *shared_counter );
  void lfds611_liblfds_abstraction_test_helper_cas( volatile lfds611_atom_t *shared_counter, lfds611_atom_t *local_counter );
  void lfds611_liblfds_abstraction_test_helper_dcas( volatile lfds611_atom_t *shared_counter, lfds611_atom_t *local_counter );





  /***** lfds611_queue *****/

  /***** enums *****/
  enum lfds611_queue_query_type
  {
    LFDS611_QUEUE_QUERY_ELEMENT_COUNT,
    LFDS611_QUEUE_QUERY_VALIDATE
  };

  /***** incomplete types *****/
  struct lfds611_queue_state;

  /***** public prototypes *****/
  int lfds611_queue_new( struct lfds611_queue_state **sq, lfds611_atom_t number_elements );
  void lfds611_queue_use( struct lfds611_queue_state *qs );
  void lfds611_queue_delete( struct lfds611_queue_state *qs, void (*user_data_delete_function)(void *user_data, void *user_state), void *user_state );

  int lfds611_queue_enqueue( struct lfds611_queue_state *qs, void *user_data );
  int lfds611_queue_guaranteed_enqueue( struct lfds611_queue_state *qs, void *user_data );
  int lfds611_queue_dequeue( struct lfds611_queue_state *qs, void **user_data );

  void lfds611_queue_query( struct lfds611_queue_state *qs, enum lfds611_queue_query_type query_type, void *query_input, void *query_output );





  /***** lfds611_ringbuffer *****/

  /***** enums *****/
  enum lfds611_ringbuffer_query_type
  {
    LFDS611_RINGBUFFER_QUERY_VALIDATE
  };

  /***** incomplete types *****/
  struct lfds611_ringbuffer_state;

  /***** public prototypes *****/
  int lfds611_ringbuffer_new( struct lfds611_ringbuffer_state **rs, lfds611_atom_t number_elements, int (*user_data_init_function)(void **user_data, void *user_state), void *user_state );
  void lfds611_ringbuffer_use( struct lfds611_ringbuffer_state *rs );
  void lfds611_ringbuffer_delete( struct lfds611_ringbuffer_state *rs, void (*user_data_delete_function)(void *user_data, void *user_state), void *user_state );

  struct lfds611_freelist_element *lfds611_ringbuffer_get_read_element( struct lfds611_ringbuffer_state *rs, struct lfds611_freelist_element **fe );
  struct lfds611_freelist_element *lfds611_ringbuffer_get_write_element( struct lfds611_ringbuffer_state *rs, struct lfds611_freelist_element **fe, int *overwrite_flag );

  void lfds611_ringbuffer_put_read_element( struct lfds611_ringbuffer_state *rs, struct lfds611_freelist_element *fe );
  void lfds611_ringbuffer_put_write_element( struct lfds611_ringbuffer_state *rs, struct lfds611_freelist_element *fe );

  void lfds611_ringbuffer_query( struct lfds611_ringbuffer_state *rs, enum lfds611_ringbuffer_query_type query_type, void *query_input, void *query_output );





  /***** lfds611_slist *****/

  /***** incomplete types *****/
  struct lfds611_slist_state;
  struct lfds611_slist_element;

  /***** public prototypes *****/
  int lfds611_slist_new( struct lfds611_slist_state **ss, void (*user_data_delete_function)(void *user_data, void *user_state), void *user_state );
  void lfds611_slist_use( struct lfds611_slist_state *ss );
  void lfds611_slist_delete( struct lfds611_slist_state *ss );

  struct lfds611_slist_element *lfds611_slist_new_head( struct lfds611_slist_state *ss, void *user_data );
  struct lfds611_slist_element *lfds611_slist_new_next( struct lfds611_slist_element *se, void *user_data );

  int lfds611_slist_logically_delete_element( struct lfds611_slist_state *ss, struct lfds611_slist_element *se );
  void lfds611_slist_single_threaded_physically_delete_all_elements( struct lfds611_slist_state *ss );

  int lfds611_slist_get_user_data_from_element( struct lfds611_slist_element *se, void **user_data );
  int lfds611_slist_set_user_data_in_element( struct lfds611_slist_element *se, void *user_data );

  struct lfds611_slist_element *lfds611_slist_get_head( struct lfds611_slist_state *ss, struct lfds611_slist_element **se );
  struct lfds611_slist_element *lfds611_slist_get_next( struct lfds611_slist_element *se, struct lfds611_slist_element **next_se );
  struct lfds611_slist_element *lfds611_slist_get_head_and_then_next( struct lfds611_slist_state *ss, struct lfds611_slist_element **se );





  /***** lfds611_stack *****/

  /***** enums *****/
  enum lfds611_stack_query_type
  {
    LFDS611_STACK_QUERY_ELEMENT_COUNT,
    LFDS611_STACK_QUERY_VALIDATE
  };

  /***** incomplete types *****/
  struct lfds611_stack_state;

  /***** public prototypes *****/
  int lfds611_stack_new( struct lfds611_stack_state **ss, lfds611_atom_t number_elements );
  void lfds611_stack_use( struct lfds611_stack_state *ss );
  void lfds611_stack_delete( struct lfds611_stack_state *ss, void (*user_data_delete_function)(void *user_data, void *user_state), void *user_state );

  void lfds611_stack_clear( struct lfds611_stack_state *ss, void (*user_data_clear_function)(void *user_data, void *user_state), void *user_state );

  int lfds611_stack_push( struct lfds611_stack_state *ss, void *user_data );
  int lfds611_stack_guaranteed_push( struct lfds611_stack_state *ss, void *user_data );
  int lfds611_stack_pop( struct lfds611_stack_state *ss, void **user_data );

  void lfds611_stack_query( struct lfds611_stack_state *ss, enum lfds611_stack_query_type query_type, void *query_input, void *query_output );





  #define __LIBLFDS611_H

#endif

