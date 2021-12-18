#include "kerf.h"

#pragma mark - Globals

K gby_hack = NULL;
I gby_hack_count = 0;

char** The_C_Args = NULL;
C  The_History_File[255] = {0}; 
I  The_Precision = 7;
K  The_Kerf_Tree = NULL;
K  The_Kerf_Args = NULL;
K  The_Reserved = NULL;
K  The_Allocations = NULL;
C  The_Allocations_Dir[64] = {0};
C  The_Allocations_Path[64] = {0};
C  The_Allocations_Stay_Flag = false;
K  The_Cache = NULL;
C  The_Language_Initialized_Flag = false;
C  The_Past_The_Test_Suite_Flag = false;
C  The_Timing = PUTS_TIMING_ON_BY_DEFAULT;
S  The_Kerf_IPC_Port = NULL;
S  The_Kerf_HTTP_Port = NULL;
C  The_Authentication_Required_Flag = false;
S  The_Editline_Buffer = NULL;
C  The_Quiet_Flag = false;
C  The_Logfile_Flag = false;
K  The_Logfile_Object = NULL;
C  The_Intentional_Midstream_Exit_Flag = false;
K  The_Print_Table_Pages[PRINT_TABLE_MAX_PAGES_TO_SHOW];
I  The_Print_Table_Available_Pages = 0;
I  The_Print_Table_Current_Page  = 0;
I  The_Print_Table_Rows_Per_Page = 8;
I  The_Print_Table_Sampled_Row_Count = 0;
C  The_Clearing_Term_Flag = false;
C  The_Last_Expression_Was_Assignment_Flag = 0;
C  The_Hacky_Reused_A_Mapping_Flag = false;
C  The_Debug_OK_Flag = true;
I  The_Real_RAM_Memory_Bound = -1;
I  The_Outstanding_Memory_Counter = 0;
C  The_Fork_on_Connect_Flag = 0;
I  The_Fork_on_Connect_Socket = -1;
C  The_Passed_License_Check_Flag = false;

//Parallelization
C  The_Process_is_Active_Parent = false;
C  The_Process_is_Child_Flag = false;
C  The_Process_is_Child_Server_Flag = false;
I  The_Process_is_Child_Number = 0;
I  The_Process_Family_Size = 0;
I* The_Process_Created_Registry = NULL;
I* The_Process_Inherited_Registry = NULL;
I  The_Process_is_at_Depth = 0;
I  The_Process_Created_Registry_Bytes = 0;
C  The_Process_Executes_Interleaved = false;
I  The_Process_Segment_Starter = 0;
I  The_Process_Segment_Window = 0;
I  The_Process_Shm_Handle = 0;
I  The_Process_Shm_Handles[256] = {0};
C  The_Process_Shm_Name[256] = {0};

pthread_t The_Socket_HUP_Thread = 0;

C The_Thread_Safe_Flag = false;
//POTENTIAL_FEATURE_POINT
//in DEBUG mode we can use PTHREAD_MUTEX_ERRORCHECK

//POTENTIAL_FEATURE_POINT
//if each thread gets its own pool and stack,
//[you can combine pools at the end], and most
//contention is avoided, except global writes.
//Which you may be able to solve by making copies/read only?
//Or by adding mutexes to them perhaps. Or other.
pthread_mutex_t The_Pool_Mutex;
pthread_mutex_t The_Work_Stack_Mutex;
pthread_mutex_t The_Disk_Mmap_Mutex;

#pragma mark - Main

volatile bool kerf_ready = false;

int kevin_try_stuff()
{ if(!IS_DEFINED(KEVIN)) return 0;
  //Testing goes here

  //The_Fork_on_Connect_Flag = true;

  return 0;
  K x = ex("0 0 1 1");

  DO(26, show(x); x = cow_next_perm(x))

  rd(x);

  //drive_go();

  return 0;
}

bool is_foreground_process()
{ 
#ifdef DEBUG
  //This hack breaks backgrounding `kerf_test &; fg` 
  //This hack fixes `lldb kerf_test`
  //Dunno what the correct fix is (for both), maybe s/t related to
  //tcgetpgrp that identifies that lldb is feeding kerf_test info
  return isatty(STDIN_FILENO);
#endif
  return getpgrp() == tcgetpgrp(STDIN_FILENO); 
}
bool is_background_process() { return !is_foreground_process(); }

int main(int argc, char** argv)
{
  volatile int val;
  sigjmp_buf jmp_env;
  The_C_Args = argv;

  if(!(val = sigsetjmp(jmp_env, true)))
  {
    hard_jmp_env = &jmp_env;
    soft_jmp_env = hard_jmp_env;
    handle_pre_args(argc, argv);
    kerf_init();
    atexit(kerf_atexit);
    handle_args(argc, argv);
    banner();
    expires();
    if(!The_Quiet_Flag)O("\n");
    try_tests();
    The_Past_The_Test_Suite_Flag = true;
    kevin_try_stuff();
  }
  else
  {
    K error = new_error_map_execution(val, KVM);
    show(error);
    rd(error);

    fully_recover_from_hard_jmp();

    if(!kerf_ready)
    {
      fprintf(stderr, "Error in initialization. Kerf will exit as a precaution.\n");
      kerf_exit(1);
    }

  }

  if(EXPIRES && !The_Passed_License_Check_Flag)
  {
    //fprintf(stderr, "License check failed. Exiting.\n");
    //need to change it so that ctrl+c'ing "kerf myscript.kerf" doesn't say license failed
    fprintf(stderr, "Error in initialization. Kerf will exit as a precaution.\n");
    kerf_exit(1);
  }

  if(IS_DEFINED(CONSOLE))
  {
    select_attend();
  }

  return EXIT_SUCCESS;
}

#pragma mark - Init

int try_tests()
{

#ifdef DEBUG
  run_tests();
  random_init();//reset randomness after tests
#endif

  return 0;
}

int namespace_init()
{
  if(TEST_TRACK_ALLOCATIONS)
  {
    snprintf(The_Allocations_Dir, sizeof(The_Allocations_Dir), "_alloc-kerf");
    if(!file_path_is_directory(The_Allocations_Dir)) recursive_delete(The_Allocations_Dir);//not truly necessary
    kerf_mkdir(The_Allocations_Dir, false);
    char *suffix = ".dat";
    snprintf(The_Allocations_Path, sizeof(The_Allocations_Path), "%s/_alloc-kerf-XXXXXX%s", The_Allocations_Dir, suffix);
    int fd = mkstemps(The_Allocations_Path, strlen(suffix));
    unlink(The_Allocations_Path);
    close(fd);

    K map = new_map();
    write_k_to_path(map,The_Allocations_Path);
    rd(map);
    The_Allocations = disk_map_file_shared(The_Allocations_Path);
  }

  The_Kerf_Tree = new_map();

  return 0;
}

int cache_init()
{
  The_Cache = new_map();
  return 0;
}

int library_init()
{
  rd(ex(".Math.TAU: 6.2831853071795864769252"));
  rd(ex(".Math.E:   2.7182818284590452353602"));
  rd(ex(".Math.BILLION: floor 10**9"));
  rd(ex(".Parse.strptime_format: '%d-%b-%y %H:%M:%S'"));
  rd(ex(".Parse.strptime_format2:'%d-%b-%y %H:%M:%S'"));
  rd(ex(".Print.stamp_format: null"));
  rd(ex(".Net.parse_request: {[request] 'Working Kerf HTTP Server.\nEdit response by editing .Net.parse_request\n\n' # request}")); //identity
  rd(ex(".Net.client: $1", ki(STDIN_FILENO)));
  rd(ex(".Net.on_close: {[socket] socket}"));
  K giant = charvec_from_cstring(GIANT_HELP_STRING);
  rd(ex(".Help: kerf_from_json($1)",giant));
  rd(giant);
  return 0;
}

int startup_script()
{
  S startup = "startup.kerf";

  if(file_exists_valid(startup))
  {
    system_script_load(startup, false);
  }

  //kevin 2016.09.01
  //why is this env vars instead of getpwuid as w/ license??
  //maybe change - have not researched. see emails or github issues
  S user_home = getenv("HOME");
  if (user_home)
  {
    char home_path[4096];
    snprintf(home_path, 4096, "%s/.kerf/startup.kerf", user_home);
    if (0 == access(user_home, R_OK)) { startup = home_path; }
  }

  S home = getenv("KERF_HOME");
  if (home)
  {
    char temp[4096];
    snprintf(temp, 4096, "%s/startup.kerf", home);
    startup = temp;
  }

  system_script_load(startup, false);
  return 0;
}

void logfile_init()
{
  if(!The_Logfile_Flag) return;
  S path = "kerf.log";

  //unlink(path);

  The_Logfile_Object = disk_map_file_shared_maybe_flat(path, kcv(""), false);
}

bool chatty_environment()
{
  return isatty(STDIN_FILENO) && isatty(fileno(stdout));
}

void banner()
{
  if(The_Quiet_Flag) return;
  if(!chatty_environment()) return;
  char buf[70] = {0};

  struct tm t = tm_from_date_define();

  char compile_date[256] = {0};
  snprintf((S)&compile_date, sizeof(compile_date), "%04d.%02d.%02d",  1900 + t.tm_year, 1 + t.tm_mon, t.tm_mday);

  S arch   = IS_DEFINED(_LP64)?"64-BIT":"32-BIT";

  S scheme = IS_DEFINED(DEBUG)?"DEBUG":"RELEASE";

  S optimized = IS_DEFINED(__OPTIMIZE_SIZE__)?"-Os":IS_DEFINED(__OPTIMIZE__)?"-O3":"-O0";

  snprintf(buf, sizeof(buf), "{%s, %s, %s, %s, %s}", "SOLO", arch, scheme, optimized, compile_date);

  S version_info_string = buf;
  
  O(kerf, version_info_string);

  if(The_Kerf_IPC_Port)
  {
    O("Kerf IPC Server listening on port: %s\n", The_Kerf_IPC_Port);
  }

  if(The_Kerf_HTTP_Port)
  {
    O("Kerf HTTP Server listening on port: %s\n", The_Kerf_HTTP_Port);
  }

}

int kerf_init()
{
  if(The_Language_Initialized_Flag)
  {
    return 1;
  }

  //POTENTIAL_FEATURE_POINT
  //I think we can make is so that a single thread's
  //intrathread stuff uses a counting mutex instead of
  //a boolean mutex. nicer, prob. doesn't matter
  pthread_mutex_init(&The_Pool_Mutex, NULL);
  pthread_mutex_init(&The_Work_Stack_Mutex, NULL);
  pthread_mutex_init(&The_Disk_Mmap_Mutex, NULL);

  memory_init();    //call first

  signal_init();    //call early

  random_init();    //no dependencies
  hash_init();      //no dependencies
  pool_init();      //no dependencies
  namespace_init(); //dependencies: hash_init
  vm_init();        //dependencies: pool_init
  verb_init();
  lex_init();       
  //bootstrapped//
  cache_init();     //dependencies: lex_init
  library_init();   //dependencies: lex_init

  pool_assertions();

  The_Language_Initialized_Flag = true;

  return 0;
}

void kerf_exit(int exit_code) 
{
  //wrapper to notify debug that we may have left things in unclean state
  The_Intentional_Midstream_Exit_Flag = true;
  exit(exit_code);
}

void kerf_reset(bool wipe_args)
{
  // this should either clobber a pointer which is already NULL
  // or truncate a longer argument list to just the executable:
  if (wipe_args) { The_C_Args[1] = NULL; }

  execvp(The_C_Args[0], The_C_Args);
}

void kerf_atexit()
{
  //You can put cleanup() other places besides here, but then you need to control the exit
  //maybe switch to "kerf_exit(bool cleanup)" or something
  //(reason is ctrl+d inside select causes exit there)
  cleanup();
}

void cleanup()
{
#ifdef DEBUG
  if(!kerf_ready)return;
  //OS would clean up all our allocations and maps,
  //but doing it ourselves exposes bugs faster

  reset_table_printing_feature();

  //Could do 2 fun things here (see unit.c)
  //1. You can track over-frees via rd by testing for [non-]membership in the alloc tracker thing
  //2. You can allow to -DEBUG binaries to run by not crashing and recovering on file lock

  if(TEST_TRACK_ALLOCATIONS && !The_Intentional_Midstream_Exit_Flag)
  {
    if(KVM->workspace->n != 0) //vm_reset cleans this so check beforehand
    {
      The_Debug_OK_Flag = false;
      fprintf(stderr, "[DEBUG] Error: KVM's workspace is not clean: %lld item(s)\n", KVM->workspace->n);
      show(KVM->workspace);
    }
  }

  if(The_Editline_Buffer) free(The_Editline_Buffer);

  DO(1 + fd_channel_max, wipe_channel(i))//safe rd's (not close(fd))

  if(The_Cache)     rd(The_Cache);//free cache before Kerf Tree
  if(The_Kerf_Args) rd(The_Kerf_Args);
  if(The_Kerf_Tree) rd(The_Kerf_Tree);
  if(The_Reserved)  rd(The_Reserved);

  if(The_Holding_Temphandle) close(The_Holding_Temphandle);

  vm_reset(KVM);
  vm_dealloc(KVM);

  if(TEST_TRACK_ALLOCATIONS)
  {
    test_allocations();
  }

  I expected = 0;
  //Truth be told, if you make a ton of allocations, The_Allocations can take up more than one block 
  if(TEST_TRACK_ALLOCATIONS) expected = 1;
  I occupied = count_occupied_virtual_memory_blocks();

  if(expected != occupied)
  {
    The_Debug_OK_Flag = false;
    fprintf(stderr, "[DEBUG] Error: Virtual memory blocks are not clean. %lld in use.\n", occupied);
    //show_virtual_memory_blocks();
  }

  //POTENTIAL_OPTIMIZATION_POINT
  //unmapping 2^46 + 2^45 is slow, on OSX at least. you can drop down to 42 or so
  //TIME(dd(mmap(MAPPED_ORIGIN, The_Mapped_Virtual_Reserved_Bytes, PROT_NONE, 0, temphandle(), 0)))
  //TIME(munmap(MAPPED_ORIGIN, The_Mapped_Virtual_Reserved_Bytes);)

  if(TEST_TRACK_ALLOCATIONS)
  {
    if(!The_Allocations_Stay_Flag)
    {
      //unlink(The_Allocations_Path);
      recursive_delete(The_Allocations_Dir);;
    }
  }

  pthread_mutex_destroy(&The_Pool_Mutex);
  pthread_mutex_destroy(&The_Work_Stack_Mutex);

  if(The_Debug_OK_Flag)
  {
    fprintf(stderr,"[DEBUG] \x1B[0;32mOK: Done OK.\x1B[0m\n");
  }
  else
  {
    fprintf(stderr,"[DEBUG]\x1B[1;37;41mFAILED: Debug failure.\x1B[0m\n");
  }

#endif
}

#pragma mark - API

//we could probablt factor this with json_net_call
K net_kcall(K built)
{
  volatile K result = NULL;

  volatile int val;
  volatile sigjmp_buf *previous_global_env = soft_jmp_env;//"push"
  sigjmp_buf jmp_env;

  if(!(val = sigsetjmp(jmp_env, true)))
  {
    //Warning: you don't want a hard jump here because
    //this is not the root of all future execution branches.
    soft_jmp_env = &jmp_env;

#ifdef DEBUG
    //for our "soft" jump
    if(KVM->local_pointer != 0 ||  KVM->workspace->n != 0)
    {
      fprintf(stderr, "[DEBUG] inet VM is not as clear as it should be for net kcall.\n");
    }
#endif

  }
  else
  {
    K error = new_error_map_execution(val, KVM);
    fully_recover_from_hard_jmp();//can use this recovery for this soft jump
    result = error;
    goto finish;
  }

  if(LIST != built->t || 2 != COUNT(built)) //TODO: first item string, second array
  {
    ERROR(ERROR_ARGS);//hey what're you tryin' to push on us
    //rd(args);
    //args = new_k(LIST, 0);
  }

  //We could remove this after symbolic global vars are implemented
  recursive_func_compile_globals_after_read(built, KVM->KERFDIR);

  K string = kN(built,0);
  K args   = kN(built,1);

  result = kcall(string, args);

finish:
  soft_jmp_env = (sigjmp_buf *)previous_global_env;//"pop"

  return result;
}

//we could probablt factor this with net_kcall
K json_net_call(K json) //K CHARVEC of JSON
{
  volatile K result = NULL;

  volatile int val;
  volatile sigjmp_buf *previous_global_env = soft_jmp_env;//"push"
  sigjmp_buf jmp_env;

  if(!(val = sigsetjmp(jmp_env, true)))
  {
    //Warning: you don't want a hard jump here because
    //this is not the root of all future execution branches.
    soft_jmp_env = &jmp_env;

#ifdef DEBUG
    //for our "soft" jump
    if(KVM->local_pointer != 0 ||  KVM->workspace->n != 0)
    {
      fprintf(stderr, "[DEBUG] inet VM is not as clear as it should be for json net call.\n");
    }
#endif

  }
  else
  {
    K error = new_error_map_execution(val, KVM);
    fully_recover_from_hard_jmp();//can use this recovery for this soft jump
    result = error;
    goto finish;
  }

  K list = new_from_json(json);

  work_push(list);
  if(LIST != list->t || 2 != COUNT(list)) //TODO: first item string, second array
  {
    ERROR(ERROR_ARGS);//hey what're you tryin' to push on us
    //rd(args);
    //args = new_k(LIST, 0);
  }

  K0 o1,o2;
  K string = strong(AT2(list, ki(0), o1));
  K args   = strong(AT2(list, ki(1), o2));

  work_push(string);
  work_push(args);

  result = kcall(string, args);

  work_pop_n_rd(3,true);

finish:

  soft_jmp_env = (sigjmp_buf *)previous_global_env;//"pop"

  work_push(result);

  K result_in_json = new_json_from(result);

  work_pop_rd(true);

  return result_in_json;
}


S json_api_call(S string, S arg_in_json, double *inside_time)
{
  kerf_init();

  K result = NULL;

  volatile int val;
  sigjmp_buf jmp_env;

  if(!(val = sigsetjmp(jmp_env, true)))
  {
    hard_jmp_env = &jmp_env;
    soft_jmp_env = hard_jmp_env;
  }
  else
  {
    K error = new_error_map_execution(val, KVM);
    fully_recover_from_hard_jmp();
    result = error;
    goto finish;
  }

  K json = charvec_from_cstring(arg_in_json);  

  work_push(json);

  K args = new_from_json(json);

  if(!IS_ARRAY(args))
  {
    ERROR(ERROR_ARGS);//hey what're you tryin' to push on us
    //rd(args);
    //args = new_k(LIST, 0);
  }

  work_pop_rd(true);
  work_push(args);

  double before = clock();
  result = call(string, args);
  double after = clock();
  
  if(inside_time) *inside_time = (after - before)/CLOCKS_PER_SEC;

  work_pop_rd(true);

finish:

  work_push(result);

  K result_in_json = new_json_from(result);

  work_pop_rd(true);


  I n = COUNT(result_in_json);
  
  S str = malloc((unsigned long)n+1);
  ENUM(result_in_json, str[i] = rc);
  str[n]=0;

  rd(result_in_json);

  return str;
}

#pragma mark - Execution / Interpretation Methods
///////////////////////////////////////////////////////////////////////////////
//The Kerf "ex" Family of Methods
//
//Preliminary notions:
//  c_string:    "mystring" - NULL terminated char pointer in C
//  k_function:  a compiled Kerf object (a K) of type FUNC == K->t
//  c_vargs:     the "..." in printf(fmt,...), var args in a C signature, of K objects
//  k_list_args: a Kerf object (a K) of type LIST containing 0 or more Kerf object args
//
//Then execution method signatures are:
//  {c_string, k_function} X {c_vargs, k_list_args}
//
//  yielding
//
//  (k_function, k_list_args) -> K apply(), the most fundamental of the family
//  (k_function, c_vargs    ) -> [vaguely the NILAD_EX() family, which has fixed args]
//  (k_string,   k_list_args) -> K kcall()
//  (c_string,   k_list_args) -> K call()
//  (c_string,   c_vargs    ) -> K ex(), the most abstract/C friendly
//
//Note also in the emulator "local_ex" and "...protected_ex" and the APPLY instruction.
//
//Possibly the preliminary "k_function" could be generalized to any noun/object,
//perhaps via modification of apply() and/or local_ex()
//It would be especially smart to do this and then when errors are returned
//from compilation/parsing/lexing, have them execute as nothing and be
//returned off the stack. This would simplify error returns in many places.
//
//The interpret() function is ex() except it instead accepts Kerf CHARVECs 
//(kcharvec) -> K interpret()
//The play() function is similar to interpret except it auto prints output.
//
///////////////////////////////////////////////////////////////////////////////

K interpret(K snippet)
{
  K func = compile(snippet, NULL);

  if(IS_ERROR(func))
  {
    return func;
  }

  work_push(func);

  K result = NILAD_EX(func);

  work_pop_rd(true);

  return result;
}

K _ex(V fake, ...)
{
  va_list arglist;
  K stop = (K)SENTINEL;
  K arg = NULL;

  va_start(arglist, fake);

  S string = va_arg(arglist, S);

  K list = new_k(LIST, 0);

  {
    while(stop != (arg = va_arg(arglist, K)))
    {
      list = cow_add(list, arg);
    }
  }

  va_end(arglist);

  work_push(list);

  K result = call(string, list);

  work_pop_n_rd(1, true);

  return result;
}

K call(S string, K args)
{
  K snippet = charvec_from_cstring(string);
  work_push(snippet);
  K result = kcall(snippet, args);
  work_pop_rd(true);
  return result;
}

K kcall(K snippet, K args)
{
  K func = NULL;
  
  //POTENTIAL_OPTIMIZATION_POINT
  //This function caching strategy isn't bad. However,
  //for known built-ins that we're always going to need,
  //we can exempt them from hash lookups, cache clearings,
  //etc., and store permanent versions.
  K0 o;
  func = LOOKUP_(The_Cache, snippet, o);

  if(!func)
  {
    I argc = COUNT(args);
    bool has_args = (argc > 0);

    C attr = ATTR_NONE;
    //this whole loop is mildly hacky
    //if(has_args) attr = FUNC_ATTR_GLOBALIZE;

    //This is K dollars = ex("'$' join mapright 1 + range $1", ki(argc));
    //but we can't use it because `ex` recursively calls `call` ;)
    K dollars = new_k(LIST, 0);
    DO(argc, K other = string_cast(ki(1+i));  
             K thing = join(kc('$'), other); 
             rd(other);  
             dollars = cow_add(dollars, thing);
             rd(thing);
    )

    work_push(dollars);

    func = compile_with_parent_args(snippet, NULL, NULL, dollars, FUNC_KIND_LAMBDA, attr);
      
    work_pop_rd(true);

    if(!func)
    {
      //TODO this can actually happen until you fix compile
    }

    if(IS_ERROR(func))
    {
      return func;
    }

    bool too_big = false;
    I keys = lenI(kN(The_Cache, KEYS));
    I magic_number = KERF_CACHE_MAX_SIZE;

    if(keys > magic_number) too_big = true;

    //One strategy (here) is to wipe it and start over
    //Another is to freeze and stop adding new items
    //Of course there are others still (keep MRU...creative, easy ways to do this)
    if(too_big)
    {
      rd(The_Cache);
      The_Cache = new_map();
    }

    The_Cache = update(The_Cache, snippet, func);
    rd(func);
  }

  work_push(strong(func));

  K result = apply(func, args);

  //We currently use none of these: we fixed the console to interpret as parentless lambdas
  //  Well, the way you do compile for the console, if you punch out a lambda
  //  like {[a] 1+a}, that merely pushes a constant lamba without applying it;
  //  there's a wrapper method, *another* function, that contains that lambda.
  //  In such methods nouns are not APPLY-ed until there is more than 
  //  one in sequence. And other such considerations.
  //  So, if you take this practice as legitimate (it may not be), there are
  //  at least three ways to go about making it so that you can execute such
  //  functions, including derived-verbs, from C code.
  //  1. You can extract the CONSTANT and execute it directly
  //  2. You can append an APPLY instruction to the end of such code
  //  3. You could create a method that pushed nothing, a NOOP I suppose,
  //     invocable from the command line, and always append it to such calls.
  //     This is kind of wonky since you have to add it everywhere.
  //  
  //  1 is faster than 2, both in execution time and in development time.
  //  2 is slower than 1, takes more code, but is arguably less brittle
  //  K result = NULL;
  //  if(0 && has_args) // ex("1+", ki(1));  we assume 1 FUNCTIONAL NOUN in string at leftmost
  //  {
  //    K constants = kN(func, FUNC_CONSTANTS);
  //    K desired = AT2(constants,ki(0));
  //
  //    result = apply(desired, args);
  //  }
  //  else // ex("1+1");
  //  {
  //    result = apply(func, args);
  //  }

  work_pop_n_rd(1, true);

  return result;
}

K apply(K function, K list_of_args)
{
  K list = list_of_args;

  frame_open(KVM);

  //Backwards Push
  bool reverso = true;

  K0 o0;
  if(reverso)
  {
    DO(COUNT(list), push_ri(KVM, AT2(list, ki(list->n-1-i), o0), true))
  }
  else
  {
    DO(COUNT(list), push_ri(KVM, AT2(list, ki(i),o0), true))
  }
  //It looks like functions expect args on the stack in the opposite 
  //order from verbs (say "-"). That's...probably OK? Could it cause
  //complications with complex adverbs later? However, it does have the
  //benefit of allowing "func(a+a,b+b,c+c)" to execute a, b, c.

  local_ex(KVM, function);

  K0 o;
  K k = peeko(KVM,0,o); 
  scrub(KVM); 
  
  //always heap alloc
  K result = strong(k);
  rd(k); 

  frame_close(KVM);

  return result;
}


