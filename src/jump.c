#include "kerf.h"

__kerfthread sigjmp_buf *soft_jmp_env = NULL;//the nested one
__kerfthread sigjmp_buf *hard_jmp_env = NULL;//ignore nesting

volatile bool canjump = false;

volatile int The_Did_Interrupt_Flag = false;

pid_t kerf_fork()
{
  I rand_offset = rI();

  pid_t p = fork();

  SW(p)
  {
    CS(-1,  fprintf(stderr, "Error: could not create child process\n"))
    CS( 0,  //Child
            The_Process_is_Active_Parent     = false;
            The_Process_is_Child_Flag        = true; 
            The_Process_is_Child_Server_Flag = false;
            The_Process_is_at_Depth         += 1;
            seedPRNG(SEED + rand_offset);
    )
    CD:     //Parent
            The_Process_is_Child_Flag = The_Process_is_Child_Flag;//sic, recursion. don't falsify this flag.
            break;
  }               

  return p;
}

int signal_init()
{

  //SIGPIPE ignore
  struct sigaction iact = {0};
  iact.sa_handler = SIG_IGN;
  iact.sa_flags = SA_RESTART;
  sigemptyset(&iact.sa_mask);
  sigaction(SIGPIPE, &iact, NULL);

  ////SIGWINCH ignore
  //struct sigaction wact = {0};
  //wact.sa_handler = SIG_IGN;
  //wact.sa_flags = SA_RESTART;
  //sigemptyset(&wact.sa_mask);
  //sigaction(SIGWINCH, &wact, NULL);

  //SIGINT
  struct sigaction sact = {0};
  sact.sa_handler = handle_SIGINT;
  sact.sa_flags = SA_RESTART;
  sigemptyset(&sact.sa_mask);
  sigaction(SIGINT, &sact, NULL);

  //SIGUSR1
  struct sigaction uact = {0};
  uact.sa_handler = handle_SIGUSR1;
  uact.sa_flags = SA_RESTART;
  sigemptyset(&uact.sa_mask);
  sigaction(SIGUSR1, &uact, NULL);

  return 0;
}

void handle_SIGUSR1(int sig)
{
  if(INET_SOCKET_HUP_ENDS_EXECUTION)
  {
    if(polled_fd_is_hup_socket(iterating_fd_set_descriptor))
    {
      //We've detected the executing socket closed remotely.
      //We can do a few things here...

      if(SECURITY_ALLOW_LONGJMP_FROM_SIGNAL)
      {
        siglongjmp(*hard_jmp_env, ERROR_REMOTE_HUP);
      }
    }
  }
}

int spawn_socket_hup_thread()
{
  pthread_attr_t attr;

  sigset_t replace;
  sigset_t save;
  sigfillset(&replace);//ignore all ignorable signals in child thread
  int err =0;
  
  err = pthread_sigmask(SIG_SETMASK, &replace, &save);

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&The_Socket_HUP_Thread, &attr, socket_hup_thread, NULL);
  pthread_attr_destroy(&attr);

  err = pthread_sigmask(SIG_SETMASK, &save, NULL);

  //pthread_cancel(The_Socket_HUP_Thread);

  return 0;
}

void* socket_hup_thread(void* arg)
{
  int millisecond = 1000;
  int second = MILLION;
 
  //Do as little as possible here.
  //Technically we just want to notify the main process/thread/server
  //when a socket closes remotely and let it figure
  //out what to do. don't help here if possible.

  //assert: ignoring all signals. 
  //we should do this before spawning thread

  //poll() is a cancellation point as would be sleep()
  while(true)
  {
    int i = 0;

    for(i = 0; i <= fd_channel_max; i++)
    {
      if(polled_fd_is_hup_socket(i))
      {
        //fprintf(stderr, "\nserver: socket closed %d\n", i);

        //this is not a race condition because
        //the other process won't queue signals infinitely
        //and signaling it twice or somesuch is OK
        //see: http://stackoverflow.com/a/5285647/365478 
        kill(getpid(), SIGUSR1);
        usleep(10.0*millisecond);
      }
    }

   int sleep_duration = 10.0*millisecond;

   usleep(sleep_duration);
  }

  return NULL;
}

void handle_SIGINT(int sig)
{

  if(The_Kerf_IPC_Port || The_Kerf_HTTP_Port)
  {
    if(!INET_SERVER_RESPECTS_CTRL_C)
    {
      return;
    }
    else
    {
      bool is_listener =    iterating_fd_set_descriptor == ipc_socket_listener
                         || iterating_fd_set_descriptor == http_socket_listener;
      bool is_socket = detects_as_socket(iterating_fd_set_descriptor);

      if(is_socket && !is_listener)
      {
        //TODO gracefully handle ctrl+c on a socket? by sending err response (if expected) & so on.
        //for now, blow it away 
        fprintf(stderr,"Closing socket %d channel on CTRL+C\n", iterating_fd_set_descriptor);
        close_channel(iterating_fd_set_descriptor);
      }
    }
  }


  //The policy for CTRL+C is that memory leaks
  //are acceptable, but try to avoid them.
  //The policy for other kinds of sigsetjmp
  //handling is that memory leaks must be
  //avoided.

  //fprintf(stderr,"\nGot signal: %d\n", sig);

  The_Did_Interrupt_Flag = true; 
  gby_hack = NULL;
  gby_hack_count = 0;

  //In our error-throwing method we just cause children to come back here via raise()
  //maybe we should refactor this into a method though that gets called in both places?
  if(The_Process_is_Child_Flag)
  {
    fully_recover_from_hard_jmp();
    //fprintf(stderr, "Child %2lld terminating in response to CTRL+C.\n", The_Process_is_Child_Number);
    if(The_Process_Shm_Handle>2)
    {
      close(The_Process_Shm_Handle);
      The_Process_Shm_Handle = 0;
    }
    _exit(CTRL_C_EXIT_CODE);
    return;
  }
  else if(The_Process_is_Active_Parent)
  {
    while(-1 != wait(NULL)) { }
  
    DO(sizeof(The_Process_Shm_Handles)/sizeof(The_Process_Shm_Handles[0]), 
      if(The_Process_Shm_Handles[i]>2)
      {
        close(The_Process_Shm_Handles[i]);
        The_Process_Shm_Handles[i]=0;
      }
    )
  }

  if(!canjump || !kerf_ready)
  {
    //jmp_buf uninitialized
    //(or [re-]initializing function has returned
    // and set canjump=0)
    fprintf(stderr, "Terminating in response to CTRL+C.\n");
    kerf_exit(CTRL_C_EXIT_CODE);
    return;
  }

  if(SECURITY_ALLOW_LONGJMP_FROM_SIGNAL)
  {
    siglongjmp(*hard_jmp_env, ERROR_CTRL_C);
  }
}

S error_string(int val)
{
  //As a matter of policy, it's fine to do the slower
  //version of error throwing where the putative
  //returned structure is allocated first
  //(provided its freed before the error is thrown,
  // or managed in a safe way, as with KVM->workspace)
  //The reason is that errors are not usually part
  //of a normal execution flow, so doing this won't
  //affect any performance that matters.

  SW(val)
  {
    CR(ERROR_DISK,             "Disk error")
    CR(ERROR_FILE,             "File error")
    CR(ERROR_VMEM,             "Virtual memory error")
    CR(ERROR_DEPTH,            "Depth limit exceeded error")
    CR(ERROR_CTRL_C,           "Caught interrupt signal")
    CR(ERROR_REMOTE_HUP,       "Remote socket closed during execution")
    CR(ERROR_SIZE,             "Size error")
    CR(ERROR_SIGN,             "Sign error")
    CR(ERROR_LENGTH,           "Length error")
    CR(ERROR_REFERENCE,        "Reference error")
    CR(ERROR_VARIABLE,         "Undefined token error")
    CR(ERROR_RANK,             "Rank error")
    CR(ERROR_INDEX,            "Index error")
    CR(ERROR_ARITY,            "Arity error")
    CR(ERROR_VALENCE,          "Valence error")
    CR(ERROR_REPEAT,           "Repeat error")
    CR(ERROR_ARGS,             "Argument error")
    CR(ERROR_CONFORMABLE,      "Conformable error")
    CR(ERROR_TYPE,             "Type error")
    CR(ERROR_TIME,             "Time error")
    CR(ERROR_STRING,           "String error")
    CR(ERROR_ARRAY,            "Array error")
    CR(ERROR_MAP,              "Map error")
    CR(ERROR_TABLE,            "Table error")
    CR(ERROR_KEYS,             "Key mismatch error")
    CR(ERROR_COLUMN,           "Column error")
    CR(ERROR_ROW,              "Row error")
    CR(ERROR_RAGGED,           "Ragged table error")
    CR(ERROR_LEX_UNKNOWN,      "Unknown token error")
    CR(ERROR_LEX_INCOMPLETE,   "Incomplete token error")
    CR(ERROR_PARSE_UNKNOWN,    "Unknown parse group error")
    CR(ERROR_PARSE_INCOMPLETE, "Incomplete parse group error")
    CR(ERROR_PARSE_UNMATCH,    "Unmatched parse group error")
    CR(ERROR_PARSE_OVERMATCH,  "Parse group overmatch error")
    CR(ERROR_PARSE_MISMATCH,   "Parse group mismatch error")
    CR(ERROR_PARSE_NESTED_SQL, "Improperly nested SQL parse error")
    CR(ERROR_PARSE_DEPTH,      "Parse group depth error")
    CR(ERROR_PARSE_DERIVED,    "Derived verbs disallowed in this context error")
    CR(ERROR_PARSE_LAMBDA_ARGS,"Function arguments contained disallowed name error")
    CR(ERROR_PARSE_SQL_VALUES, "Malformed SQL INSERT syntax for VALUES error")
    CR(ERROR_PARSE_NUM_VAR,    "Variables cannot start with numbers error")
    CR(ERROR_SUBERROR,         "Inherited error")
    CR(ERROR_NET,              "Network error")
    CR(ERROR_HOST,             "Host error")
    CR(ERROR_DYLIB,            "Dynamic library error")
    CR(ERROR_PARALLEL,         "Parallel execution error")
    CR(ERROR_ATLAS_MAP,        "Atlas passed non-map error")
    CR(ERROR_JSON_KEY,         "Bad JSON key error")
    CR(ERROR_RADIX,            "Invalid radix error")
    CR(ERROR_FORMAT_STRING,    "Invalid format string error")
    CR(ERROR_ZIP,              "Compression error")
    CR(ERROR_FORKED_VERB,      "Forked verb write error")
    CR(ERROR_MISSING,          "Missing feature error")
  }
  
  return "Uncaught error";
}

void reset_lex_parse_globals()
{
  lex_errno = 0;
  lex_errno_location = SENTINEL;

  parse_errno = 0;
  parse_errno_location = SENTINEL;
}

void error_snippet_caret(K text, I position, K *snippet, I *caret)
{
  *caret = SENTINEL;
  *snippet = NULL;

  if(!text) return;
  if(SENTINEL == position) return;
  if(position < 0 || COUNT(text) <= position) return;

  I width = 25;
  I start = MAX(0, position - width);
  I end   = MIN(COUNT(text), position + width);

  I point = position - start;
  K sub = new_k(CHARVEC, 0);

  if(start > 0)
  {
    point += 3;
    ENUM(kcv("..."), sub = cow_add(sub, v))
  }

  DO(end-start, sub = cow_add(sub, kc(kC(text)[start+i])))

  if(end < COUNT(text))
  {
    ENUM(kcv("..."), sub = cow_add(sub, v))
  }

  sub = cow_ensure_null_terminated_chars(sub);

  *caret = point;
  *snippet = sub;
  
  return;
}

I error_position(VM m)
{
  I invalid_position = SENTINEL;

  K current = m->function;//could also be ex method's in-scope "function"

  if(NULL == current) return invalid_position;

  K debug = kN(current, FUNC_DEBUG);
  K text =  kN(current, FUNC_TEXT);
  I step =  m->instruction_pointer_prev;
  I network = -1;
  I flaw    = -1;

  //DO(COUNT(debug), show(ki(ntohll(AT2(debug,ki(i))->i))); O(" ")) O("\n");

  //The reason we do all this validity checking is
  //that we expect some functions will be compiled
  //with the debug/text information stripped.

  bool valid_debug = debug && INTVEC  == debug->t; 
  bool valid_text  = text  && CHARVEC ==  text->t; 
  bool valid_step = false;
  bool valid_flaw  = false;

  if(valid_debug)
  {
    bool nonnegative = (0 <= step);
    bool in_range    = (step < COUNT(debug));
    valid_step = nonnegative && in_range;
  }

  if(valid_text && valid_step)
  {
    K0 o;
    network = AT2(debug, ki(step),o)->i;
    flaw = ntohll(network);

    bool nonnegative = (0 <= flaw);
    bool in_range    = (flaw < COUNT(text));
    valid_flaw = nonnegative && in_range;
  }

  bool valid = valid_debug && valid_text && valid_step && valid_flaw;

  if(!valid)
  {
    return invalid_position;
  }

  return flaw;
}

K new_error_map_base(int val)
{
  //this and other error methods cannot
  //modify the stack unless they
  //want to screw up the process
  K error = new_map();
  SET_ALT_ATTR(error, MAP_ATTR_ERROR);

  I code = val;

  S c_string = error_string(code);
  K string   = charvec_from_cstring(c_string);

  string = cow_ensure_null_terminated_chars(string);

  error = update(error, kcv("is_error"),              ki(true));

  error = update(error, kcv("has_error_code"),        ki(true));
  error = update(error, kcv("has_error_notice"),      ki(true));
  error = update(error, kcv("has_error_instruction"), ki(false));
  error = update(error, kcv("has_error_position"),    ki(false));
  error = update(error, kcv("has_error_snippet"),     ki(false));
  error = update(error, kcv("has_error_caret"),       ki(false));
  error = update(error, kcv("has_error_line"),        ki(false));
  error = update(error, kcv("has_error_line_place"),  ki(false));

  error = update(error, kcv("error_code"),            ki(code));
  error = update(error, kcv("error_notice"),          string); rd(string);

  error = update(error, kcv("error_instruction"),     ki(SENTINEL));
  error = update(error, kcv("error_position"),        ki(SENTINEL));
  error = update(error, kcv("error_snippet"),         ki(SENTINEL));
  error = update(error, kcv("error_caret"),           ki(SENTINEL));

  return error;
}

K update_error_with_position_text(K error, I position, K text)
{
  I caret   = SENTINEL;
  K snippet = NULL;

  if(text && SENTINEL != position)
  {
    error_snippet_caret(text, position, &snippet, &caret);

    if(SENTINEL != caret && snippet)
    {
      error = update(error, kcv("has_error_caret"),   ki(true));
      error = update(error, kcv("has_error_snippet"), ki(true));

      error = update(error, kcv("error_caret"),       ki(caret));
      error = update(error, kcv("error_snippet"),     snippet); rd(snippet);

      //POTENTIAL_EXPANSION_POINT
      //we could add error_line and error_line_place here (Line# and char# in line)
    }
  }

  return error;
}

K new_error_map_execution(int val, VM m)
{
  K error = new_error_map_base(val);

  I instruction = m->instruction_pointer_prev;
  I position = error_position(m);

  error = update(error, kcv("has_error_instruction"), ki(true));
  error = update(error,     kcv("error_instruction"), ki(instruction));

  if(SENTINEL != position)
  {
    error = update(error, kcv("has_error_position"), ki(true));
    error = update(error, kcv("error_position"),     ki(position));
  }

  I caret = SENTINEL;
  K snippet = NULL;
  K text = NULL;

  K function = m->function;

  if(function)
  {
    text = kN(function, FUNC_TEXT);
  }

  error = update_error_with_position_text(error, position, text);

  return error;
}

K new_error_map_lexing_parsing(int val, I position, K text)
{
  K error = new_error_map_base(val);

  error = update_error_with_position_text(error, position, text);

  return error;
}


