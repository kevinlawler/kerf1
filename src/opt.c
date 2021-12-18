#include "kerf.h"

void show_options()
{
  O("Usage: kerf [-hqlaF] [-e code] [-x code] [-pP portnumber] [-V vmem] files...\n"
    " -h : display usage information\n"
    " -q : quiet mode; avoid printing the startup banner\n"
    " -l : enable logging\n"
    " -e : evaluate an expression\n"
    " -x : evaluate an expression and print the result\n"
    " -p : specify a binary IPC port\n"
    " -P : specify a textual HTTP port\n"
	  " -R : specify upper-bound on workspace ram in bytes\n"
    " -V : specify upper-bound on virtual memory for disk-backed structures in bytes\n"
    " -a : authenticate server connections\n"
    " -F : IPC/HTTP connections fork child\n"
    "\n");
	kerf_exit(1);
}

int handle_pre_args(int argc, char** argv)
{
  int c;

  //HACK: this lets us get around `getopt`
  //on some systems permuting our argv
  char *fake_argv[4096] = {0};
  int fake_argc = MIN(4096, argc);

  DO(fake_argc, fake_argv[i] = argv[i])

  while(-1!=(c=getopt(fake_argc, fake_argv, ":V:R:"))) //values here should have placeholders in regular args
  {
    SW(c)
    {
      CS('R', I i = strtoll(optarg, 0, 10); The_Real_RAM_Memory_Bound=i;)
      CS('V', I i = strtoll(optarg, 0, 10); The_Mapped_Virtual_Reserved_Bytes=i;)//quantized downstream
      CD: break;//no-op, handled by next
    }
  }

  optind = 1;

  return 0;
}

int handle_args(int argc, char** argv)
{
  int c;

  The_Kerf_Args = new_k(LIST, argc);
  DO(argc, nestset_ri_rd(The_Kerf_Args, i, charvec_from_cstring(argv[i]), false, false))

  rd(ex(".Argv: $1", The_Kerf_Args));

  while(-1!=(c=getopt(argc, argv, ":e:x:p:P:V:R:ahqlF")))//keep placeholders for pre-args here
  {
    SW(c)
    {
      CS('q',  The_Quiet_Flag = true)
      CS('e',  rd(ex(optarg));       kerf_exit(0); )
      CS('x',  rd(show(ex(optarg))); kerf_exit(0); )
      CS('p',  The_Kerf_IPC_Port  = optarg)
      CS('P',  The_Kerf_HTTP_Port = optarg)
      CS('l',  The_Logfile_Flag = true)
      CS('h',  show_options())
      CS('F',  The_Fork_on_Connect_Flag = true)
     CSF('R',)
      CS('V',)//no-op, reserved
      CS('a',  The_Authentication_Required_Flag = true;)
     CSF(':', )
      CS('?',  O("Unknown option: %c\n", optopt); show_options())
    }
  }

  ///////////////////////////////
  //we want this here ((really we want it split)) so that executing scripts with error doesn't cause early termination
  //We're putting this here since we need server launched/log started
  //after arg-opts but before scriptfiles are processed
  //arguably we should split arg processing into two distinct methods instead
  auth_init();
  logfile_init();
  launch_or_relaunch_server();
  if(The_Kerf_IPC_Port || The_Kerf_HTTP_Port) spawn_socket_hup_thread();
  startup_script();
  kerf_ready = true;
  ///////////////////////////////


  bool script_happened = false;

  while(optind < argc)
  {
    S arg = argv[optind++];

    //Loading CSVs/etc from the command-line would be nice, but can't specify fields/date

    if(file_path_is_directory(arg))
    {
      //fprintf(stderr, "file path was dir: %s\n", arg);
      //last split(char('/'),.)
      K eval = open_dir_object(arg);
      work_push(eval);

      K y = charvec_from_cstring(arg);
      if(yn > 0 && '/'==yC[yn-1])
      {
        yC[yn-1]='\0';
        yn--;
      }

      char buf[_POSIX_ARG_MAX] = {0};
      snprintf(buf, sizeof(buf), "%s: $1", yC);

      work_push(y);

      rd(ex(buf, eval));
      
      work_pop_n_rd(2, true);
    }
    else if(file_exists_valid(arg))//we could further split this on file extension...
    {
      system_script_load(arg, true);
      script_happened = true;
      break;
    }
  }

  return 0;
}

int auth_init()
{
  return 0;
  if(!The_Authentication_Required_Flag) return 0;

  //struct passwd* p = getpwnam("nobody");

  //printf("getpwnam: un|%s|, pw|%s|, uid|%u|, gid|%u| gecos|%s| dir|%s| shell|%s| \n", 
  //p->pw_name, p->pw_passwd, p->pw_uid, p->pw_gid, p->pw_gecos, p->pw_dir, p->pw_shell);

  //uid_t nobody_uid = p->pw_uid;
  //int r = seteuid(nobody_uid);

  //if(r)
  //{
  //  perror("Could not setuid");
  //}

  //
  //

  return 0;
}

bool is_incomplete(K x) //here, more natural to phrase in the negative sense
{
  if(!IS_STRING(x)) return false;

  K func = compile(x, NULL); //because `compile` longjumps (ERRORs) currently, this method longjumps now

  if(!func) return false;

  work_push(func);

  bool incomplete_flag = false;

  K0 o;
  if(IS_ERROR(func) && ERROR_PARSE_UNMATCH == AT2(func, kcv("error_code"),o)->i )
  {
    incomplete_flag = true;
  }

  work_pop_rd(true);

  return incomplete_flag;
}

int system_script_load(S cstring, bool warn_on_missing)
{
  if(-1 == access(cstring, F_OK|R_OK))//exists & is readable
  {
    if(warn_on_missing)
    {
      fprintf(stderr,"Warning: script '%s' does not exist.\n", cstring);
    }
    return -1;
  }

  K kstring = charvec_from_cstring(cstring);
  work_push(kstring);
  K result = xload(kstring);
  rd(result);result = NULL;
  work_pop_rd(true);
  return 0;
}

K xload(K x)
{
  if(!IS_STRING(x)) ERROR(ERROR_TYPE);

  K result = NULL;

  bool flat = true;

  K cstring = copy(x);
  cstring = cow_ensure_null_terminated_chars(cstring);
  work_push(cstring);

  K shared = disk_map_file_shared_maybe_flat(kC(cstring), NULL, flat);
  work_pop_rd(true);
  work_push(shared);

  K z = ((V)shared) + PAGE_SIZE_BYTES - sizeof(K0);
  K text = copy(z);

  work_pop_rd(true); work_push(text);

  K chunks = explode(kc('\n'),text);

  work_pop_rd(true); work_push(chunks);

  K build = NULL;

  ENUM(chunks,

    I length = COUNT(v);

    if(0==length)
    {
      continue;
    }

    if(!build)
    {
      build = strong(v);
    }
    else
    {
      work_push(build);
      K4 o;
      K two = implode(kc('\n'), klist2(build, v,o));
      work_pop_rd(true);
      build = two;
    }

    //POTENTIAL_OPTIMIZATION_POINT
    //I think this is O(n^2) on maps split by line
    //eg 300ms for 100 line map
    //1. we could trivially join lines ending in ",SPACE*" without incomplete test
    //2. or maybe just slurp the whole file and go? prob. not ideal for vars

    work_push(build);
    if(is_incomplete(build) && i < _i - 1)
    {
      //^^This can jump via ERROR now... eg ERROR(ERROR_VARIABLE)
      work_pop();
      continue;  
    }
    else
    {
      work_pop();
    }

    work_push(build);
    result = interpret(build);
    work_pop();

    bool error_flag = IS_ERROR(result);

    if(error_flag)
    {
      result = strong(result);//returning error - bubbles up through scripts
      show(result);
    }

    rd(result);
    rd(build); build = NULL;

    if(error_flag)
    {
      break;
    }
    else
    {
      result = NULL;
    }

  )

  work_pop_rd(true);

  if(!result) return Kn();
  return result;
}

