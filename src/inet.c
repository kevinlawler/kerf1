#include "kerf.h"

S KERFPROMPT = "KeRF> ";
S KERFCONTINUEPROMPT = "> ";

MESSAGE1 CONNECTION_POOL[FD_SETSIZE];
fd_set fd_master_data_channels;
int fd_channel_max = -1;
int ipc_socket_listener  = 0;
int http_socket_listener = 0;

int iterating_fd_set_descriptor = 0;

C The_Term_Reading_Inited_Once_Flag = false;

void init_or_reinit_term_reading()
{
  kerf_history_init();

#ifndef __linux //we want this on linux but I think editline has trouble with it there?
  rl_completion_append_character = '\0'; //this stops a space from appearing after tab completion
#endif

  rl_callback_handler_remove();
  if(The_Editline_Buffer)
  {
    add_history(The_Editline_Buffer); //had to add this to cause history to remember errors
    free(The_Editline_Buffer);
  }
  The_Editline_Buffer = NULL;
  rl_callback_handler_install(KERFPROMPT, &line_handler);

  The_Term_Reading_Inited_Once_Flag = true;
}

bool reads_term_input()
{
  //I'm not sure what the minimal necessary combination is between 
  //!The_Process_is_Child_Flag
  //getpgrp() == tcgetpgrp(STDIN_FILENO)  (is_foreground_process)
  //isatty(STDIN_FILENO)
  return !The_Process_is_Child_Flag && is_foreground_process();
}


void select_attend()
{
  fd_set fd_read_data_channels;
  FD_ZERO(&fd_read_data_channels);

  //Initialize or re-initialize (for errors), provided we are in foreground
  if(reads_term_input()) init_or_reinit_term_reading(); 

  for(;;) // main loop  
  {

    //We need an init check inside the for(;;) select loop and not in front
    //because 1. we can't init if the process starts in the background
    //        2. if so, we'll block inside here until stdin becomes ready
    if(reads_term_input() && !The_Term_Reading_Inited_Once_Flag) init_or_reinit_term_reading(); 
    fd_read_data_channels = fd_master_data_channels;//copy them 

    //null timeval -> select blocks
    bool use_timeout = true;
    I timeout_seconds = 1;
    I timeout_microseconds = 0;
    struct timeval timeval_struct = {timeout_seconds, timeout_microseconds};
    struct timeval *timeout = &timeval_struct;

    if(!use_timeout)
    {
      timeout = NULL;
    }

    int selected = select(fd_channel_max + 1, &fd_read_data_channels, 0, 0, timeout);

    if(0 == selected)//timeout
    {
      //Actions to perform when timeout happens go here

      static I last_time = 0; 

      bool flushes_mmap_caches = true;

      if(flushes_mmap_caches)
      {
        I interval_seconds = 5;
        I now_time = NOW_STAMP();

        if((now_time - last_time)/BILLION > interval_seconds)
        {
          disk_flush_mmap_caches_via_registry();
          last_time = now_time;
        }
      }

    }
    else if(-1 == selected)
    {
      if (errno == EINTR)
      {
        errno = 0; //ignore, was interrupted by C-c (or SIGWINCH)
        //fprintf(stderr, "Select caught interrupt signal. (Resized terminal event?)\n");

        //Screen resize breaks editline line via SIGWINCH
        //Tried ignoring SIGWINCH, didn't SEEM to work
        //Another solution might be to check editline for various window resizing/SIGWINCH methods
        //Could be the way we recover too

        //don't seem to appear in libedit or our version of it
        //rl_reset_line_state();//no work
        //rl_on_new_line_with_prompt(); //no work
        //el_reset(); //no work
        //rl_tty_set_default_bindings(0);//no work
        //rl_resize_terminal(); //rl_catch_signals(1); //rl_catch_sigwinch(1); //rl_reset_after_signal(); //rl_set_signals(); //rl_clear_signals();
        //rl_catch_signals = 0; //rl_catch_sigwinch = 0;
        ///////////////

        //promising but don't seem to work correctly
        //DO(100, printf("\b"))
        //rl_already_prompted = 1; //rl_free_line_state(); //rl_cleanup_after_signal(); //rl_on_new_line(); //rl_reset_terminal(NULL);
        //rl_prep_terminal(0); //rl_initialize(); //rl_redisplay(); //rl_forced_update_display(); //kerf_history_init();
        //int a,b; rl_get_screen_size(&a,&b); rl_set_screen_size(a,b);
        //el_resize(0);

        //could be a bug in OSX
        //note that python seems to have something similar buggy
        //happens on linux though so poss. editline bug

        //could be due to the way *history* is stored.
        //see: http://www.linuxquestions.org/questions/linux-newbie-8/command-line-interface-915973/

        //try: clear & reload history
        //try: native libedit non-compatibility commands?
        //try: email libedit people first to see if worth it

        //seems like maybe this is the guy to talk to. see origin
        //https://github.com/troglobit/editline
        //yes. joachim says the bug is fixed in his so
        //we can switch to that (static linux include).
        //could also static include on OSX
        //he says dropping readline compat WON'T fix
        //***
        //I looked at this library & it seems like it doesn't support
        //any of the stuff we need

        continue;  //needed or terminal hangs until second ctrl+c
      }
      else
      {
        perror("select");
        kerf_exit(4);
      }
    }

    for(iterating_fd_set_descriptor = 0; iterating_fd_set_descriptor <= fd_channel_max; iterating_fd_set_descriptor++)
    {
      if (FD_ISSET(iterating_fd_set_descriptor, &fd_read_data_channels))
      {
        if(iterating_fd_set_descriptor==STDIN_FILENO)
        {
          if(reads_term_input()) //merging w/ would cause socket error in later if-case (stdin not socket)
          {
            if(!The_Term_Reading_Inited_Once_Flag) init_or_reinit_term_reading(); 
            rl_callback_read_char();
          }
        }
        else if(    iterating_fd_set_descriptor == ipc_socket_listener
                ||  iterating_fd_set_descriptor == http_socket_listener
                ) // handle new connections 
        {
          handle_new_server_connection();
        }
        else if(CONNECTION_POOL[iterating_fd_set_descriptor].eventing)
        {
          void *kerf_hook = (K)(I)iterating_fd_set_descriptor;//set as handle for now
          int (*p_func)(void *) = CONNECTION_POOL[iterating_fd_set_descriptor].eventing_callback;
          p_func(kerf_hook);
        }
        else if(CONNECTION_POOL[iterating_fd_set_descriptor].http_connection)
        {
          read_http_channel(iterating_fd_set_descriptor);
        }
        else
        {
          read_channel(iterating_fd_set_descriptor, 0);
        }
      }//if
    }//for iterating_fd_set_descriptor<fd_channel_max
  }//for(;;)
}

#pragma mark - 
bool polled_fd_is_hup_socket(int fd)
{
  // use the poll system call to be notified about socket status changes
  struct pollfd pfd;
  pfd.fd = fd;
  pfd.events = POLLHUP|POLLNVAL|POLLERR; //sic. manual says no need, OSX needs at least. POLLNVAL, POLLERR
  pfd.revents = 0;

  int timeout = 0;

  if(poll(&pfd, 1, timeout) > 0)
  {
    if(pfd.revents & POLLHUP)
    {
      return true;
    }
  }

  return false;
}

char detects_as_socket(int fd)
{
  struct stat c;
  if(0 > fstat(fd,&c))
  {
    return false;
  }

  bool socket_handle = S_ISSOCK(c.st_mode);
  //bool pipe_fifo     = S_ISFIFO(c.st_mode);

  return socket_handle;
}

#pragma mark - Eventing

void kerf_api_register_for_eventing(void** p_kerf_hook, int (*p_func)(void *) )
{
  int fd_new_incoming = -1;

#ifdef __linux
  fd_new_incoming = eventfd(0, 0);
#else
  //todo: pipes
  fprintf(stderr,"Error: Eventing currently Linux only. Complain to kerf.concerns@gmail.com");
#endif

  *p_kerf_hook = (void *)(I)fd_new_incoming;

  wipe_channel(fd_new_incoming);
  CONNECTION_POOL[fd_new_incoming].eventing = true;
  CONNECTION_POOL[fd_new_incoming].eventing_callback = p_func;
  FD_SET(fd_new_incoming, &fd_master_data_channels);
  if (fd_new_incoming > fd_channel_max)
  { 
    fd_channel_max = MAX(fd_new_incoming, fd_channel_max);
  }
}

void kerf_api_deregister_for_eventing(void* kerf_hook)
{
  I handle = (I)kerf_hook;
  close_channel(handle);
}

void kerf_notify_event(void *kerf_hook)
{
  I handle = (I)kerf_hook;

#ifdef __linux
  I increment = 1; assert(sizeof(increment)==8);
  I e = write(handle, &increment, sizeof(increment));

  if(e < 0)
  {
    perror("Signal event failed");

  }
#else
  handle = handle;
  //todo: pipes
#endif


}

void kerf_denotify_event(void *kerf_hook)
{
  I handle = (I)kerf_hook;

#ifdef __linux
  I decrement = 1; assert(sizeof(decrement)==8);
  read(handle, &decrement, sizeof(decrement));
#else
  handle = handle;
  //todo: pipes
#endif

}


#pragma mark - 

void *get_in_addr(struct sockaddr *sock_address) // get sockaddr, IPv4 or IPv6
{ 
  if (sock_address->sa_family == AF_INET)
  {
    return &(((struct sockaddr_in*)sock_address)->sin_addr);
  }

  return &(((struct sockaddr_in6*)sock_address)->sin6_addr); 
}


void handle_new_server_connection()
{
  ipc_socket_listener = ipc_socket_listener;   //global
  http_socket_listener = http_socket_listener; //global

  //we forked things like this, but it's fine to do it any other way
  //including passing *socket_listener and such
  bool is_http = false;
  if(     iterating_fd_set_descriptor ==  ipc_socket_listener) is_http = false;
  else if(iterating_fd_set_descriptor == http_socket_listener) is_http = true;

  int *socket_listener = 0;

  if(is_http)
  {
    socket_listener = &http_socket_listener;
  }
  else
  {
    socket_listener = &ipc_socket_listener;
  }

  struct sockaddr_storage remoteaddr; // client address
  socklen_t addrlen = sizeof(remoteaddr);
  int fd_new_incoming = accept(*socket_listener, (struct sockaddr *)&remoteaddr, &addrlen); //newly accepted socket descriptor

  if (fd_new_incoming == -1)
  {
    perror("accept");
    return;
  }

  if(The_Fork_on_Connect_Flag)
  {
    pid_t pid = kerf_fork();

    if(pid) //parent
    {
      close(fd_new_incoming);
      return;
    }
    else    //child
    {
      The_Process_is_Child_Server_Flag = true;
      The_Fork_on_Connect_Socket = fd_new_incoming;
      close_channel(*socket_listener);//candidate for promoting to inside kerf_fork
      *socket_listener = -1;          //candidate for promoting to inside kerf_fork
      rl_callback_handler_remove();   //candidate for promoting to inside kerf_fork
      close_channel(STDIN_FILENO);    //candidate for promoting to inside kerf_fork
      spawn_socket_hup_thread();      //threads won't clone with forks
      //close(STDOUT_FILENO);
      //close(STDERR_FILENO);
    }
  }

  wipe_channel(fd_new_incoming); //new conn needs this since connections can die without notification (right?)

  if(is_http)
  {
    CONNECTION_POOL[fd_new_incoming].http_connection = true;
  }

  FD_SET(fd_new_incoming, &fd_master_data_channels); // add to master set 
  if (fd_new_incoming > fd_channel_max)
  { 
    fd_channel_max = MAX(fd_new_incoming, fd_channel_max);
  }
 
  int yes=1;
  //setsockopt(fd_new_incoming, IPPROTO_TCP, TCP_NODELAY, (void *)&yes, sizeof(I)); //disable nagle
  
  char remote_ip[INET6_ADDRSTRLEN] = {0};
  const char* remote_address = inet_ntop(remoteaddr.ss_family, get_in_addr((struct sockaddr*)&remoteaddr), remote_ip, INET6_ADDRSTRLEN);

  if(!is_http)
  {
    fprintf(stderr, "\nserver: new connection from %s on %s socket %d\n", remote_address, is_http?"http":"ipc", fd_new_incoming);
  }

  return;
}

void wipe_channel(int i) //safe to call >1 time
{
  K nested = CONNECTION_POOL[i].nested_k;

  if(nested)
  {
    rd(nested);
  }

  memset(&CONNECTION_POOL[i],0,sizeof(CONNECTION_POOL[0]));

  return;
}

void close_channel(int i)
{
  wipe_channel(i);

  int result = close(i); 
  //shutdown(fd_socket, SHUT_WR);

  //if(-1 == result)
  //{
  //  perror("error closing inet channel handle");
  //}

  if(i==fd_channel_max) fd_channel_max--;
  FD_CLR(i, &fd_master_data_channels); 


  if(i == The_Fork_on_Connect_Socket)
  {
    //The only socket we're supposed to handle has been closed,
    //so we can exit now.
    _exit(0);
  }

  return;
}

bool msg_header_check(MESSAGE0 m0) 
{
  return ((m0.response_type>=MESSAGE_RESPONSE_MAX) || (m0.response_type<0) ||
	  (m0.zip_type>=MESSAGE_ZIP_MAX) || (m0.zip_type<0) ||
	  (m0.execution_type>=MESSAGE_EXECUTION_MAX) || (m0.execution_type<0) ||
	  (m0.display_type>=MESSAGE_DISPLAY_MAX) || (m0.display_type<0) ||
	  (!is_power_of_2(ntohll(m0.nested_k_wire_size))) || (0>(I)ntohll(m0.nested_k_wire_size)));
}

void msg_header_dump(MESSAGE0 m0) {
  fprintf(stderr,"ws=%lld, p2=%d\n",ntohll(m0.nested_k_wire_size),is_power_of_2(ntohll(m0.nested_k_wire_size)));
}

K read_http_channel(int channel_fd)
{
  int i = channel_fd;

  I c = CONNECTION_POOL[i].read_so_far;

  bool first_time_through = (0 == c);

  I m = 0; //not sizeof(MESSAGE0);
  K z = NULL;

  if(INET_SOCKET_HUP_ENDS_EXECUTION)
  {
    //see comments at read_channel [ipc]
    if(polled_fd_is_hup_socket(i))
    {
      goto cleanup;
    }
  }

  if(first_time_through)
  {
    I starting_buffer_size = 4096;
    K y = new_k(CHARVEC, starting_buffer_size);
    yn = 0;

    OFF_ATTR(y,ATTR_SORTED);

    zero_unused_space(y);//recv needs or crash

    CONNECTION_POOL[i].nested_k = y;
  }
  else
  {
    K y = CONNECTION_POOL[i].nested_k;

    if(max_possible_count_K(y) == yn)
    {
      bool zero_out = true;//recv needs or crash
      y = expand_zero(y, ym+1, zero_out);
      OFF_ATTR(y,ATTR_SORTED);
      CONNECTION_POOL[i].nested_k = y;
    }
  }

  //you could factor this with above, but you have to be *very* careful
  K y = CONNECTION_POOL[i].nested_k;
  I remain = max_possible_count_K(y) - yn;
  char *buffer = yC+yn;

  I nbytes = recv(i, buffer, remain, 0);

  if(nbytes <= 0)
  {
    if (nbytes == 0)
    {
      //fprintf(stderr, "server: client socket %d hung up\n", i);
      close_channel(i);
      //
      rd(ex(".Net.on_close $1", ki(i)));
    }
    else
    {
      perror("http recv");
    }

    goto cleanup;
  }

  CONNECTION_POOL[i].read_so_far += nbytes; 
  yn += nbytes;

  bool request_complete = false;

  //POTENTIAL_FEATURE_POINT
  //If you want to allow persistent connections,
  //then don't close the socket here. You'll also
  //need to suss out the first completed request (since you assume repeats)
  //(probably the first location of "\r\n\r\n"), pull that
  //out for processing, then shift over to the left everything else that remains

  //GET requests end in "\r\n\r\n" under most conditions (?)
  // 1. `Transfer-Encoding: chunked` also terminates this way
  // 2. For non-GET requests, there's `Content-Length`
  // 3. Closed socket (EOF / 0==recv) won't do that, but handled above.
  // 4. More research would be needed w/ the HTTP request documentation.
  I n = yn;
  S catch = "\r\n\r\n";
  I k = strlen(catch);
  if(n >= k && !strncmp(yC+yn-k, catch, k))
  {
    request_complete = true;
  }

  //for now, we assume only 1 request at a time, then close
  if(request_complete)
  {

    //eval
    K x = ex(".Net.parse_request $1", y);

    work_push(x);

    if(IS_CHARVEC(x))
    {
      sendall(i, xC, xn);
    }

    work_pop_rd(true);

    //reset length of y
    yn = 0;
  
    CONNECTION_POOL[i].read_so_far = 0;

    close_channel(i);
  }

  //er(y) show(y); show(reverse(y)); rd(ex("display $1",y)); rd(ex("display reverse $1",y));
  //fprintf(stderr, "|%*s|\n",(int)nbytes, buffer);

  return z;
cleanup:
  close_channel(i);
  return (K)-1;
}

K read_channel(int channel_fd, int reading_type)
{
  int i = channel_fd;

  I c = CONNECTION_POOL[i].read_so_far;
  I m = sizeof(MESSAGE0);
  K z = 0;
  
  S buffer = NULL;
  I byte_appetite = 0;

  bool ongoing_header = (c < m);

  if(ongoing_header)
  {
    buffer = c + (S)&CONNECTION_POOL[i].m0;
    byte_appetite = m-c;
  }
  else //header finished
  {
    buffer = (c-m) + (S)CONNECTION_POOL[i].nested_k;
    byte_appetite = CONNECTION_POOL[i].m0.nested_k_wire_size - (c-m);
  }

  //POTENTIAL_OPTIMIZATION_POINT
  //We can do plenty of fun tricks like
  //1. mmapping a big buffer on disk and writing there 
  //2. interleave zipping the data and unzipping PAGE_SIZE==byte_appetite at a time
  //3. Here's how to write a file: save as unique tmp filename, return name
  //4. [For Sender] Hashtable of addresses/ports -> handles. Auto manage fd sockets.
  //   test if alive. if so reuse. otherwise cleanup + create

  if(INET_SOCKET_HUP_ENDS_EXECUTION)
  {
    //technically, this is ending recv bytes before we ever can execute
    //
    //2016.06.10 kevin
    //so I guess the reasoning here is that we don't want to siphon
    //all the recv'able bytes out before deciding to abandon
    //a socket that was remotely closed. i guess that's ok, otoh,
    //we are polling the socket first every time (speed?). so it might be
    //better to read the bytes out, which can't be that bad (I mean, 
    //we're going to close this socket anyway. I *guess* it could be bad if
    //reading junk blocks the main thread? Dunno). Anyway, the alternative
    //would be to combine this with the "nbytes == 0" check below.

    if(polled_fd_is_hup_socket(i))
    {
      goto cleanup;
    }
  }

  I nbytes = recv(i, buffer, byte_appetite, 0); //or MSG_PEEK

  if(nbytes <= 0)
  {
    if (nbytes == 0)
    {
      //fprintf(stderr, "server: client socket %d hung up\n", i);
      close_channel(i);
      //
      rd(ex(".Net.on_close $1", ki(i)));
    }
    else
    {
      perror("ipc recv");
    }

    goto cleanup;
  }


  CONNECTION_POOL[i].read_so_far += nbytes; 

  //DO(nbytes, O("b%lld : %02x - %c\n",i,(UC)buffer[i], buffer[i]) ) O("\n");

  I so_far = CONNECTION_POOL[i].read_so_far;

  if(so_far < m)
  {
    //no op.
    //if-structure necessary since `nested_k_wire_size` uninitialized at the beginning
    //which would otherwise be read as initialized later
  }
  else if(so_far == m)
  {

    //fill message header struct data + k data
    //We've read enough bytes to fill our struct m0 with transmission data
    if(msg_header_check(CONNECTION_POOL[i].m0)) // header sanity check
    { 
      fprintf(stderr,"malformed header on wire\n"); 
      // dumping the header to stderr may be desirable
      I tmp = ntohll(CONNECTION_POOL[i].m0.nested_k_wire_size);
      /* bool tmp1 = is_power_of_2(tmp); */
      /* if(!tmp1){ */
      msg_header_dump(CONNECTION_POOL[i].m0);
      //	}
      goto cleanup; 
    }

    //so that we get the right sizes, etc, in the MESSAGE0, rearrange bytes based on endianness indicator CONNECTION_POOL[i].m0.endiannness
    CONNECTION_POOL[i].m0.nested_k_wire_size = ntohll(CONNECTION_POOL[i].m0.nested_k_wire_size);

    I size = CONNECTION_POOL[i].m0.nested_k_wire_size;
    
    if((size > POOL_SANE_MEMORY_BOUND)|(size<0)) goto cleanup; //protect against too big and retards telnetting to ports?

    K y = pool_alloc_struct(size);

    C m_to_save = ym;

    CONNECTION_POOL[i].nested_k = y;
    CONNECTION_POOL[i].m_saver_memo = m_to_save;
    //This is crucial. If you don't initialize (zero) the [mmapped] buffer,
    //even for reads, then it can cause a fault as memory is paged in 
    //which will affect `recv` and cause it to return with "Bad Address"
    //which causes a crash basically.
    memset(y, 0, size);
    ym = m_to_save;

  }
  else if(so_far == m + CONNECTION_POOL[i].m0.nested_k_wire_size)
  {
    //the k being read from the handle is completed. perform modified execution, potentially respond

    //TODO: (here or in data_from_bytes?) rearrange bytes based on little-endianness indicator CONNECTION_POOL[i].m0.a
    MESSAGE0 *p = &CONNECTION_POOL[i].m0;

    C zip_type       = p->zip_type;       //dissappears after wipe_channel
    C response_type  = p->response_type;  //dissappears after wipe_channel
    C execution_type = p->execution_type; //dissappears after wipe_channel
    C display_type   = p->display_type;   //dissappears after wipe_channel

    //gotcha whether `built` lives or not. wipe_channel would clear associated object
    volatile K built = CONNECTION_POOL[i].nested_k;
    built->m = CONNECTION_POOL[i].m_saver_memo;//restore m
    built->r = 1; //set reference count in case sender sent invalid
    CONNECTION_POOL[i].nested_k = NULL;//save build

    if(The_Logfile_Flag)
    {
      //Note: this is probably the right general strategy for logging.
      //      It works with all network message types.
      //      It is not currently the most disk efficient: to fix this
      //      implement INET message zipping - that should make it fine
      //      Well, I guess you still lose a factor of 2 (post compression) if you rely on m
      //      That should be OK. 
      //      Restricting to JSON-esque strings ignores general binary format...
      //POTENTIAL_OPTIMIZATION_POINT
      //1. multithreading: this write can be multithreaded
      //2. we know the amount we're writing in advance:  I adding = sizeof(M0) + POW2(m);
      //3. cow_add(, kc()) is probably not the most efficient: consider eg, memcpy
      //4. implement INET message zipping

      S s;
  
      s = (S)p;
      DO(sizeof(MESSAGE0), The_Logfile_Object = cow_add(The_Logfile_Object, kc(*(s++))))
      s = (S)built;
      DO(POW2(built->m), C c = *(s++); The_Logfile_Object = cow_add(The_Logfile_Object, kc(c))) 
    }

    //any data decoding on `built` goes here
    ////////////////
    wipe_channel(i); //clean it for next message
    ////////////////

    K z = NULL; 

    //POTENTIAL_OPTIMIZATION_POINT: 
    //Option 1 static var, rd-ex-update only on change (ehh..)
    //Option 2 reserved word, avoids slowdown entirely (happens iff client invokes)
    //Option 3 keep pointer to global value in tree, update dereferenced pointer
    //         (minor consideration if someone overwrites .Net)
    //Option 4 we can pre-compile this function
    //Option 5 implement cacher for ex()
    rd(ex(".Net.client: $1", ki(channel_fd)));
    //////////////


    SW(zip_type)
    {
      CS(MESSAGE_ZIP_COMPRESSED_PAYLOAD, K de = new_unzipped_charvec_temp(built);
                                         rd(built);
                                         built = de; 
      ) 
    }
 

    SW(execution_type)
    {
      CS(MESSAGE_EXECUTION_NONE,)
      CS(MESSAGE_EXECUTION_STRING_EVAL, if(IS_STRING(built))
                                        {
                                          z = interpret(built);
                                        }
      )
      CS(MESSAGE_EXECUTION_STRING_CALL, 
                                        //don't work_push(built) while net_kcall still hard jumps
                                        z = net_kcall(built);
      )
      CS(MESSAGE_EXECUTION_APPLY, )
      CS(MESSAGE_EXECUTION_JSON,        
                                        if(IS_STRING(built))
                                        {
                                          //don't work_push(built) while json_net_call still hard jumps
                                          z = json_net_call(built);
                                        }
      )

    }

    SW(response_type)
    {
      CS(MESSAGE_RESPONSE_NO_ACK,)
      CS(MESSAGE_RESPONSE_FULL, 
                                if(!z) z = Kn();
                                bool zip = false;
                                I eval = write_k_to_handle_options(z, i, zip, 
                                     MESSAGE_EXECUTION_NONE,
                                     MESSAGE_RESPONSE_NO_ACK,
                                     MESSAGE_DISPLAY_NONE
                                    );
 
      )
      CS(MESSAGE_RESPONSE_ABBREVIATED,)
      CS(MESSAGE_RESPONSE_TMPFILENAME,)
    }

    SW(display_type)
    {
      CS(MESSAGE_DISPLAY_NONE,)
      CS(MESSAGE_DISPLAY_SHOW_MESSAGE, show(built))
      CS(MESSAGE_DISPLAY_SHOW_EX,                  if(z)show(z))
      CS(MESSAGE_DISPLAY_SHOW_MSG_EX,  show(built);if(z)show(z))
    }

    if(reading_type)//er..this logic works but may not be the best such
    {
      if(!z) return built;
      rd(built);
      return z;
    }

    rd(built);
    if(z) rd(z);
    z = (K)0;
  }

  return z;

cleanup:
  close_channel(i);
  return (K)-1;
}

K open_socket(K x, K y)
{
  if(!IS_STRING(x) || !IS_STRING(y)) ERROR(ERROR_TYPE);

  K host = copy(x);
  K port = copy(y);

  host = cow_ensure_null_terminated_chars(host);
  port = cow_ensure_null_terminated_chars(port);

  work_push(host);
  work_push(port);

  //It's likely better to check for client==server (a bad thing)
  //in the server accept thing (if possible), instead of
  //in the client outgoing thing, but this was a lot faster
  //This probably also misses the case where you're sending it to itself
  //via plain old regular ip
  bool same_host = !KC(host, kcv("localhost")) || !KC(host,kcv("127.0.0.1"));
  bool same_port = The_Kerf_IPC_Port && !strcmp(The_Kerf_IPC_Port, kC(port));
  bool connecting_to_self = same_host && same_port;

  if(connecting_to_self)
  {
    fprintf(stderr, "Server and client can be on same host but must be separate instances.\n");
    ERROR(ERROR_HOST);
  }

  int fd_socket = -1;

  {
    S errstr;
  
    struct addrinfo hints, *servinfo, *p;
    int rv;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
  
    if ((rv = getaddrinfo(kC(host), kC(port), &hints, &servinfo)) != 0)
    {
      fprintf(stderr, "conn: %s\n", gai_strerror(rv));
      ERROR(ERROR_NET);
    }

    // loop through all the results and connect to the first we can
    for(p = servinfo; p != NULL; p = p->ai_next)
    {
      if (-1 == (fd_socket = socket(p->ai_family, p->ai_socktype, p->ai_protocol)))
      {
        perror("client: socket");
        continue; 
      }
      else if (-1 == connect(fd_socket, p->ai_addr, p->ai_addrlen))
      { 
        errstr=strerror(errno);
        //perror("client: connect");
        close(fd_socket);
        continue;
      } 
      else if(-1 == fcntl(fd_socket, F_SETFD, FD_CLOEXEC)) //this is so reset() drops existing connections
      {
        perror("Client socket could not set close-on-exec");
      }
      else
      {
        //success
        break;
      }
    }
  
    if (p == NULL)
    {
      fprintf(stderr, "conn: failed to connect (%s)\n", errstr);
      freeaddrinfo(servinfo); 
      ERROR(ERROR_NET);
    }
  
    //char s[INET6_ADDRSTRLEN];
    //inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr), s, sizeof s);
    //O("client: connecting to %s\n", s);
    I yes=1;

    //setsockopt(fd_socket, SOL_SOCKET, SO_KEEPALIVE, (void *)&yes, sizeof(I));

    //POTENTIAL_OPTIMIZATION_POINT: nagle dependent on sync/async?
    //on localhost nagle-enabled reduces xfer time of single vector of 2^20 ints from 20s to 1s 
    //BUT nagled-enabled increases time for lots of small messages
    //setsockopt(fd_socket, IPPROTO_TCP, TCP_NODELAY, &yes, sizeof(I));//disable nagle

#if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__)  || defined(__NetBSD__)
    setsockopt(fd_socket, SOL_SOCKET, SO_NOSIGPIPE, (void *)&yes, sizeof(I));
#endif

    struct linger timewait;
    timewait.l_onoff = 1;
    timewait.l_linger = SHRT_MAX;
    setsockopt(fd_socket, SOL_SOCKET, SO_LINGER, &timewait, sizeof(timewait));

    freeaddrinfo(servinfo);

    wipe_channel(fd_socket);
  }

  work_pop_n_rd(2, true);

  return Ki(fd_socket);
}

K close_socket(K x)
{
  if(xt != INT) ERROR(ERROR_TYPE);

  close(xi);

  return Kn();
}

void warn_bad_socket()
{
  fprintf(stderr, "Warning: socket appears invalid (non-integer). Did you set a socket?\n");
}

K send_async_json(K x, K y, K z)
{
  if(xt != INT)
  {
    warn_bad_socket();
    ERROR(ERROR_TYPE);
  }

  if(!z) z = kk;

  if(!IS_ARRAY(z)) ERROR(ERROR_TYPE);

  K4 o;
  K json = new_json_from(klist2(y,z,o));

  work_push(json);

  char zip_type = MESSAGE_ZIP_NONE;
  if(use_compressed_payload(json))
  {
    zip_type = MESSAGE_ZIP_COMPRESSED_PAYLOAD;
  } 

  I eval = 0;
  eval = write_k_to_handle_options(json, xi, zip_type, 
                                   MESSAGE_EXECUTION_JSON,
                                   MESSAGE_RESPONSE_NO_ACK,
                                   MESSAGE_DISPLAY_NONE);

  work_pop_rd(true);

  return Ki(1);
}

K send_sync_json(K x, K y, K z)
{
  if(xt != INT)
  {
    warn_bad_socket();
    ERROR(ERROR_TYPE);
  }

  if(!z) z = kk;

  if(!IS_ARRAY(z)) ERROR(ERROR_TYPE);

  K4 o;
  K json = new_json_from(klist2(y,z, o));

  work_push(json);

  char zip_type = MESSAGE_ZIP_NONE;
  if(use_compressed_payload(json))
  {
    zip_type = MESSAGE_ZIP_COMPRESSED_PAYLOAD;
  } 

  I eval = 0;
  eval = write_k_to_handle_options(json, xi, zip_type, 
                                   MESSAGE_EXECUTION_JSON,
                                   MESSAGE_RESPONSE_FULL,
                                   MESSAGE_DISPLAY_NONE);

  work_pop_rd(true);

  K w = NULL;
  //Synchronous response:
  while(!(w=read_channel(xi,1)));

  if(!w || w==(K)-1)
  {
    ERROR(ERROR_NET);
  }

  work_push(w);
  K a = new_from_json(w);
  work_pop_rd(true);

  return a;
}

bool use_compressed_payload(K x)
{
  if(ZIP==xt) return false;

  I wire = wire_size_K(x);

  //lower bound so we don't have to do wkdm on sub-page sizes
  //upper bound b/c you don't want to compress in memory, stream those
  //arbitrary rules
  return (wire > 4096) && (wire < POW2(32));
}

K send_async(K x, K y, K z)
{
  if(xt != INT)
  {
    warn_bad_socket();
    ERROR(ERROR_TYPE);
  }

  if(!z) z = kk;

  if(!IS_ARRAY(z))
  {
    fprintf(stderr, "Note: If present, final item must be an array of arguments. For instance, send_async(socket, 'v:$1', [100])\n");
    ERROR(ERROR_ARGS);
  }

  K4 o;
  K payload = klist2(y,z, o);

  char zip_type = MESSAGE_ZIP_NONE;
  if(use_compressed_payload(payload))
  {
    zip_type = MESSAGE_ZIP_COMPRESSED_PAYLOAD;
  } 
  
  I eval = 0;
  eval = write_k_to_handle_options(payload, xi, zip_type, 
                                   MESSAGE_EXECUTION_STRING_CALL,
                                   MESSAGE_RESPONSE_NO_ACK,
                                   MESSAGE_DISPLAY_NONE);

  return Ki(1);
}

K send_sync(K x, K y, K z)
{

  if(xt != INT)
  {
    warn_bad_socket();
    ERROR(ERROR_TYPE);
  }

  if(!z) z = kk;

  if(!IS_ARRAY(z))
  {
    fprintf(stderr, "Note: If present, final item must be an array of arguments. For instance, send_sync(socket, 'v:$1+$2', [400,500])\n");
    ERROR(ERROR_ARGS);
  }

  K4 o;
  K payload = klist2(y,z, o);

  char zip_type = MESSAGE_ZIP_NONE;
  if(use_compressed_payload(payload))
  {
    zip_type = MESSAGE_ZIP_COMPRESSED_PAYLOAD;
  } 
 

  I eval = 0;
  eval = write_k_to_handle_options(payload, xi, zip_type, 
                                   MESSAGE_EXECUTION_STRING_CALL,
                                   MESSAGE_RESPONSE_FULL,
                                   MESSAGE_DISPLAY_NONE);

  K w = NULL;
  //Synchronous response:
  while(!(w=read_channel(xi,1)));

  if(!w || w==(K)-1)
  {
    ERROR(ERROR_NET);
  }

  //kevin 2016.05.02
  //this may need to go a few lines up and reference z or build
  //but no time atm for that
  //We could remove this after symbolic global vars are implemented
  if(wt==FUNC)
  {
    //We are cheating here, because really this needs to be recursive anyway, to
    //capture subfunctions and so on. but we are planning to rip this out
    //anyway along with recursive_func_compile_globals_after_read below
    K t = w;
    w = copy(w);
    rd(t);
  }
  recursive_func_compile_globals_after_read(w, KVM->KERFDIR);

  return w;
}


void bind_socket(S port, int *socket_listener )
{
  if(!port) return;

  int getaddr_result;
  struct addrinfo hints, *address_info, *p;

  // get us a socket and bind it 
  memset(&hints, 0, sizeof hints);
  hints.ai_family   = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM; //TCP not UDP
  hints.ai_flags    = AI_PASSIVE;

  if((getaddr_result = getaddrinfo(NULL, port, &hints, &address_info)) != 0)
  {
    fprintf(stderr, "Aborting. Kerf Server error: %s\n", gai_strerror(getaddr_result));
    kerf_exit(1);
  }

  for(p = address_info; p != NULL; p = p->ai_next)
  {
    *socket_listener = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (*socket_listener < 0)
    {
      continue;
    }

    int yes=1;
    // lose the "address already in use" error message 
    setsockopt(*socket_listener, SOL_SOCKET, SO_REUSEADDR, (void *)&yes, sizeof(I));
    setsockopt(*socket_listener, SOL_SOCKET, SO_KEEPALIVE, (void *)&yes, sizeof(I));
#if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__)
    setsockopt(*socket_listener, SOL_SOCKET, SO_NOSIGPIPE, (void *)&yes, sizeof(I));
#endif
    if (bind(*socket_listener, p->ai_addr, p->ai_addrlen) < 0)
    {
      close(*socket_listener);
      continue; 
    }
    break;
  }

  if (p == NULL) 
  {
    fprintf(stderr, "Aborting. Kerf Server: failed to bind. Is the port already in-use?\n");
    kerf_exit(2); 
  }

  freeaddrinfo(address_info);

  if (listen(*socket_listener, 10) == -1)
  {
    perror("listen");
    kerf_exit(3);
  }

  FD_SET(*socket_listener, &fd_master_data_channels);
  fd_channel_max = MAX(fd_channel_max, *socket_listener);

  return;
}

void launch_or_relaunch_server()
{
  fd_channel_max = STDIN_FILENO;
  ipc_socket_listener  = 0;
  http_socket_listener = 0;

  FD_ZERO(&fd_master_data_channels); // clear the master and temp sets
  FD_SET(STDIN_FILENO, &fd_master_data_channels);

  if(The_Kerf_IPC_Port)  bind_socket(The_Kerf_IPC_Port,  &ipc_socket_listener);
  if(The_Kerf_HTTP_Port) bind_socket(The_Kerf_HTTP_Port, &http_socket_listener);
}

void special_case_commands(S *s)
{
  if(!strcmp(*s,"exit" ))  *s = "exit(0)";
  if(!strcmp(*s,"quit" ))  *s = "exit(0)";
  if(!strcmp(*s,"\\\\" ))  *s = "exit(0)";
  if(!strcmp(*s,"reset"))  *s = "reset()";
  if(!strcmp(*s,"help" ))  *s = "help('')";
  if(!strcmp(*s,"clear" )) 
  {
    The_Clearing_Term_Flag = true;
    *s = "system('clear')";
  }
}

void line_handler(char* line)
{
  if(!line) //ctrl+d
  {
    O("\n");

    if(INET_SERVER_LIVES_WHEN_STDIN_CLOSED)
    {
      FD_CLR(STDIN_FILENO, &fd_master_data_channels);
      close(STDIN_FILENO);
      return;
    }
    else
    {
      //This will cause `kerf -p 1234 < /dev/null` to exit on launch
      kerf_exit(0);
    }
  }

  if(!The_Editline_Buffer)
  {
    The_Editline_Buffer = line;
  }
  else
  {
    //implode strings
    I len_i = strlen(The_Editline_Buffer); 
    I len_j = strlen(line);
    I ck = len_i + len_j + 1;
    
    char* combo = malloc(ck+1);
    memcpy(combo + 0,     The_Editline_Buffer,  len_i); 
    combo[len_i] = '\n';
    memcpy(combo + len_i + 1, line, len_j); 
    combo[ck] = '\0';
    
    free(The_Editline_Buffer);
    The_Editline_Buffer = combo;
  }

  S golive = The_Editline_Buffer;
  special_case_commands(&golive);

  {//measure input nested incompleteness, hold off on execution until complete OR irrevocably broken
    K snippet = charvec_from_cstring(golive);
    work_push(snippet);

    bool incomplete_flag = is_incomplete(snippet);

    work_pop_n_rd(1, true);

    if(incomplete_flag)
    {
      goto incomplete;
    }
  }
  
  I length = strlen(golive);

  bool empty_line = (0 == length);
  bool commented = (length >= 2) && '/'==golive[0] && '/'==golive[1] ;

  bool skipping = (empty_line || commented);

  if(skipping) //skip empty newlines
  { 
    try_table_printing_feature();
    //O("\n");
  }
  else
  {
    add_history(The_Editline_Buffer);
    //TODO: use append_history or similar so that multiple 
    //      open kerf instances play nice together with the history file?
    //      you can either mass overwrite file (current) or intermingle in the file (maybe better?)
    write_history(The_History_File);
    rd(ex(".Net.client: $1", ki(fileno(stdin))));
    reset_table_printing_feature();
    _play(golive, "  ", The_Timing);
    if(!The_Clearing_Term_Flag) O("\n");
    The_Clearing_Term_Flag = false;

  }

  free(The_Editline_Buffer);
  The_Editline_Buffer = NULL;

  rl_callback_handler_remove();
  rl_callback_handler_install(KERFPROMPT, &line_handler);
  return;

incomplete:
  rl_callback_handler_remove();
  rl_callback_handler_install(KERFCONTINUEPROMPT, &line_handler);
  return;
}


