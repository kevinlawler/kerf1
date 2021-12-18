#include <arpa/inet.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <netdb.h>
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
#include <time.h>
#include <unistd.h>

#if defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__)
#include <netinet/in.h>
#endif

#ifdef __MACH__ //Apple OSX
#include <mach/clock.h>
#include <mach/mach.h>
#include <netinet/tcp.h>
#include <sys/sysctl.h>
#endif

#ifndef ntohll
#define ntohll(x) ( ((uint64_t)(ntohl((uint32_t)((x << 32) >> 32) )) << 32) | ntohl(((uint32_t)(x >> 32))) )
#endif
#ifndef htonll
#define htonll(x) ntohll(x)
#endif

typedef struct inet_message0 {
  char endianness; 
  char channel_type;
  char zip_type;
  char packing_type;
  char execution_type;
  char response_type;
  char display_type;
  char align[1];
  long long nested_k_wire_size;} MESSAGE0; 

enum MESSAGE_RESPONSE_TYPES  {MESSAGE_RESPONSE_NO_ACK=0, MESSAGE_RESPONSE_FULL=1, MESSAGE_RESPONSE_ABBREVIATED=2, MESSAGE_RESPONSE_TMPFILENAME=3 };
enum MESSAGE_EXECUTION_TYPES {MESSAGE_EXECUTION_NONE=0, MESSAGE_EXECUTION_STRING_EVAL=1, MESSAGE_EXECUTION_STRING_CALL=2, MESSAGE_EXECUTION_APPLY=3, MESSAGE_EXECUTION_JSON=4};
enum MESSAGE_DISPLAY_TYPES   {MESSAGE_DISPLAY_NONE=0, MESSAGE_DISPLAY_SHOW_MESSAGE=1, MESSAGE_DISPLAY_SHOW_EX=2, MESSAGE_DISPLAY_SHOW_MSG_EX=3 };


int _kerf_open_handle(char *host, char *port)
{
  int fd_socket = -1;

  char *errstr = "";

  struct addrinfo hints, *servinfo, *p;
  int rv;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;

  if ((rv = getaddrinfo(host, port, &hints, &servinfo)) != 0)
  {
    fprintf(stderr, "conn: %s\n", gai_strerror(rv));
    return -1;
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
      perror("client: connect");
      close(fd_socket);
      continue;
    }
    else break;
  }

  if (p == NULL)
  {
    fprintf(stderr, "conn: failed to connect (%s)\n", errstr);
    freeaddrinfo(servinfo);
    return -1;
  }

  //char s[INET6_ADDRSTRLEN];
  //inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr), s, sizeof s);
  //O("client: connecting to %s\n", s);

  int yes=1;

  //setsockopt(fd_socket, SOL_SOCKET, SO_KEEPALIVE, (void *)&yes, sizeof(yes));
  //setsockopt(fd_socket, IPPROTO_TCP, TCP_NODELAY, &yes, sizeof(I));//disable nagle

#if defined(__MACH__) && defined(__APPLE__) || defined(__FreeBSD__)  || defined(__NetBSD__)
  setsockopt(fd_socket, SOL_SOCKET, SO_NOSIGPIPE, (void *)&yes, sizeof(yes));
#endif

  struct linger timewait;
  timewait.l_onoff = 1;
  timewait.l_linger = SHRT_MAX;
  setsockopt(fd_socket, SOL_SOCKET, SO_LINGER, &timewait, sizeof(timewait));

  freeaddrinfo(servinfo);

  return fd_socket;
}

int sendall(int socket_fd, char* buffer, long long length)
{
  long long progress = 0;
  long long remain = length;
  long long sent = 0;

  while(progress < length)
  {
    //int flags = MSG_MORE | MSG_DONTWAIT | MSG_NOSIGNAL ;
    int flags = 0;

    sent = send(socket_fd, buffer + progress, remain, flags);
    if(-1 == sent)break;
    progress += sent;
    remain -= sent;
  }

  if(-1 == sent)
  {
    perror("Error in send");
    return -1;
  }

  return 0;
}


int _kerf_send_message_cstring(int socket_fd, char *message)
{
  int64_t length = strlen(message);
  int64_t wire_size = length + 16;

  MESSAGE0 header =
  {
    .endianness         = 0,
    .channel_type       = 0,
    .zip_type           = 0,
    .packing_type       = 0,
    .execution_type     = MESSAGE_EXECUTION_STRING_EVAL,
    .response_type      = MESSAGE_RESPONSE_NO_ACK,
    //.display_type       = MESSAGE_DISPLAY_NONE,
    .display_type       = MESSAGE_DISPLAY_SHOW_MESSAGE,
    .nested_k_wire_size = htonll(wire_size),
  };

  //POTENTIAL_OPTIMIZATION_POINT: MSG_MORE
  int64_t sent = 0;

  sent = sendall(socket_fd, (char *)&header, sizeof(header));
  if(-1 == sent)
  {
    perror("connection: send header error");
    return -1;
  }

  char fake[16] = {0};
  fake[3] = -1;
  int32_t refcount = 1;
  memcpy(4 + (char *)fake,   &refcount, sizeof(refcount));
  memcpy(8 + (char *)fake,   &length,   sizeof(length));
 
  sent = sendall(socket_fd, fake, sizeof(fake));
  if(-1 == sent)
  {
    perror("connection: send fake error");
    return -1;
  }
 
  sent = sendall(socket_fd, message, length);
  if(-1 == sent)
  {
    perror("connection: send fake error");
    return -1;
  }
 
  return 0;
}

int main()
{
  char *host = "127.0.0.1";
  char *port = "1234";

  int socket = 0;

  socket = _kerf_open_handle(host, port);

  if(-1 == socket)
  {
    fprintf(stderr, "Cannot send with bad handle\n");
    exit(1);
  }
  
  fprintf(stderr, "Descriptor: %d\n", socket); 

  int sent = _kerf_send_message_cstring(socket, "nettest: 1");

  //done sending messages? leave open if not
  close(socket);

  return 0;
}
