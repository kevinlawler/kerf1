//LINUX: cc -rdynamic -shared -o dyn.so -fPIC dyn.c
//OSX:   cc -rdynamic -m64 -flat_namespace -undefined suppress -dynamiclib dyn.c -o dyn.dylib

#include <assert.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>  /* exit */
#include <string.h>  /* for memset(3C) */
#include <time.h>
#include <unistd.h>  /* for pause(2)   */ 
#include <stdbool.h> /* bool, true, false */ 

#include <blpapi_correlationid.h>
#include <blpapi_element.h>
#include <blpapi_event.h>
#include <blpapi_message.h>
#include <blpapi_request.h>
#include <blpapi_session.h>
#include <blpapi_subscriptionlist.h>

#include "liblfds611.h"
#include "kerf_api.h"
//auth string like "AuthenticationMode=APPLICATION_ONLY;ApplicationAuthenticationType=APPNAME_AND_KEY;ApplicationName=my_co:co trading blackbox app;"; 
//topic list like ["SPY US Equity, "/ticket/GOOG US Equity", "/cusip/097023105"]
static char auth_string[256];
static char fields_string[4096];

static int  KERF_AUTH_CORR = 777;

static blpapi_Identity_t *IDENT;
static blpapi_Session_t  *BLPAPI_SESSION = 0;
static char token_buf[512] = {0};

static int connect();
static int auth();
static int subscribe();

static char waiting_on_blpapi_token = false;
static char waiting_on_blpapi_auth  = false;

typedef void* V;
typedef char C;
typedef C* S;
typedef int64_t I;
typedef uint64_t UI;
typedef double F;


void *kerf_hook = NULL;
KERF topics = NULL;
KERF kerf_handler = NULL;

struct lfds611_queue_state *queue;

static int streamWriter(const char* data, int length, void *stream)
{
  assert(data);
  assert(stream);
  return fwrite(data, length, 1, (FILE *)stream);
}

typedef struct UserData {
    const char *d_label;
    FILE       *d_stream;
} UserData_t;

static void dumpEvent(const blpapi_Event_t *event, const UserData_t *userData)
{
  blpapi_MessageIterator_t *iter    = 0;
  blpapi_Message_t         *message = 0;

  iter = blpapi_MessageIterator_create(event);
  assert(iter);

  while (0 == blpapi_MessageIterator_next(iter, &message))
  {
          blpapi_CorrelationId_t  correlationId;
          blpapi_Element_t       *messageElements = 0;
          //assert(message);
          printf("messageType=%s\n", blpapi_Message_typeString(message));
          messageElements=blpapi_Message_elements(message);
          correlationId = blpapi_Message_correlationId(message, 0);

          if(correlationId.valueType == BLPAPI_CORRELATION_TYPE_INT)
          {
            printf("correlationId=%d %d %lld\n", correlationId.valueType, correlationId.classId, correlationId.value.intValue);
          }
          else if(correlationId.valueType == BLPAPI_CORRELATION_TYPE_POINTER)
          {
            printf("correlationId=%d %d %s\n", correlationId.valueType, correlationId.classId, correlationId.value.ptrValue.pointer);
          }

          blpapi_Element_print(messageElements, &streamWriter, stdout, 0, 4);
          printf("\n");
  } 

  blpapi_MessageIterator_destroy(iter);
 
}

static void handleDataEvent(const blpapi_Event_t *event, const blpapi_Session_t *session, const UserData_t *userData)
{
  //dumpEvent(event, userData);
  blpapi_Event_addRef(event);
  int e = lfds611_queue_enqueue(queue, (void *)event);
  if(!e)
  {
    e = lfds611_queue_guaranteed_enqueue(queue, (void *)event);
  }
  blpapi_Event_release(event);

  kerf_api_notify_event(kerf_hook);
}

static void handleStatusEvent(const blpapi_Event_t   *event, const blpapi_Session_t *session, const UserData_t *userData)
{
  //dumpEvent(event, userData);
}

static bool process_token_event(const blpapi_Event_t *event, const UserData_t *userData)
{
  /*
    handler label=myLabel
    eventType=14
    messageType=TokenGenerationSuccess
    correlationId=1 0 777
    TokenGenerationSuccess = {
        token = "-v4;1hbb...dmU6SI/w/Drt..."
        }
  */

  blpapi_MessageIterator_t *iter    = 0;
  blpapi_Message_t         *message = 0;
  assert(event);
  assert(userData);
  assert(userData->d_label);
  assert(userData->d_stream);

  iter = blpapi_MessageIterator_create(event);
  assert(iter);

  while (0 == blpapi_MessageIterator_next(iter, &message))
  {
          blpapi_CorrelationId_t  correlationId;
          blpapi_Element_t       *messageElements = 0;
          assert(message);

          messageElements=blpapi_Message_elements(message);

          blpapi_Element_t       *tokenElement = 0;
          blpapi_Element_getElement(messageElements, &tokenElement, "token", 0);

          const char *token;
    
          if(tokenElement)
          {
            blpapi_Element_getValueAsString(tokenElement, &token,0);

            if(!token)
            {
              fprintf(stderr, "Failed: null token string\n");
              return false;
            }

            strncpy(token_buf, token, sizeof(token_buf));
            //fprintf(stderr, "Got good token: %s\n", token_buf);
            return true;
          }

          correlationId = blpapi_Message_correlationId(message, 0);
          //printf("correlationId=%d %d %lld\n", correlationId.valueType, correlationId.classId, correlationId.value.intValue);
          blpapi_Element_print(messageElements, &streamWriter, stdout, 0, 4);
  } 

  blpapi_MessageIterator_destroy(iter);

  return false;
}

static bool process_auth_event(const blpapi_Event_t *event, const UserData_t *userData)
{
  /* 
   handleOtherEventHandler: enter
   handler label=myLabel
   eventType=5
   messageType=AuthorizationSuccess
   correlationId=1 0 1
   AuthorizationSuccess = {
   }
   handleOtherEventHandler: leave
  */
  blpapi_MessageIterator_t *iter    = 0;
  blpapi_Message_t         *message = 0;
  assert(event);
  assert(userData);
  assert(userData->d_label);
  assert(userData->d_stream);

  iter = blpapi_MessageIterator_create(event);
  assert(iter);

  while (0 == blpapi_MessageIterator_next(iter, &message))
  {
          blpapi_CorrelationId_t  correlationId;
          blpapi_Element_t       *messageElements = 0;
          assert(message);

          //printf("messageType=%s\n", blpapi_Message_typeString(message));

          if(0==strcmp("AuthorizationSuccess", blpapi_Message_typeString(message)))
          {
            fprintf(stderr, "Authorization success.\n");
            return true;
          }
  } 

  blpapi_MessageIterator_destroy(iter);

  fprintf(stderr, "Failed: Authorization failed.\n");

  return false;
}

static void handleOtherEvent(const blpapi_Event_t *event, const blpapi_Session_t *session, const UserData_t *userData)
{
  if(waiting_on_blpapi_token)
  {
    if(!process_token_event(event, userData))
    {
      fprintf(stderr, "Token failed. Token event didn't match\n");
      exit(1);
    }

    waiting_on_blpapi_token = false;
    return;
  }
  else if(waiting_on_blpapi_auth)
  {

    if(!process_auth_event(event, userData))
    {
      fprintf(stderr, "Auth failed. Auth event didn't match\n");
      exit(1);
    }
   
    waiting_on_blpapi_auth = false;
    return;
  }

  dumpEvent(event, userData);
}

#ifdef __cplusplus
extern "C"
#endif
static void processEvent(blpapi_Event_t *event, blpapi_Session_t *session, void *buffer)
{
    UserData_t *userData = (UserData_t *)buffer;
    assert(event);
    assert(session);
    assert(userData);

    switch (blpapi_Event_eventType(event))
    {
      case BLPAPI_EVENTTYPE_SUBSCRIPTION_DATA:
          handleDataEvent(event, session, userData);
          break;
      case BLPAPI_EVENTTYPE_SESSION_STATUS:
      case BLPAPI_EVENTTYPE_SERVICE_STATUS:
      case BLPAPI_EVENTTYPE_SUBSCRIPTION_STATUS:
          handleStatusEvent(event, session, userData);
          break;
      default:
          handleOtherEvent(event, session, userData);
          break; 
    }
}

int main()
{
  connect();
  return 0;
}

I queue_capacity()
{
  lfds611_atom_t k;
  lfds611_queue_query(queue, LFDS611_QUEUE_QUERY_ELEMENT_COUNT, NULL, &k);
  return k;
}

KERF value_from_bloomberg(blpapi_Element_t *e)
{
  I t = blpapi_Element_datatype(e);

  KERF r = NULL;

  switch(t)
  {
    case BLPAPI_DATATYPE_INT32:{
      int32_t i32 = 0;
      blpapi_Element_getValueAsInt32(e, &i32, 0);
      r = kerf_api_new_int(i32);
      break;
    }
    case BLPAPI_DATATYPE_INT64:{
      long long i64 = 0;
      blpapi_Element_getValueAsInt64(e, &i64, 0);
      r = kerf_api_new_int(i64);
      break;
    }
    case BLPAPI_DATATYPE_FLOAT32:{
      float f32 = 0;
      blpapi_Element_getValueAsFloat32(e, &f32, 0);
      r = kerf_api_new_float(f32);
      break;
    }
    case BLPAPI_DATATYPE_FLOAT64:{
      double f64 = 3.4;
      blpapi_Element_getValueAsFloat64(e, &f64, 0);
      r = kerf_api_new_float(f64);
      break;
    }

    case BLPAPI_DATATYPE_DATE:
    case BLPAPI_DATATYPE_TIME:
    case BLPAPI_DATATYPE_DATETIME: {
      blpapi_Datetime_t dt = {0};
      blpapi_Element_getValueAsDatetime(e, &dt, 0);
      //I BILLION = 1000000000;
      I MILLION = 1000000;
      //I nanos = BILLION * ((dt.hours * 60 * 60) + (dt.minutes * 60) + dt.seconds) + (dt.milliSeconds * MILLION);
      struct tm t = ((struct tm){.tm_year =(1970 - 1900), .tm_mon = 0, .tm_mday = 1});

      if(1!=dt.year)
      {
        //TODO: Haven't actually received BBG date to check
        t.tm_year  = dt.year;
        //t.tm_year  = -70 + t.tm_year;
        t.tm_mon   = dt.month;
        t.tm_mday  = dt.day;
      }
      t.tm_hour  = dt.hours;
      t.tm_min   = dt.minutes;
      t.tm_sec   = dt.seconds;
      //milliSeconds currently zero from BBG for 'TIME' field
      //      I nanos = stampI_from_tm(&t, false) + (dt.milliSeconds * MILLION);
      I nanos = kerf_api_nanos_from_stamp(&t) + (dt.milliSeconds * MILLION);
      
      r = kerf_api_new_stamp(nanos);
      break;
    }
    //unfinished but easy
    case BLPAPI_DATATYPE_CHAR:
    case BLPAPI_DATATYPE_BYTE:
    case BLPAPI_DATATYPE_BYTEARRAY:
    case BLPAPI_DATATYPE_DECIMAL:
    case BLPAPI_DATATYPE_ENUMERATION:
    case BLPAPI_DATATYPE_SEQUENCE:
    case BLPAPI_DATATYPE_CHOICE:
    case BLPAPI_DATATYPE_CORRELATION_ID:
    ////////////////////////////
    case BLPAPI_DATATYPE_STRING:
    default: {
      const char *s = NULL;
      blpapi_Element_getValueAsString(e, &s, 0);
      r = kerf_api_new_charvec((char *)s);
      break;
    }
  }

  return r;
}

int consume(void *kerf_hook)
{
  kerf_api_denotify_event(kerf_hook);

  static bool queue_init;

  if(!queue_init)
  {
    lfds611_queue_use(queue);
    queue_init = true;
  }

  blpapi_Event_t *event = NULL; 

  I i = 0;
  I max_read_per_consume = 10000;

  KERF list = NULL;

  for(i = 0; i < max_read_per_consume; i++)
  {
    int eval = lfds611_queue_dequeue(queue, (void**)&event);
    if(!eval) break;

    if(!list) list = kerf_api_new_list(); 

    blpapi_MessageIterator_t *iter    = 0;
    blpapi_Message_t         *message = 0;

    iter = blpapi_MessageIterator_create(event);

    while (0 == blpapi_MessageIterator_next(iter, &message))
    {
      blpapi_CorrelationId_t  correlationId;
      blpapi_Element_t       *messageElements = 0;

      messageElements=blpapi_Message_elements(message);
      correlationId = blpapi_Message_correlationId(message, 0);

      int k = blpapi_Element_numElements(messageElements);
      int j = 0;

      KERF map = kerf_api_new_map();

      //POTENTIAL_OPTIMIZATION_POINT - if this is slow, maybe better to use BLAPI corrId integers
      //this gives and index lookup instead of hashing the strings and what not
      if(correlationId.valueType == BLPAPI_CORRELATION_TYPE_POINTER)
      {
        char * topic = (char *)correlationId.value.ptrValue.pointer;
        //POTENTIAL_OPTIMIZATION_POINT - re-alloc'ing currently
        KERF key   = kerf_api_new_charvec("TOPIC");
        KERF value = kerf_api_new_charvec( topic );
        kerf_api_set(map, key, value);
        kerf_api_release(key);
        kerf_api_release(value);
      }

      blpapi_Element_t *x; 

      for(j = 0; j < k; j++)
      {
        blpapi_Element_getElementAt(messageElements, &x, j);
        const char *skey = blpapi_Element_nameString(x);
        KERF key = kerf_api_new_charvec((char *)skey);
        KERF value = value_from_bloomberg(x); 
        //POTENTIAL_OPTIMIZATION_POINT
        //possibly faster to do all the keys, values as lists
        //and then build the map
        map = kerf_api_set(map, key, value);
        kerf_api_release(key);
        kerf_api_release(value);
      }
      
      list = kerf_api_append(list, map);
      kerf_api_release(map);
    }

    blpapi_MessageIterator_destroy(iter);

    //dumpEvent(event, 0);

    blpapi_Event_release(event);
  }

  I currently_read = i;

  //if there were no events in the queue we never allocate a list
  //while we're using the "+1 select" method this can happen (see below)
  if(list)
  {
    I i = 0;
    for(i = 0; i < list->n; i++)
    {
      KERF item = kerf_api_get(list, kerf_api_new_int(i)); 
      kerf_api_release(kerf_api_call_monad(kerf_handler, item));
      kerf_api_release(item);
    }

    //kerf_api_show(list);
    kerf_api_release(list);
  }
  
  //doing it like this solves a race condition that might otherwise occur
  //the downside is we check the file descriptor +1 times in the select loop
  //at the end of each successful sequence where we would designal it
  //(namely there is a superfluous consume at the end on the "elements in queue"==0 state)
  //which is perfectly OK
  //however, there are other ways to handle this (mutexes and so on)
  //lfds700 (today unreleased) has a "real" queue count that should solve this 
  if(currently_read > 0)
  {
    kerf_api_notify_event(kerf_hook);
  }

  return 1;
}

KERF start(KERF string_auth, KERF list_topics, KERF string_fields)
{
  //TODO: block SIGINT from hitting BBG threads (which don't seem to handle it)
  //see: http://man7.org/linux/man-pages/man3/pthread_sigmask.3.html
  //or block SIGINT entirely (on eventing registration)

  int n = 0;
  int i = 0;

  for(i = 0; i < sizeof(auth_string); i++) {
    //    auth_string[i]='\0';
    auth_string[i] = (char) kerf_api_get(string_auth,kerf_api_new_int(i)); 
  }
  //  n = fmin(sizeof(auth_string) - 1, string_auth->n);
  //strncpy(auth_string, kC(string_auth), n); 

  topics = list_topics;

  for(i = 0; i < sizeof(fields_string); i++) {
    //    fields_string[i]='\0';
    fields_string[i]= (char) kerf_api_get(string_fields,kerf_api_new_int(i)); 
  }
  //  n = fmin(sizeof(fields_string) - 1, string_fields->n);
  // strncpy(fields_string, kC(string_fields), n); 

  I queue_size = 0;
  int r = lfds611_queue_new(&queue, queue_size);

  //kerf_handler = Kn();
  //kerf_handler = ex(".Feeds.Bloomberg.handler");



  kerf_handler = kerf_api_interpret(kerf_api_new_charvec(".Feeds.Bloomberg.handler")); kerf_api_release(kerf_handler);

  kerf_api_register_for_eventing(&kerf_hook, consume);  

  connect();

  return kerf_api_new_int(123);
}

int connect()
{
  fprintf(stderr, "\nConnecting through Bloomberg API...\n");
  blpapi_SessionOptions_t *sessionOptions = 0;
       
  BLPAPI_SESSION = 0;

  UserData_t userData = {"myUserDataLabel", stderr};

  sessionOptions = blpapi_SessionOptions_create();
  assert(sessionOptions);

  blpapi_SessionOptions_setServerHost(sessionOptions, "localhost");
  blpapi_SessionOptions_setServerPort(sessionOptions, 8194); 

  blpapi_SessionOptions_setConnectTimeout(sessionOptions, 7500);
  blpapi_SessionOptions_setDefaultKeepAliveInactivityTime(sessionOptions, 20000);
  blpapi_SessionOptions_setDefaultKeepAliveResponseTimeout(sessionOptions, 30000); //default 5000
  blpapi_SessionOptions_setMaxEventQueueSize(sessionOptions, 10000);

  char as[256] = {0};
  strcpy(as, auth_string);
  blpapi_SessionOptions_setAuthenticationOptions(sessionOptions, as);

  BLPAPI_SESSION = blpapi_Session_create(sessionOptions, &processEvent, 0, &userData);
  //session = blpapi_Session_create(sessionOptions, &processEvent, 0, 0);

  assert(BLPAPI_SESSION);

  blpapi_SessionOptions_destroy(sessionOptions);

  if (0 != blpapi_Session_start(BLPAPI_SESSION)) {
      fprintf(stderr, "Failed to start session.\n");
      blpapi_Session_destroy(BLPAPI_SESSION);
      return 1;
  }

  if (0 != blpapi_Session_openService(BLPAPI_SESSION,"//blp/apiauth")){
      fprintf(stderr, "Failed to open service //blp/apiauth.\n");
      blpapi_Session_destroy(BLPAPI_SESSION);
      return 1;
  }

  if (0 != blpapi_Session_openService(BLPAPI_SESSION,"//blp/mktdata")){
      fprintf(stderr, "Failed to open service //blp/mktdata.\n");
      blpapi_Session_destroy(BLPAPI_SESSION);
      return 1;
  }

  if (0 != blpapi_Session_openService(BLPAPI_SESSION,"//blp/srcref")){
      fprintf(stderr, "Failed to open service //blp/srcref.\n");
      blpapi_Session_destroy(BLPAPI_SESSION);
      return 1;
  }

  blpapi_CorrelationId_t corID;
  memset(&corID, '\0', sizeof(corID));
  corID.size = sizeof(corID);
  corID.valueType = BLPAPI_CORRELATION_TYPE_INT;
  corID.value.intValue = KERF_AUTH_CORR;

  blpapi_Session_generateToken(BLPAPI_SESSION, &corID, 0);

  waiting_on_blpapi_token = true;
  
  while(waiting_on_blpapi_token)
  {
    sleep(1);
  }

  return auth();
}

int auth()
{
  blpapi_Service_t *a;
  blpapi_Session_getService(BLPAPI_SESSION, &a, "//blp/apiauth");

  IDENT = blpapi_Session_createIdentity(BLPAPI_SESSION);

  blpapi_Request_t *r;

  blpapi_Service_createAuthorizationRequest(a, &r, "AuthorizationRequest");

  blpapi_Element_t *e;
  e = blpapi_Request_elements(r);

  blpapi_Element_t *t;
  blpapi_Element_getElement(e, &t, "token", 0);

  int cid = 1;

  blpapi_Element_setValueString(t, token_buf, 0);

  blpapi_CorrelationId_t c;
  memset(&c, '\0', sizeof(c));
  c.size = sizeof(c);
  c.valueType = BLPAPI_CORRELATION_TYPE_INT;
  c.value.intValue = cid;

  blpapi_Session_sendAuthorizationRequest(BLPAPI_SESSION, r, IDENT, &c, 0, 0, 0);
  blpapi_Request_destroy(r);

  waiting_on_blpapi_auth = true;

  while(waiting_on_blpapi_auth)
  {
    sleep(1);
  }

  return subscribe();
}

int subscribe()
{
  fprintf(stderr,"Subscribing...\n");

  blpapi_SubscriptionList_t *subscriptions = blpapi_SubscriptionList_create();

  int i = 0;
  int n = 0;

  for(i = 0; i < kerf_api_len(topics); i++)
  {
    KERF sym = kerf_api_get(topics, kerf_api_new_int(i));

    char *topic = malloc(sym->n + 1); //we'll keep these resident permanently - very OK

    int j = 0; 
    for(j = 0; j < sym->n; j++)
    {
      //      topic[j] = kC(sym)[j];
      topic[j] = (char) kerf_api_get(sym,kerf_api_new_int(j));
    }
    topic[sym->n] = '\0';

    const char *options[] = {};
    int numOptions = sizeof(options)/sizeof(char *);
  
    const char *fields_array[] = {fields_string};
    int numFields = sizeof(fields_array)/sizeof(*fields_array);
  
    blpapi_CorrelationId_t subscriptionId;
  
    memset(&subscriptionId,  '\0',  sizeof(subscriptionId));
    subscriptionId.size           = sizeof(subscriptionId);
    subscriptionId.valueType      = BLPAPI_CORRELATION_TYPE_INT;
    subscriptionId.value.intValue = (blpapi_UInt64_t)10;
    subscriptionId.valueType      = BLPAPI_CORRELATION_TYPE_POINTER;
    subscriptionId.value.ptrValue.pointer = (void *)topic; //we could malloc without ever freeing...

    blpapi_SubscriptionList_add(subscriptions,
                                        topic,
                              &subscriptionId,
                                 fields_array,
                                      options,
                                    numFields,
                                   numOptions);

    kerf_api_release(sym);
  }


  blpapi_Session_subscribe(BLPAPI_SESSION, subscriptions, IDENT, 0, 0);

  //pause();

  fprintf(stderr,"Finished subscribing.\n");

  return 0; 
}

KERF finalize(KERF a)
{
  //blpapi_SubscriptionList_destroy(subscriptions);
  //blpapi_Session_destroy(BLPAPI_SESSION);
  //if(kerf_handler) kerf_api_release(kerf_handler);

  //void lfds611_queue_delete( struct lfds611_queue_state *qs, void (*user_data_delete_function)(void *user_data, void *user_state), void *user_state ); //?
  return NULL;
}

