#include <dlfcn.h>
#include "kerf_api.h"

//OSX:   cc -rdynamic -m64 -flat_namespace -undefined suppress  sigma.c  -o sigma
//LINUX: cc -rdynamic -fPIC sigma.c -o sigma -ldl

int main()
{
  void *lib = dlopen("../kerf", RTLD_LAZY);

  kerf_api_init();

  KERF s = kerf_api_new_int(33);
  kerf_api_show(s);
  kerf_api_release(s);

  KERF c = kerf_api_new_charvec("1+1");
  kerf_api_show(c);

  KERF k = kerf_api_interpret(c);
  kerf_api_show(k);
  kerf_api_release(k);

  return 0;
}
