#include "RCCE_lib.h"
#include "RCCE.h"
#include "SCC_API.h"
#include <stdlib.h>
#include <string.h>

int RCCE_DCMflush(  ) {
int retval=0;
#ifdef SHMADD_CACHEABLE
   retval = DCMflush();
#endif
   return retval;
}
