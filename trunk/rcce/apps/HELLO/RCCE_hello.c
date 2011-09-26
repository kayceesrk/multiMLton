// 
// Copyright 2010 Intel Corporation
// 
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
// 
//        http://www.apache.org/licenses/LICENSE-2.0
// 
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// 
#include <string.h>
#include <stdio.h>
#include "RCCE.h"


int RCCE_APP(int argc, char **argv){

  RCCE_init(&argc, &argv);

  //  RCCE_debug_set(RCCE_DEBUG_ALL);

#ifdef RCCE_VERSION
  printf("Hello from RCCE ... I am %s\n",RCCE_VERSION);
#else
  printf("Hello from RCCE \n");
#endif

  RCCE_finalize();

  return(0);
}

