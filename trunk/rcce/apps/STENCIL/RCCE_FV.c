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
#include "RCCE.h"
#include <stdio.h>

void print_dividers(void);

int RCCE_APP(int argc, char **argv){

  RCCE_REQUEST req;
  int fdiv, fdiv_in, vlevel, ID;

  RCCE_init(&argc, &argv);

  if (argc==2) fdiv_in=atoi(*++argv);  
  else return(0);
  
  ID = RCCE_ue();

  if (ID==RCCE_power_domain_master()) {
    if(RCCE_iset_power(fdiv_in, &req, &fdiv, &vlevel)) {
      printf("UE %d failed ISET_POWER %d\n", ID, fdiv_in);
      fflush(NULL);
    }
    else {
      printf("Requested fdiv: %d, actual fdiv, vlevel: %d, %d\n", 
             fdiv_in, fdiv, vlevel);
      fflush(NULL);
    }
    if (RCCE_wait_power(&req)) {
      printf("UE %d failed WAIT_POWER %d\n", ID, fdiv_in);
      fflush(NULL);
    }
  }
  
  RCCE_barrier(&RCCE_COMM_WORLD);

  if (ID==0)print_dividers();
  RCCE_finalize();

  return(0);
}
