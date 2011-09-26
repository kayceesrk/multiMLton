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
  int fdiv, fdiv_in, ID;

  RCCE_init(&argc, &argv);

  if (argc==2) fdiv_in=atoi(*++argv);  
  else return(0);
  
  ID = RCCE_ue();

  if (ID==RCCE_power_domain_master()) {

    if(RCCE_set_frequency_divider(fdiv_in, &fdiv)) {
        printf("UE %d failed SET_FREQUENCY_DIVIDER 8\n", ID);
        fflush(NULL);
    } 
    else {
      printf("Requested fdiv: %d, actual fdiv: %d\n", fdiv_in, fdiv);
      fflush(NULL);
    }
  }

  RCCE_barrier(&RCCE_COMM_WORLD);

  if (ID==0)print_dividers();
  RCCE_finalize();

  return(0);
}
