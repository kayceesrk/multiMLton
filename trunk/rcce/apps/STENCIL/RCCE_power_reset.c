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
  int fdiv, vlevel, ID;

  RCCE_init(&argc, &argv);

  RCCE_debug_set(RCCE_DEBUG_ALL);
  ID = RCCE_ue();

  // first turn the frequency so low that any voltage level is valid
  if(RCCE_set_frequency_divider(8, &fdiv)) {
    printf("UE %d failed SET_FREQUENCY_DIVIDER 8\n", ID);
    fflush(NULL);
  }
  
  // Ask for a tile clock divider of 2. This corresponds to voltage 
  // level 4, i.e. 1.1 V. 
  if(RCCE_iset_power(2, &req, &fdiv, &vlevel)) {
    printf("UE %d failed ISET_POWER 2\n", ID);
    fflush(NULL);
  }

  // need to wait for thenew voltage target to be reached
  if (RCCE_wait_power(&req)) {
    printf("UE %d failed WAIT_POWER 2\n", ID);
    fflush(NULL);
  }
  
  // now set a tile clock divider of 3, to arrive at the default
  // frequency of 533 MHz
  if (RCCE_set_frequency_divider(3, &fdiv)) {
    printf("UE %d failed SET_FREQUENCY_DIVIDER 3\n", ID);
    fflush(NULL);
  }

  RCCE_barrier(&RCCE_COMM_WORLD);
  if (ID==0)print_dividers();
  RCCE_finalize();

  return(0);
}
