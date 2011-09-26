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

#define max(x,y) ((x)>(y)?(x):(y))

int RCCE_APP(int argc, char **argv){
  int YOU, ME, nrounds = 64*1024, actualrounds, size, N=32, round, pair, index;
  int bigsize, subindex, roundsize;
  double timer;
  char buffer[1024*1024*4];

  RCCE_init(&argc, &argv);

  //  RCCE_debug_set(RCCE_DEBUG_ALL);
  ME = RCCE_ue();
  YOU = !ME;

  if (argc>1) nrounds = atoi(*++argv);
  if (nrounds<1) {
    if (!ME) printf("Pingpong needs at least 1 round; try again\n");
    return(1);
  }
  if (RCCE_num_ues() != 2) {
    if (!ME) printf("Pingpong needs at two UEs; try again\n");
    return(1);
  }

  bigsize = 32;
  for (index=0; index<17; index++) {
    size = bigsize;
    for (subindex=0; subindex<4; subindex++) {

      roundsize = max(32,size - size%32);
      // synchronize before starting the timer
      RCCE_barrier(&RCCE_COMM_WORLD);
      timer = RCCE_wtime();
    
      actualrounds = max(10,(nrounds*32)/roundsize);
      for (round=0; round <actualrounds; round++) {
        if (ME)  {
          RCCE_send(buffer, roundsize, YOU);
          RCCE_recv(buffer, roundsize, YOU);
        } 
        else {
          RCCE_recv(buffer, roundsize, YOU);
          RCCE_send(buffer, roundsize, YOU);
        }
      }
      timer = RCCE_wtime()-timer;

      if (ME) printf("%d  %1.9lf\n", roundsize, timer/actualrounds);
      size *= 1.18920712;
   
    }

    bigsize *= 2;

  }

  RCCE_finalize();

  return(0);
}

