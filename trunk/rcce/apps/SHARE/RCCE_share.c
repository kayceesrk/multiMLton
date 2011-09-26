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

int RCCE_APP(int argc, char **argv){

  int ID, ID_right, ID_left, nrounds, round, NP, size, baton, bufsize, i;

  double *buffer, sum, refval;

  RCCE_init(&argc, &argv);
  NP = RCCE_num_ues();
  ID = RCCE_ue();
  if (NP<2) {
    if (ID==0) printf("Need at least two cores to run this code\n");
    return(1);
  }

  if (argc != 3) {
    if (ID==0) printf("Need to specify buffer size and # rounds\n");
      return(1);
  }
  bufsize = atoi(*++argv);
  if ((bufsize<1)) {
    if (ID==0) printf("Buffer size must be greater than or equal to 1\n");
    return(1);
  }

  ID_right = (ID+1)%NP;
  ID_left = (ID-1+NP)%NP;

  nrounds = atoi(*++argv);
  if (nrounds < 0) {
    if (ID==0) printf("Number of rounds should be non-negative: %d\n", nrounds);
    return(1);
  }

  size = bufsize*sizeof(double);
  buffer = (double *) RCCE_shmalloc(size);
  if (!buffer) printf("Mark 02:RCCE failed to shmalloc %d doubles on proc %d\n",
      bufsize, ID);
  if (ID==0) {
    printf("Buffer is allocated %d doubles\n",bufsize);
    printf("allocated size in bytes: %d \n",size);
  }

  /* initialize shared buffer */
  sum = 0.0;  
  if (ID==0) {
    for (i=0; i<bufsize; i++) {
      buffer[i] = (double)(1+i);
      sum += buffer[i];
    }
    printf("Initial sum on UE %03d equals %14.13e\n", ID, sum);
  }

  refval = 0.0;
  for (i=0; i<bufsize; i++) refval += (double)(i+1);
  for (round=0; round<nrounds; round++) {

    if (ID==(round%NP))   RCCE_send((char*)&baton, sizeof(int), ID_right);
    for (i=0; i<bufsize; i++) refval += (round+1)%NP;
    if (ID==(round+1)%NP) {
      RCCE_recv((char*)&baton, sizeof(int), ID_left);
      RCCE_shflush(); // need to make sure buffer is up to date before we read it

      for (i=0; i<bufsize; i++) buffer[i] += ID;
      RCCE_shflush(); // need to make sure other UEs see updated buffer contents
      if (round==nrounds-1) {
        sum = 0.0;
        for (i=0; i<bufsize; i++) sum += buffer[i];
        printf("\n*****\nFinal sum on UE %03d equals %14.13e, refval = %14.13e\n*****\n", ID, sum, refval);
      }
    }

  }

  return(0);
}

