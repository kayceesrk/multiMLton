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

#define BUFSIZE  16

int RCCE_APP(int argc, char **argv){

  int ID, ID_nb, ID_donor, nrounds, error, strlength, phase, c, i, round;

  double *buffer, *buffer2, sum;
  char msg[RCCE_MAX_ERROR_STRING];

  if(RCCE_init(&argc, &argv)) {
    printf("Error in RCCE_init; bailing\n");
    exit(1);
  }
  
  ID = RCCE_ue();
  ID_nb = (ID+1)%RCCE_num_ues();
  ID_donor = (ID-1+RCCE_num_ues())%RCCE_num_ues();

  if (argc != 2) {
    if (ID==0) printf("Executable requires one parameter (number of rounds): %d\n",argc-1);
    return(1);
  }
  nrounds = atoi(*++argv);
  if (nrounds < 0) {
    if (ID==0) printf("Number of rounds should be non-negative: %d\n", nrounds);
    return(1);
  }

  /* allocate private memory */
  buffer  = (double *) malloc(2*BUFSIZE*sizeof(double));
  if (!buffer) printf("Mark 01: Failed to allocate private buffer on proc %d\n", ID);
  buffer2 = buffer + BUFSIZE;

  /* initialize buffer with UE-specific data  */
  for (i=0; i<BUFSIZE; i++) buffer[i] = (double)(ID+1);
  sum = 0.0;  
  for (i=0; i<BUFSIZE; i++) sum += buffer[i];
  printf("Initial sum on UE %03d equals %f\n", ID, sum); fflush(0);

  for (round=0; round<nrounds; round++) {

    /* do the circular shift of data */
    for (phase = 0; phase<2; phase++) {
      if ((ID+phase)%2) RCCE_send((char *)buffer, BUFSIZE*sizeof(double), ID_nb);
      if (!((ID+phase)%2)) RCCE_recv((char *)buffer2, BUFSIZE*sizeof(double), ID_donor);
    }
    memcpy(buffer, buffer2, BUFSIZE*sizeof(double));

  }

  /* compute local sum */
  sum = 0.0;
  for (i=0; i<BUFSIZE; i++) sum += buffer[i];
  printf("Final sum on UE %03d equals %f\n", ID, sum); fflush(0);

  RCCE_finalize();
  return(0);
}

