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

#define BUFSIZE  8

int RCCE_APP(int argc, char **argv){

  int ID, ID_nb, ID_donor, nrounds, error, strlength;
  RCCE_FLAG flag_sent, flag_ack;

  double *cbuffer, *buffer, sum;
  char msg[RCCE_MAX_ERROR_STRING];

  RCCE_init(&argc, &argv);

#ifdef TESTFLAGALLOC
  ID=0;
  while (!RCCE_flag_alloc(&flag_sent)) ID++;
  printf("Able to allocate %d flags\n", ID);
  exit(0);
#endif
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

  /* allocate private memory and comm buffer space */
  buffer  = (double *) malloc(BUFSIZE*sizeof(double));
  if (!buffer) printf("Mark 01: Failed to allocate private buffer on proc %d\n", ID);
  cbuffer = (double *) RCCE_malloc(BUFSIZE*sizeof(double));
  if (!buffer) printf("Mark 02:RCCE failed to allocate %d doubles on proc %d\n",
      BUFSIZE, ID);

  /* initialize buffer with UE-specific data  */
  for (int i=0; i<BUFSIZE; i++) buffer[i] = (double)(ID+1+i);
  sum = 0.0;  
  for (int i=0; i<BUFSIZE; i++) sum += buffer[i];
  printf("Initial sum on UE %03d equals %f\n", ID, sum);

  /* create and initialize flag variables */
  if (error=RCCE_flag_alloc(&flag_sent))
    printf("Mark 03a: Could not allocate flag_sent on %d, error=%d\n", ID, error);
  if (error=RCCE_flag_alloc(&flag_ack))
    printf("Mark 03b: Could not allocate flag_ack on %d, error=%d\n", ID, error);

  if(error=RCCE_flag_write(&flag_sent, RCCE_FLAG_UNSET, ID)) 
    printf("Mark 04: Could not initialize flag_sent on %d, error=%d\n", ID, error);
  if(error=RCCE_flag_write(&flag_ack, RCCE_FLAG_SET, ID_donor)) 
    printf("Mark 05: Could not initialize flag_ack on %d, error=%d\n", ID_donor, error);

  for (int round=0; round<nrounds; round++) {

    int size = BUFSIZE*sizeof(double);
    RCCE_wait_until(flag_ack, RCCE_FLAG_SET);
    RCCE_flag_write(&flag_ack, RCCE_FLAG_UNSET, ID);
    RCCE_put((t_vcharp)cbuffer, (t_vcharp)buffer, size, ID_nb);
    RCCE_flag_write(&flag_sent, RCCE_FLAG_SET, ID_nb);

    RCCE_wait_until(flag_sent, RCCE_FLAG_SET);
    RCCE_flag_write(&flag_sent, RCCE_FLAG_UNSET, ID);
    RCCE_get((t_vcharp)buffer, (t_vcharp)cbuffer, size, ID);
    RCCE_flag_write(&flag_ack, RCCE_FLAG_SET, ID_donor);

  }

  /* compute local sum */
  sum = 0.0;
  for (int i=0; i<BUFSIZE; i++) sum += buffer[i];
  printf("Final sum on UE %03d equals %f\n", ID, sum);

  RCCE_finalize();

  return(0);
}

