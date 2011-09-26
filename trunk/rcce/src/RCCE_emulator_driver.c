//****************************************************************************************
// Driver program for RCCE functional emulator. 
//****************************************************************************************
// This file will only get linked if the OpenMP-based emulator mode is 
// used, so we know OpenMP is always available. 
//
// Author: Rob F. Van der Wijngaart
//         Intel Corporation
// Date:   12/22/2010
//
//****************************************************************************************
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
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "RCCE_lib.h"

//----------------------------------------------------------------------------------------
// global variables used for OpenMP emulation:
// - shared array of openmp locks (emulating single test-and-set register per core)
// - shared MPB space                                         
//----------------------------------------------------------------------------------------
omp_lock_t  RCCE_corelock[RCCE_MAXNP];
t_vchar     RC_comm_buffer[RCCE_MAXNP*RCCE_BUFF_SIZE_MAX];
t_vchar     RC_shm_buffer[RCCE_SHM_SIZE_MAX];

// name of user application. In non-OpenMP emulation mode this resolves to main. In 
// OpenMP emulation mode it is unchanged
int    RCCE_APP(int argc, char **argv);

int main(int argc, char *argv[]){

  int i;
  int RCCE_NPE;

  int RCCE_error = 0;
  if (argc <3) {
    printf("RCCE DRIVER ERROR: no executable or parameters specified\n");
    exit(1);
  }

  RCCE_NP = RCCE_NPE = atoi(*(argv+1));
  if (RCCE_NPE<1 || RCCE_NPE>RCCE_MAXNP) {
    printf("Invalid number of UEs: %d\n", RCCE_NPE);
    exit(1);
  } 

  // initialize comm buffers with all constants 
  for (i=0; i<RCCE_MAXNP*RCCE_BUFF_SIZE_MAX/sizeof(int); i++) 
    *(((int *) RC_comm_buffer)+i) = RCCE_comm_init_val;

  // initialize core locks 
  for (i=0; i<RCCE_NPE; i++) omp_init_lock(&(RCCE_corelock[i]));

  // start parallel region. If any UE (thread) returns a non-zero error
  // value, the total error, reduced to RCCE_error, will be non-zero
  #pragma omp parallel reduction(||:RCCE_error) num_threads(RCCE_NP) firstprivate(argc,argv,RCCE_NPE) 	
  {
  if (omp_get_num_threads() != RCCE_NPE) {
    #pragma omp master
    {
    RCCE_error = RCCE_ERROR_NUM_UES;
    }
  }
  else{
     RCCE_error = RCCE_APP(argc, argv);
    }
  }
  exit(RCCE_error);
}

