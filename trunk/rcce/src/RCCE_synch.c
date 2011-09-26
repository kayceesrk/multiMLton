///*************************************************************************************
// Synchronization functions. 
// Single-bit and whole-cache-line flags are sufficiently different that we provide
// separate implementations of the synchronization routines for each case
//**************************************************************************************
//
// Author: Rob F. Van der Wijngaart
//         Intel Corporation
// Date:   12/22/2010
//
//**************************************************************************************
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
#include "RCCE_lib.h"

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_wait_until
//--------------------------------------------------------------------------------------
// wait until flag in local MPB becomes set or unset. To avoid reading stale data from 
// the cache instead of new flag value from the MPB, issue MPB cache invalidation before 
// each read, including within the spin cycle 
//--------------------------------------------------------------------------------------
int RCCE_wait_until(RCCE_FLAG flag, RCCE_FLAG_STATUS val) {

  t_vcharp cflag = flag.line_address;
  t_vcharp flaga = flag.flag_addr;

// avoid tests if we use the simplified API 
#ifdef GORY
  if (val != RCCE_FLAG_UNSET && val != RCCE_FLAG_SET) 
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_STATUS_UNDEFINED));
  if (!cflag || !flaga)
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_NOT_ALLOCATED));
  // check to see if flag is properly contained in the local comm buffer  
  if (flaga - RCCE_comm_buffer[RCCE_IAM]>=0 &&
      flaga - (RCCE_comm_buffer[RCCE_IAM] + RCCE_BUFF_SIZE)<0){}
  else {
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_NOT_IN_COMM_BUFFER));
  }
#endif

  // always flush/invalidate to ensure we read the most recent value of *flag
  // keep reading it until it has the required value 
  do {
#ifdef _OPENMP
    #pragma omp flush  
#endif
    RC_cache_invalidate();
  } 
#ifdef SINGLEBITFLAGS
  while (RCCE_bit_value(flaga, (flag.location)%RCCE_FLAGS_PER_BYTE) != val);
#else
  while ((RCCE_FLAG_STATUS)(*flaga) != val);
#endif
  return(RCCE_SUCCESS);
}
  
//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_barrier
//--------------------------------------------------------------------------------------
// very simple, linear barrier 
//--------------------------------------------------------------------------------------
int RCCE_barrier(RCCE_COMM *comm) {
 
  int               counter, i;
  int               ROOT = 0;
  t_vchar           cyclechar;
  t_vchar           valchar;
  t_vcharp          gatherp;
  RCCE_FLAG_STATUS  cycle;
  int               bit_location = comm->gather.location%RCCE_FLAGS_PER_BYTE;

  counter = 0;
  gatherp = comm->gather.flag_addr;

  if (RCCE_debug_synch) 
    fprintf(STDERR,"UE %d has checked into barrier\n", RCCE_IAM);
  if (!comm || !(comm->initialized)) 
    RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_COMM_UNDEFINED); 
  // flip local barrier variable                                      
  RCCE_get_char(&cyclechar, gatherp, RCCE_IAM);
#ifdef SINGLEBITFLAGS
  cycle = RCCE_flip_bit_value(&cyclechar, bit_location);
#else
  cyclechar = (t_vchar)(!((unsigned int)(cyclechar)));
  cycle = (unsigned int)cyclechar;
#endif
  RCCE_put_char(gatherp, &cyclechar, RCCE_IAM);

  if (RCCE_IAM==comm->member[ROOT]) {
    // read "remote" gather flags; once all equal "cycle" (i.e counter==comm->size), 
    // we know all UEs have reached the barrier                   
    while (counter != comm->size) {
      // skip the first member (#0), because that is the ROOT         
      for (counter=i=1; i<comm->size; i++) {
        // copy flag values out of comm buffer                        
        RCCE_get_char(&valchar, gatherp, comm->member[i]);
#ifdef SINGLEBITFLAGS
        if (RCCE_bit_value(&valchar, bit_location) == cycle) counter++;
#else 
	if (valchar == cyclechar) counter++;
#endif
      }
    }
    // set release flags                                              
    for (i=1; i<comm->size; i++) 
      RCCE_flag_write(&(comm->release), cycle, comm->member[i]);
  }
  else {
    RCCE_wait_until(comm->release, cycle);
  }
  if (RCCE_debug_synch) fprintf(STDERR,"UE %d has cleared barrier\n", RCCE_IAM);  
  return(RCCE_SUCCESS);
}

void RCCE_fence() {
  return;
}
