//***************************************************************************************
// Synchronized receive routines. 
//***************************************************************************************
//
// Author: Rob F. Van der Wijngaart
//         Intel Corporation
// Date:   12/22/2010
//
//***************************************************************************************
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
#include <stdlib.h>
#include <string.h>
#ifdef SCC
// include the next file instead of linking to facilitate easy inlining
#include "RCCE_memcpy.c"
#endif

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_send_general
//--------------------------------------------------------------------------------------
// Synchronized send function (gory and non-gory mode)
//--------------------------------------------------------------------------------------
static int RCCE_send_general(
  char *privbuf,    // source buffer in local private memory (send buffer)
  t_vcharp combuf,  // intermediate buffer in MPB
  size_t chunk,     // size of MPB available for this message (bytes)
  RCCE_FLAG *ready, // flag indicating whether receiver is ready
  RCCE_FLAG *sent,  // flag indicating whether message has been sent by source
  size_t size,      // size of message (bytes)
  int dest          // UE that will receive the message
  ) {

  char padline[RCCE_LINE_SIZE]; // copy buffer, used if message not multiple of line size
  size_t wsize,    // offset within send buffer when putting in "chunk" bytes
        remainder, // bytes remaining to be sent
        nbytes;    // number of bytes to be sent in single RCCE_put call
  char *bufptr;    // running pointer inside privbuf for current location

  // send data in units of available chunk size of comm buffer 
  for (wsize=0; wsize< (size/chunk)*chunk; wsize+=chunk) {
    bufptr = privbuf + wsize;
    nbytes = chunk;
    // copy private data to own comm buffer
    RCCE_put(combuf, (t_vcharp) bufptr, nbytes, RCCE_IAM);
    RCCE_flag_write(sent, RCCE_FLAG_SET, dest);
    // wait for the destination to be ready to receive a message          
    RCCE_wait_until(*ready, RCCE_FLAG_SET);
    RCCE_flag_write(ready, RCCE_FLAG_UNSET, RCCE_IAM);
  }

  remainder = size%chunk; 
  // if nothing is left over, we are done 
  if (!remainder) return(RCCE_SUCCESS);

  // send remainder of data--whole cache lines            
  bufptr = privbuf + (size/chunk)*chunk;
  nbytes = remainder - remainder%RCCE_LINE_SIZE;
  if (nbytes) {
    // copy private data to own comm buffer
    RCCE_put(combuf, (t_vcharp)bufptr, nbytes, RCCE_IAM);
    RCCE_flag_write(sent, RCCE_FLAG_SET, dest);
    // wait for the destination to be ready to receive a message          
    RCCE_wait_until(*ready, RCCE_FLAG_SET);
    RCCE_flag_write(ready, RCCE_FLAG_UNSET, RCCE_IAM);
  }
   
  remainder = remainder%RCCE_LINE_SIZE;
  if (!remainder) return(RCCE_SUCCESS);

  // remainder is less than a cache line. This must be copied into appropriately sized 
  // intermediate space before it can be sent to the receiver 
  bufptr = privbuf + (size/chunk)*chunk + nbytes;
  nbytes = RCCE_LINE_SIZE;
  // copy private data to own comm buffer 
#ifdef SCC
  memcpy_put(padline,bufptr,remainder);
#else
  memcpy(padline,bufptr,remainder);
#endif
  RCCE_put(combuf, (t_vcharp)padline, nbytes, RCCE_IAM);
  RCCE_flag_write(sent, RCCE_FLAG_SET, dest);
  // wait for the destination to be ready to receive a message          
  RCCE_wait_until(*ready, RCCE_FLAG_SET);
  RCCE_flag_write(ready, RCCE_FLAG_UNSET, RCCE_IAM);

  return(RCCE_SUCCESS);
}

#ifndef GORY
// this is the simplified synchronized message passing API      

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_send
//--------------------------------------------------------------------------------------
// send function for simplified API; use library-maintained variables for synchronization
//--------------------------------------------------------------------------------------
int RCCE_send(char *privbuf, size_t size, int dest) {
  if (dest<0 || dest >= RCCE_NP) 
    return(RCCE_error_return(RCCE_debug_comm,RCCE_ERROR_ID));
  else
    return(RCCE_send_general(privbuf, RCCE_buff_ptr, RCCE_chunk, 
                &RCCE_ready_flag[dest], &RCCE_sent_flag[RCCE_IAM], 
                size, dest));
}

#else
// this is the gory synchronized message passing API      

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_send
//--------------------------------------------------------------------------------------
// send function for simplified API; use user-supplied variables for synchronization
//--------------------------------------------------------------------------------------
int RCCE_send(char *privbuf, t_vcharp combuf, size_t chunk, RCCE_FLAG *ready, 
              RCCE_FLAG *sent, size_t size, int dest) {
  return(RCCE_send_general(privbuf, combuf, chunk, ready, sent, size, dest));
}
#endif
