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
// FUNCTION: RCCE_recv_general
//--------------------------------------------------------------------------------------
// Synchronized receive function (gory and non-gory mode)
//--------------------------------------------------------------------------------------
static int RCCE_recv_general(
  char *privbuf,    // destination buffer in local private memory (receive buffer)
  t_vcharp combuf,  // intermediate buffer in MPB
  size_t chunk,     // size of MPB available for this message (bytes)
  RCCE_FLAG *ready, // flag indicating whether receiver is ready
  RCCE_FLAG *sent,  // flag indicating whether message has been sent by source
  size_t size,      // size of message (bytes)
  int source,       // UE that sent the message
  int *test         // if 1 upon entry, do nonblocking receive; if message available
                    // set to 1, otherwise to 0
  ) {

  char padline[RCCE_LINE_SIZE]; // copy buffer, used if message not multiple of line size
  size_t wsize,   // offset within receive buffer when pulling in "chunk" bytes
       remainder, // bytes remaining to be received
       nbytes;    // number of bytes to be received in single RCCE_get call
  int first_test; // only use first chunk to determine if message has been received yet
  char *bufptr;   // running pointer inside privbuf for current location

  first_test = 1;

  // receive data in units of available chunk size of MPB 
  for (wsize=0; wsize< (size/chunk)*chunk; wsize+=chunk) {
    bufptr = privbuf + wsize;
    nbytes = chunk;
    // if function is called in test mode, check if first chunk has been sent already. 
    // If so, proceed as usual. If not, exit immediately 
    if (*test && first_test) {
      first_test = 0;
      if (!(*test = RCCE_probe(*sent))) return(RCCE_SUCCESS);
    }
    RCCE_wait_until(*sent, RCCE_FLAG_SET);
    RCCE_flag_write(sent, RCCE_FLAG_UNSET, RCCE_IAM);
    // copy data from local MPB space to private memory 
    RCCE_get((t_vcharp)bufptr, combuf, nbytes, source);

    // tell the source I have moved data out of its comm buffer
    RCCE_flag_write(ready, RCCE_FLAG_SET, source);
  }

  remainder = size%chunk; 
  // if nothing is left over, we are done 
  if (!remainder) return(RCCE_SUCCESS);

  // receive remainder of data--whole cache lines               
  bufptr = privbuf + (size/chunk)*chunk;
  nbytes = remainder - remainder%RCCE_LINE_SIZE;
  if (nbytes) {
    // if function is called in test mode, check if first chunk has been sent already. 
    // If so, proceed as usual. If not, exit immediately 
    if (*test && first_test) {
      first_test = 0;
      if (!(*test = RCCE_probe(*sent))) return(RCCE_SUCCESS);
    }
    RCCE_wait_until(*sent, RCCE_FLAG_SET);
    RCCE_flag_write(sent, RCCE_FLAG_UNSET, RCCE_IAM);
    // copy data from local MPB space to private memory 
    RCCE_get((t_vcharp)bufptr, combuf, nbytes, source);

    // tell the source I have moved data out of its comm buffer
    RCCE_flag_write(ready, RCCE_FLAG_SET, source);
  }

  remainder = remainder%RCCE_LINE_SIZE;
  if (!remainder) return(RCCE_SUCCESS);

  // remainder is less than cache line. This must be copied into appropriately sized 
  // intermediate space before exact number of bytes get copied to the final destination 
  bufptr = privbuf + (size/chunk)*chunk + nbytes;
  nbytes = RCCE_LINE_SIZE;
    // if function is called in test mode, check if first chunk has been sent already. 
    // If so, proceed as usual. If not, exit immediately 
    if (*test && first_test) {
      first_test = 0;
      if (!(*test = RCCE_probe(*sent))) return(RCCE_SUCCESS);
    }
    RCCE_wait_until(*sent, RCCE_FLAG_SET);
    RCCE_flag_write(sent, RCCE_FLAG_UNSET, RCCE_IAM);
    // copy data from local MPB space to private memory   
    RCCE_get((t_vcharp)padline, combuf, nbytes, source);
#ifdef SCC
    memcpy_get(bufptr,padline,remainder);
#else
    memcpy(bufptr,padline,remainder);
#endif

    // tell the source I have moved data out of its comm buffer
    RCCE_flag_write(ready, RCCE_FLAG_SET, source);

  return(RCCE_SUCCESS);
}

#ifndef GORY
// this is the simplified synchronized message passing API      

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_recv
//--------------------------------------------------------------------------------------
// recv function for simplified API; use library-maintained variables for synchronization
// and set the test variable to 0 (ignore)
//--------------------------------------------------------------------------------------
int RCCE_recv(char *privbuf, size_t size, int source) {
  int ignore;
  if (source<0 || source >= RCCE_NP) 
    return(RCCE_error_return(RCCE_debug_comm,RCCE_ERROR_ID));
  else {
    ignore = 0;
    return(RCCE_recv_general(privbuf, RCCE_buff_ptr, RCCE_chunk, 
                &RCCE_ready_flag[RCCE_IAM], &RCCE_sent_flag[source], 
                size, source, &ignore));
  }
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_recv_test
//--------------------------------------------------------------------------------------
// recv_test function for simplified API; use library-maintained variables for 
// synchronization and set the test variable to 1 (do test)
//--------------------------------------------------------------------------------------
int RCCE_recv_test(char *privbuf, size_t size, int source, int *test) {
  if (source<0 || source >= RCCE_NP) 
    return(RCCE_error_return(RCCE_debug_comm,RCCE_ERROR_ID));
  else {
    /* make sure the test flag is set, regardless of input value */
    *test = 1;
    return(RCCE_recv_general(privbuf, RCCE_buff_ptr, RCCE_chunk, 
                &RCCE_ready_flag[RCCE_IAM], &RCCE_sent_flag[source], 
                size, source, test));
  }
}
#else
// this is the gory synchronized message passing API      

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_recv
//--------------------------------------------------------------------------------------
// recv function for simplified API; use user-supplied variables for synchronization
// and set the test variable to 0 (ignore)
//--------------------------------------------------------------------------------------
int RCCE_recv(char *privbuf, t_vcharp combuf, size_t chunk, RCCE_FLAG *ready, 
              RCCE_FLAG *sent, size_t size, int source) {
  int ignore;
  return(RCCE_recv_general(privbuf, combuf, chunk, ready, sent, size, source,
                           &ignore));
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_recv_test
//--------------------------------------------------------------------------------------
// recv_test function for simplified API; use user-supplied variables for 
// synchronization and set the test variable to 1 (do test)
//--------------------------------------------------------------------------------------
int RCCE_recv_test(char *privbuf, t_vcharp combuf, size_t chunk, RCCE_FLAG *ready, 
              RCCE_FLAG *sent, size_t size, int source, int *test) {
  /* make sure the test flag is set, regardless of input value */
  *test = 1;
  return(RCCE_recv_general(privbuf, combuf, chunk, ready, sent, size, source,
                           test));
}
#endif

