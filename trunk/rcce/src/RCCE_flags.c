//**************************************************************************************
// Flag manipulation and access functions. 
// Single-bit and whole-cache-line flags are sufficiently different that we provide
// separate implementations of all the flag routines for each case
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

//......................................................................................
// GLOBAL VARIABLES USED BY THE LIBRARY
//......................................................................................
RCCE_FLAG_LINE RCCE_flags;
//......................................................................................
// END GLOBAL VARIABLES USED BY THE LIBRARY
//......................................................................................

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_flag_alloc
//--------------------------------------------------------------------------------------
// allocate space for one flag. Since multiple fit on a single cache line, we only
// need to allocate new MPB space when all the existing lines are completely filled. A
// flag line is a data structure that contains an array ("flag") of size RCCE_LINE_SIZE 
// characters. Each element in "flag" corresponds to a flag being in use  (value is 1) 
// or not (value is 0). The actual value of the flag is stored in the MPB line pointed 
// to be the field "line_address," at the corresponding bit/byte location as in field 
// "flag."
//--------------------------------------------------------------------------------------
int    RCCE_flag_alloc(RCCE_FLAG *flag) {
  RCCE_FLAG_LINE *flagp;
  t_vcharp flag_addr;
  int c, loc;

  // find the head of the data structure that administers the flag variables
  flagp = &RCCE_flags;
  while (flagp->members == RCCE_FLAGS_PER_LINE && flagp->next) {
    flagp = flagp->next;
  }

  // if this is a new flag line, need to allocate MPB for it 
  if (!flagp->line_address) flagp->line_address = RCCE_malloc(RCCE_LINE_SIZE);
  if (!flagp->line_address) return(RCCE_error_return(RCCE_debug_synch,
                                   RCCE_ERROR_FLAG_NOT_ALLOCATED));

  if (flagp->members < RCCE_FLAGS_PER_LINE) {
    // there is space in this line for a new flag; find first open slot    
    for (loc=0; loc<RCCE_FLAGS_PER_LINE; loc++) {
      flag_addr =  flagp->line_address + loc/RCCE_FLAGS_PER_BYTE;
      if (!((int)(flagp->flag[loc]))) {
        flagp->flag[loc] = (char) ((unsigned int) 1);
        flagp->members++;
        flag->location = loc;
        flag->line_address = flagp->line_address;
        flag->flag_addr = flag_addr;
        return(RCCE_SUCCESS);
      }
    }
  }
  else {
    // must create new flag line if last one was full
    flagp->next = (RCCE_FLAG_LINE *) malloc(sizeof(RCCE_FLAG_LINE));
    if (!(flagp->next)) return(RCCE_error_return(RCCE_debug_synch,
                                   RCCE_ERROR_FLAG_NOT_ALLOCATED));
    flagp = flagp->next;
    flagp->line_address = RCCE_malloc(RCCE_LINE_SIZE);
    if (!(flagp->line_address)) return(RCCE_error_return(RCCE_debug_synch,
                                   RCCE_ERROR_FLAG_NOT_ALLOCATED));
    // initialize the flag line 
    flagp->members=1;
    flagp->next = NULL;
    for (c=1; c<RCCE_LINE_SIZE; c++) flagp->flag[c] = (char)((unsigned int) 0);
    
    // set first flag field to indicate the corresponding flag is now in use
    flagp->flag[0] = (char)((unsigned int) 1);
    flag->location = 0;
    flag->line_address = flagp->line_address;
    flag->flag_addr = flag->line_address;
  } 
  return(RCCE_SUCCESS);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_flag_free
//--------------------------------------------------------------------------------------
// free space for one flag. Since multiple fit on a single cache line, we only
// need to free claimed MPB space when the all existing lines are completely emptied.
//--------------------------------------------------------------------------------------
int    RCCE_flag_free(RCCE_FLAG *flag) {

  RCCE_FLAG_LINE *flagp, *flagpminus1 = NULL;
  int loc;

  // check wether flag exists, and whether the location field is valid 
  if (!flag || flag->location < 0) 
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_UNDEFINED));
  // find flag line in globally maintained structure                   
  flagp  = &RCCE_flags;
  while (flagp->next && flag->line_address != flagp->line_address) {
    flagpminus1 = flagp;
    flagp = flagp->next;
  }
  if (flag->line_address != flagp->line_address) 
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_UNDEFINED));

  // error checking is done
  flagp->members--; 
  loc = flag->location;
#ifdef SINGLEBITFLAGS
  RCCE_flip_bit_value(flagp->flag+loc/RCCE_FLAGS_PER_BYTE,loc%RCCE_FLAGS_PER_BYTE);
#else
  flagp->flag[flag->location] = (char) ((unsigned int) 0);
#endif
  // something special happens if we've emptied an entire line         
  if (flagp->members==0) {
    if (flagpminus1) {
      // there is a predecessor; splice out current flag line from linked list
      RCCE_free(flagp->line_address);
      flagpminus1->next = flagp->next;
      free(flagp); 
    } 
    // if there is a successor but no predecessor, do nothing          
  }
  // invalidate location field to make sure we won't free again by mistake
  flag->location = -1;
  flag->line_address = NULL;

  return(RCCE_SUCCESS);
}

#ifdef SINGLEBITFLAGS

//////////////////////////////////////////////////////////////////
// LOCKING SYNCHRONIZATION USING ONE BIT PER FLAG 
//////////////////////////////////////////////////////////////////

// next three utility functions are only used by the library, not the user. We assume 
// there will never be errrors, so we do not return any error code. "location" of a 
// flag bit // inside a cache line is reckoned from the most significant (leftmost) 
// bit. Within a word, flag zero is also in the leftmost bit

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_bit_value
//--------------------------------------------------------------------------------------
// return status of single bit flag at a specific location within a word
//--------------------------------------------------------------------------------------
RCCE_FLAG_STATUS RCCE_bit_value(t_vcharp word, int location) {
  unsigned char mask = (char)(1<<location);
  return (int)(((*word) & mask)>>location);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_flip_bit_value
//--------------------------------------------------------------------------------------
// flip single bit in a word and return value of changed bit. The location is that 
// of the bit inside the word. 
//--------------------------------------------------------------------------------------
RCCE_FLAG_STATUS RCCE_flip_bit_value(t_vcharp word, int location) {
  unsigned char mask = (char)(1<<location);
  (*word) ^= mask;
  return (int)(((*word) &mask)>>location);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_write_bit_value
//--------------------------------------------------------------------------------------
// write single bit in word and return value of changed bit. The location is that 
// of the bit inside the word.
//--------------------------------------------------------------------------------------
int RCCE_write_bit_value(t_vcharp word, int location, RCCE_FLAG_STATUS val) {
  unsigned char mask;
  switch (val) {
  case RCCE_FLAG_UNSET: mask = (char)(~(1<<location));
                          (*word) &= mask;
                          break;
  case RCCE_FLAG_SET:   mask = (char)(1<<location);
                          (*word) |= mask;
                          break;
  }
  return (RCCE_SUCCESS);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_probe
//--------------------------------------------------------------------------------------
// nonblocking function to check if a flag has been set
//--------------------------------------------------------------------------------------
int RCCE_probe(RCCE_FLAG flag) {
#ifdef _OPENMP
  #pragma omp flush  
#endif
  RC_cache_invalidate();
  return (RCCE_bit_value(flag.flag_addr, flag.location%RCCE_FLAGS_PER_BYTE) == RCCE_FLAG_SET);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_flag_write
//--------------------------------------------------------------------------------------
// This is the core flag manipulation routine. It requires locking to guarantee atomic
// access while updating one of a line of flags.
//--------------------------------------------------------------------------------------
int    RCCE_flag_write(RCCE_FLAG *flag, RCCE_FLAG_STATUS val, int ID) {
  t_vchar val_array[1];

#ifdef GORY
  // check input parameters 
  if (!flag || flag->location < 0 || flag->location > RCCE_FLAGS_PER_LINE)  
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_UNDEFINED));
  if (val != RCCE_FLAG_UNSET && val != RCCE_FLAG_SET)
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_STATUS_UNDEFINED));
  if (ID<0 || ID>=RCCE_NP) 
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_ID));
#endif

  // acquire lock to make sure nobody else fiddles with the flags on the target core 
  RCCE_acquire_lock(ID);
  // copy word from MPB containing flag to private memory
  RCCE_get_char(val_array, flag->flag_addr, ID);

  // overwrite single bit within local copy of flag word
  RCCE_write_bit_value(val_array, (flag->location)%RCCE_FLAGS_PER_BYTE, val);

  // write copy back to the MPB
  RCCE_put_char(flag->flag_addr, val_array, ID);
  
  // release write lock for the flags on the target core 
  RCCE_release_lock(ID);

  return(RCCE_SUCCESS);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_flag_read
//--------------------------------------------------------------------------------------
// This routine is rarely needed. We typically only read a flag when we're waiting for
// it to change value (function RCCE_wait_until). Reading does not require locking. The
// moment the target flag we're trying to read changes value, it is OK to read and
// return that value
//--------------------------------------------------------------------------------------
int    RCCE_flag_read(RCCE_FLAG flag, RCCE_FLAG_STATUS *val, int ID) {
  t_vchar val_array[1];

#ifdef GORY
  if (flag.location < 0 || flag.location > RCCE_FLAGS_PER_LINE)  
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_UNDEFINED));
  if (!val)   return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_VAL_UNDEFINED));
  if (ID<0 || ID>=RCCE_NP) 
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_ID));
#endif

// Should be able to use same technique as in RCCE_wait_until, i.e., should not need 
// to copy out of MPB first. However, this function is not time critical
  RCCE_get_char(val_array, flag.flag_addr, ID);
  *val = RCCE_bit_value(val_array, (flag.location)%RCCE_FLAGS_PER_BYTE);
  return(RCCE_SUCCESS);
}

#else

//////////////////////////////////////////////////////
// LOCKLESS SYNCHRONIZATION USING ONE BYTE PER FLAG //
//////////////////////////////////////////////////////

// next three utility functions are only used by the library, not the user. We assume 
// there will never be errrors, so we do not return any error code. "location" of a 
// flag byte inside a cache line is reckoned from the most significant (leftmost) 
// byte. 

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_write_byte_value
//--------------------------------------------------------------------------------------
// return status of single byte flag at a specific location within cache line
//--------------------------------------------------------------------------------------
RCCE_FLAG_STATUS RCCE_byte_value(t_vcharp word) {
  return ((int)(*word));
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_write_byte_value
//--------------------------------------------------------------------------------------
// write single byte in cache line.
//--------------------------------------------------------------------------------------
int RCCE_write_byte_value(t_vcharp word, RCCE_FLAG_STATUS val) {
  volatile unsigned char cval = (char)val;
  return(RCCE_put_char(word, &cval, RCCE_IAM));
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_probe
//--------------------------------------------------------------------------------------
// nonblocking function to check if a flag has been set
//--------------------------------------------------------------------------------------
int RCCE_probe(RCCE_FLAG flag) {
#ifdef _OPENMP
  #pragma omp flush  
#endif
  RC_cache_invalidate();
  return (RCCE_byte_value(flag.flag_addr) == RCCE_FLAG_SET);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_flag_write
//--------------------------------------------------------------------------------------
// This is the core flag manipulation routine. 
//--------------------------------------------------------------------------------------
int    RCCE_flag_write(RCCE_FLAG *flag, RCCE_FLAG_STATUS val, int ID) {

#ifdef GORY
  // check input parameters 
  if (!flag || flag->location < 0 || flag->location > RCCE_LINE_SIZE)  
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_UNDEFINED));
  if (val != RCCE_FLAG_UNSET && val != RCCE_FLAG_SET)
     return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_STATUS_UNDEFINED));
  if (ID<0 || ID>=RCCE_NP) 
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_ID));
#endif

  RCCE_put_char(flag->flag_addr, (t_vcharp) &val, ID);
  return(RCCE_SUCCESS);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_flag_read
//--------------------------------------------------------------------------------------
// This routine is rarely needed. We typically only read a flag when we're waiting for
// it to change value (function RCCE_wait_until). Reading does not require locking. The
// moment the target flag we're trying to read changes value, it is OK to read and
// return that value
//--------------------------------------------------------------------------------------
int    RCCE_flag_read(RCCE_FLAG flag, RCCE_FLAG_STATUS *val, int ID) {
  volatile unsigned char val_loc;

#ifdef GORY
  if (flag.location < 0 || flag.location > RCCE_LINE_SIZE)  
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_FLAG_UNDEFINED));
  if (!val)   return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_VAL_UNDEFINED));
  if (ID<0 || ID>=RCCE_NP) 
    return(RCCE_error_return(RCCE_debug_synch,RCCE_ERROR_ID));
#endif

// Should be able to use same technique as in RCCE_wait_until, i.e., should not need 
// to copy out of MPB first. However, this function is not time critical
  RCCE_get_char(&val_loc, flag.flag_addr, ID);
  *val = val_loc;
  return(RCCE_SUCCESS);
}

#endif
