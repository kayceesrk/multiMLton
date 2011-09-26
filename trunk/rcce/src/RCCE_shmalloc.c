//***************************************************************************************
// Off-chip shared memory allocation routines. 
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

//......................................................................................
// GLOBAL VARIABLES USED BY THE LIBRARY
//......................................................................................
static RCCE_BLOCK_S RCCE_space;   // data structure used for tracking MPB memory blocks
static RCCE_BLOCK_S *RCCE_spacep; // pointer to RCCE_space
#ifdef _OPENMP
#pragma omp threadprivate (RCCE_space, RCCE_spacep)
#endif

// END GLOBAL VARIABLES USED BY THE LIBRARY
//......................................................................................

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_shmalloc_init
//--------------------------------------------------------------------------------------
// initialize memory allocator
//--------------------------------------------------------------------------------------
void RCCE_shmalloc_init(
  t_vcharp mem, // pointer to shared space that is to be managed by allocator
  size_t size   // size (bytes) of managed space
) {

  // create one block containing all memory for truly dynamic memory allocator
  RCCE_spacep = &RCCE_space;
  RCCE_spacep->tail = (RCCE_BLOCK *) malloc(sizeof(RCCE_BLOCK));
  RCCE_spacep->tail->free_size = size;
  RCCE_spacep->tail->space = mem;
  /* make a circular list by connecting tail to itself */
  RCCE_spacep->tail->next = RCCE_spacep->tail;
#ifdef SHMDBG 
printf("%s: %d: RCCE_spacep->tail->free_size, RCCE_spacep->tail->space: %x %x\n",
__FILE__, __LINE__,RCCE_spacep->tail->free_size, RCCE_spacep->tail->space);
#endif
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_shmalloc
//--------------------------------------------------------------------------------------
// Allocate memory in off-chip shared memory. This is a collective call that should be
// issued by all participating cores if consistent results are required. All cores will
// allocate space that is exactly overlapping. Alternatively, determine the beginning of
// the off-chip shared memory on all cores and subsequently let just one core do all the
// allocating and freeing. It can then pass offsets to other cores who need to know what
// shared memory regions were involved.
//--------------------------------------------------------------------------------------
t_vcharp RCCE_shmalloc(
  size_t size // requested space
) {

  t_vcharp result;

  // simple memory allocator, loosely based on public domain code developed by
  // Michael B. Allen and published on "The Scripts--IT /Developers Network".
  // Approach: 
  // - maintain linked list of pointers to memory. A block is either completely
  //   malloced (free_size = 0), or completely free (free_size > 0).
  //   The space field always points to the beginning of the block
  // - malloc: traverse linked list for first block that has enough space    
  // - free: Check if pointer exists. If yes, check if the new block should be 
  //         merged with neighbors. Could be one or two neighbors.

  RCCE_BLOCK *b1, *b2, *b3;   // running pointers for blocks              

  // Unlike the MPB, the off-chip shared memory is uncached by default, so can
  // be allocated in any increment, not just the cache line size
  if (size==0) return 0;

  // always first check if the tail block has enough space, because that
  // is the most likely. If it does and it is exactly enough, we still
  // create a new block that will be the new tail, whose free space is 
  // zero. This acts as a marker of where free space of predecessor ends   
//printf("RCCE_spacep->tail: %x\n",RCCE_spacep->tail);
  b1 = RCCE_spacep->tail;
  if (b1->free_size >= size) {
    // need to insert new block; new order is: b1->b2 (= new tail)         
    b2 = (RCCE_BLOCK *) malloc(sizeof(RCCE_BLOCK));
    b2->next      = b1->next;
    b1->next      = b2;
    b2->free_size = b1->free_size-size;
    b2->space     = b1->space + size;
    b1->free_size = 0;
    // need to update the tail                                             
    RCCE_spacep->tail = b2;
    return(b1->space);
  }

  // tail didn't have enough space; loop over whole list from beginning    
  while (b1->next->free_size < size) {
    if (b1->next == RCCE_spacep->tail) {
      return NULL; // we came full circle 
    }
    b1 = b1->next;
  }

  b2 = b1->next;
  if (b2->free_size > size) { // split block; new block order: b1->b2->b3  
    b3            = (RCCE_BLOCK *) malloc(sizeof(RCCE_BLOCK));
    b3->next      = b2->next; // reconnect pointers to add block b3        
    b2->next      = b3;       //     "         "     "  "    "    "        
    b3->free_size = b2->free_size - size; // b3 gets remainder free space  
    b3->space     = b2->space + size; // need to shift space pointer       
  } 
  b2->free_size = 0;          // block b2 is completely used               
  return (b2->space);
}

//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_shfree
//--------------------------------------------------------------------------------------
// Deallocate memory in off-chip shared memory. Also collective, see RCCE_shmalloc
//--------------------------------------------------------------------------------------
void RCCE_shfree(
  t_vcharp ptr // pointer to data to be freed
  ) {

  RCCE_BLOCK *b1, *b2, *b3;   // running block pointers                    
  int j1, j2;                 // booleans determining merging of blocks    

  // loop over whole list from the beginning until we locate space ptr     
  b1 = RCCE_spacep->tail;
  while (b1->next->space != ptr && b1->next != RCCE_spacep->tail) { 
    b1 = b1->next;
  }

  // b2 is target block whose space must be freed    
  b2 = b1->next;              
  // tail either has zero free space, or hasn't been malloc'ed             
  if (b2 == RCCE_spacep->tail) return;      

  // reset free space for target block (entire block)                      
  b3 = b2->next;
  b2->free_size = b3->space - b2->space;

  // determine with what non-empty blocks the target block can be merged   
  j1 = (b1->free_size>0 && b1!=RCCE_spacep->tail); // predecessor block    
  j2 = (b3->free_size>0 || b3==RCCE_spacep->tail); // successor block      

  if (j1) {
    if (j2) { // splice all three blocks together: (b1,b2,b3) into b1      
      b1->next = b3->next;
      b1->free_size +=  b3->free_size + b2->free_size;
      if (b3==RCCE_spacep->tail) RCCE_spacep->tail = b1;
      free(b3);
    } 
    else {    // only merge (b1,b2) into b1                                
      b1->free_size += b2->free_size;
      b1->next = b3;
    }
    free(b2);
  } 
  else {
    if (j2) { // only merge (b2,b3) into b2                                
      b2->next = b3->next;
      b2->free_size += b3->free_size;
      if (b3==RCCE_spacep->tail) RCCE_spacep->tail = b2;
      free(b3);
    } 
  }
}


//--------------------------------------------------------------------------------------
// FUNCTION: RCCE_shflush
//--------------------------------------------------------------------------------------
// this function makes sure that data in shared memory can be read or written
//--------------------------------------------------------------------------------------
void RCCE_shflush() {
#ifdef _OPENMP
#pragma omp flush
#else

#endif
}  

