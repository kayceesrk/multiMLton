//------------------------------------------------------------------------------------------
//
// Project       : bareMetal BIOS
// File name     : config.h
// Author        : mriepen
// Date          : 2008-06-24
// Revision      : 1.01
//
// Description   : Header file for config.c
//
// Revision history:
//
// mri 1.01 2008-06-24
// - Initial implementation
// rvdw 2010-08-25
// - support Linux and MS Baremetal environment
// - renamed file: SCC_API.c
// - modified by Rob F. Van der Wijngaart
//-------------------------------------------------------------------------------------------
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
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
typedef volatile unsigned char * t_vcharp;
#include "SCC_API.h"

// Variables
#ifndef MS_BAREMETAL
int NCMDeviceFD; // File descriptor for non-cachable memory (e.g. config regs).

#ifdef SHMADD_CACHEABLE
int DCMDeviceFD; // File descriptor for definitely cacheable memory
#endif

int MPBDeviceFD; // File descriptor for message passing buffers.
#endif
int PAGE_SIZE;   // shortcut to getpagesize()

// InitAPI opens the RCKMEM device drivers. This routine needs to be invoked
// once before using any other API functions! The successmessage can be disabled.
//
// Parameter: printMessages (0: No messages / 1: Messages enabled)
// Return value: %
//
void InitAPI(int printMessages) {
#ifndef MS_BAREMETAL
  // Open driver device "/dev/rckncm" for memory mapped register access

  if ((NCMDeviceFD=open("/dev/rckncm", O_RDWR|O_SYNC))<0) {
    perror("open");
    exit(-1);
  }

  // Open driver device "/dev/rckmpb" for message passing buffer access...
  if ((MPBDeviceFD=open("/dev/rckmpb", O_RDWR))<0) {
      perror("open");
      exit(-1);
  }

#ifdef SHMADD_CACHEABLE
  // Open driver device "/dev/rckdcm" for access to cacheable memory locations...
  if ((DCMDeviceFD=open("/dev/rckdcm", O_RDWR|O_SYNC))<0) {
    perror("open");
    exit(-1);
  }
#endif

#endif
  // Store page size
  PAGE_SIZE = getpagesize();

  // Success message
  if (printMessages) printf("Successfully opened RCKMEM driver devices!\n");
  return;
}

// SetConfigReg writes a value to a specified config register using a single write. Only use
// function to access memory locations that are not (!) performance critical (e.g. Tile-ID).
// Use MallocConfigReg() function for performance critical memory locations!
//
// Parameter: ConfigAddr                - Address of configuration register...
//            RegValue                  - Value to write to specified register...
//
void SetConfigReg(unsigned int ConfigAddr, int RegValue) {
#ifndef MS_BAREMETAL
  t_vcharp MappedAddr;
  unsigned int alignedAddr = ConfigAddr & (~(PAGE_SIZE-1));
  unsigned int pageOffset = ConfigAddr - alignedAddr;

  MappedAddr = (t_vcharp) mmap(NULL, PAGE_SIZE, PROT_WRITE|PROT_READ, MAP_SHARED, NCMDeviceFD, alignedAddr);
  if (MappedAddr == MAP_FAILED) {
          perror("mmap");
          exit(-1);
  }

  *(int*)(MappedAddr+pageOffset) = RegValue;
  munmap((void*)MappedAddr, PAGE_SIZE);
#else
  // no virtual address mapping under baremetal
  *(int*)(ConfigAddr) = RegValue;
#endif
  return;
}

// ReadConfigReg reads a value from a specified config register using a single read. Only use
// function to access memory locations that are not (!) performance critical (e.g. Tile-ID).
// Use MallocConfigReg() function for performance critical memory locations!
//
// Parameter: ConfigAddr                - Address of configuration register...
//
// Return value: Content of the specified config register
//
int ReadConfigReg(unsigned int ConfigAddr) {
#ifndef MS_BAREMETAL
  int result;
  t_vcharp MappedAddr;
  unsigned int alignedAddr = ConfigAddr & (~(PAGE_SIZE-1));
  unsigned int pageOffset = ConfigAddr - alignedAddr;

  MappedAddr = (t_vcharp) mmap(NULL, PAGE_SIZE, PROT_WRITE|PROT_READ, MAP_SHARED, NCMDeviceFD, alignedAddr);
  if (MappedAddr == MAP_FAILED) {
          perror("mmap");
          exit(-1);
  }

  result = *(int*)(MappedAddr+pageOffset);
  munmap((void*)MappedAddr, PAGE_SIZE);
  return result;
#else
  // no virtual address mapping under Baremetal
  return (*(int*)ConfigAddr);
#endif
}

// MallocConfigReg performs a memory map operation on ConfigAddr (physical address) and
// returns a virtual address that can be used in the application. Use this function to
// allocate memory locations that you access frequently!
//
// Parameter: ConfigAddr                - Physical address of configuration register.
//
// Return value: ConfigRegVirtualAddr   - Virtual address of configuration register.
//
int* MallocConfigReg(unsigned int ConfigAddr) {
#ifndef MS_BAREMETAL
  t_vcharp MappedAddr;
  unsigned int alignedAddr = ConfigAddr & (~(PAGE_SIZE-1));
  unsigned int pageOffset = ConfigAddr - alignedAddr;

  MappedAddr = (t_vcharp) mmap(NULL, PAGE_SIZE, PROT_WRITE|PROT_READ, MAP_SHARED,
                               NCMDeviceFD, alignedAddr);
  if (MappedAddr == MAP_FAILED) {
          perror("mmap");
          exit(-1);
  }

  return (int*)(MappedAddr+pageOffset);
#else
  // nu virtual address mapping under Baremetal
  return ((int*) ConfigAddr);
#endif
}



// SyncConfigReg makes sure a memory operation on a memory mapped Config register
// is flushed to disk for all other cores to see
//
// Return value: ConfigRegVirtualAddr   - Virtual address of configuration register.
//
int SyncConfigReg(t_vcharp page_start) {
#ifndef MS_BAREMETAL
  int error;

  error = msync((void *)page_start, PAGE_SIZE, MS_INVALIDATE);
  if (error) {
          perror("msync");
          return(1);
  }
#endif
  return (0);
}

// FreeConfigReg unmaps a memory location that has been mapped with the MallocConfigReg()
// function...
//
// Parameter: ConfigRegVirtualAddr      - Virtual address of configuration register.
//
void FreeConfigReg(int* ConfigRegVirtualAddr) {
#ifndef MS_BAREMETAL
  t_vcharp MappedAddr;
  unsigned int alignedAddr = (int)ConfigRegVirtualAddr & (~(PAGE_SIZE-1));
  munmap((void*)alignedAddr, PAGE_SIZE);
#endif
  return;
}

// MPBalloc allocates MPBSIZE bytes of MessagePassing buffer Memory at MPB_ADDR(x,y,core).
//
// Parameter: MPB                   - Pointer to MPB area (return value, virtal address)
//            x,y,core              - Position of tile (x,y) and core...
//
void MPBalloc(t_vcharp *MPB, int x, int y, int core, int isOwnMPB) {
  t_vcharp MappedAddr;
  // enable local MPB bypass (if trusted) by uncommenting next two lines and commenting
  // out the two after that
  //  unsigned int alignedAddr = (isOwnMPB?(MPB_OWN+(MPBSIZE*core)):MPB_ADDR(x,y,core)) & (~(PAGE_SIZE-1));
  //  unsigned int pageOffset = (isOwnMPB?(MPB_OWN+(MPBSIZE*core)):MPB_ADDR(x,y,core)) -alignedAddr;
  unsigned int alignedAddr = (MPB_ADDR(x,y,core)) & (~(PAGE_SIZE-1));
  unsigned int pageOffset = (MPB_ADDR(x,y,core)) - alignedAddr;
  if ((x>=NUM_COLS) || (y>=NUM_ROWS) || (core>=NUM_CORES)) {
    printf("MPBalloc: Invalid coordinates (x=%0d, y=%0d, core=%0d)\n", x,y,core);
    *MPB = NULL;
    return;
  }
#ifndef MS_BAREMETAL
  MappedAddr = (t_vcharp) mmap(NULL, MPBSIZE, PROT_WRITE|PROT_READ, MAP_SHARED, MPBDeviceFD, alignedAddr);
  if (MappedAddr == MAP_FAILED)
  {
          perror("mmap");
          exit(-1);
  }

  *MPB = MappedAddr+pageOffset;
#else
  // no virtual address mapping under baremetal
  *MPB = (t_vcharp) alignedAddr + pageOffset;
#endif
}


// MPBunalloc unallocates an allocated MPB area.
//
// Parameter: MPB             - Pointer to MPB area (virtual address)
//
void MPBunalloc(t_vcharp *MPB) {
#ifndef MS_BAREMETAL
  munmap((void*)*MPB, MPBSIZE);
#endif
  *MPB = NULL;
}

// SHMalloc allocates off-chip shared memory at SHMADDR
//
// Parameter: SHM   - Pointer to SHM area (return value, virtal address)
//
void SHMalloc(t_vcharp *SHM) {
  t_vcharp MappedAddr;

#ifndef MS_BAREMETAL
  unsigned int alignedAddr = (SHM_ADDR) & (~(PAGE_SIZE-1));
  unsigned int pageOffset = (SHM_ADDR) - alignedAddr;

#ifdef SHMDBG
  fprintf(stderr, "SHMSIZE: %x\n",SHMSIZE);
#endif

#ifdef SHMADD_CACHEABLE
  MappedAddr = (t_vcharp) mmap((void*)0x97f00000, SHMSIZE, PROT_WRITE|PROT_READ,
                               MAP_SHARED | MAP_FIXED, DCMDeviceFD, alignedAddr);
#ifdef SHMDBG
  fprintf(stderr, "Opened CACHEABLE\n");
#endif
  if (MappedAddr == (void*)0x97f00000)
  {
          perror("mmap");
          exit(-1);
  }
#else
  MappedAddr = (t_vcharp) mmap((void*)0x97f00000, SHMSIZE, PROT_WRITE|PROT_READ,
                               MAP_SHARED | MAP_FIXED, NCMDeviceFD, alignedAddr);
#ifdef SHMDBG
  fprintf(stderr, "Opened NONCACHEABLE\n");
#endif
  if (MappedAddr != (void*)0x97f00000)
  {
          perror("mmap");
          exit(-1);
  }
#endif

  *SHM = MappedAddr+pageOffset;
#else
  *SHM = (t_vcharp) SHM_ADDR;
#endif
}


// SHMunalloc unallocates all allocated SHM memory
//
// Parameter: SHM             - Pointer to SHM area (virtual address)
//
void SHMunalloc(t_vcharp *SHM) {
#ifndef MS_BAREMETAL
  munmap((void*)*SHM, SHMSIZE);
#endif
  *SHM = NULL;
}

#ifdef SHMADD_CACHEABLE
int DCMflush() {

//   printf("Flushing .... DCMDeviceFD: %d\n",DCMDeviceFD);
//   write(DCMDeviceFD,0,65536);
   write(DCMDeviceFD,0,0);
//   printf("after write Flushing .... DCMDeviceFD: %d\n",DCMDeviceFD);
   return 1;
}
#endif

