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
typedef volatile unsigned char * t_vcharp;
#include "SCC_API.h"
#include <stdio.h>

// Main routine: This app reads and writes from/to the MPB
int main(int argc, char *argv []) {

  int tmp, x, y, z, offset;

  // Initialize API
  InitAPI(0);
  
  // Find out who I am...
  tmp=ReadConfigReg(CRB_OWN+MYTILEID); 
  x=(tmp>>3) & 0x0f; // bits 06:03
  y=(tmp>>7) & 0x0f; // bits 10:07
  z=(tmp   ) & 0x07; // bits 02:00

  // Allocate Message Passing Buffer
  t_vcharp MPB;
  MPBalloc(&MPB, x, y, z, 1); 
  if (!MPB) {
     printf("Unable to allocate MPB for core %d of Tile x=%d, y= %d! Exiting.\n", z, x, y); fflush(NULL);
     return 255;
  }

    
  // zap own MPB
  for (offset=0; offset < 0x2000; offset+=8)
     *(volatile unsigned long long int*)(MPB+offset) = 0;

  // Clear test&set register write. Next read-access will read "1" (lock granted).
  SetConfigReg(CRB_ADDR(x,y)+((z)?LOCK1:LOCK0), 1); 

  return 0;
}
