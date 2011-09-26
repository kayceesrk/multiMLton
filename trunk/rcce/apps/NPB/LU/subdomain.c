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
#include "applu_share.h"

void subdomain() {

      int mm, errorcode=99;

//c   x dimension
      mm   = nx0%xdim;
      if (row <= mm) {
        nx = nx0/xdim + 1;
        ipt = (row-1)*nx;
      }
      else {
        nx = nx0/xdim;
        ipt = (row-1)*nx + mm;
      }

//c   y dimension
      mm   = ny0%ydim;
      if (col <= mm) {
        ny = ny0/ydim + 1;
        jpt = (col-1)*ny;
      }
      else {
        ny = ny0/ydim;
        jpt = (col-1)*ny + mm;
      }

//c   z dimension
      nz = nz0;

//c---------------------------------------------------------------------
//c   check the sub-domain size
//c---------------------------------------------------------------------
      if ( (nx < 4) || (ny < 4) || (nz < 4) ) {

        printf("     SUBDOMAIN SIZE IS TOO SMALL - \n");
        printf("     ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS\n");
        printf("     SO THAT NX, NY AND NZ ARE GREATER THAN OR EQUAL\n");
        printf("     TO 4. THEY ARE CURRENTLY: %3d %3d %3d\n", nx, ny, nz);
      }

      if ( (nx > isiz1) || (ny > isiz2) || (nz > isiz3) ) {
         printf("     SUBDOMAIN SIZE IS TOO LARGE - \n");
         printf("     ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS\n");
         printf("     SO THAT NX, NY AND NZ ARE LESS THAN OR EQUAL TO \n");
         printf("     ISIZ1, ISIZ2 AND ISIZ3 RESPECTIVELY.  THEY ARE\n");
         printf("     CURRENTLY: %3d %3d %3d\n", nx, ny, nz);
      }


//c---------------------------------------------------------------------
//c   set up the start and end in i and j extents for all processors
//c---------------------------------------------------------------------
      ist = 1;
      iend = nx;
      if (north == -1) ist = 2;
      if (south == -1) iend = nx - 1;

      jst = 1;
      jend = ny;
      if (west == -1) jst = 2;
      if (east == -1) jend = ny - 1;

      return;
}


