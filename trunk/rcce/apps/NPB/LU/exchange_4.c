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
#include <stdlib.h>
#include <stdio.h>
#include "directions.h"
#include "applu_share.h"
#include "applu_macros.h"

#define g(i,j) g[i+(isiz2+2)*(j)]
#define h(i,j) h[i+(isiz2+2)*(j)]
#define COMM_SIZE 1024

void exchange_4(double *g, double *h, 
                int ibeg, int ifin1, int jbeg, int jfin1) {

      int i, j;
      size_t chunk;
      double bufin[COMM_SIZE], bufout[COMM_SIZE];
      int ny2 = ny + 2;

//c---------------------------------------------------------------------
//c   communicate in the east and west directions
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c   receive from east
//c---------------------------------------------------------------------
      if (jfin1 == ny) {
        RCCE_recv((char*)bufin, 2*nx*sizeof(double), east);

        for (int ib=0, i=1; i<=nx; i++) {
          g(i,ny+1) = bufin[ib++];
          h(i,ny+1) = bufin[ib++];
        }

      }

//c---------------------------------------------------------------------
//c   send west
//c---------------------------------------------------------------------
      if (jbeg == 1) {
        for (int ib=0,i=1; i<=nx; i++) {
          bufout[ib++] = g(i,1);
          bufout[ib++] = h(i,1);
        }

        RCCE_send((char*)bufout, 2*nx*sizeof(double), west);
      }

//c---------------------------------------------------------------------
//c   communicate in the south and north directions
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c   receive from south
//c---------------------------------------------------------------------
      if (ifin1 == nx) {
        RCCE_recv((char*)bufin, 2*ny2*sizeof(double), south);

        for (int ib=0,j=0; j<=ny+1; j++) {
          g(nx+1,j) = bufin[ib++];
          h(nx+1,j) = bufin[ib++];
        }

      }

//c---------------------------------------------------------------------
//c   send north
//c---------------------------------------------------------------------
      if (ibeg == 1) {
        for (int ib=0,j=0; j<=ny+1; j++) {
          bufout[ib++] = g(1,j);
          bufout[ib++] = h(1,j);
        }
        RCCE_send((char*)bufout, 2*ny2*sizeof(double), north);
      }

      return;
}

