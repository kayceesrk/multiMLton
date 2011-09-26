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
#define COMM_SIZE 1024

void exchange_5(double *g, int ibeg, int ifin1) {


      int k;
      size_t chunk;
      double bufin[COMM_SIZE], bufout[COMM_SIZE];

//c---------------------------------------------------------------------
//c   communicate in the south and north directions
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c   receive from south
//c---------------------------------------------------------------------
      if (ifin1 == nx) {
        RCCE_recv((char*)bufin, nz*sizeof(double), south);

        for (k=1; k<=nz; k++) g(nx+1,k) = bufin[k-1];
      }

//c---------------------------------------------------------------------
//c   send north
//c---------------------------------------------------------------------
      if (ibeg == 1) {
        for (k=1; k<=nz; k++) bufout[k-1] = g(1,k);

        RCCE_send((char*)bufout, nz*sizeof(double), north);
      }

      return;
}

