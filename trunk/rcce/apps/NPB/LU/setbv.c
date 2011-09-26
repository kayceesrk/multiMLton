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
#include "applu_share.h"
#include "applu_macros.h"

void setbv() {

      int i, j, k;
      int iglob, jglob;
      int one = 1;

//c---------------------------------------------------------------------
//c   set the dependent variable values along the top and bottom faces
//c---------------------------------------------------------------------
      for ( j = 1;  j<= ny;  j++) {
         jglob = jpt + j;
         for ( i = 1;  i<= nx;  i++) {
           iglob = ipt + i;
           exact(iglob,jglob,one,&u(1,i,j,1));
           exact(iglob,jglob,nz,&u(1,i,j,nz));
         }
      }

//c---------------------------------------------------------------------
//c   set the dependent variable values along north and south faces
//c---------------------------------------------------------------------
      if (west == -1) {
         for ( k = 1;  k<= nz;  k++) {
            for ( i = 1;  i<= nx;  i++) {
               iglob = ipt + i;
               exact(iglob,one,k,&u(1,i,1,k));
            }
         }
      }

      if (east == -1) {
          for ( k = 1;  k<= nz;  k++) {
             for ( i = 1;  i<= nx;  i++) {
                iglob = ipt + i;
                exact(iglob,ny0,k,&u(1,i,ny,k));
             }
          }
      }

//c---------------------------------------------------------------------
//c   set the dependent variable values along east and west faces
//c---------------------------------------------------------------------
      if (north == -1) {
         for ( k = 1;  k<= nz;  k++) {
            for ( j = 1;  j<= ny;  j++) {
               jglob = jpt + j;
               exact(one,jglob,k,&u(1,1,j,k));
            }
         }
      }

      if (south == -1) {
         for ( k = 1;  k<= nz;  k++) {
            for ( j = 1;  j<= ny;  j++) {
               jglob = jpt + j;
               exact(nx0,jglob,k,&u(1,nx,j,k));
            }
         }
      }

      return;
}

