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

#define g(m,i,j,k) g[m-1+5*((i+1)+(isiz1+4)*((j+1)+(isiz2+4)*(k-1)))]

void exchange_3(double *g,int iex) {

      int i, j, k;
      size_t chunk;
      double buffer[5*2*isiz2*isiz3];

      if (iex == 0) {

//c---------------------------------------------------------------------
//c   communicate in the south and north directions
//c---------------------------------------------------------------------

//c---------------------------------------------------------------------
//c   send south
//c---------------------------------------------------------------------
      if (south != -1) {
        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            buffer[ib++] = g(1,nx-1,j,k) ;
            buffer[ib++] = g(2,nx-1,j,k) ;
            buffer[ib++] = g(3,nx-1,j,k) ;
            buffer[ib++] = g(4,nx-1,j,k) ;
            buffer[ib++] = g(5,nx-1,j,k) ;
            buffer[ib++] = g(1,nx,j,k);
            buffer[ib++] = g(2,nx,j,k);
            buffer[ib++] = g(3,nx,j,k);
            buffer[ib++] = g(4,nx,j,k);
            buffer[ib++] = g(5,nx,j,k);
          }
        }

        RCCE_send((char*)buffer, 10*ny*nz*sizeof(double), south);
     }

//c---------------------------------------------------------------------
//c   receive from north
//c---------------------------------------------------------------------
      if (north != -1) {
        RCCE_recv((char*)buffer, 10*ny*nz*sizeof(double), north);

        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            g(1,-1,j,k) = buffer[ib++];
            g(2,-1,j,k) = buffer[ib++];
            g(3,-1,j,k) = buffer[ib++];
            g(4,-1,j,k) = buffer[ib++];
            g(5,-1,j,k) = buffer[ib++];
            g(1,0,j,k) = buffer[ib++];
            g(2,0,j,k) = buffer[ib++];
            g(3,0,j,k) = buffer[ib++];
            g(4,0,j,k) = buffer[ib++];
            g(5,0,j,k) = buffer[ib++];
          }
        }
       }

//c---------------------------------------------------------------------
//c   send north
//c---------------------------------------------------------------------
      if (north != -1) {
        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            buffer[ib++] = g(1,2,j,k);
            buffer[ib++] = g(2,2,j,k);
            buffer[ib++] = g(3,2,j,k);
            buffer[ib++] = g(4,2,j,k);
            buffer[ib++] = g(5,2,j,k);
            buffer[ib++] = g(1,1,j,k);
            buffer[ib++] = g(2,1,j,k);
            buffer[ib++] = g(3,1,j,k);
            buffer[ib++] = g(4,1,j,k);
            buffer[ib++] = g(5,1,j,k);
          }
        }
        RCCE_send((char*)buffer, 10*ny*nz*sizeof(double), north);
      }

//c---------------------------------------------------------------------
//c   receive from south
//c---------------------------------------------------------------------
      if (south != -1) {
        RCCE_recv((char*)buffer, 10*ny*nz*sizeof(double), south);

        for (int ib=0,k=1; k<=nz; k++) {
          for (j=1; j<=ny; j++) {
            g(1,nx+2,j,k) = buffer[ib++];
            g(2,nx+2,j,k) = buffer[ib++];
            g(3,nx+2,j,k) = buffer[ib++];
            g(4,nx+2,j,k) = buffer[ib++];
            g(5,nx+2,j,k) = buffer[ib++];
            g(1,nx+1,j,k) = buffer[ib++];
            g(2,nx+1,j,k) = buffer[ib++];
            g(3,nx+1,j,k) = buffer[ib++];
            g(4,nx+1,j,k) = buffer[ib++];
            g(5,nx+1,j,k) = buffer[ib++];
          }
        }
      }

    }

    else {

//c---------------------------------------------------------------------
//c   communicate in the east and west directions
//c---------------------------------------------------------------------
//c---------------------------------------------------------------------
//c   send east
//c---------------------------------------------------------------------
      if (east != -1) {
        for (int ib=0,k=1; k<=nz; k++) {
          for (i=1; i<=nx; i++) {
            buffer[ib++] = g(1,i,ny-1,k);
            buffer[ib++] = g(2,i,ny-1,k);
            buffer[ib++] = g(3,i,ny-1,k);
            buffer[ib++] = g(4,i,ny-1,k);
            buffer[ib++] = g(5,i,ny-1,k);
            buffer[ib++] = g(1,i,ny,k);
            buffer[ib++] = g(2,i,ny,k);
            buffer[ib++] = g(3,i,ny,k);
            buffer[ib++] = g(4,i,ny,k);
            buffer[ib++] = g(5,i,ny,k);
          }
        }
        RCCE_send((char*)buffer, 10*nx*nz*sizeof(double), east);
    }
      
//c---------------------------------------------------------------------
//c   receive from west
//c---------------------------------------------------------------------
      if (west != -1) {
        RCCE_recv((char*)buffer, 10*nx*nz*sizeof(double), west);

        for (int ib=0,k=1; k<=nz; k++) {
          for (i=1; i<=nx; i++) {
            g(1,i,-1,k) = buffer[ib++];
            g(2,i,-1,k) = buffer[ib++];
            g(3,i,-1,k) = buffer[ib++];
            g(4,i,-1,k) = buffer[ib++];
            g(5,i,-1,k) = buffer[ib++];
            g(1,i,0,k) = buffer[ib++];
            g(2,i,0,k) = buffer[ib++];
            g(3,i,0,k) = buffer[ib++];
            g(4,i,0,k) = buffer[ib++];
            g(5,i,0,k) = buffer[ib++];
          }
        }

      }

//c---------------------------------------------------------------------
//c   send west
//c---------------------------------------------------------------------
      if (west != -1) {
          for (int ib=0,k=1; k<=nz; k++) {
            for (i=1; i<=nx; i++) {
              buffer[ib++] = g(1,i,2,k);
              buffer[ib++] = g(2,i,2,k);
              buffer[ib++] = g(3,i,2,k);
              buffer[ib++] = g(4,i,2,k);
              buffer[ib++] = g(5,i,2,k);
              buffer[ib++] = g(1,i,1,k);
              buffer[ib++] = g(2,i,1,k);
              buffer[ib++] = g(3,i,1,k);
              buffer[ib++] = g(4,i,1,k);
              buffer[ib++] = g(5,i,1,k);
            }
          }
        RCCE_send((char*)buffer, 10*nx*nz*sizeof(double), west);
      }

//c---------------------------------------------------------------------
//c   receive from east
//c---------------------------------------------------------------------
      if (east != -1) {
        RCCE_recv((char*)buffer, 10*nx*nz*sizeof(double), east);

        for (int ib=0,k=1; k<=nz; k++) {
          for (i=1; i<=nx; i++) {
            g(1,i,ny+2,k) = buffer[ib++];
            g(2,i,ny+2,k) = buffer[ib++];
            g(3,i,ny+2,k) = buffer[ib++];
            g(4,i,ny+2,k) = buffer[ib++];
            g(5,i,ny+2,k) = buffer[ib++];
            g(1,i,ny+1,k) = buffer[ib++];
            g(2,i,ny+1,k) = buffer[ib++];
            g(3,i,ny+1,k) = buffer[ib++];
            g(4,i,ny+1,k) = buffer[ib++];
            g(5,i,ny+1,k) = buffer[ib++];
          }
        }

      }

    }

    return;
}
