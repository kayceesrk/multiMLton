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
long long RC_global_clock();
#include <stdio.h>

/* hardwired predefined constants */
#define NX      16
#define NY      25
#define NXNY    ((NX)*(NY))
#define NXNY1   ((NX)*(NY-1))
#define NXNY2   ((NX)*(NY-2))

#define O1      0
#define O2      NX-1
#define O3      NX
#define O4      NX+1
#define O5      2*(NX)
#define W1      0.25
#define W2      0.25
#define W4      0.25
#define W5      0.25
#define W3      -1.0

/* initialization;
   resulting 2D data set represented by a[] is as follows, where
   first and last row of each strip are fixed boundary values (1's
   and 2's) or fringe data copied from strips on neighboring tiles.


           1 1 1 1 1 1 1 1 1 1
           0 0 0 0 0 0 0 0 0 0
           ...................       CORE 0
           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           

           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           ...................       CORE 1
           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0


           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           ...................       CORE 2
           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0


           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           ...................       CORE NTILES-1
           0 0 0 0 0 0 0 0 0 0
           2 2 2 2 2 2 2 2 2 2

*/

int RCCE_APP(int argc, char **argv) {

  /* statically allocated space sits in off-chip private memory          */
  float     a[NXNY], *buff;
  int       i, offset, iter=10, tile;
  int       MY_ID;
  int       NTILES1;
  double    time;
  RCCE_FLAG flag0, flag1;

  RCCE_init(&argc, &argv);
  
  NTILES1 = RCCE_num_ues()-1;
  MY_ID = RCCE_ue();

  if (NX%8) {
    printf("Grid width should be multiple of 8: %d\n", NX);
    exit(1);
  }
  if (argc>1) iter=atoi(*++argv);
  if (MY_ID==0) printf("Executing %d iterations\n", iter);

    /* allocate space on the comm buffer                                 */
  buff = (float *) RCCE_malloc(sizeof(float)*2*NX); 
  /* Allocate flags to coordinate comm.                                  */                                 
  if (RCCE_flag_alloc(&flag0)) return(1);
  if (RCCE_flag_alloc(&flag1)) return(1);

  /* initialize array a on all tiles; this stuffs a into private caches  */
  for (offset=0,       i=0; i<NXNY; i++) a[i+offset] = 0.0;
  if (MY_ID == 0) 
     for (offset=0,    i=0; i<NX;   i++) a[i+offset] = 1.0;
  if (MY_ID == NTILES1) 
     for (offset=NXNY1,i=0; i<NX;   i++) a[i+offset] = 2.0;

  /* put in a barrier so everybody can be sure to have initialized       */
  RCCE_barrier(&RCCE_COMM_WORLD);

  /* main loop */

  if (MY_ID==0) time = RCCE_wtime();

  while ((iter--)>0){
  
    /* start with copying fringe data to neighboring tiles               */
    if (MY_ID!=NTILES1) {
      /* Initialize neighbor flag to zero                                */
      RCCE_flag_write(&flag0, RCCE_FLAG_UNSET, MY_ID+1); 
      /* copy private data to shared comm buffer of neighbor             */
      RCCE_put((t_vcharp)(&buff[0]), (t_vcharp)(&a[NXNY2]), NX*sizeof(float), MY_ID+1);
      RCCE_flag_write(&flag0, RCCE_FLAG_SET, MY_ID+1); 
    }
    if (MY_ID != 0) {
      /* Initialize neighbor flag to zero                                */
      RCCE_flag_write(&flag1, 0, MY_ID-1); 
      /* copy private data to shared comm buffer of neighbor             */
      RCCE_put((t_vcharp)(&buff[NX]), (t_vcharp)(&a[NX]), NX*sizeof(float), MY_ID-1);
      RCCE_flag_write(&flag1, RCCE_FLAG_SET, MY_ID-1); 
    }

    /* Make sure the data has been recvd and copy data out of buffer(s)  */
    if (MY_ID!=NTILES1) {
      RCCE_wait_until(flag1, RCCE_FLAG_SET);
      RCCE_get((t_vcharp)(&a[NXNY1]), (t_vcharp)(&buff[NX]), NX*sizeof(float),MY_ID);
    }

    if (MY_ID!=0) {
      RCCE_wait_until(flag0, RCCE_FLAG_SET);
      RCCE_get((t_vcharp)(&a[0]), (t_vcharp)(&buff[0]), NX*sizeof(float),MY_ID);
    }

    /* apply the stencil operation                                       */
    for (i=0; i<NXNY2; i++) {
      a[i+O3] +=
         W1*a[i+O1] + W2*a[i+O2] + W3*a[i+O3] + W4*a[i+O4] + W5*a[i+O5];
    }
  }
  RCCE_barrier(&RCCE_COMM_WORLD);
  if (MY_ID==0) { 
    time = RCCE_wtime()-time;
  }

  /* print result strip by strip; this would not be done on RC */
  for (int id=0; id<=NTILES1; id++) {
    RCCE_barrier(&RCCE_COMM_WORLD);
    if (MY_ID==id) {
      int start = NX; int end = NXNY1;
      if (MY_ID==0) start = 0;
      if (MY_ID == NTILES1) end = NXNY;
      for (offset=0, i=start; i<end; i++) {
        if (!(i%NX)) printf("\n");
//        comment out next line and uncomment subsequent three to print error
        printf("%f ",a[i+offset]);
//        int jj=i/NX+(MY_ID*(NY-1));
//        double aexact=1.0+(double)jj/((NTILES1+1)*(NY-1));
//        printf("%f ",a[i+offset]-aexact);
      }
    }
  }
  RCCE_barrier(&RCCE_COMM_WORLD);
  if (MY_ID==0) { 
    printf("\nTotal time: %lf\n", time);
  }

  RCCE_finalize();

  return(0);
}
