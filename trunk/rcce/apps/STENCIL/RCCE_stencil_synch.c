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
#include <math.h>

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
#define FABS(x) ((x)>0?(x):(-x))

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

int RCCE_APP(int argc, char **argv){

  float     a[NXNY], checksum, vchecksum, error;
  int       i, offset, iter=10, itermax, id;
  int       ID, ID_right, ID_left;
  int       NTILES1;
  double    time;
  char      *result;

  RCCE_init(&argc, &argv);
  
  NTILES1 = RCCE_num_ues()-1;
  ID = RCCE_ue();
  printf("My UE is %d\n", ID); 

  ID_right = (ID+1)%RCCE_num_ues();
  ID_left = (ID-1+RCCE_num_ues())%RCCE_num_ues();

  if (NX%8) {
    printf("Grid width should be multiple of 8: %d\n", NX);
    exit(1);
  }
  if (argc>1) iter=atoi(*++argv);
  if (!ID) printf("Core %d Executing %d iterations\n", ID, iter);
  itermax = iter;

  /* initialize array a on all tiles; this stuffs a into private caches  */

  for (offset=0,       i=0; i<NXNY; i++) a[i+offset] = 0.0;
  if (ID == 0) 
     for (offset=0,    i=0; i<NX;   i++) a[i+offset] = 1.0;
  if (ID == NTILES1) 
     for (offset=NXNY1,i=0; i<NX;   i++) a[i+offset] = 2.0;

  /* main loop */

  RCCE_barrier(&RCCE_COMM_WORLD);
  time = RCCE_wtime();

  while ((iter--)>0){
  
    /* start with copying fringe data to neighboring tiles; we need to
       group semantic send/recv pairs together to avoid deadlock         */
    if (ID_right!=0) RCCE_send((char*)(&a[NXNY2]), NX*sizeof(float), ID_right);
    if (ID != 0)     RCCE_recv((char*)(&a[0]),     NX*sizeof(float), ID_left);
    if (ID!=0)       RCCE_send((char *)(&a[NX]),    NX*sizeof(float), ID_left);
    if (ID_right!=0) RCCE_recv((char *)(&a[NXNY1]), NX*sizeof(float), ID_right);

    /* apply the stencil operation                                       */
    for (i=0; i<NXNY2; i++) {
      a[i+O3] +=
         W1*a[i+O1] + W2*a[i+O2] + W3*a[i+O3] + W4*a[i+O4] + W5*a[i+O5];
    }
  }

  RCCE_barrier(&RCCE_COMM_WORLD);
  time = RCCE_wtime()-time;

  /* print result strip by strip; avoid output mangling by letting one core print */

  checksum = 0.0;
  if (ID==0) {
    for (id=0; id<=NTILES1; id++) {
      if (id!=ID) RCCE_recv((char *)a, NXNY*sizeof(float), id);     
      int start = NX; int end = NXNY1;
      if (id==0) start = 0;
      if (id == NTILES1) end = NXNY;
      for (offset=0, i=start; i<end; i++) {
        if (!(i%NX)) {printf("\n");}
        printf("%1.4f ",a[i+offset]); 
        checksum += (a[i+offset]*a[i+offset]);
      }
    }
    checksum = sqrt(checksum/NXNY);
  }
  else RCCE_send((char *)a, NXNY*sizeof(float), 0);

  if (ID==0) { 
    printf("\n");
    printf("Total time: %lf\n", time); fflush(NULL);
    printf("Checksum = %lf", checksum);
    if (NTILES1==3 && itermax == 10) {
      vchecksum = 0.635851;
      error = FABS(vchecksum - checksum);
      if (error < 1.e-5) result = "SUCCESSFUL";
      else               result = "FAILED";
      printf("  Verification value = %lf, error = %lf; run %s",
             vchecksum, error, result);
    }
    if (NTILES1==15 && itermax == 100) {
      vchecksum = 1.019079;
      error = FABS(vchecksum - checksum);
      if (error < 1.e-5) result = "SUCCESSFUL";
      else               result = "FAILED";
      printf("  Verification value = %lf, error = %lf; run %s",
             vchecksum, error, result);
    }
    if (NTILES1==47 && itermax == 1000) {
      vchecksum = 1.741555;
      error = FABS(vchecksum - checksum);
      if (error < 1.e-5) result = "SUCCESSFUL";
      else               result = "FAILED";
      printf("  Verification value = %lf, error = %lf; run %s",
             vchecksum, error, result);
    }

  }  

  RCCE_finalize();
  return(0);
}
