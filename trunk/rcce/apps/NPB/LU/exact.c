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

#define ce(m,n) ce[m-1+5*(n-1)]
#define u000ijk(m) u000ijk[m-1]

void exact( int i, int j, int k, double *u000ijk ) {

//   compute the exact solution at (i,j,k)

      int m;
      double xi, eta, zeta;

      xi   = ( double ) (i - 1) / (nx0 - 1);
      eta  = ( double ) (j - 1) / (ny0 - 1);
      zeta = ( double ) (k - 1) / (nz0 - 1);

      for (m=1; m<=5; m++)
         u000ijk(m) =  ce(m,1)
             + ce(m,2) * xi
             + ce(m,3) * eta
             + ce(m,4) * zeta
             + ce(m,5) * xi * xi
             + ce(m,6) * eta * eta
             + ce(m,7) * zeta * zeta
             + ce(m,8) * xi * xi * xi
             + ce(m,9) * eta * eta * eta
             + ce(m,10) * zeta * zeta * zeta
             + ce(m,11) * xi * xi * xi * xi
             + ce(m,12) * eta * eta * eta * eta
             + ce(m,13) * zeta * zeta * zeta * zeta;

      return;
}

