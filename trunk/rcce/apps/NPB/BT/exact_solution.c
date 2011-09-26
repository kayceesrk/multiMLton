//---------------------------------------------------------------------
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
//---------------------------------------------------------------------
#include "header.h"

void exact_solution(double xi,double eta,double zeta,double dtemp[]) {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     this function returns the exact solution at point xi, eta, zeta  
//---------------------------------------------------------------------

      int m;
#define dtemp(m) dtemp[m-1]

      for (m = 1; m <= 5; m++) {
         dtemp(m) =  ce(m,1) +
           xi*(ce(m,2) + xi*(ce(m,5) + xi*(ce(m,8) + xi*ce(m,11)))) +
           eta*(ce(m,3) + eta*(ce(m,6) + eta*(ce(m,9) + eta*ce(m,12))))+
           zeta*(ce(m,4) + zeta*(ce(m,7) + zeta*(ce(m,10) + 
           zeta*ce(m,13))));
      }

      return;
}


