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
//---------------------------------------------------------------------
#include "header.h"

void  add() {

//---------------------------------------------------------------------
//---------------------------------------------------------------------

//---------------------------------------------------------------------
//     addition of update to the vector u
//---------------------------------------------------------------------

      int  c, i, j, k, m;

      for (c = 1; c <= ncells; c++) {
         for (k = start(3,c); k <= cell_size(3,c)-end(3,c)-1; k++) {
            for (j = start(2,c); j <= cell_size(2,c)-end(2,c)-1; j++) {
               for (i = start(1,c); i <= cell_size(1,c)-end(1,c)-1; i++) {
                  for (m = 1; m <= 5; m++) {
                     u(m,i,j,k,c) = u(m,i,j,k,c) + rhs(m,i,j,k,c);
                  }
               }
            }
         }
      }

      return;
}
