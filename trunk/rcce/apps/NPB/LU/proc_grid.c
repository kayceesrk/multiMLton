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
#include <math.h>
#include "applu_share.h"

void proc_grid() {

//c   set up a two-d grid for processors: column-major ordering of unknowns
//c   NOTE: assumes a power-of-two number of processors

      xdim   = pow(2,(ndim/2));
      if (ndim%2 == 1) xdim = xdim + xdim;
      ydim   = num/xdim;

      row    = id%xdim + 1;
      col    = id/xdim + 1;

      return;
}



