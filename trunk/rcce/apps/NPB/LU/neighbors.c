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
#include "RCCE.h"

void neighbors() {

//c---------------------------------------------------------------------
//c     figure out the neighbors and their wrap numbers for each processor
//c---------------------------------------------------------------------

      south = -1;
      east  = -1;
      north = -1;
      west  = -1;

      if (row  >1)    north = id -1;
      else            north = -1;

      if (row < xdim) south = id + 1;
      else            south = -1;

      if (col > 1)    west = id- xdim;
      else            west = -1;

      if (col < ydim) east = id + xdim;
      else            east = -1;

      return;
}
