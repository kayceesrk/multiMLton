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
#ifdef _OPENMP
#include "omp.h"
#endif
#include "timers.h"
#define elapsed(n) elapsed[n-1]
#define start_time(n)   start_time[n-1]
      
void timer_clear(int np){
      
      int n = np;
      elapsed(n) = 0.0;
      return;
}


void timer_start(int np) {

      int n = np;

      start_time(n) = RCCE_wtime();

      return;
}

void timer_stop(int np) {

      int n = np;

      double t, now;
      now = RCCE_wtime();
      t = now - start_time(n);
      elapsed(n) = elapsed(n) + t;

      return;
}


double timer_read(int np) {

      int n = np;      
      return( elapsed(n));
}

