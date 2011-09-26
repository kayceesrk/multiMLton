This directory contains software to build an run RCCE programs.
Users do not directly invoke "make." Building and executing is done
through the script rccerun (AKA rerun).

RCCE library files
------------------
RCCE_admin.c: initialization and wrap up functions plus simple utilities
RCCE_comm.c:  communication routines
RCCE_debug.c: code to (un)set debug levels, and to print debug messages
RCCE_emulator_driver.c: driver program for RCCE emulator based on OpenMP
RCCE_malloc.c: dynamic memory allocator
RCCE_synch.c:  synchronization routines (flags and barrier)
RCCE_qsort.c: quick sort (stolen); can be replaced with system qsort on
              regular Linux systems

RCCE applications
-----------------
RCCE_shift.c
RCCE_stencil.c

New applications should contain the function RCCE_app, returning an integer
error code. Modify .makefile.hidden to reflect the new program dependencies.

#  
#  Copyright 2010 Intel Corporation
#  
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#  
#         http://www.apache.org/licenses/LICENSE-2.0
#  
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#  
