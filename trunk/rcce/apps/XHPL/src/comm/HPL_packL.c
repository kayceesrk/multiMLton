/* 
 * -- High Performance Computing Linpack Benchmark (HPL)                
 *    HPL - 2.0 - September 10, 2008                          
 *    Antoine P. Petitet                                                
 *    University of Tennessee, Knoxville                                
 *    Innovative Computing Laboratory                                 
 *    (C) Copyright 2000-2008 All Rights Reserved                       
 *                                                                      
 * -- Copyright notice and Licensing terms:                             
 *                                                                      
 * Redistribution  and  use in  source and binary forms, with or without
 * modification, are  permitted provided  that the following  conditions
 * are met:                                                             
 *                                                                      
 * 1. Redistributions  of  source  code  must retain the above copyright
 * notice, this list of conditions and the following disclaimer.        
 *                                                                      
 * 2. Redistributions in binary form must reproduce  the above copyright
 * notice, this list of conditions,  and the following disclaimer in the
 * documentation and/or other materials provided with the distribution. 
 *                                                                      
 * 3. All  advertising  materials  mentioning  features  or  use of this
 * software must display the following acknowledgement:                 
 * This  product  includes  software  developed  at  the  University  of
 * Tennessee, Knoxville, Innovative Computing Laboratory.             
 *                                                                      
 * 4. The name of the  University,  the name of the  Laboratory,  or the
 * names  of  its  contributors  may  not  be used to endorse or promote
 * products  derived   from   this  software  without  specific  written
 * permission.                                                          
 *                                                                      
 * -- Disclaimer:                                                       
 *                                                                      
 * THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
 * OR  CONTRIBUTORS  BE  LIABLE FOR ANY  DIRECT,  INDIRECT,  INCIDENTAL,
 * SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL DAMAGES  (INCLUDING,  BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA OR PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,  OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 * ---------------------------------------------------------------------
 */ 
/*
 * Include files
 */
#include "hpl.h"

#ifdef STDC_HEADERS
int HPL_packL
(
   HPL_T_panel *                    PANEL,
   const int                        INDEX,
   const int                        LEN,
   const int                        IBUF
)
#else
int HPL_packL
( PANEL, INDEX, LEN, IBUF )
   HPL_T_panel *                    PANEL;
   const int                        INDEX;
   const int                        LEN;
   const int                        IBUF;
#endif
{
/* 
 * Purpose
 * =======
 *
 * HPL_packL forms  the MPI data type for the panel to be broadcast.
 * Successful  completion  is  indicated  by  the  returned  error  code
 * MPI_SUCCESS.
 *
 * Arguments
 * =========
 *
 * PANEL   (input/output)                HPL_T_panel *
 *         On entry,  PANEL  points to the  current panel data structure
 *         being broadcast.
 *
 * INDEX   (input)                       const int
 *         On entry,  INDEX  points  to  the  first entry of the  packed
 *         buffer being broadcast.
 *
 * LEN     (input)                       const int
 *         On entry, LEN is the length of the packed buffer.
 *
 * IBUF    (input)                       const int
 *         On entry, IBUF  specifies the panel buffer/count/type entries
 *         that should be initialized.
 *
 * ---------------------------------------------------------------------
 */ 
          /* HPL_USE_MPI_DATATYPE not defined - Oops, there is a bug
             somewhere, so, just in case  and until I find it ... */
   return( MPI_SUCCESS );   
/*
 * End of HPL_packL
 */
}
