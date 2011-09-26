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
#include "RCCE.h"
int MPI_Send(void *, int, int, int, int, RCCE_COMM);
int MPI_Recv(void *, int, int, int, int, RCCE_COMM, int *);
int MPI_Comm_size(RCCE_COMM, int *);
int MPI_Comm_rank(RCCE_COMM, int *);
int MPI_Abort(RCCE_COMM, int);

#define   I_SEND    0
#define   I_RECV    1

#ifdef STDC_HEADERS
void HPL_rollN
(
   HPL_T_panel *                    PBCST,
   int *                            IFLAG,
   HPL_T_panel *                    PANEL,
   const int                        N,
   double *                         U,
   const int                        LDU,
   const int *                      IPLEN,
   const int *                      IPMAP,
   const int *                      IPMAPM1
)
#else
void HPL_rollN
( PBCST, IFLAG, PANEL, N, U, LDU, IPLEN, IPMAP, IPMAPM1 )
   HPL_T_panel *                    PBCST;
   int *                            IFLAG;
   HPL_T_panel *                    PANEL;
   const int                        N;
   double *                         U;
   const int                        LDU;
   const int *                      IPLEN;
   const int *                      IPMAP;
   const int *                      IPMAPM1;
#endif
{
/* 
 * Purpose
 * =======
 *
 * HPL_rollN rolls the local arrays containing the local pieces of U, so
 * that on exit to this function  U  is replicated in every process row.
 * In addition, this function probe for the presence of the column panel
 * and forwards it when available.
 *
 * Arguments
 * =========
 *
 * PBCST   (local input/output)          HPL_T_panel *
 *         On entry,  PBCST  points to the data structure containing the
 *         panel (to be broadcast) information.
 *
 * IFLAG   (local input/output)          int *
 *         On entry, IFLAG  indicates  whether or not  the broadcast has
 *         already been completed.  If not,  probing will occur, and the
 *         outcome will be contained in IFLAG on exit.
 *
 * PANEL   (local input/output)          HPL_T_panel *
 *         On entry,  PANEL  points to the data structure containing the
 *         panel (to be rolled) information.
 *
 * N       (local input)                 const int
 *         On entry, N specifies the number of columns of  U.  N must be
 *         at least zero.
 *
 * U       (local input/output)          double *
 *         On entry,  U  is an array of dimension (LDU,*) containing the
 *         local pieces of U in each process row.
 *
 * LDU     (local input)                 const int
 *         On entry, LDU specifies the local leading dimension of U. LDU
 *         should be at least  MAX(1,IPLEN[NPROW]).
 *
 * IPLEN   (global input)                const int *
 *         On entry, IPLEN is an array of dimension NPROW+1.  This array
 *         is such that IPLEN[i+1] - IPLEN[i] is the number of rows of U
 *         in each process row.
 *
 * IPMAP   (global input)                const int *
 *         On entry, IMAP  is an array of dimension  NPROW.  This  array
 *         contains  the  logarithmic mapping of the processes. In other
 *         words,  IMAP[myrow]  is the absolute coordinate of the sorted
 *         process.
 *
 * IPMAPM1 (global input)                const int *
 *         On entry,  IMAPM1  is an array of dimension NPROW. This array
 *         contains  the inverse of the logarithmic mapping contained in
 *         IMAP: For i in [0.. NPROW) IMAPM1[IMAP[i]] = i.
 *
 * ---------------------------------------------------------------------
 */ 
/*
 * .. Local Variables ..
 */
   MPI_Datatype               type[2];
   MPI_Status                 status;
   MPI_Request                request;
   MPI_Comm                   comm;
   int                        Cmsgid=MSGID_BEGIN_PFACT, ibufR, ibufS,
                              ierr=MPI_SUCCESS, il, k, l, lengthR,
                              lengthS, mydist, myrow, next, npm1, nprow,
                              partner, prev, block;
   double *buf;

// IN THE CURRENT VERSION OF THE CODE + PARAMETERS THIS ROUTINE IS NEVER REACHED
/* ..
 * .. Executable Statements ..
 */
   if( N <= 0 ) return;

   npm1 = ( nprow = PANEL->grid->nprow ) - 1; myrow = PANEL->grid->myrow;
   comm = PANEL->grid->col_comm;
/*
 * Rolling phase
 */
   mydist = IPMAPM1[myrow];
   prev   = IPMAP[MModSub1( mydist, nprow )];
   next   = IPMAP[MModAdd1( mydist, nprow )];
 
   for( k = 0; k < npm1; k++ )
   {
      l = (int)( (unsigned int)(k) >> 1 );
 
      if( ( ( mydist + k ) & 1 ) != 0 )
      {
         il      = MModAdd( mydist, l,   nprow );
         lengthS = IPLEN[il+1] - ( ibufS = IPLEN[il] ); 
         il      = MModSub( mydist, l+1, nprow );
         lengthR = IPLEN[il+1] - ( ibufR = IPLEN[il] ); partner = prev;
      }
      else
      {
         il    = MModSub( mydist, l,   nprow );
         lengthS = IPLEN[il+1] - ( ibufS = IPLEN[il] ); 
         il    = MModAdd( mydist, l+1, nprow );
         lengthR = IPLEN[il+1] - ( ibufR = IPLEN[il] ); partner = next;
      }
 
      if( lengthR > 0 )
      {
//         if( ierr == MPI_SUCCESS )
//            ierr =   MPI_Type_vector( N, lengthR, LDU, MPI_DOUBLE,
//                                      &type[I_RECV] );
//         if( ierr == MPI_SUCCESS )
//	            ierr =   MPI_Type_commit( &type[I_RECV] );
//         if( ierr == MPI_SUCCESS )
////            ierr =   MPI_Irecv( Mptr( U, ibufR, 0, LDU ), 1, type[I_RECV],
////                                partner, Cmsgid, comm, &request );
//            ierr =   MPI_Recv( Mptr( U, ibufR, 0, LDU ), 1, type[I_RECV],
//                                partner, Cmsgid, comm, &status );
//RvdW: avoid derived data type for this communication
           buf = (double *)malloc(sizeof(double)*N*lengthR);
           MPI_Recv(buf, N*lengthR, MPI_DOUBLE, partner, Cmsgid, comm, &status);
           for (block=0; block<N; block++) 
              memcpy(Mptr( U, ibufR, 0, LDU )+block*LDU, buf+block*LDU,
                     sizeof(double)*lengthR);
           free(buf);
      }
 
      if( lengthS > 0 )
      {
//         if( ierr == MPI_SUCCESS )
//            ierr =   MPI_Type_vector( N, lengthS, LDU, MPI_DOUBLE,
//                                      &type[I_SEND] );
//         if( ierr == MPI_SUCCESS )
//            ierr =   MPI_Type_commit( &type[I_SEND] );
//         if( ierr == MPI_SUCCESS )
//            ierr =   MPI_Send( Mptr( U, ibufS, 0, LDU ), 1, type[I_SEND],
//                               partner, Cmsgid, comm );
//         if( ierr == MPI_SUCCESS )
//            ierr =   MPI_Type_free(   &type[I_SEND] );
//RvdW: avoid derived data type for this communication
           buf = (double *)malloc(sizeof(double)*N*lengthS);
           for (block=0; block<N; block++) 
              memcpy(buf+block*LDU, Mptr( U, ibufS, 0, LDU )+block*LDU, 
                     sizeof(double)*lengthS);
           MPI_Send(buf, N*lengthS, MPI_DOUBLE, partner, Cmsgid, comm);
           free(buf);
      }

//      if( lengthR > 0 )
//      {
//         if( ierr == MPI_SUCCESS )
//            ierr =   MPI_Wait( &request, &status );
//         if( ierr == MPI_SUCCESS )
//            ierr =   MPI_Type_free(   &type[I_RECV] );
//      }
/*
 * Probe for column panel - forward it when available
 */
      if( *IFLAG == HPL_KEEP_TESTING ) (void) HPL_bcast( PBCST, IFLAG );
   }

   if( ierr != MPI_SUCCESS )
   { HPL_pabort( __LINE__, "HPL_rollN", "MPI call failed" ); }
/*
 * End of HPL_rollN
 */
}
