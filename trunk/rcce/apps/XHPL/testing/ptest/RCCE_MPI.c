#include "RCCE.h"
#include "RCCE_lib.h"
#include <stdlib.h>
#include <string.h>

// We abuse the MPI_Datatype to convery information about the size of the
// data type
int MPI_Send(void *buf, int count, int type_size, int dest, int tag, RCCE_COMM comm) {

  return(RCCE_send((char *)buf, count*type_size, comm.member[dest]));
}

int MPI_Recv(void *buf, int count, int type_size, int source, int tag, 
             RCCE_COMM comm, int *status) {
  return(RCCE_recv((char *)buf, count*type_size, comm.member[source]));
}

int MPI_Comm_size(RCCE_COMM comm, int *size) {
  *size = comm.size;
  return(RCCE_SUCCESS);
}

int MPI_Comm_rank(RCCE_COMM comm, int *rank) {
  *rank = comm.my_rank;
  return(RCCE_SUCCESS);
}

int MPI_Init(int *argc, char ***argv) {
  return(RCCE_init(argc, argv));
}

int MPI_Finalize(void) {
  return(RCCE_finalize());
}

double MPI_Wtime(void) {
  // Somehow, this does not work; must replace MPI_Wtime with RCCE_wtime directly  
  return(RCCE_wtime());
}

int MPI_Abort(RCCE_COMM comm, int code) {
  exit(1);
}

int MPI_Comm_free(RCCE_COMM *comm) {
   return(RCCE_comm_free(comm));
}

