#include "hpl.h"
#include "RCCE.h"

#define MPI_COMM_WORLD RCCE_COMM_WORLD
#define MPI_SUCCESS    RCCE_SUCCESS
// We abuse the MPI data type to store the size of the data type
#define MPI_DOUBLE     (sizeof(double))
#define MPI_FLOAT      (sizeof(float))
#define MPI_INT        (sizeof(int))

typedef RCCE_COMM MPI_Comm ;
typedef int MPI_Datatype;
typedef int MPI_Request;
typedef int MPI_Status;
