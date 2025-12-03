#include <stdio.h>
#include <mpi.h>

int main(int argc, char *argv){
    int rank;
    int proc;

    printf("yahhou\n");

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &proc);

    printf("Hello, World! From Process %d of %d\n", rank, proc);

    MPI_Finalize();
    
    return 0;
}
