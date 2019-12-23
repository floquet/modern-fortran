program main

    use mpi_f08, only : MPI_COMM_WORLD,MPI_INIT,MPI_COMM_RANK,MPI_COMM_SIZE,MPI_FINALIZE 
    implicit none 
    integer :: rank, size, len 

    call MPI_INIT() 
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank) 

    call MPI_COMM_SIZE(MPI_COMM_WORLD, size) 

    print *, ' No Errors' 

    call MPI_FINALIZE() 

end 
