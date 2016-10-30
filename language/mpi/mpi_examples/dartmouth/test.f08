! https://www.dartmouth.edu/~rc/classes/intro_mpi/hello_world_ex.html#top
program test

    use mpi, only : MPI_COMM_WORLD!, MPI_STATUS_SIZE

    integer rank, size, ierror!, tag, status ( MPI_STATUS_SIZE )

        call MPI_INIT ( ierror )
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, size, ierror )
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, ierror )

        print*, 'node', rank, ': Hello world'

        call MPI_FINALIZE ( ierror )

end program test

! Muntz-Szasz:dartmouth dantopa$ date
! Sun Mar 20 13:45:29 CDT 2016
! Muntz-Szasz:dartmouth dantopa$
! Muntz-Szasz:dartmouth dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/MPI/mpi examples/dartmouth
! Muntz-Szasz:dartmouth dantopa$ mpif90 -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o test test.f08
! Muntz-Szasz:dartmouth dantopa$ mpirun -np 8 ./test
!  node           0 : Hello world
!  node           1 : Hello world
!  node           2 : Hello world
!  node           3 : Hello world
!  node           6 : Hello world
!  node           7 : Hello world
!  node           4 : Hello world
!  node           5 : Hello world
