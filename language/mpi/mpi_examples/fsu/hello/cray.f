! https://people.sc.fsu.edu/~jburkardt/f_src/hello_mpi/hello_mpi.f90
program test

    use mpi, only : MPI_COMM_WORLD
    implicit none
    
    integer :: error, id, p

        call MPI_Init ( error ) !  Initialize MPI.
        call MPI_Comm_size ( MPI_COMM_WORLD, p,  error ) !  Get the number of processes.
        call MPI_Comm_rank ( MPI_COMM_WORLD, id, error ) !  Get the individual process ID.

        if ( id == 0 ) then
            write ( *, '( A )' ) ' '
            write ( *, '( A )' ) 'HELLO_MPI - Master process:'
            write ( *, '( A )' ) '  FORTRAN90/MPI version'
            write ( *, '( A )' ) ' '
            write ( *, '( A )' ) '  An MPI test program.'
            write ( *, '( A )' ) ' '
            write ( *, '( A, g0 )' ) '  The number of processes is ', p
            write ( *, '( A )' ) ' '
        end if

        write ( *, '( A )' ) ' '
        write ( *, '( A, g0, A)' ) '  Process ', id, ' says "Hello, world!"'

        if ( id == 0 ) then
            write ( *, '( A )' ) ' '
            write ( *, '( A )' ) 'HELLO_MPI - Master process:'
            write ( *, '( A )' ) '  Normal end of execution: "Goodbye, world!".'
            write ( *, '( A )' ) ' '
        end if

        call MPI_Finalize ( error ) !  Shut down MPI.

    stop
end program test

! Muntz-Szasz:fsu dantopa$ date
! Sun Mar 20 14:06:03 CDT 2016
! Muntz-Szasz:fsu dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/MPI/mpi examples/fsu
! Muntz-Szasz:fsu dantopa$
! Muntz-Szasz:fsu dantopa$ mpif90 -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 test.f90
! Muntz-Szasz:fsu dantopa$ mpirun -np 6 ./a.out
!
!   Process 4 says "Hello, world!"
!
!   Process 5 says "Hello, world!"
!
! HELLO_MPI - Master process:
!   FORTRAN90/MPI version
!
!   An MPI test program.
!
!
!   Process 1 says "Hello, world!"
!
!   Process 3 says "Hello, world!"
!   The number of processes is 6
!
!
!   Process 0 says "Hello, world!"
!
! HELLO_MPI - Master process:
!   Normal end of execution: "Goodbye, world!".
!
!
!   Process 2 says "Hello, world!"
