! https://people.sc.fsu.edu/~jburkardt/f_src/prime_mpi/prime_mpi.f90
program parallel_primes

    use, intrinsic :: iso_fortran_env,  only : REAL64
    use mpi,                            only : MPI_COMM_WORLD, MPI_INTEGER, MPI_SUM, MPI_Wtime
    use mTimeStamp,                     only : timestamp
    use mPrimeNumber,                   only : prime_number

    implicit none

    integer, parameter  :: rp = REAL64
    integer             :: id = 0, ierr = 0, n = 0, n_factor = 0, n_hi = 0, n_lo = 0, p = 0, primes = 0, primes_part = 0
    real ( rp )         :: wtime = 0.0

        n_lo = 1
        n_hi = 1048576  ! 2**20
        n_factor = 2

        call MPI_Init ( ierr ) !  Initialize MPI.
        call MPI_Comm_rank ( MPI_COMM_WORLD, id, ierr ) !  Get this process's ID.
        call MPI_Comm_size ( MPI_COMM_WORLD,  p, ierr ) !  Find out how many processes are available.

        if ( id == 0 ) then
            call timestamp ( )
            write ( *, '( A )' ) ' '
            write ( *, '( A )' ) 'PRIME_MPI'
            write ( *, '( A )' ) '  FORTRAN90/MPI version'
            write ( *, '( A )' ) ' '
            write ( *, '( A )' ) '  An MPI example program to count the number of primes.'
            write ( *, '( A, g0 )' ) '  The number of processes is ', p
            write ( *, '( A )' ) ' '
            write ( *, '( A )' ) '         N        Pi          Time'
            write ( *, '( A )' ) ' '
        end if

        n = n_lo

        do
            if ( id == 0 ) then
                wtime = MPI_Wtime ( )
            end if

            call MPI_Bcast ( n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr )
            call prime_number ( n, id, p, primes_part )
            call MPI_Reduce ( primes_part, primes, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr )

            if ( id == 0 ) then
                wtime = MPI_Wtime ( ) - wtime
                write ( *, '( 2x, I8, 2x, I8, g14.6 )' ) n, primes, wtime
            end if

            n = n * n_factor
            if ( n > n_hi ) exit
        end do

        call MPI_Finalize ( ierr ) !  Terminate MPI.

        if ( id == 0 ) then
            write ( *, '( A )' ) ' '
            write ( *, '( A )' ) 'PRIME_MPI:'
            write ( *, '( A )' ) '  Normal end of execution.'
            write ( *, '( A )' ) ' '
            call timestamp ( )
        end if

        stop
    end

! Muntz-Szasz:prime dantopa$ date
! Sat Apr  2 15:59:10 CDT 2016
! Muntz-Szasz:prime dantopa$ pwd
! /Users/dantopa/github/fortran/mpi/mpi_examples/fsu/prime
! Muntz-Szasz:prime dantopa$ cn
! Muntz-Szasz:prime dantopa$ make
! mpif90  -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_PrimeNumber.o mod_PrimeNumber.f08
! mpif90  -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_TimeStamp.o mod_TimeStamp.f08
! mpif90  -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o parallel_primes.o parallel_primes.f08
! mpif90  -g -o parallel_primes mod_PrimeNumber.o mod_TimeStamp.o parallel_primes.o
! Muntz-Szasz:prime dantopa$ mpirun -np 2 ./parallel_primes
!  2 April 2016   3:59:26.870 PM
!
! PRIME_MPI
!   FORTRAN90/MPI version
!
!   An MPI example program to count the number of primes.
!   The number of processes is 2
!
!          N        Pi          Time
!
!          1         0  0.122117E-03
!          2         1  0.482704E-05
!          4         2  0.356301E-05
!          8         4  0.161096E-05
!         16         6  0.157114E-05
!         32        11  0.185007E-05
!         64        18  0.158690E-04
!        128        31  0.109030E-04
!        256        54  0.306091E-04
!        512        97  0.926212E-04
!       1024       172  0.285496E-03
!       2048       309  0.989024E-03
!       4096       564  0.337298E-02
!       8192      1028  0.981459E-02
!      16384      1900  0.378297E-01
!      32768      3512  0.137306
!      65536      6542  0.490759
!     131072     12251   1.81684
!     262144     23000   6.83741
!     524288     43390   25.6230
!    1048576     82025   98.3795
!
! PRIME_MPI:
!   Normal end of execution.
!
!  2 April 2016   4:01:40.204 PM
! Muntz-Szasz:prime dantopa$ mpirun -np 128 ./parallel_primes
!  2 April 2016   4:02:18.159 PM
!
! PRIME_MPI
!   FORTRAN90/MPI version
!
!   An MPI example program to count the number of primes.
!   The number of processes is 128
!
!          N        Pi          Time
!
!          1         0  0.242160E-01
!          2         1  0.474583E-02
!          4         2  0.429199E-02
!          8         4  0.429020E-02
!         16         6  0.438762E-02
!         32        11  0.425740E-02
!         64        18  0.433911E-02
!        128        31  0.105315E-01
!        256        54  0.550843E-02
!        512        97  0.605928E-02
!       1024       172  0.658362E-02
!       2048       309  0.519628E-02
!       4096       564  0.497712E-02
!       8192      1028  0.120670E-01
!      16384      1900  0.142723E-01
!      32768      3512  0.358114E-01
!      65536      6542  0.126764
!     131072     12251  0.451474
!     262144     23000   1.66123
!     524288     43390   6.35533
!    1048576     82025   25.3138
!
! PRIME_MPI:
!   Normal end of execution.
!
!  2 April 2016   4:02:52.294 PM

! N = 2**20 = 1 048 576
!
! processes time
!         2   98.3795
!         4   59.6243
!         8   45.5754
!        12   47.0487
!        14   31.5616
!        16   25.6219
!        24   25.5675
!        64   26.3576
!       128   25.3138
!       256   28.9224
