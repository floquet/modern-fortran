! Jerry Morris, loosely based on any of several similar web examples.
! This code approximates the value of pi by approximating
! the area under the curve x^2 + y^2 = r^2 between 0 and 1.
! Assume r = 1 -> y = sqrt(1 - x^2).  Sum all the little
! rectangles (slices) of area y*dx to approximate area under curve.
! Since area = pi * r^2 -> (with r = 1) pi = area.
!                               y
!                               ^
!                               |   top half of a unit circle
!  _    y = sqrt(1 - x^2)     . -  .  use quadrant I & multiply by 4
!  |                \     .     |      .
!  |                 \.         |     |   .
!  dx*sqrt(1-x^2)  .            |     |      .
!  |             .              |     |     |  .
!  |           .                |     |     |    .
!  -     -----.-----------------+-----|-----|-----.------> x
!            -1              x  0  dx    dx    dx 1
! Each PE computes multiple dx sections, ergo we have parallelism.

!!!!!!!!!!!!!
! Linux/OS X (assumes gcc with Fortran and OpenMPI)
! change to source directory, e.g., cd ~/mpiprim/pi
! compile: mpif90 piMPI -O3 -lm -o piMPIf
! run: mpirun -np <n> ./piMPIf <m> # m=# of slices, e.g., 50000000

!!!!!!!!!!!!!
! Garnet (interactive)
! a)  qsub -q standard -l select=1:ncpus=32:mpiprocs=32 \
!       -l walltime=1:00:00 -A ERDCS97290STA  -l ccm=1 -X -I
! b) change to source directory, e.g., cd ~/mpiprim/pi
! c) compile:  ftn piMPI -O3 -lm -o piMPIf
! d) run: aprun -n <n> ./piMPIf <m>  # m=# of slices, e.g.,50000000

!!!!!!!!!!!!!
! Garnet (batch)
! qsub -v CC=ftn,SFX=f,EXT=f90,M=50000000,N=8 piMPI.pbs # any m/n value

! rank    ! MPI rank (PE number [0,n-1])
! ierr    ! Fortran MPI call error var.
! n       ! number of PE's
! m       ! number of intervals
! dpi     ! my delta pi
! pi      ! sum of all dpi's
! argv1   ! command line version of m
! x       ! current x value
! dx      ! delta x
! x0, xm  ! my starting & ending x value
! t0, t1  ! start and finish time
program piMPI

    use mpi, only : MPI_COMM_WORLD, MPI_INT, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use, intrinsic :: iso_fortran_env, only : REAL64, INT32

    implicit none

    integer,     parameter :: ip = INT32, rp = REAL64
    real ( rp ), parameter :: M_PI = acos ( -1.0_rp ), zero = 0.0_rp, one = 1.0_rp

    integer ( ip ) :: rank = 0_ip,  ierr = 0_ip,  n = 0_ip,  m = 0_ip

    real ( rp ) :: dpi = zero, pi = zero, x = zero, dx = zero
    real ( rp ) :: x0 = zero, xm = zero ! my starting & ending x value
    real ( rp ) :: t0 = zero, t1 = zero ! start and finish time

    character ( len = 10 ) :: argv1 = ''  ! command line version of m

        call MPI_INIT ( ierr )               ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, ierr ) ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, n,    ierr ) ! number of PE's

        if (rank == 0) then ! rank 0 grabs start time & # of intervals
            t0 = MPI_WTIME ( )                  ! start time
            call getarg ( 1, argv1 )            ! number of intervals
            read  ( argv1, '( I10 )' ) m        ! convert to integer
            write ( *, "( 3a )" ) 'using ', trim ( argv1 ), " intervals (dx's)."
        end if
        ! maybe all ranks can see argv, but want to illustrate
        ! broadcast: sender sends m value, all other ranks receive m
        call MPI_BCAST ( m, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )
        !                ^  ^     ^     ^        ^           ^
        !                |  |     |     |        |           +--- error var.
        !                |  |     |     |        +--- communicator
        !                |  |     |     +--- rank of sender
        !                |  |     +--- datatype of buffer
        !                |  +--- count of items in buffer
        !                +--- buffer being broadcast
        dpi = zero               ! my piece of the pi
        dx = one / m             ! width of each interval
        x0 = rank / n + dx       ! start x value for this PE
        xm = ( rank + 1 ) / n    ! end x value for this PE

        x = x0                   ! start here for this PE
        do                       ! sum all slices for this PE
            dpi = dpi + dx * sqrt( one - x * x )
            x = x + dx           ! next x
            if ( x > xm ) exit
        end do

        ! use a summing reduction to add up all the pieces
        ! rank 0 will have the final result, other ranks simply send
        call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )
        !                  ^   ^   ^     ^           ^     ^          ^
        !                  |   |   |     |           |     |          +--- comm
        !                  |   |   |     |           |     +--- destination rank
        !                  |   |   |     |           +--- summing reduction
        !                  |   |   |     +--- datatype of send buffer
        !                  |   |   +--- count of items in send buffer
        !                  |   +--- receive buffer (destination rank)
        !                  +--- send buffer (all ranks)

        if ( rank == 0 ) then ! rank 0 finalizes and reports
            pi = 4 * pi   ! need 4x the area since only quadrant i used
            t1 = MPI_WTIME ( )              ! finish time
            write ( *, "( a, f10.8, a )" ) 'runtime: ', t1 - t0, ' s'
            write ( *, "( a, f17.14 )" ) 'pi (to 14 decimal places): ', M_PI
            write ( *, "( a, f17.14 )" ) 'pi was approximated as:    ', pi
            write ( *, "( a, f17.14 )" ) 'error:                     ', pi - M_PI
        end if

        call MPI_FINALIZE ( ierr )  ! clean up and go home

end program piMPI

! dan-topas-pro-2:pi rditldmt$ date
! Mon Mar 21 14:19:00 CDT 2016
! dan-topas-pro-2:pi rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/mpi/mpi examples/erdc/pi
! dan-topas-pro-2:pi rditldmt$ echo $flags
! -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5
! dan-topas-pro-2:pi rditldmt$ mpirun -np 2 ./mpipi 100000
! using 100000 intervals (dx's).
! runtime: 0.00141835 s
! pi (to 14 decimal places):  3.14159265358979
! pi was approximated as:     3.14157261648453
! error:                     -0.00002003710526
! dan-topas-pro-2:pi rditldmt$ mpirun -np 8 ./mpipi 100000
! using 100000 intervals (dx's).
! runtime: 0.00159804 s
! pi (to 14 decimal places):  3.14159265358979
! pi was approximated as:     3.14157261648453
! error:                     -0.00002003710526
! dan-topas-pro-2:pi rditldmt$ mpirun -np 8 ./mpipi 100000000
! using 100000000 intervals (dx's).
! runtime: 1.16557006 s
! pi (to 14 decimal places):  3.14159265358979
! pi was approximated as:     3.14159262825976
! error:                     -0.00000002533003
