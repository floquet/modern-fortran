! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

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

!  adding MPI processes increases precision, but not the run time
!  total number of integrations is fixed, therefore precision is fixed
!  more processors = faster execution
program pi_constant_precision

    use, intrinsic :: iso_fortran_env,  only : REAL64, INT64
    use mpt,                            only : MPI_COMM_WORLD, MPI_INTEGER8, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use mFileHandling,                  only : safeopen_writeappend

    implicit none

    external :: MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_BCAST, MPI_REDUCE, MPI_FINALIZE

    integer,     parameter :: ip = INT64, rp = REAL64 ! control precision in one place
    real ( rp ), parameter :: M_PI = acos ( -1.0_rp ), zero = 0.0_rp, one = 1.0_rp

    integer        :: io_out = 0
    integer        :: rank = 0, ierr = 0
    integer ( ip ) :: j = 0_ip, k = 0_ip
    integer ( ip ) :: numIntDomain = 0_ip, numIntPE = 0_ip, numPE = 0_ip

    real ( rp ) :: dpi = zero, pi = zero
    real ( rp ) :: x0 = zero, x = zero, dx = zero
    real ( rp ) :: t0 = zero, t1 = zero                     ! start and finish time

    character ( len = 128 ) :: host_name = 'nemo'
    character ( len = 256 ) :: results_file = ''

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        !   read machine name
        if ( COMMAND_ARGUMENT_COUNT( ) > 0 ) call getarg ( 1, host_name )
        results_file = 'results/' // trim( host_name ) // '_constant_precision_gnu_i8.csv'
        !   create a log file to hold results of runs
        if ( rank == 0 ) then
            io_out = safeopen_writeappend ( trim ( results_file ) )
                write ( unit = io_out, fmt = 200 ) 'integration intervals', 'MPI procs', 'cpu time', 'precision'
            close ( io_out )
        end if

        ! refine the mesh by a factor of 10 to see how the time increases and the precision remains static
        refine_mesh: do j = 0, 4                            ! larger j = finer mesh
            if ( rank == 0 ) then                           ! rank 0 grabs start time & # of intervals
                t0 = MPI_WTIME ( )                          ! start time
                numIntDomain = 10 ** j * 1441440            ! scale up: 720720 = LCM( Range[16] )
                if ( numIntDomain .lt. 0 ) exit refine_mesh
            end if
            ! maybe all ranks can see argv, but want to illustrate
            ! broadcast: sender sends m value, all other ranks receive m
            call MPI_BCAST ( numIntDomain, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr )
            !                ^             ^     ^          ^        ^           ^
            !                |             |     |          |        |           +--- error var.
            !                |             |     |          |        +--- communicator
            !                |             |     |          +--- rank of sender
            !                |             |     +--- datatype of buffer
            !                |             +--- count of items in buffer
            !                +--- buffer being broadcast
            numIntPE = numIntDomain / numPE                 ! integer

            dpi = zero                                      ! my piece of the pi
            dx = one / numIntPE / numPE                     ! interval width (integration measure)
            x0 = real ( rank, rp ) / real ( numPE, rp )     ! start partition value for this PE

            do k = 0, numIntPE - 1                          ! sum all slices for this PE
                x = x0 + k * dx                             ! next x
                dpi = dpi + dx * sqrt ( one - x * x )       ! integrate
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

            if ( rank == 0 ) then               ! rank 0 finalizes and reports
                pi = 4 * pi                     ! need 4x the area since only quadrant i used
                t1 = MPI_WTIME ( )              ! finish time
                io_out = safeopen_writeappend ( trim ( results_file ) )
                    write ( unit = io_out, fmt = 200 ) numIntDomain, numPE, t1 - t0, pi - M_PI
                close ( io_out )
            end if

        end do refine_mesh

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    200 format ( g0, 3( ',  ', g0 ) )

end program pi_constant_precision

! rditldmt@ITL-DTOPA-MP:constant_precision $ date
! Mon Sep 12 14:09:55 CDT 2016
! rditldmt@ITL-DTOPA-MP:constant_precision $ pwd
! /Users/rditldmt/hpc/fortran/language/mpi/pi/release/constant_precision
! rditldmt@ITL-DTOPA-MP:constant_precision $
! rditldmt@ITL-DTOPA-MP:constant_precision $ make
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o pi_constant_precision.o pi_constant_precision.f08
! mpif90 -g -o pi_constant_precision mod_file_handling.o pi_constant_precision.o
! rditldmt@ITL-DTOPA-MP:constant_precision $ mpirun -np 4 ./pi_constant_precision MacPro

! integration intervals,  MPI procs,  cpu time,  precision
! 1441440,  4,  0.10824039985891432E-001,  0.13868218742274507E-005
! 14414400,  4,  0.61242457013577223E-001,  0.13872873338982572E-006
! 144144000,  4,  0.53801261697662994,  0.13873765247751635E-007
! 1441440000,  4,  4.6914655140135437,  0.13875363080728675E-008
! 14414400000,  4,  45.300327625009231,  0.13836798373745296E-009
