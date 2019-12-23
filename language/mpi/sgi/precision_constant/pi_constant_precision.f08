! 3456789 123456789 223456789 323456789 423456789 5234506789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program pi_constant_time

    use, intrinsic :: iso_fortran_env,  only : REAL64, INT64
    !use mpi_f08,                        only : MPI_COMM_WORLD, MPI_INTEGER8, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use mpif.h,                         only : MPI_COMM_WORLD, MPI_INTEGER8, MPI_DOUBLE, MPI_SUM, MPI_WTIME
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

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        !   read machine name
        if ( COMMAND_ARGUMENT_COUNT( ) > 0 ) call getarg ( 1, host_name )

        refine_mesh: do j = 1, 5                            ! larger j = finer mesh
            if ( rank == 0 ) then                           ! rank 0 grabs start time & # of intervals
                t0 = MPI_WTIME ( )                          ! start time
                numIntDomain = 10 ** j * 1441440            ! scale up: 720720 = LCM( Range[16] )
                if ( numIntDomain .lt. 0 ) exit refine_mesh
            end if

            call MPI_BCAST ( numIntDomain, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr )
            numIntPE = numIntDomain / numPE                 ! integer

            dpi = zero                                      ! my piece of the pi
            dx = one / numIntPE / numPE                     ! integration measure
            x0 = real ( rank, rp ) / real ( numPE, rp )     ! start partition value for this PE

            do k = 0, numIntPE - 1                          ! sum all slices for this PE
                x = x0 + k * dx
                dpi = dpi + dx * sqrt ( one - x * x )
            end do

            ! use a summing reduction to add up all the pieces
            ! rank 0 will have the final result, other ranks simply send
            call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )

            if ( rank == 0 ) then               ! rank 0 finalizes and reports
                pi = 4 * pi                     ! need 4x the area since only quadrant i used
                t1 = MPI_WTIME ( )              ! finish time
                io_out = safeopen_writeappend ( 'results/' // trim( host_name ) // '_gnu_i8.csv' )
                    write ( unit = io_out, fmt = 200 ) numIntDomain, numPE, t1 - t0, pi - M_PI
                close ( io_out )
            end if

        end do refine_mesh

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    200 format ( g0, 3( ',  ', g0 ) )

end program pi_constant_time

! dantopa@Muntz-Szasz.attlocal.net:pi_constant_precision $ date
! Sun Apr 17 19:51:49 CDT 2016
! dantopa@Muntz-Szasz.attlocal.net:pi_constant_precision $ pwd
! /Users/dantopa/github/singles/pi_constant_precision
! dantopa@Muntz-Szasz.attlocal.net:pi_constant_precision $ make
! mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -o pi_constant_precision.o pi_constant_precision.f08
! mpif90 -g -o pi_constant_precision mod_file_handling.o pi_constant_precision.o
! dantopa@Muntz-Szasz.attlocal.net:pi_constant_precision $ mpirun -np 4 ./pi_constant_precision $host_name
