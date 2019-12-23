! 3456789 123456789 223456789 323456789 423456789 5234506789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program pi_constant_time

    use, intrinsic :: iso_fortran_env,  only : REAL64, INT64
    use mpi,                            only : MPI_COMM_WORLD, MPI_INT, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use mFileHandling,                  only : safeopen_writeappend

    implicit none

    external :: MPI_INIT, MPI_BARRIER, MPI_BCAST, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_FINALIZE, MPI_REDUCE

    integer,     parameter :: ip = INT64, rp = REAL64 ! control precision in one place
    real ( rp ), parameter :: M_PI = acos ( -1.0_rp ), zero = 0.0_rp, one = 1.0_rp

    integer ( ip ) :: rank = 0_ip, ierr = 0_ip
    integer ( ip ) :: k0 = 0_ip, j = 0_ip, k = 0_ip, io_out = 0_ip
    integer ( ip ) :: numInt = 0_ip, numProc = 0_ip, numSlicePP = 0_ip

    real ( rp ) :: dpi = zero, pi = zero
    real ( rp ) :: x = zero, dx = zero
    real ( rp ) :: t0 = zero, t1 = zero      ! start and finish time

    character ( len = * ), parameter :: file_results = 'pi_desktop_gnu.csv'

        call MPI_INIT ( ierr )               ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,    ierr ) ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numProc, ierr ) ! number of PE's

        do j = 1000, 10000, 1000  ! larger j = finer mesh
            if ( j .lt. 0 ) exit
            if ( rank == 0 ) then ! rank 0 grabs start time & # of intervals
                t0 = MPI_WTIME ( )                  ! start time
                numInt = j * 1024 * 1024            ! scale up
            end if

            call MPI_BCAST ( numInt, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )

            dpi = zero                              ! my piece of the pi
            dx = one / numInt                       ! integration measure
            numSlicePP = numInt / numProc           ! width of each interval; must be integer
            k0 = rank * numSlicePP                  ! start partition value for this PE

            do k = 0, numSlicePP - 1                ! sum all slices for this PE
                x = ( k0 + k ) * dx
                dpi = dpi + dx * sqrt ( one - x * x )
            end do

            ! use a summing reduction to add up all the pieces
            ! rank 0 will have the final result, other ranks simply send
            call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )

            if ( rank == 0 ) then               ! rank 0 finalizes and reports
                pi = 4 * pi                     ! need 4x the area since only quadrant i used
                t1 = MPI_WTIME ( )              ! finish time
                io_out = safeopen_writeappend ( file_results )
                    write ( unit = io_out, fmt = 200 ) j, numProc, t1 - t0, pi - M_PI
                close ( io_out )
            end if

        end do

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    200 format ( g0, 3( ',  ', g0 ) )

end program pi_constant_time

! dan-topas-pro-2:desktop rditldmt$ date
! Wed Mar 30 10:19:08 CDT 2016
! dan-topas-pro-2:desktop rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/mpi/mpi_examples/erdc/pi/desktop
! dan-topas-pro-2:desktop rditldmt$ make
! mpif90 -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f90
! mpif90 -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o desktop_gnu_ctime.o desktop_gnu_ctime.f90
! mpif90 -g -o desktop_gnu_ctime desktop_gnu_ctime.o mod_file_handling.o
! dan-topas-pro-2:desktop rditldmt$ mpirun -np 4 ./desktop_gnu_ctime
