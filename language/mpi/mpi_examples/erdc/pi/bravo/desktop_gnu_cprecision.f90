! 3456789 123456789 223456789 323456789 423456789 5234506789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program pi_constant_precision

    use, intrinsic :: iso_fortran_env,  only : REAL64, INT64
    use mpi,                            only : MPI_COMM_WORLD, MPI_INT, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use mFileHandling,                  only : safeopen_writeappend

    implicit none

    integer,     parameter :: ip = INT64, rp = REAL64 ! control precision in one place
    real ( rp ), parameter :: M_PI = acos ( -1.0_rp ), zero = 0.0_rp, one = 1.0_rp

    integer ( ip ) :: rank = 0_ip, ierr = 0_ip
    integer ( ip ) :: k0 = 0_ip, j = 0_ip, k = 0_ip, io_out = 0_ip
    integer ( ip ) :: numInt = 0_ip, numProc = 0_ip, numSlicePP = 0_ip, host_status = 0_ip

    real ( rp ) :: dpi = zero, pi = zero
    real ( rp ) :: x = zero, dx = zero
    real ( rp ) :: t0 = zero, t1 = zero      ! start and finish time

    character ( len = 64 )           :: file_prefix = '', host = '', file_name = ''
    character ( len = * ), parameter :: file_suffix = '.csv'


        call MPI_INIT ( ierr )               ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,    ierr ) ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numProc, ierr ) ! number of PE's

        do j = 1, 3   ! larger j = finer mesh
            if ( rank == 0 ) then ! rank 0 grabs start time & # of intervals
                host_status = hostnm( host )
                file_prefix = 'pi_' // trim ( host ) // '_'
                t0 = MPI_WTIME ( )                  ! start time
                numInt = 10 ** j * 1024 * 1024      ! scale up
                if ( numInt .lt. 0 ) exit
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
                write ( unit = file_name, fmt =  100 ) trim ( file_prefix ), 'times_', numProc, file_suffix
                io_out = safeopen_writeappend ( file_name )
                    write ( unit = io_out, fmt = 200 ) numProc, numInt, t1 - t0
                close ( io_out )
                write ( unit = file_name, fmt =  100 ) trim ( file_prefix ), 'precisions_', numProc, file_suffix
                io_out = safeopen_writeappend ( file_name )
                    write ( unit = io_out, fmt = 200 ) numProc, numInt, pi - M_PI
                close ( io_out )
            end if

        end do

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    100 format ( 4( g0 ) )

    200 format ( g0, 2( ',  ', g0 ) )

end program pi_constant_precision

! rditldmt@ITL-DTOPA-MP:desktop copy $ make
! mpif90 -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f90
! mpif90 -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o desktop_gnu_cprecision.o desktop_gnu_cprecision.f90
! mpif90 -g -o desktop_gnu_cprecision desktop_gnu_cprecision.o mod_file_handling.o
! rditldmt@ITL-DTOPA-MP:desktop copy $ mpirun -np 4 ./desktop_gnu_cprecision
! Warning: pi_ITL-DTOPA-MP_times.csv doesn't exist; new empty file will be created.
! Warning: pi_ITL-DTOPA-MP_precisions.csv doesn't exist; new empty file will be created.
