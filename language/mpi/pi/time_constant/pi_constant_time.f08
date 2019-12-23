! 3456789 123456789 223456789 323456789 423456789 5234506789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program pi_constant_time

    use, intrinsic :: iso_fortran_env,  only : REAL64, INT64
    use mpi_f08,                        only : MPI_COMM_WORLD, MPI_INT, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use mFileHandling,                  only : safeopen_writeappend

    implicit none

    integer,     parameter :: ip = INT64, rp = REAL64 ! control precision in one place
    real ( rp ), parameter :: M_PI = acos ( -1.0_rp ), zero = 0.0_rp, one = 1.0_rp

    integer        :: io_out = 0
    integer ( ip ) :: rank = 0_ip, ierr = 0_ip
    integer ( ip ) :: j = 0_ip, k = 0_ip
    integer ( ip ) :: numIntDomain = 0_ip, numPE = 0_ip

    real ( rp ) :: dpi = zero, pi = zero
    real ( rp ) :: x0 = zero, x = zero, dx = zero
    real ( rp ) :: t0 = zero, t1 = zero                     ! start and finish time

    character ( len = 128 ) :: host_name = 'nemo'

    external :: MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_BCAST, MPI_REDUCE, MPI_FINALIZE

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        !   read machine name
        if ( COMMAND_ARGUMENT_COUNT( ) > 0 ) call getarg ( 1, host_name )

        do j = 1, 5 ! larger j = finer mesh
            if ( rank == 0 ) then ! rank 0 grabs start time & # of intervals
                t0 = MPI_WTIME ( )                          ! start time
                numIntDomain = 10 ** j * 1024 * 1024        ! scale up
                if ( numIntDomain .lt. 0 ) exit
            end if

            call MPI_BCAST ( numIntDomain, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )

            dpi = zero                                      ! my piece of the pi
            dx = one / numIntDomain / numPE                 ! integration measure
            x0 = real ( rank, rp ) / real ( numPE, rp )     ! start partition value for this PE

            do k = 0, numIntDomain - 1                      ! sum all slices for this PE
                x = x0 + k * dx
                dpi = dpi + dx * sqrt ( one - x * x )
            end do

            ! use a summing reduction to add up all the pieces
            ! rank 0 will have the final result, other ranks simply send
            call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )

            if ( rank == 0 ) then               ! rank 0 finalizes and reports
                pi = 4 * pi                     ! need 4x the area since only quadrant i used
                t1 = MPI_WTIME ( )              ! finish time
                io_out = safeopen_writeappend ( 'gnu_' // trim( host_name ) // '.csv' )
                    write ( unit = io_out, fmt = 200 ) numIntDomain, numPE, t1 - t0, pi - M_PI
                close ( io_out )
            end if

        end do

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    200 format ( g0, 3( ',  ', g0 ) )

end program pi_constant_time

!  09:48 ITL-DTOPA-MP rditldmt $ date
! Thu Apr 14 09:48:37 CDT 2016
!  09:48 ITL-DTOPA-MP rditldmt $ pwd
! /Users/rditldmt/Documents/singles/pi_constant_time
!  09:48 ITL-DTOPA-MP rditldmt $ make clean
! rm -rf mod_file_handling.o pi_constant_time.o pi_constant_time mod_file_handling.mod
! rm -f *.mod *.smod *.o
!  09:48 ITL-DTOPA-MP rditldmt $ make
! mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o pi_constant_time.o pi_constant_time.f08
! pi_constant_time.f08:24:69:
!
!          call MPI_INIT ( ierr )               ! initialize mpi runtime
!                                                                      1
! Warning: Procedure 'mpi_init' called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_constant_time.f08:26:75:
!
!          call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr ) ! PE's grab rank
!                                                                            1
! Warning: Procedure 'mpi_comm_rank' called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_constant_time.f08:27:75:
!
!          call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr ) ! number of PE's
!                                                                            1
! Warning: Procedure 'mpi_comm_size' called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_constant_time.f08:39:80:
!
!              call MPI_BCAST ( numIntDomain, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )
!                                                                                 1
! Warning: Procedure 'mpi_bcast' called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_constant_time.f08:52:88:
!
!              call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )
!                                                                                         1
! Warning: Procedure 'mpi_reduce' called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_constant_time.f08:57:25:
!
!                  io_out = safeopen_writeappend ( trim( host_name ) // file_results )
!                          1
! Warning: Conversion from INTEGER(4) to INTEGER(8) at (1) [-Wconversion-extra]
! pi_constant_time.f08:64:58:
!
!          call MPI_FINALIZE ( ierr )  ! clean up and go home
!                                                           1
! Warning: Procedure 'mpi_finalize' called at (1) is not explicitly declared [-Wimplicit-procedure]
! mpif90 -g -o pi_constant_time mod_file_handling.o pi_constant_time.o
!  09:48 ITL-DTOPA-MP rditldmt $ mpirun -np 4 ./pi_constant_time desktop
