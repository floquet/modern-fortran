! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program piMPI

    use mpi_f08,                        only : MPI_COMM_WORLD, MPI_INT, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use, intrinsic :: iso_fortran_env,  only : REAL64, INT32, INT64
    use mFileHandling,                  only : safeopen_writeappend

    implicit none

    integer,     parameter :: ip = INT32, lp = INT64, rp = REAL64 ! control precision in one place
    real ( rp ), parameter :: M_PI = acos ( -1.0_rp ), zero = 0.0_rp, one = 1.0_rp

    integer ( ip ) :: rank = 0_ip, ierr = 0_ip, io_out = 0_ip
    integer ( ip ) :: k0 = 0_lp, j = 0_lp, k = 0_lp
    integer ( ip ) :: numInt = 0_lp, numProc = 0_lp, numSlicePP = 0_lp

    real ( rp ) :: dpi = zero, pi = zero
    real ( rp ) :: x = zero, dx = zero
    real ( rp ) :: t0 = zero, t1 = zero      ! start and finish time

    character ( len = * ), parameter :: file_results = 'pi_desktop_gnu.csv'

        call MPI_INIT ( ierr )               ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,    ierr ) ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numProc, ierr ) ! number of PE's

        main_loop: do j = 2000, 3000, 10
            if ( rank == 0 ) then ! rank 0 grabs start time & # of intervals
                t0 = MPI_WTIME ( )                  ! start time
                numInt = j * 1024 * 1024            ! scale up
                if ( numInt < 0 ) exit main_loop    ! insurance
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

        end do main_loop

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    200 format ( g0, 3( ',  ', g0 ) )

end program piMPI

! module switch compiler/intel/15.0.3 compiler/gcc/5.3.0

! dantopa@topaz06:~/github/fortran/mpi/PBS/topaz/gnu/alpha> make
! mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_mpif.o mod_mpif.f08
! mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o pi_topaz_gnu.o pi_topaz_gnu.f08
! pi_topaz_gnu.f08:23:69:
!
!          call MPI_INIT ( ierr )               ! initialize mpi runtime
!                                                                      1
! Warning: Procedure ‘mpi_init’ called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_topaz_gnu.f08:25:77:
!
!         call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,    ierr ) ! PE's grab rank
!                                                                              1
! Warning: Procedure ‘mpi_comm_rank’ called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_topaz_gnu.f08:26:77:
!
!          call MPI_COMM_SIZE ( MPI_COMM_WORLD, numProc, ierr ) ! number of PE's
!                                                                              1
! Warning: Procedure ‘mpi_comm_size’ called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_topaz_gnu.f08:34:74:
!
!              call MPI_BCAST ( numInt, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )
!                                                                           1
! Warning: Procedure ‘mpi_bcast’ called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_topaz_gnu.f08:48:88:
!
!              call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )
!                                                                                         1
! Warning: Procedure ‘mpi_reduce’ called at (1) is not explicitly declared [-Wimplicit-procedure]
! pi_topaz_gnu.f08:60:58:
!
!          call MPI_FINALIZE ( ierr )  ! clean up and go home
!                                                           1
! Warning: Procedure ‘mpi_finalize’ called at (1) is not explicitly declared [-Wimplicit-procedure]
! mpif90 -g -o pi_topaz_gnu mod_file_handling.o mod_mpif.o pi_topaz_gnu.o
! dantopa@topaz06:~/github/fortran/mpi/PBS/topaz/gnu/alpha>
! dantopa@topaz06:~/github/fortran/mpi/PBS/topaz/gnu/alpha> date
! Thu Apr  7 13:05:08 CDT 2016
! dantopa@topaz06:~/github/fortran/mpi/PBS/topaz/gnu/alpha> pwd
