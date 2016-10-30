! 3456789 123456789 223456789 323456789 423456789 5234506789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program lucas_idea

    use, intrinsic :: iso_fortran_env,  only : INT64
    use mpi,                            only : MPI_COMM_WORLD, MPI_INT, MPI_INTEGER8, MPI_CHARACTER
    !use mpi,                            only : MPI_CHARACTER
    use mFileHandling,                  only : safeopen_writereplace

    implicit none

    external :: MPI_INIT, MPI_BARRIER, MPI_BCAST, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_FINALIZE, MPI_REDUCE

    integer, parameter :: ip = INT64 ! control precision in one place

    integer        :: rank = 0, ierr = 0, io_out_debug = 0, io_stat = 0
    integer ( ip ) :: numInt = 0_ip, numInt8 = 0_ip, numIntm = 0_ip, numIntp = 0_ip, numIntp8 = 0_ip
    integer ( ip ) :: j = 0_ip

    integer ( ip ), parameter :: large_int ( 0 : 3 ) = [ ( 10 ** j * 123456789, j = 0, 3 ) ]

    character ( len = 1 )   :: mark = '?'  ! exceeds 2**31 - 1
    character ( len = 256 ) :: io_msg = '', rank_str = ''

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, ierr )  ! PE's grab rank

        write ( unit = rank_str, fmt = '( g0 )', IOSTAT = io_stat, IOMSG = io_msg  ) rank
        if ( io_stat /= 0 ) then
            write ( unit = *, fmt = '( "WRITE error writing integer ", g0, " to rank_str" )' ) rank
            write ( unit = *, fmt = '( "IOSTAT = ", g0, /, "IOMSG = ", g0, "." )' )
            stop 'Fatal error...'
        end if
        io_out_debug = safeopen_writereplace ( 'debug_rank_' // trim ( rank_str ) // '.txt' )

        refine_mesh: do j = 0, 3                            ! larger j = finer mesh
            if ( rank == 0 ) then                           ! rank 0 grabs # of intervals
                numInt  = large_int ( j )
                numInt8 = large_int ( j )
                mark = '<'
                if ( numInt > huge ( rank ) ) mark = '>'
            end if
            write ( io_out_debug, '( "+ + + j = ", g0 )' ) j

            call MPI_BCAST ( mark,   1, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr )
            call MPI_BCAST ( j,      1, MPI_INT,       0, MPI_COMM_WORLD, ierr )
            call MPI_BCAST ( numInt, 1, MPI_INT,       0, MPI_COMM_WORLD, ierr )
            write ( io_out_debug, 200 ) 'MPI_INT', large_int ( j ), mark, numInt, ierr
            numIntp = numInt + rank
            write ( io_out_debug, 100 ) 'MPI_INT', rank, numIntp, large_int ( j ), rank
            numIntm = numIntp - numInt
            write ( io_out_debug, 110 ) 'MPI_INT', rank, numIntm, rank

            call MPI_BCAST ( numInt8, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr )
            write ( io_out_debug, 200 ) 'MPI_INTEGER8', large_int ( j ), mark, numInt8, ierr
            numIntp8 = numInt8 + rank
            write ( io_out_debug, 100 ) 'MPI_INTEGER8', rank, numIntp8, large_int ( j ), rank
            numIntm = numIntp8 - numInt8
            write ( io_out_debug, 110 ) 'MPI_INTEGER8', rank, numIntm, rank

            call MPI_BARRIER ( MPI_COMM_WORLD, ierr )

        end do refine_mesh

        call MPI_FINALIZE ( ierr )  ! clean up and go home

        100 format ( g0, ': rank = ', g0, ', numInt + rank = ', g0, ' (should be ', g0, ' + ', g0, ')' )
        110 format ( g0, ': rank = ', g0, ', numInt + rank - rank = ', g0, ' (should be ', g0, ')', / )

        200 format ( g0, ': MPI_BCAST sent: ', g0, ' ( ', g0, ' 2**31 - 1),  received: ', g0, ', ierr = ', g0, '.' )

end program lucas_idea

!  15:22 ITL-DTOPA-MP rditldmt $ date
! Thu Apr 21 15:22:50 CDT 2016
!  15:22 ITL-DTOPA-MP rditldmt $ echo $gflags
! -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5
!  15:23 ITL-DTOPA-MP rditldmt $ mpif90 $gflags lucas_idea.f08
!  15:23 ITL-DTOPA-MP rditldmt $ mpirun -np 4 ./lucas_idea
!   numInt = 1441440000, j = 3
!
! After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 0, numInt + rank = 1441440000 (should be 1441440000 + 0)
! MPI_INT:: rank = 0, numInt + rank - rank = 0 (should be 0)
!
! After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 0, numInt + rank = 1441440000 (should be 1441440000 + 0)
! MPI_INTEGER8:: rank = 0, numInt + rank - rank = 0 (should be 0)
!
! After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 1, numInt + rank = 1441440001 (should be 1441440000 + 1)
! MPI_INT:: rank = 1, numInt + rank - rank = 1 (should be 1)
!
! After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 1, numInt + rank = 1441440001 (should be 1441440000 + 1)
! MPI_INTEGER8:: rank = 1, numInt + rank - rank = 1 (should be 1)
!
! After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 2, numInt + rank = 1441440002 (should be 1441440000 + 2)
! MPI_INT:: rank = 2, numInt + rank - rank = 2 (should be 2)
!
! After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 3, numInt + rank = 1441440003 (should be 1441440000 + 3)
! After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 2, numInt + rank = 1441440002 (should be 1441440000 + 2)
! MPI_INT:: rank = 3, numInt + rank - rank = 3 (should be 3)
!
! After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 3, numInt + rank = 1441440003 (should be 1441440000 + 3)
! MPI_INTEGER8:: rank = 2, numInt + rank - rank = 2 (should be 2)
!
! MPI_INTEGER8:: rank = 3, numInt + rank - rank = 3 (should be 3)
!
! * numInt = 14414400000, j = 4
!
! After MPI_BCAST of 14414400000 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 0, numInt + rank = 14414400000 (should be 14414400000 + 0)
! MPI_INT:: rank = 0, numInt + rank - rank = 0 (should be 0)
!
! After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 0, numInt + rank = 14414400000 (should be 14414400000 + 0)
! MPI_INTEGER8:: rank = 0, numInt + rank - rank = 0 (should be 0)
!
! After MPI_BCAST of 1529498112 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 1, numInt + rank = 1529498113 (should be 1529498112 + 1)
! MPI_INT:: rank = 1, numInt + rank - rank = 1 (should be 1)
!
! After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 1, numInt + rank = 1529498113 (should be 14414400000 + 1)
! MPI_INTEGER8:: rank = 1, numInt + rank - rank = 1 (should be 1)
!
! After MPI_BCAST of 1529498112 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 2, numInt + rank = 1529498114 (should be 1529498112 + 2)
! MPI_INT:: rank = 2, numInt + rank - rank = 2 (should be 2)
!
! After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 2, numInt + rank = 1529498114 (should be 14414400000 + 2)
! MPI_INTEGER8:: rank = 2, numInt + rank - rank = 2 (should be 2)
!
! After MPI_BCAST of 1529498112 as MPI_INT, ierr = 0.
! MPI_INT:: rank = 3, numInt + rank = 1529498115 (should be 1529498112 + 3)
! MPI_INT:: rank = 3, numInt + rank - rank = 3 (should be 3)
!
! After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
! MPI_INTEGER8:: rank = 3, numInt + rank = 1529498115 (should be 14414400000 + 3)
! MPI_INTEGER8:: rank = 3, numInt + rank - rank = 3 (should be 3)
