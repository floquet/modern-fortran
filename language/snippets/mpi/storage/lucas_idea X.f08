! 3456789 123456789 223456789 323456789 423456789 5234506789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program lucas_idea

    use, intrinsic :: iso_fortran_env,  only : INT64
    use mpi,                            only : MPI_COMM_WORLD, MPI_INT, MPI_INTEGER8

    implicit none

    external :: MPI_INIT, MPI_BARRIER, MPI_BCAST, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_FINALIZE, MPI_REDUCE

    integer, parameter :: ip = INT64 ! control precision in one place

    integer        :: rank = 0, ierr = 0
    integer ( ip ) :: numIntDomain = 0_ip, numIntDomainm = 0_ip, numIntDomainp = 0_ip, j = 0_ip
    character ( len = 1 ) :: mark = '?'                     ! tag numbers > huge ( )

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, ierr )   ! PE's grab rank
        write ( * , '( "rank = ", g0 )' ) rank
        call MPI_BARRIER ( MPI_COMM_WORLD, ierr )

        refine_mesh: do j = 3, 4                            ! larger j = finer mesh
            if ( rank == 0 ) then                           ! rank 0 grabs start time & # of intervals
                numIntDomain = 10 ** j * 1441440            ! scale up: 720720 = LCM( Range[16] )
                if ( numIntDomain .lt. 0 ) exit refine_mesh
                !if ( j == 0 ) write ( * , '( "2**31 - 1 = ", g0, / )' ) huge ( rank )
                mark = ' '
                if ( numIntDomain > huge ( 1 ) ) mark = '*'
                write ( * , '( g0, " Preparing to broadcast numIntDomain = ", g0, ", j = ", g0 / )' ) mark, numIntDomain, j
            end if

            call MPI_BCAST ( numIntDomain, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )
            write ( * , 200 ) numIntDomain, 'MPI_INT', ierr
            numIntDomainp = numIntDomain + rank
            write ( * , 100 ) 'MPI_INT:', rank, j, numIntDomainp, numIntDomain, rank
            ! numIntDomainm = numIntDomainp - numIntDomain
            ! write ( * , 110 ) 'MPI_INT:', rank, j, numIntDomainm, rank

            ! call MPI_BCAST ( numIntDomain, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr )
            ! write ( * , 200 ) numIntDomain, 'MPI_INTEGER8', ierr
            ! write ( * , 100 ) 'MPI_INTEGER8:', rank, j, numIntDomainp, numIntDomain, rank
            ! write ( * , 110 ) 'MPI_INTEGER8:', rank, j, numIntDomainm, rank

            !call MPI_BARRIER ( MPI_COMM_WORLD, ierr )

        end do refine_mesh

        call MPI_FINALIZE ( ierr )  ! clean up and go home

        stop

        100 format ( g0, ': rank = ', g0, ', numIntDomain + rank = ', g0, ' (should be ', g0, ' + ', g0, ')' )
        110 format ( g0, ': rank = ', g0, ', numIntDomain + rank - rank = ', g0, ' (should be ', g0, ')', / )

        200 format ( 'After MPI_BCAST of ', g0, ' as ', g0, ' ierr = ', g0, '.' )

end program lucas_idea
