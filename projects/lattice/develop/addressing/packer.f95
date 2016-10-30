!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include '../../modules/mod precision definitions.f90'

program packer

    use precision_definitions, only : is, wp!, zero

    implicit none

    integer ( is )                   :: nDim = 0, nMesh = 0, nPoints = 0 
    integer ( is )                   :: mu = 0                            ! loop over spatial dimensions
    integer ( is )                   :: i = 0, j = 0, k = 0, l = 0        ! dummy counters
    integer ( is )                   :: alloc_status = 0
    integer ( is ), allocatable      :: repeat_index ( : ), repeat_block ( : ), extent ( : ), addresses ( : , : )

    real    ( wp )                   :: cpu_0 = 0, cpu_1 = 0!, t_0 = 0, t_1 = 0

    character ( len = 16 )           :: c_arg
    character ( len = 512 )          :: alloc_msg = ""
    character ( len = * ), parameter :: me_program = 'program packer'  ! self-identification
    character ( len = * ), parameter :: stop_msg   = 'Fatal error, ending '

        call cpu_time ( cpu_0 ) ! global cpu time - start

!           harvest command line arguments  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
            call get_command_argument ( 1, c_arg )

            if ( len_trim ( c_arg ) == 0 ) then
                nDim  = 2
                nMesh = 3
            else
                call get_command_argument ( 2, c_arg )
                read ( c_arg, '( I10 )' ) nDim
                call get_command_argument ( 3, c_arg )
                read ( c_arg, '( I10 )' ) nMesh
            end if

!           allocations  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =

            allocate ( repeat_index ( 1 : nDim ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 300 ) "integer ( is )", "repeat_index"
                write ( *, 320 ) nDim
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            allocate ( repeat_block ( 1 : nDim ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 300 ) "integer ( is )", "repeat_block"
                write ( *, 320 ) nDim
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            allocate ( extent ( 1 : nDim ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 300 ) "integer ( is )", "extent"
                write ( *, 320 ) nDim
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            extent ( : ) = [ ( k * nMesh, k = 1, nDim ) ]  ! cube
            nPoints = product ( extent )

            allocate ( addresses ( 1 : nPoints, 1 : nDim ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 300 ) "integer ( is )", "extent"
                write ( *, 320 ) nDim
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

!           repeat_index and repeat_block are two arrays used in index gymnastics
            repeat_index ( : ) = 1
            repeat_block ( : ) = repeat_index ( : )

!           how many times to repeat the mu-th index
            do j = 2, nDim
              do k = j, nDim
                repeat_index ( k ) = repeat_index ( k ) * extent ( j - 1 )
              end do
            end do

!           how many times to duplicate the block of repeated indices
            do j = nDim, 2, -1
              do k = j - 1, 1, -1
                repeat_block ( k )  = repeat_block ( k ) * extent ( j )
              end do
            end do

!           assignment  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =

!           sweep through lattice and assign addresses
            sweep_dimensions:     do mu = 1,      nDim                               ! mu  sweep over the dimensions
              i = 0                                                                  ! counter goes from 1 to num_nodes
              sweep_repeat_block:   do l = 1,     repeat_block ( mu )                ! l  # times to repeat blocks of indices
                sweep_index:          do k = 1,   extent  ( mu )                     ! k  index to repeat
                  sweep_repeat_index:   do j = 1, repeat_index ( mu )                ! j  # times to repeat index

                    i = i + 1                                                        ! increment counter
                    addresses ( i, nDim - mu + 1 ) = k                               ! load the mu-th digit of the address

                  end do sweep_repeat_index                                          ! j  # times to repeat index
                end do sweep_index                                                   ! k  index to repeat
              end do sweep_repeat_block                                              ! l  # times to repeat blocks of indices
            end do sweep_dimensions                                                  ! mu  sweep over the dimensions

            do i = 1, nPoints
                write ( *, 100 ) i, addresses ( i, : )
            end do

!           deallocations  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =

            deallocate ( repeat_index, stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 310 ) "integer ( is )", "repeat_index"
                write ( *, 330 ) size ( repeat_index )
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            deallocate ( repeat_block, stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 310 ) "integer ( is )", "repeat_index"
                write ( *, 330 ) size ( repeat_block )
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            deallocate ( extent, stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 310 ) "integer ( is )", "extent"
                write ( *, 330 ) size ( extent )
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            deallocate ( addresses, stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 310 ) "integer ( is )", "extent"
                write ( *, 330 ) size ( addresses )
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, '( /, "total cpu time used = ", g0, ": seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

  100   format ( /, "addresses(", g0, ") = ", 15( 2X, I4 ), "." )

  300   format ( /, "Error allocating memory for ", g0, " array ", g0, "." )
  310   format ( /, "Error deallocating memory for ", g0, " array ", g0, "." )
  320   format (    "  requested size is ", g0, " x ", g0, " elements" )
  330   format (    "  currect size is ", g0, " elements" )
  340   format (    "  stat = ", g0 )
  350   format (    "  errmsg = ", g0, "." )

end program packer