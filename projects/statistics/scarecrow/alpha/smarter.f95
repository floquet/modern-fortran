!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! write (filename, "(A5,I2)") "hello", 10
!include 'modules/mod precision definitions.f90'

program write

    use iso_fortran_env
    !use precision_definitions, only : is, wp, zero

    implicit NONE


    integer, parameter               :: ap = selected_int_kind  ( INT8 )
    integer, parameter               :: bp = selected_int_kind  ( INT64 )
    integer, parameter               :: ip = ap
    integer, parameter               :: wp = selected_int_kind  ( REAL64 )
    integer, parameter               :: nUnique = 10

    integer ( ip )                   :: j = 0, k = 0, h = 0, l = 0, m = 0, n = 0, o = 0, p = 0
    integer ( ip )                   :: numCards = 0
    integer ( bp )                   :: count = 1_bp, inc = 1_bp

    integer ( ip ), allocatable      :: cards ( : , : )

    integer ( ip )                   :: io = 0, io_status = 0, alloc_status = 0

    real    ( wp )                   :: cpu_0 = 0, cpu_1 = 0, t_0 = 0, t_1 = 0

    character ( len = 512 )          :: io_msg = "", alloc_msg = ""
    character ( len = 128 )          :: file_name = ""

    character ( len = * ), parameter :: me_program = 'program write'  ! self-identification
    character ( len = * ), parameter :: stop_msg   = 'Fatal error. Ending session of '

        call cpu_time ( cpu_0 ) ! global cpu time - start

            numCards = 2

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start
                allocate ( cards ( 1 : nUnique**numCards, 1 : numCards ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 300 ) "integer ( INT8 )", "cards"
                    write ( *, 320 ) nUnique**numCards, nUnique
                    write ( *, 340 ) alloc_status
                    write ( *, 350 ) trim ( alloc_msg )
                    stop stop_msg // me_program // '.'
                end if

                count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        count = count + inc
                        cards ( count, : ) = [ k, j ]
                        write ( *, '( "cards (", I4, ", : ) = ", 2I3 )' ) count, cards ( count, : )
                    end do
                end do
                write ( *, '( "size ( cards, 1 ) = ", g0, "; size ( cards, 2 ) = ", g0 )') size ( cards, 1 ), size ( cards, 2 )
                write ( *, '( "bytes needed to store cards = ", g0 )' ) sizeof ( cards )

                open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'replace', form = 'unformatted', &
                       access = 'stream', iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'OPEN', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) cards
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'READ', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                close ( unit = io, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'CLOSE', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                end if

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, 110 ) numCards, t_1 - t_0

            numCards = 3

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start
                deallocate ( cards, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 310 ) "integer ( INT8 )", "cards"
                    write ( *, 330 ) size ( cards )
                    write ( *, 340 ) alloc_status
                    write ( *, 350 ) trim ( alloc_msg )
                    stop stop_msg // me_program // '.'
                end if
                allocate ( cards ( 1 : nUnique**numCards, 1 : numCards ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 300 ) "integer ( INT8 )", "cards"
                    write ( *, 320 ) nUnique**numCards, nUnique
                    write ( *, 340 ) alloc_status
                    write ( *, 350 ) trim ( alloc_msg )
                    stop stop_msg // me_program // '.'
                end if

                count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            count = count + inc
                            cards ( count, : ) = [ j, k, h ]
                            !write ( *, '( "cards (", I4, ", : ) = ", 3I3 )' ) count, cards ( count, : )
                        end do
                    end do
                end do

                open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'replace', form = 'unformatted', &
                       access = 'stream', iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'OPEN', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) cards
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'READ', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                close ( unit = io, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'CLOSE', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                end if

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, 110 ) numCards, t_1 - t_0

            numCards = 4

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start
                deallocate ( cards, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 310 ) "integer ( INT8 )", "cards"
                    write ( *, 330 ) size ( cards )
                    write ( *, 340 ) alloc_status
                    write ( *, 350 ) trim ( alloc_msg )
                    stop stop_msg // me_program // '.'
                end if
                allocate ( cards ( 1 : nUnique**numCards, 1 : numCards ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 300 ) "integer ( INT8 )", "cards"
                    write ( *, 320 ) nUnique**numCards, nUnique
                    write ( *, 340 ) alloc_status
                    write ( *, 350 ) trim ( alloc_msg )
                    stop stop_msg // me_program // '.'
                end if

            call cpu_time ( t_0 ) ! local cpu time - start
                count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                count = count + inc
                                cards ( count, : ) = [ j, k, h, l ]
                            end do
                        end do
                    end do
                end do

                open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'replace', form = 'unformatted', &
                       access = 'stream', iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'OPEN', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) cards
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'READ', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                close ( unit = io, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'CLOSE', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                end if

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, 110 ) numCards, t_1 - t_0

            numCards = 5

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start

                deallocate ( cards, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 310 ) "integer ( INT8 )", "cards"
                    write ( *, 330 ) size ( cards )
                    write ( *, 340 ) alloc_status
                    write ( *, 350 ) trim ( alloc_msg )
                    stop stop_msg // me_program // '.'
                end if
                allocate ( cards ( 1 : nUnique**numCards, 1 : numCards ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 300 ) "integer ( INT8 )", "cards"
                    write ( *, 320 ) nUnique**numCards, nUnique
                    write ( *, 340 ) alloc_status
                    write ( *, 350 ) trim ( alloc_msg )
                    stop stop_msg // me_program // '.'
                end if

                count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                do m = 1, nUnique
                                    count = count + inc
                                    cards ( count, : ) = [ j, k, h, l, m ]
                                 end do
                            end do
                        end do
                    end do
                end do

                open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'replace', form = 'unformatted', &
                       access = 'stream', iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'OPEN', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) cards
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'READ', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                close ( unit = io, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'CLOSE', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                end if

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, 110 ) numCards, t_1 - t_0

            numCards = 6

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start

            deallocate ( cards, stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 310 ) "integer ( INT8 )", "cards"
                write ( *, 330 ) size ( cards )
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if
            allocate ( cards ( 1 : nUnique**numCards, 1 : numCards ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 300 ) "integer ( INT8 )", "cards"
                write ( *, 320 ) nUnique**numCards, nUnique
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                do m = 1, nUnique
                                    do n = 1, nUnique
                                        count = count + inc
                                        cards ( count, : ) = [ j, k, h, l, m, n ]
                                    end do
                                 end do
                            end do
                        end do
                    end do
                end do

                open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'replace', form = 'unformatted', &
                       access = 'stream', iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'OPEN', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) cards
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'READ', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                close ( unit = io, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'CLOSE', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                end if

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, 110 ) numCards, t_1 - t_0

            numCards = 7

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start

            deallocate ( cards, stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 310 ) "integer ( INT8 )", "cards"
                write ( *, 330 ) size ( cards )
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if
            allocate ( cards ( 1 : nUnique**numCards, 1 : numCards ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 300 ) "integer ( INT8 )", "cards"
                write ( *, 320 ) nUnique**numCards, nUnique
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                do m = 1, nUnique
                                    do n = 1, nUnique
                                        do o = 1, nUnique
                                            count = count + inc
                                            cards ( count, : ) = [ j, k, h, l, m, n, o ]
                                        end do
                                    end do
                                 end do
                            end do
                        end do
                    end do
                end do

                open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'replace', form = 'unformatted', &
                       access = 'stream', iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'OPEN', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) cards
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'READ', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                close ( unit = io, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'CLOSE', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                end if

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, 110 ) numCards, t_1 - t_0

            numCards = 8

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start

            deallocate ( cards, stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 310 ) "integer ( INT8 )", "cards"
                write ( *, 330 ) size ( cards )
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if
            allocate ( cards ( 1 : nUnique**numCards, 1 : numCards ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, 300 ) "integer ( INT8 )", "cards"
                write ( *, 320 ) nUnique**numCards, nUnique
                write ( *, 340 ) alloc_status
                write ( *, 350 ) trim ( alloc_msg )
                stop stop_msg // me_program // '.'
            end if

            count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                do m = 1, nUnique
                                    do n = 1, nUnique
                                        do o = 1, nUnique
                                            do p = 1, nUnique
                                                count = count + inc
                                                cards ( count, : ) = [ j, k, h, l, m, n, o, p ]
                                            end do
                                        end do
                                    end do
                                 end do
                            end do
                        end do
                    end do
                end do

                open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'replace', form = 'unformatted', &
                       access = 'stream', iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'OPEN', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) cards
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'READ', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                    stop 'fatal error for ' // me_program // '.'
                end if

                close ( unit = io, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( *, 200 ) 'CLOSE', file_name, io
                    write ( *, 210 ) io_status
                    write ( *, 220 ) trim( io_msg )
                end if

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, 110 ) numCards, t_1 - t_0

        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, '( /, "total cpu time used = ", g0, ": seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

  100   format ( /, "Writing permutations of ", g0, " to ", g0, "." )
  110   format ( "cpu time used for numCards = ", g0, ": ", g0, "s", / )

  200   format ( /, g0, ' error for file "', g0, '"  on unit ', g0 )
  210   format ( 'iostat = ', g0 )
  220   format ( 'io_msg = ', g0, '.', / )

  300         format ( /, "Error allocating memory for ", g0, " array ", g0, "." )
  310         format ( /, "Error deallocating memory for ", g0, " array ", g0, "." )
  320         format (    "  requested size is ", g0, " x ", g0, " elements" )
  330         format (    "  currect size is ", g0, " elements" )
  340         format (    "  stat = ", g0 )
  350         format (    "  errmsg = ", g0, "." )

end program write

! gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 runner.f95