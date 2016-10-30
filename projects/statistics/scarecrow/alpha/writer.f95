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

    integer ( ip )                   :: j = 0, k = 0, h = 0, l = 0, m = 0, n = 0
    integer ( ip )                   :: numCards = 0
    integer ( bp )                   :: count = 1, inc = 1_bp

    integer ( ip )                   :: list2 ( 1 : nUnique**2, 1 : 2 )
    integer ( ip )                   :: list3 ( 1 : nUnique**3, 1 : 3 )
    integer ( ip )                   :: list4 ( 1 : nUnique**4, 1 : 4 )
    integer ( ip )                   :: list5 ( 1 : nUnique**5, 1 : 5 )
    integer ( ip )                   :: list6 ( 1 : nUnique**6, 1 : 6 )

    integer ( ip )                   :: io = 0, io_status = 0

    real    ( wp )                   :: cpu_0 = 0, cpu_1 = 0, t_0 = 0, t_1 = 0

    character ( len = 512 )          :: io_msg = ""
    character ( len = 128 )          :: file_name = ""

    character ( len = * ), parameter :: me_program = 'program write'  ! self-identification

        call cpu_time ( cpu_0 ) ! global cpu time - start

            numCards = 2

            write ( file_name, '( A, g0, A )' ) "cards_", numCards, ".int8"
            write ( *, 100 ) numCards, trim ( file_name )

            call cpu_time ( t_0 ) ! local cpu time - start
                count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        count = count + inc
                        list2 ( count, : ) = [ j, k ]
                        write ( *, '( "list2 (", I4, ", : ) = ", 2I3 )' ) count, list2 ( count, : )
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

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) list2
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
                count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            count = count + inc
                            list3 ( count, : ) = [ j, k, h ]
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

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) list3
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
                count = 0
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                count = count + inc
                                list4 ( count, : ) = [ j, k, h, l ]
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

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) list4
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
                count = 0_bp
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                do m = 1, nUnique
                                    count = count + inc
                                    list5 ( count, : ) = [ j, k, h, l, m ]
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

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) list4
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
                count = 0_bp
                do j = 1, nUnique
                    do k = 1, nUnique
                        do h = 1, nUnique
                            do l = 1, nUnique
                                do m = 1, nUnique
                                    do n = 1, nUnique
                                        count = count + inc
                                        list6 ( count, : ) = [ j, k, h, l, m, n ]
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

                write  ( unit = io, iostat = io_status, iomsg = io_msg ) list4
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

end program write

! gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 runner.f95