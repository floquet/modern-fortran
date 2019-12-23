module mEpsilonTests

    use mPrecisionDefinitions,  only : ip, sp, dp, qp, ascii
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg
    use mUnitValues

    implicit none

    contains

        subroutine epsilon_test ( io_unit, file_out )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +    epsilon_test

            integer ( ip ),        intent ( in )           :: io_unit
            character ( len = * ), intent ( in ), optional :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_epsilon_test ( io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
            else
                call print_epsilon_test ( io_unit )
            end if

        end subroutine epsilon_test

        subroutine print_epsilon_test ( io_unit )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +  print_epsilon_test

            integer ( ip ) , intent ( in )  :: io_unit
            real                            :: adef, bdef, cdef, edef
            real ( sp )                     :: asp,  bsp,  csp,  esp
            real ( dp )                     :: adp,  bdp,  cdp,  edp
            real ( qp )                     :: aqp,  bqp,  cqp,  eqp

                !   Annunciation
                call announce ( "Machine epsilon", io_unit )

                write ( io_unit, '( /, "intrinsic EPSILON ( 1 ):" )' )

                write ( io_unit, 110 ) epsilon ( xdef )
                write ( io_unit, 120 ) epsilon ( x032 )
                write ( io_unit, 130 ) epsilon ( x064 )
                write ( io_unit, 140 ) epsilon ( x128 )

                write ( io_unit, 100 )
                write ( io_unit, 110 ) xdef + epsilon ( xdef )
                write ( io_unit, 120 ) x032 + epsilon ( x032 )
                write ( io_unit, 130 ) x064 + epsilon ( x064 )
                write ( io_unit, 140 ) x128 + epsilon ( x128 )


                write ( io_unit, '( //, "Moler epsilon:" )' )

                    adef = 4.0 / 3
                    bdef = adef - 1
                    cdef = bdef + bdef + bdef
                    edef = cdef - 1

                    asp = 4.0_sp / 3
                    bsp = asp - 1
                    csp = bsp + bsp + bsp
                    esp = csp - 1

                    adp = 4.0_dp / 3
                    bdp = adp - 1
                    cdp = bdp + bdp + bdp
                    edp = cdp - 1

                    aqp = 4.0_qp / 3
                    bqp = aqp - 1
                    cqp = bqp + bqp + bqp
                    eqp = cqp - 1

                write ( io_unit, 110 ) edef
                write ( io_unit, 120 ) esp
                write ( io_unit, 130 ) edp
                write ( io_unit, 140 ) eqp

                write ( io_unit, 100 )
                write ( io_unit, 110 ) xdef + edef
                write ( io_unit, 120 ) x032 + esp
                write ( io_unit, 130 ) x064 + edp
                write ( io_unit, 140 ) x128 + eqp


                write ( io_unit, '( //, "bisection epsilon:" )' )

                    edef = 1.0
                    do
                        edef = edef / 2
                        if ( .not. ( 1 + edef > 1 ) ) exit
                    end do
                    edef = edef * 2

                    esp = 1.0
                    do
                        esp = esp / 2
                        if ( .not. ( 1 + esp > 1 ) ) exit
                    end do
                    esp = esp * 2

                    edp = 1.0
                    do
                        edp = edp / 2
                        if ( .not. ( 1 + edp > 1 ) ) exit
                    end do
                    edp = edp * 2

                    eqp = 1.0
                    do
                        eqp = eqp / 2
                        if ( .not. ( 1 + eqp > 1 ) ) exit
                    end do
                    eqp = eqp * 2

                write ( io_unit, 110 ) edef
                write ( io_unit, 120 ) esp
                write ( io_unit, 130 ) edp
                write ( io_unit, 140 ) eqp

                write ( io_unit, 100 )
                write ( io_unit, 110 ) xdef + edef
                write ( io_unit, 120 ) x032 + esp
                write ( io_unit, 130 ) x064 + edp
                write ( io_unit, 140 ) x128 + eqp

                write ( io_unit, '( //, "decimation epsilon:" )' )

                    edef = 1.0
                    do
                        edef = edef / 10
                        if ( .not. ( 1 + edef > 1 ) ) exit
                    end do
                    edef = edef * 10

                    esp = 1.0
                    do
                        esp = esp / 10
                        if ( .not. ( 1 + esp > 1 ) ) exit
                    end do
                    esp = esp * 10

                    edp = 1.0
                    do
                        edp = edp / 10
                        if ( .not. ( 1 + edp > 1 ) ) exit
                    end do
                    edp = edp * 10

                    eqp = 1.0
                    do
                        eqp = eqp / 10
                        if ( .not. ( 1 + eqp > 1 ) ) exit
                    end do
                    eqp = eqp * 10

                write ( io_unit, 110 ) edef
                write ( io_unit, 120 ) esp
                write ( io_unit, 130 ) edp
                write ( io_unit, 140 ) eqp

                write ( io_unit, 100 )
                write ( io_unit, 110 ) xdef + edef
                write ( io_unit, 120 ) x032 + esp
                write ( io_unit, 130 ) x064 + edp
                write ( io_unit, 140 ) x128 + eqp

                return

            100 format ( /, "EPSILON TEST: 1 + epsilon = ?" )
            110 format ( "default precision: ", g0 )
            120 format ( "single precision:  ", g0 )
            130 format ( "double precision:  ", g0 )
            140 format ( "quad precision:    ", g0 )

        end subroutine print_epsilon_test

end module mEpsilonTests
