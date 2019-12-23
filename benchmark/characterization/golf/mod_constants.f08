module mConstants

    use mPrecisionDefinitions,  only : ip, sp, dp, qp, ascii
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg
    use mUnitValues

    implicit none

    contains

        subroutine constants ( io_unit, file_out )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +  constants

            integer ( ip ),        intent ( in )           :: io_unit
            character ( len = * ), intent ( in ), optional :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_constants ( io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
            else
                call print_constants ( io_unit )
            end if

        end subroutine constants

        subroutine print_constants ( io_unit )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +    print_constants

            integer ( ip ), intent ( in )   :: io_unit
            integer ( ip )                  :: divisor = 1_ip

            ! constants pi and e
            real                            :: pidef = 0.0,    edef = 0.0
            real ( sp )                     :: pi32  = 0.0_sp, e32  = 0.0_sp
            real ( dp )                     :: pi64  = 0.0_dp, e64  = 0.0_dp
            real ( qp )                     :: pi128 = 0.0_qp, e128 = 0.0_qp

                !   Annunciation
                call announce ( "Basic constants", io_unit )

                !   write the unit values in each precision
                write  ( io_unit, 100 )
                write  ( io_unit, 110 ) xdef
                write  ( io_unit, 120 ) x032
                write  ( io_unit, 130 ) x064
                write  ( io_unit, 140 ) x128
                write  ( io_unit, 150 )

                !   basic constants which humans recognize
                divisor = 3
                write  ( io_unit, 200 ) "one third"
                write  ( io_unit, 210 ) "3", xdef / divisor
                write  ( io_unit, 220 ) "3", x032 / divisor
                write  ( io_unit, 230 ) "3", x064 / divisor
                write  ( io_unit, 240 ) "3", x128 / divisor
                write  ( io_unit, 150 )

                divisor = 11
                write  ( io_unit, 200 ) "one eleventh"
                write  ( io_unit, 210 ) "11", xdef / divisor
                write  ( io_unit, 220 ) "11", x032 / divisor
                write  ( io_unit, 230 ) "11", x064 / divisor
                write  ( io_unit, 240 ) "11", x128 / divisor
                write  ( io_unit, 150 )

                !   create pi to machine precision
                pidef = acos( -xdef )
                pi32  = acos( -x032 )
                pi64  = acos( -x064 )
                pi128 = acos( -x128 )

                write  ( io_unit, 300 )
                write  ( io_unit, 310 )
                write  ( io_unit, 320 ) "pi", pidef
                write  ( io_unit, 330 ) "pi", pi32
                write  ( io_unit, 340 )
                write  ( io_unit, 350 ) "pi", pi64
                write  ( io_unit, 360 ) "pi", pi128
                write  ( io_unit, 150 )

                !   create e to machine precision
                edef  = exp( xdef )
                e32   = exp( x032 )
                e64   = exp( x064 )
                e128  = exp( x128 )

                write  ( io_unit, 370 )
                write  ( io_unit, 320 ) "e ", edef
                write  ( io_unit, 330 ) "e ", e32
                write  ( io_unit, 380 )
                write  ( io_unit, 350 ) "e ", e64
                write  ( io_unit, 360 ) "e ", e128
                write  ( io_unit, 150 )


                write  ( io_unit, 400 )
                write  ( io_unit, 410 ) cos( pidef ), sin ( pidef )
                write  ( io_unit, 420 ) cos( pi32 ),  sin ( pi32 )
                write  ( io_unit, 430 ) cos( pi64 ),  sin ( pi64 )
                write  ( io_unit, 440 ) cos( pi128 ), sin ( pi128 )
                write  ( io_unit, 150 )

                return

            100 format ( "Unit values" )
            110 format ( "default   = ", g0)
            120 format ( "single    = ", g0)
            130 format ( "double    = ", g0)
            140 format ( "quadruple = ", g0)
            150 format ( " " )

            200 format ( A )
            210 format ( "1 / ", A," = ", g0,          ": default precision" )
            220 format ( "1 / ", A," = ", g0,          ": single precision" )
            230 format ( "1 / ", A," = ", g0, ": double precision" )
            240 format ( "1 / ", A," = ", g0, ": quadruple precision" )

            300 format ( "fundamental constants", /, "pi = arccos ( -1 )" )
            310 format ( "pi = 3.1415927 Mathematica N[pi, 8]" )
            320 format ( A, " = ", g0, " default precision" )
            330 format ( A, " = ", g0, " single precision" )
            340 format ( "pi = 3.1415926535897932 Mathematica N[pi, 17]" )
            350 format ( A, " = ", g0, " double precision" )
            360 format ( A, " = ", g0, " quadruple precision" )
            370 format ( "e  = exp ( 0 )"/, "e  = 2.7182818 Mathematica N[e, 8]" )
            380 format ( "e  = 2.7182818284590452 Mathematica N[e, 17]" )

            400 format ( "computations using pi:" )
            410 format ( "cos( pi ) = ", g0, ",         sin ( pi ) = ", g0, ": default precision" )
            420 format ( "cos( pi ) = ", g0, ",         sin ( pi ) = ", g0, ": single precision" )
            430 format ( "cos( pi ) = ", g0, ", sin ( pi ) = ", g0, ": double precision" )
            440 format ( "cos( pi ) = ", g0, ", sin ( pi ) = ", g0, ": quadruple precision" )

        end subroutine print_constants

end module mConstants
