module mVariableTypes

    use mPrecisionDefinitions,  only : ip
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg
    use mUnitValues

    implicit none

    contains

        subroutine variable_types ( io_unit, file_out )

            integer ( ip ),        intent ( in )            :: io_unit
            character ( len = * ), intent ( in ), optional  :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_variable_types ( io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
            else
                call print_variable_types ( io_unit )
            end if

        end subroutine variable_types

        subroutine print_variable_types ( myIO ) !   +   +   +   +   +   +   +   +   +   +   +   +   +   +      print_variable_types

            integer ( ip ), intent ( in ) :: myIO

                ! Annunciation
                call announce ( "Characterization of data types ", myIO )

                write ( myIO, 100 )
                write ( myIO, 110 )
                !   integer returned
                write ( myIO, 200 ) kind       ( xdef ),              REAL32,             REAL64,             REAL128
                write ( myIO, 210 ) sizeof     ( xdef ),      sizeof( x032 ),      sizeof( x064 ),      sizeof( x128 )
                write ( myIO, 220 ) digits     ( xdef ),      digits( x032 ),      digits( x064 ),      digits( x128 )
                write ( myIO, 230 ) precision  ( xdef ),   precision( x032 ),   precision( x064 ),   precision( x128 )
                write ( myIO, 240 ) range      ( xdef ),       range( x032 ),       range( x064 ),       range( x128 )
                write ( myIO, 250 ) radix      ( xdef ),       radix( x032 ),       radix( x064 ),       radix( x128 )
                write ( myIO, 260 ) maxexponent( xdef ), maxexponent( x032 ), maxexponent( x064 ), maxexponent( x128 )
                write ( myIO, 270 ) minexponent( xdef ), minexponent( x032 ), minexponent( x064 ), minexponent( x128 )
                !   real value returned
                write ( myIO, 300 ) epsilon( xdef ),     epsilon( x032 ),     epsilon( x064 ),     epsilon( x128 )
                write ( myIO, 310 ) tiny   ( xdef ),     tiny   ( x032 ),     tiny   ( x064 ),     tiny   ( x128 )
                write ( myIO, 320 ) huge   ( xdef ),     huge   ( x032 ),     huge   ( x064 ),     huge   ( x128 )
                write ( myIO, * )

                flush ( myIO )

                write ( myIO, 400 )
                write ( myIO, 410 )
                write ( myIO, 200 ) INT8,                             INT16,                INT32,                INT64
                write ( myIO, 210 ) sizeof     ( INT8 ),      sizeof( INT16 ),      sizeof( INT32 ),      sizeof( INT64 )
                write ( myIO, 220 ) digits     ( INT8 ),      digits( INT16 ),      digits( INT32 ),      digits( INT64 )

                write ( myIO, 240 ) range      ( INT8 ),       range( INT16 ),       range( INT32 ),       range( INT64 )
                write ( myIO, 250 ) radix      ( INT8 ),       radix( INT16 ),       radix( INT32 ),       radix( INT64 )
                write ( myIO, * )

                flush ( myIO )

                return

            100 format ( "REAL variables characterization" )
            110 format ( "REAL                         default         single                   double                quadruple" )

            200 format ( "KIND value:          ", 2I15, 2I25 )
            210 format ( "bytes:               ", 2I15, 2I25 )
            220 format ( "digits:              ", 2I15, 2I25 )
            230 format ( "precision:           ", 2I15, 2I25 )
            240 format ( "range:               ", 2I15, 2I25 )
            250 format ( "radix:               ", 2I15, 2I25 )
            260 format ( "maximum exponent:    ", 2I15, 2I25 )
            270 format ( "minimum exponent:    ", 2I15, 2I25 )

            300 format ( "epsilon:             ", 4 ( 2X, g0 ) )
            310 format ( "tiny:                ", 4 ( 2X, g0 ) )
            320 format ( "huge:                ", 4 ( 2X, g0 ) )

            400 format ( "INTEGER variables characterization" )
            410 format ( "INTEGER                         INT8          INT16                     INT32                    INT64" )

    end subroutine print_variable_types

end module mVariableTypes
