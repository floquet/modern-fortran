module mISOVariables

    use mPrecisionDefinitions,  only : ip, ascii
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg

    implicit none

    contains

        subroutine iso_variables ( io_unit, file_out )

            integer ( ip ),        intent ( in )           :: io_unit
            character ( len = * ), intent ( in ), optional :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_iso_variables ( io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
            end if

            call print_iso_variables ( io_unit )

        end subroutine iso_variables

        subroutine print_iso_variables ( io_unit )                                                                  !  iso_variables

            use, intrinsic :: iso_fortran_env

            integer ( ip ) , intent ( in ) :: io_unit

                !   Annunciation
                call announce ( "ISO Fortran environment variables", io_unit )

                !   ISO_FORTRAN environment variables
                write ( io_unit, 100 )
                write ( io_unit, 110 )
                write ( io_unit, 120 ) INPUT_UNIT
                write ( io_unit, 130 ) OUTPUT_UNIT
                write ( io_unit, 140 ) ERROR_UNIT
                write ( io_unit, 150 )
                write ( io_unit, 160 ) IOSTAT_END
                write ( io_unit, 170 ) IOSTAT_EOR
                write ( io_unit, 180 )
                write ( io_unit, 190 ) CHARACTER_STORAGE_SIZE
                write ( io_unit, 200 ) NUMERIC_STORAGE_SIZE
                write ( io_unit, 210 ) FILE_STORAGE_SIZE

                write ( io_unit, 300 )
                write ( io_unit, 310 ) REAL32, REAL64, REAL128

                write ( io_unit, 400 )
                write ( io_unit, 410 ) INT8, INT16, INT32, INT64

                flush ( io_unit )

                return

            100 format ( "use ISO_FORTRAN_ENV", / )
            110 format ( "IO units" )
            120 format ( "INPUT_UNIT  = ", g0, ": preconnected standard input  unit" )
            130 format ( "OUTPUT_UNIT = ", g0, ": preconnected standard output unit" )
            140 format ( "ERROR_UNIT  = ", g0, ": preconnected output unit for error reporting" )
            150 format ( "IOSTAT return values " )
            160 format ( "IOSTAT_END  = ", g0, ": end-of-file signal" )
            170 format ( "IOSTAT_EOR  = ", g0, ": end-of-record signal" )
            180 format ( "storage sizes" )
            190 format ( "CHARACTER_STORAGE_SIZE =  ", g0, ": Size in bits of the character storage unit" )

            200 format ( "NUMERIC_STORAGE_SIZE   = ", g0, ": Size in bits of the numeric storage unit" )
            210 format ( "FILE_STORAGE_SIZE      =  ", g0, ": Size in bits of the file storage unit", / )

            300 format ( "real kind types REAL(bits) = kind type value" )
            310 format ( "REAL32 = ", g0, ", REAL64 = ", g0,  ", REAL128 = ", g0, / )

            400 format ( "integer kind types: INT(bits) = kind type value" )
            410 format ( "INT8 = ", g0, ", INT16 = ", g0,  ", INT32 = ", g0,  ", INT64 = ", g0, / )

        end subroutine print_iso_variables

end module mISOVariables
