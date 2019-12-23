module mCharacterEncodings

    use mPrecisionDefinitions,  only : ip, sp, dp, qp, ascii
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg

    implicit none

    contains

        subroutine character_sets ( io_unit, file_out )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +    character_sets

            integer ( ip ),        intent ( in )           :: io_unit
            character ( len = * ), intent ( in ), optional :: file_out

            integer ( ip )                                 :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_character_sets ( io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
                call print_character_sets ( io_unit )
            else
                call print_character_sets ( io_unit )
            end if

        end subroutine character_sets

        subroutine print_character_sets ( io_unit )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +  print_character_sets

            integer ( ip ), intent ( in ) :: io_unit

            integer, parameter :: def     = selected_char_kind ( 'DEFAULT' )    ! required by Fortran standard
            integer, parameter :: kindA   =               kind ( 'A' )          ! Metcalf, Reid, Cohen: p. 309
            integer, parameter :: ucs4    = selected_char_kind ( 'ISO_10646' )  ! optional
            integer, parameter :: ascii   = selected_char_kind ( 'ASCII' )      ! optional
            integer, parameter :: ebcdic  = selected_char_kind ( 'EBCDIC' )     ! optional
            integer, parameter :: unicode = selected_char_kind ( 'UNICODE')     ! optional
            integer, parameter :: greek   = selected_char_kind ( 'ISO-8859-7' ) ! optional
            integer, parameter :: guobiao = selected_char_kind ( 'Guobiao' )    ! optional
            integer, parameter :: gb18030 = selected_char_kind ( 'GB 18030' )   ! optional
            integer, parameter :: big5    = selected_char_kind ( 'Big 5' )      ! optional
            integer, parameter :: utf8    = selected_char_kind ( 'UTF-8' )      ! optional

                call announce ( "Character set kind requests", io_unit )

                write ( io_unit, '( "default     = ", I4 )' )                                                         def
                write ( io_unit, '( "kind( ''A'' ) = ", I4 )' )                                                       kindA
                write ( io_unit, '( "ASCII       = ", I4, ": American Standard Code for Information Interchange" )' ) ascii
                write ( io_unit, '( "EBCDIC      = ", I4, ": Extended Binary Coded Decimal Interchange Code" )' )     ebcdic
                write ( io_unit, '( "UNICODE     = ", I4, ": UNICODE consortium" )' )                                 unicode
                write ( io_unit, '( "ISO_10646   = ", I4, ": ISO/IEC 10646 UCS-4" )' )                                ucs4
                write ( io_unit, '( "ISO-8859-7  = ", I4, ": ISO/IEC 8859-7: Latin/Greek alphabet" )' )               greek
                write ( io_unit, '( "Guobiao     = ", I4, ": Chinese (Mainland, Singapore)" )' )                      guobiao
                write ( io_unit, '( "GB18030     = ", I4, ": Chinese (Current Guobiao version)" )' )                  gb18030
                write ( io_unit, '( "Big5        = ", I4, ": Chinese (Taiwan, Hong Kong, Macau" )' )                  big5
                write ( io_unit, '( "UTF-8       = ", I4, ": UCS Transformation Format—8-bit" )' )                    utf8

        end subroutine print_character_sets

end module mCharacterEncodings
