! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 02 07

! subroutine fileInquire
! subroutine byteSizeInquire
! subroutine test_stat

module mFileInquire

    use iso_fortran_env
    implicit none

    !   flags
    logical, private                 :: fname, fexist, fopen
    !   basics
    integer, private                 :: inumber
    character ( len = 64 ), private  :: cname
    !   records
    integer, private                 :: irecl, inextrec, ipos, iiolength
    !   access
    character ( len = 64 ), private  :: caccess, csequential, cdirect, cstream, cform, cformatted, cunformatted, cposition
    !   action
    character ( len = 64 ), private  :: caction, cread, cwrite, creadwrite
    !   asynchronous
    integer, private                 :: iid
    logical, private                 :: fpending
    character ( len = 64 ), private  :: casynchronous, cencoding
    !   preferences
    character ( len = 64 ), private  :: cblank, cdelim, cpad, cround, csign

    character ( len = * ), parameter, private :: myModule = 'module mFileInquire'  ! self-identification, error tracking

    contains

        !   ==============================================================================================

        subroutine fileInquire ( io_unit )                                                                             ! fileInquire

            integer, intent ( in )           :: io_unit
            integer                          :: io_status
            character ( len = 512 )          :: io_message

            character ( len = * ), parameter :: yn  = "'YES', 'NO'"
            character ( len = * ), parameter :: ynu = yn // " 'UNKNOWN'"
            character ( len = * ), parameter :: tf  = "'TRUE', 'FALSE'"
            character ( len = * ), parameter :: rnd = "'UP', 'DOWN', 'ZERO', 'NEAREST', 'COMPATIBLE', 'PROCESSOR DEFINED'"

            character ( len = * ), parameter :: mySubroutine = 'subroutine fileInquire'  ! self-identification
            character ( len = * ), parameter :: callChain    = 'Call chain: ' // myModule // ', ' // mySubroutine // '.'
            character ( len = * ), parameter :: message_stop = 'Fatal error. ' // callChain

            ! error check
            inquire ( unit = io_unit, iostat = io_status, iomsg = io_message )
            if ( io_status /= 0 ) then
                write ( * , fmt = '( "Error on INQUIRE for file unit ", g0 )' ) io_unit
                write ( * , fmt = '( "  iostat = ", g0 )' ) io_status
                write ( * , fmt = '( "  iomsg  = ", g0 )' ) trim ( io_message )
                stop message_stop
            end if

            ! basics
            inquire ( unit = io_unit, number = inumber, name = cname )
            ! flags
            inquire ( unit = io_unit, exist = fexist, opened = fopen, named = fname )
            ! records
            inquire ( unit = io_unit, recl = irecl, nextrec = inextrec, pos = ipos )!, iolength = iiolength )
            ! access
            inquire ( unit = io_unit, access = caccess, sequential = csequential, direct = cdirect, stream = cstream )
            inquire ( unit = io_unit, form = cform, formatted = cformatted, unformatted = cunformatted, position = cposition )
            ! action
            inquire ( unit = io_unit, action = caction, read = cread, write = cwrite, readwrite = creadwrite )
            ! asynchronous
            inquire ( unit = io_unit, asynchronous = casynchronous, encoding = cencoding, id = iid, pending = fpending )
            ! preferences
            inquire ( unit = io_unit, blank = cblank, delim = cdelim, pad = cpad, round = cround, sign = csign )

            ! Fortran 95/2003 for Scientists and Engineers, Stephen Chapman
            ! Table 14-5, p. 655

            write ( * , fmt = '( /, "File inquiry results on unit ", g0, ":" )' ) io_unit

            ! flags
            write ( * , fmt = '( "file name:    ", g0, T35, g0 )' ) trim ( cname ), ''
            write ( * , fmt = '( "exist:        ", g0, T35, g0 )' ) fexist, tf
            write ( * , fmt = '( "opened:       ", g0, T35, g0 )' ) fopen,  tf
            write ( * , fmt = '( "named:        ", g0, T35, g0 , / )' ) fname,  tf
            ! records
            write ( * , fmt = '( "recl:         ", g0, T35, g0 )' ) irecl,     'RECORD LENGTH for direct access file'
            write ( * , fmt = '( "nextrec:      ", g0, T35, g0 )' ) inextrec,  'last record + 1 for direct access file'
            !write ( * , fmt = '( "iolength:",     T15, g0, T15, g0 )' ) iiolength, 'length of unformatted record in pdu'
            write ( * , fmt = '( "pos:          ", g0, T35, g0 , / )' ) ipos,      'POSITION for next read or write'
            ! access
            write ( * , fmt = '( "access:       ", g0, T35, g0 )' ) trim ( caccess ), "'SEQUENTIAL', 'DIRECT', 'STREAM'"
            write ( * , fmt = '( "sequential:   ", g0, T35, g0 )' ) trim ( csequential ), ynu
            write ( * , fmt = '( "direct:       ", g0, T35, g0 )' ) trim ( cdirect ), ynu
            write ( * , fmt = '( "stream:       ", g0, T35, g0 )' ) trim ( cstream ), ynu
            write ( * , fmt = '( "position:     ", g0, T35, g0 )' ) trim ( cposition ), "'REWIND', 'APPEND', 'ASIS', 'UNDEFINED'"
            write ( * , fmt = '( "form:         ", g0, T35, g0 )' ) trim ( cform ), "'UNFORMATTED', 'FORMATTED'"
            write ( * , fmt = '( "formatted:    ", g0, T35, g0 )' ) trim ( cformatted ), ynu
            write ( * , fmt = '( "unformatted:  ", g0, T35, g0 , / )' ) trim ( cunformatted ), ynu
            ! action
            write ( * , fmt = '( "action:       ", g0, T35, g0 )' ) trim ( caction ), "'READ', 'WRITE', 'READWRITE', 'UNDEFINED'"
            write ( * , fmt = '( "read:         ", g0, T35, g0 )' ) trim ( cread ), ynu
            write ( * , fmt = '( "write:        ", g0, T35, g0 )' ) trim ( cwrite ), ynu
            write ( * , fmt = '( "readwrite:    ", g0, T35, g0 , / )' ) trim ( creadwrite ), ynu

            ! asynchronous
            write ( * , fmt = '( "asynchronous: ", g0, T35, g0 )' ) trim ( casynchronous ), yn
            write ( * , fmt = '( "encoding:     ", g0, T35, g0 )' ) trim ( cencoding ), "'UTF-8', 'UNDEFINED', 'UNKNOWN'"
            write ( * , fmt = '( "ID:           ", g0, T35, g0 )' ) iid, "id for pending asynchronous transfer"
            write ( * , fmt = '( "pending:      ", g0, T35, g0 , / )' ) fpending, tf

            ! preferences
            write ( * , fmt = '( "blank:        ", g0, T35, g0 )' ) trim ( cblank ), "'ZERO', 'NULL'"
            write ( * , fmt = '( "delim:        ", g0, T35, g0 )' ) trim ( cdelim ), "'APOSTROPHE', 'QUOTE', 'NONE', 'UNKNOWN'"
            write ( * , fmt = '( "pad:          ", g0, T35, g0 )' ) trim ( cpad ), yn
            write ( * , fmt = '( "round:        ", g0, T35, g0 )' ) trim ( cround ), rnd
            write ( * , fmt = '( "sign:         ", g0, T35, g0 )' ) trim ( csign ), "'PLUS', 'SUPPRESS', 'PROCESSOR DEFINED'"

        return

        end subroutine fileInquire

        !   ==============================================================================================

        subroutine byteSizeInquire ( )                                                                            ! byteSizeInquire

            ! type sizes
            ! https://stackoverflow.com/questions/14164967/sizeof-in-fortran
            write ( * , fmt = '( /, "Byte sizes for ISO_FORTRAN_ENV types:" )' )

            inquire ( iolength = iiolength ) 1.0_real32
            write ( * , fmt = '( "real32:       ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) 1.0_real64
            write ( * , fmt = '( "real64:       ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) 1.0_real128
            write ( * , fmt = '( "real128:      ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) 1_int8
            write ( * , fmt = '( "int8:         ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) 1_int16
            write ( * , fmt = '( "int16:        ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) 1_int32
            write ( * , fmt = '( "int32:        ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) 1_int64
            write ( * , fmt = '( "int64:        ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) .true.
            write ( * , fmt = '( ".true.:       ", I2, " bytes" )' ) iiolength

            inquire ( iolength = iiolength ) 'A'
            write ( * , fmt = '( "''A'':          ", I2, " bytes" )' ) iiolength

        return

        end subroutine byteSizeInquire

        !   ==============================================================================================

        ! https://gcc.gnu.org/onlinedocs/gfortran/STAT.html
        subroutine test_stat ( myFile )                                                                                  ! test_stat

            integer               :: buffer ( 1 : 13 )
            integer               :: status
            character ( len = * ) :: myFile

                call stat ( myFile, buffer, status)

                if ( status == 0 ) then
                    write ( * , fmt = " ( 'myFile:',                  T30,  g0 ) " ) myFile
                    write ( * , fmt = " ( 'Device ID:',               T30, I19 ) " ) buffer ( 1 )
                    write ( * , fmt = " ( 'Inode number:',            T30, I19 ) " ) buffer ( 2 )
                    write ( * , fmt = " ( 'File mode (octal):',       T30, O19 ) " ) buffer ( 3 )
                    write ( * , fmt = " ( 'Number of links:',         T30, I19 ) " ) buffer ( 4 )
                    write ( * , fmt = " ( 'Owner''s uid:',            T30, I19 ) " ) buffer ( 5 )
                    write ( * , fmt = " ( 'Owner''s gid:',            T30, I19 ) " ) buffer ( 6 )
                    write ( * , fmt = " ( 'Device where located:',    T30, I19 ) " ) buffer ( 7 )
                    write ( * , fmt = " ( 'File size:',               T30, I19 ) " ) buffer ( 8 )
                    write ( * , fmt = " ( 'Last access time:',        T30, A19 ) " ) ctime ( buffer ( 9 ) )
                    write ( * , fmt = " ( 'Last modification time',   T30, A19 ) " ) ctime ( buffer ( 10 ) )
                    write ( * , fmt = " ( 'Last status change time:', T30, A19 ) " ) ctime ( buffer ( 11 ) )
                    write ( * , fmt = " ( 'Preferred block size:',    T30, I19 ) " ) buffer ( 12 )
                    write ( * , fmt = " ( 'No. of blocks allocated:', T30, I19 ) " ) buffer ( 13 )
                end if

        end subroutine test_stat


end module mFileInquire
