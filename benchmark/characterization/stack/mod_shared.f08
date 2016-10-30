! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mShared

    use mPrecisionDefinitions, only : ip, zint, ascii
    use mParameters,           only : stdout

    implicit none

    ! shared variables
    integer ( ip )          :: io_unit   = 0
    integer ( ip )          :: kRead     = 0
    integer ( ip )          :: nElements = 0
    integer ( ip )          :: io_status = 0, alloc_status = 0

    character ( len = 512 ) :: io_msg, alloc_msg
    character ( len = 512 ) :: precision_type, de

    contains

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                          alert_io

    subroutine alert_io ( task, file_name, myIO, fatal, myStatus, myAction, myPosition, stop_msg, io_handle )

        integer ( ip ),        intent ( in ), optional :: myIO, io_handle
        character ( len = * ), intent ( in )           :: task, file_name
        character ( len = * ),                optional :: myStatus, myAction, myPosition, stop_msg
        logical,                              optional :: fatal

        integer ( ip ) :: io_out

            if ( io_status == 0  ) return

            io_out = stdout
            if ( present ( myIO ) ) io_out = myIO

            write ( io_out, * )

            if ( present ( io_handle ) ) then
                write ( io_out, 100 ) trim ( task ), trim ( file_name ), io_handle
            else
                write ( io_out, 110 ) trim ( task ), trim ( file_name ), io_unit
            endif

            write ( io_out, 120 ) io_status
            write ( io_out, 130 ) trim ( io_msg )

            ! conditionals
            if ( present ( myStatus ) ) write ( io_out, 200 ) trim ( myStatus )
            if ( present ( myStatus ) ) write ( io_out, 210 ) trim ( myAction )
            if ( present ( myStatus ) ) write ( io_out, 220 ) trim ( myPosition )

            if ( present ( fatal ) ) then
                if ( fatal ) then
                    if ( present ( stop_msg ) ) write ( io_unit, '( g0 )' ) trim ( stop_msg )
                    stop "Execution terminated."
                else
                    write ( io_out, 300 )
                    write ( io_out, * )
                endif
            endif

            flush ( io_out )

            return

            100 format ( /, g0, " error for file ", g0, " on io unit ", g0, "." )
            110 format ( /, g0, " error for file ", g0, " on newunit ", g0, "." )
            120 format ( "Error code: ", g0,   "." )
            130 format ( "Error message: ", A, "." )

            200 format ( "status = ", A, "." )
            210 format ( "action = ", A, "." )
            220 format ( "position = ", A, "." )

            300 format ( "Returning to calling routine ...", / )

    end subroutine alert_io

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         open_file

    subroutine alert_alloc ( de, myArray, nElements, data_type, myIO, fatal )

        character ( len = * ), intent ( in ) :: de, myArray, data_type
        integer ( zint ),        intent ( in ) :: nElements
        integer ( ip ), optional             :: myIO
        integer ( ip )                       :: io_out
        logical,        optional             :: fatal

            if ( io_status == 0  ) return

            io_out = stdout
            if ( present ( myIO ) ) io_out = myIO

            write ( io_out, fmt = 100 ) trim ( de ), trim ( myArray ), nElements, trim ( data_type )
            write ( io_out, fmt = 110 ) trim ( de ), alloc_status
            write ( io_out, fmt = 120 ) trim ( alloc_msg )

            if ( present ( fatal ) ) then
                if ( fatal ) stop "Fatal error"
            else
                write ( io_out, 300 )
                write ( io_out, * )
            endif

            return

            100 format ( "Failure to ", g0, "allocate the array ", g0, " which has ", g0, " elements of type ", g0, "." )
            110 format ( "stat = ", g0, "." )
            120 format ( "errmsg  = ", g0, "." )

            300 format ( "Returning to calling routine ...", / )

            flush ( io_out )

    end subroutine alert_alloc

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  open_file_output

    subroutine open_file_output ( file_name, io_out, stop_msg )

        integer ( ip ),        intent ( out )           :: io_out
        character ( len = * ), intent ( in )            :: file_name
        character ( len = * ), intent ( in ), optional  :: stop_msg

        character ( len = 512 )             :: myStatus    = 'old'
        character ( len = * ), parameter    :: myAction    = 'write'
        character ( len = * ), parameter    :: myPosition  = 'rewind'!'append'

        logical                             :: file_exists = .false., stop_on_error = .false.

            if ( present ( stop_msg ) ) stop_on_error = .true.

            ! open file for writing
            ! https://stackoverflow.com/questions/15526203/single-command-to-open-a-file-or-create-it-and-the-append-data
            inquire ( file = file_name, exist = file_exists )

            if ( file_exists ) then
                myStatus = "old"
                open ( newunit = io_out, file = file_name, status = myStatus, action = myAction, position = myPosition )

                call alert_io ( task = 'OPEN', file_name = file_name, myIO = stdout, fatal = stop_on_error, stop_msg = stop_msg, &
                                myStatus = myStatus, myAction = myAction, myPosition = myPosition )
            else
                myStatus = "new"
                open ( newunit = io_out, file = file_name, status = myStatus, action = myAction )

                call alert_io ( task = 'OPEN', file_name = file_name, myIO = stdout, fatal = stop_on_error, stop_msg = stop_msg, &
                                myStatus = myStatus, myAction = myAction )
            end if

    end subroutine open_file_output

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         timestamp

    character ( kind = ascii, len = 30 ) function timestamp ( ) result ( now ) ! 2013-06-06  19:47:03  UCT-0600

        integer ( ip ) , dimension ( 8 )        :: values  ! DTG ( date time group )

        character ( kind = ascii, len =  8 )    :: date ! DTG
        character ( kind = ascii, len = 10 )    :: time ! DTG
        character ( kind = ascii, len =  5 )    :: zone ! DTG

            ! timestamp
            call date_and_time ( date, time, zone, values )

            write  ( now, 100 )  date ( 1 : 4 ), date ( 5 : 6 ), date ( 7 : 8 ), &
                                 time ( 1 : 2 ), time ( 3 : 4 ), time ( 5 : 6 ), "UCT", zone
        100 format ( a, "-", a, "-", a, 2X, a, ":", a, ":", a, 2X, a, a )

    end function timestamp

end module mShared
