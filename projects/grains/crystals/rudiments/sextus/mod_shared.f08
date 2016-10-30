! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mShared

    use mPrecisionDefinitions, only : ip
    use mFormatDescriptors

    implicit none

    ! shared variables
    integer ( ip )                           :: kRead = 0
    integer ( ip )                           :: nElements = 0
    integer ( ip )                           :: io_stat = 0, alloc_status = 0

    character ( len = 512 )                  :: io_msg = '', alloc_msg = ''
    character ( len = 512 )                  :: precision_type = '', de = ''

contains

    subroutine alloc_alert ( de, myArray, nElements, data_type )

        character ( len = * ), intent ( in ) :: de, myArray, data_type
        integer ( ip )       , intent ( in ) :: nElements

        if ( alloc_status /= 0 ) then
            write ( * , fmt = fmt_allocerror ) trim ( de ), trim ( myArray ), nElements, trim ( data_type )
            write ( * , fmt = fmt_allocstat  ) trim ( de ), alloc_status
            write ( * , fmt = fmt_allocmsg   ) trim ( alloc_msg )
        end if

    end subroutine alloc_alert

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         open_file

    subroutine open_file_output ( file_name, io_unit, stop_msg )

        integer ( ip ),        intent ( out )           :: io_unit
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
                open ( newunit = io_unit, file = file_name, status = myStatus, action = myAction, position = myPosition )
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror     ) 'OPEN', trim ( file_name ), io_unit
                    write ( * , fmt = fmt_iostat      ) io_stat
                    write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings3 ) myStatus, myAction, myPosition
                    if ( stop_on_error ) stop
                    write ( * , * )                     stop_msg
                    stop
                end if
            else
                myStatus = "new"
                open ( newunit = io_unit, file = file_name, status = myStatus, action = myAction )
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror     ) 'OPEN', trim ( file_name ), io_unit
                    write ( * , fmt = fmt_iostat      ) io_stat
                    write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings4 ) myStatus, myAction
                    if ( stop_on_error ) stop
                    write ( * , * )                     stop_msg
                    stop
                end if
            end if

    end subroutine open_file_output

end module mShared
