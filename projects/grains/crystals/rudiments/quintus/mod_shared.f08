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

end module mShared
