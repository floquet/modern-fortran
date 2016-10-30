module mModConverters

    use, intrinsic :: iso_fortran_env, only : INT8, INT16, INT32, INT64
    implicit none

    character ( len = * ), parameter :: fmt_alpha = '( "READ error reading string ''", g0, "'' to number" )'

contains

    function str_to_int_def ( string ) result ( number )

        character ( len = * ), intent ( in )  :: string
        integer                               :: number

        character ( len = 512 ) :: io_msg = ''
        integer :: io_stat = 0

            read ( unit = string, fmt = *, IOSTAT = io_stat, IOMSG = io_msg  ) number

            if ( io_stat /= 0 ) then
                write ( unit = *, fmt = '( "READ error reading string ''", g0, "'' to number" )' ) trim( string )
                write ( unit = *, fmt = '( "IOSTAT = ", g0, /, "IOMSG = ", g0, "." )' ) io_stat, trim( io_msg )
                stop 'Fatal error in str_to_int_def...'
            end if

    end function str_to_int_def

    function str_to_INT8 ( string ) result ( number )

        character ( len = * ), intent ( in )  :: string
        integer ( INT8 )                      :: number

        character ( len = 512 ) :: io_msg = ''
        integer :: io_stat = 0

            read ( unit = string, fmt = *, IOSTAT = io_stat, IOMSG = io_msg  ) number

            if ( io_stat /= 0 ) then
                write ( unit = *, fmt = fmt_alpha ) trim( string )
                write ( unit = *, fmt = '( "IOSTAT = ", g0, /, "IOMSG = ", g0, "." )' ) io_stat, trim( io_msg )
                stop 'Fatal error in str_to_INT8...'
            end if

    end function str_to_INT8

end module mModConverters
