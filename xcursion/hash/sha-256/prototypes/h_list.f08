program hex_reader
    use, intrinsic :: iso_fortran_env, only : IOSTAT_END, IOSTAT_EOR, INT64
    use mFileHandling, only : safeopen_readonly, safeopen_writereplace
    implicit none
    integer, parameter :: ip = INT64
    integer ( ip )     :: decimal = 0
    integer            :: iostat = 0, io_hex = 0, io_int = 0
    character ( len = 512 )  :: iomsg = ''

        io_hex = safeopen_readonly     ( 'H_16.txt' )
        io_int = safeopen_writereplace ( 'H_10.txt' )
        do
            read ( io_hex, '( Z8 )', iostat = iostat, iomsg = iomsg ) decimal
            if ( iostat == IOSTAT_END ) then
                write ( *, '( ''IOSTAT_END = '', g0 )' ) IOSTAT_END
                exit
            end if
            if ( iostat == IOSTAT_EOR ) then
                write ( *, '( ''IOSTAT_EOR = '', g0 )' ) IOSTAT_EOR
                exit
            end if
            if ( iostat /= 0 ) then
                write ( *, '( ''How did I get here?'' )' )
                exit
            end if
            write ( *, '( "decimal value = ", g0 )' ) decimal
            write ( *, '( "decimal value = ", g0, "; hex value = ", Z8.8 )' ) decimal, decimal
            write ( io_int, '( I10.10 )' ) decimal
        end do

end program hex_reader
