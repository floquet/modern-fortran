program hex_writer
    use, intrinsic :: iso_fortran_env, only : IOSTAT_END, IOSTAT_EOR, INT64
    use mFileHandling, only : safeopen_readonly, safeopen_writereplace
    implicit none
    integer, parameter :: ip = INT64
    integer ( ip )     :: decimal = 0
    integer            :: iostat = 0, io_hex = 0, io_int = 0
    character ( len = 512 )  :: iomsg = ''

        io_int = safeopen_readonly     ( 'K_10.txt' )
        io_hex = safeopen_writereplace ( 'K_16.txt' )
        do
            read ( unit = io_int, fmt = '( I10 )', iostat = iostat, iomsg = iomsg ) decimal
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
                write ( *, '( ''iostat = '', g0 )' ) iostat
                write ( *, '( ''iomsg  = '', g0 )' ) iomsg
                exit
            end if
            write ( *, '( "decimal value = ", g0, "; hex value = ", Z8.8 )' ) decimal, decimal
            write ( io_hex, '( Z8.8 )' ) decimal
        end do

end program hex_writer
