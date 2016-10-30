program hex_reader
    use, intrinsic :: iso_fortran_env, only : IOSTAT_END, IOSTAT_EOR, INT32
    use mFileHandling, only : safeopen_readonly
    implicit none
    integer, parameter :: ip = INT32
    integer   :: iostat = 0, decimal = 0, io_hex = 0
    !integer ( ip ), parameter :: myInteger ( 1 : 2 ) = [ z'6a09e667', z'bb67ae85' ]
    !integer ( ip ) :: myInteger ( 1 : 2 ) = [ z'6a09e667', z'bb67ae85' ]
    character ( len = 512 )  :: iomsg = ''
        io_hex = safeopen_readonly ( 'hex.txt' )
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
        end do

        write ( *, '( ''hex constant = '', g0 )' ) z'6a09e667'

        ! write ( *, '( ''myInteger = '', g0 )' ) myInteger

end program hex_reader
