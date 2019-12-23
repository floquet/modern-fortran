program driver

    use, intrinsic :: iso_fortran_env,  only : INT32
    use mSHA,                           only : Ch, Ma, Sigma_0, Sigma_1, mod_safe_232, two32

    implicit none

    integer, parameter  :: ip = INT32, bp = INT32
    character ( len = * ), parameter :: fmt1 = '( b32.32, 2X, g0, 2X, a )', &
                                        fmt2 = '( ''Ch ( E, F, G )'' )'
    character ( len = 8 )   :: hex = ''
    character ( len = 256 ) :: iomsg = ''
    integer ( bp ) :: E = 0, F = 0, G = 0
    integer ( bp ) :: myInt = 0, iostat = 0

        E = 14_ip
        F =  3_ip
        G = -8_ip

        ! myInt = Ch ( E, F, G )
        ! write ( *, 100 ) myInt, myInt
        ! write ( *, 110 ) myInt, myInt
        write ( *, fmt1 ) E, E, 'E'
        write ( *, fmt1 ) not ( E ), not ( E ), 'not ( E )'
        write ( *, fmt1 ) F, F, 'F'
        write ( *, fmt1 ) not ( F ), not ( F ), 'not ( F )'
        write ( *, fmt1 ) G, G, 'G'
        write ( *, fmt1 ) not ( G ), not ( G ), 'not ( G )'

        write ( *, fmt1 ) rshift ( E, 6 ), rshift ( E, 6 ), 'rshift ( E, 6 )'
        write ( *, fmt1 ) rshift ( F, 6 ), rshift ( F, 6 ), 'rshift ( F, 6 )'
        write ( *, fmt1 ) rshift ( G, 6 ), rshift ( G, 6 ), 'rshift ( G, 6 )'


        write ( *, fmt1 ) iand ( E, F ), iand ( E, F ), 'iand ( E, F )'
        write ( *, fmt1 ) not ( F ), not ( F ), 'not ( F )'
        write ( *, fmt1 ) not ( G ), G, 'G'
        write ( *, fmt1 ) iand ( not ( F ), G ), iand ( not ( F ), G ), 'iand ( not ( F ), G )'
        !write ( *, fmt1 ) 'iand ( not ( F ), G )', iand ( not ( F ), G ), iand ( not ( F ), G )
        !myResult = mod ( iand ( E, F ) + iand ( not ( F ), G ), two32 )
        write ( *, fmt2 )
        write ( *, fmt1 ) Ch ( E, F, G )

        write ( *, '( ''2**32'' )' )
        write ( *, fmt1 ) two32

        ! write ( *, '( ''2**32 + 1'' )' )
        ! write ( *, fmt1 ) two32 + 1
        !
        ! write ( *, '( ''2**32 + 2'' )' )
        ! write ( *, fmt1 ) two32 + 2

        write ( *, '( ''mod ( 2147483647_ip, two32 )'' )' )
        write ( *, fmt1 ) mod_safe_232 ( 2147483647_ip, two32 )

        write ( *, '( ''mod ( 2147483647_ip + 1 )'' )' )
        write ( *, fmt1 ) mod_safe_232 ( 2147483647_ip, 1 )

        write ( *, '( ''mod ( 2147483647_ip + 2 )'' )' )
        write ( *, fmt1 ) mod_safe_232 ( 2147483647_ip, 2 )

        write ( *, '( ''Sigma_0 ( E )'' )' )
        write ( *, fmt1 ) Sigma_0 ( E ), Sigma_0 ( E ), Sigma_0 ( E )

        hex = '6a09e667'
        read ( unit = myInt, format = 110, iostat = iostat, iomsg = iomsg ) hex
        if ( iostat /= 0 ) then
            write ( *, '( "iostat =  ", g0 )' ) iostat
            write ( *, '( "iomsg  =  ", g0, "." )' ) iomsg
        end if
        write ( *, 100 ) hex, myInt
    100 format ( 'hex = ', g0, '; integer value = ', g0 )
    110 format ( Z8 )

    ! 100 format ( Z12, ' hexadecimal value: ', /, I12, ' decimal value', / )
    ! 110 format ( 'Ch( E, F, G ) = ', g0, ': ', b32.32 )

    stop "program driver..."

end program driver
