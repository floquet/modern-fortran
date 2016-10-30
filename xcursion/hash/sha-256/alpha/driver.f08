program driver

    use, intrinsic :: iso_fortran_env,  only : INT32, INT64
    use mSHA,                           only : Ch, Ma, two32

    implicit none

    integer, parameter  :: ip = INT32, bp = INT64
    character ( len = * ), parameter :: fmt1 = '( b32.32, 2X, g0, 2X, a )', &
                                        fmt2 = '( ''Ch ( E, F, G )'' )'

    integer ( ip ) :: E = 0, F = 0, G = 0

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

        write ( *, '( ''mod ( 2147483647_ip )'' )' )
        write ( *, fmt1 ) mod ( 2147483647_ip, two32 )

        write ( *, '( ''mod ( 2147483648_bp )'' )' )
        write ( *, fmt1 ) mod ( 2147483648_bp, 2147483647_bp )

        write ( *, '( ''mod ( 2147483649_bp )'' )' )
        write ( *, fmt1 ) mod ( 2147483649_bp, 2147483647_bp )

    ! 100 format ( Z12, ' hexadecimal value: ', /, I12, ' decimal value', / )
    ! 110 format ( 'Ch( E, F, G ) = ', g0, ': ', b32.32 )

    stop "program driver..."

end program driver
