program parser
    implicit none
    character ( len = 7 ), parameter :: blade = 'r23i6n0'
    character ( len = 2 )            :: rString = '', iString = '', nString = ''
    integer :: length = -1, length_blade = -1, rpos = -1, ipos = -1, npos = -1

        length_blade = len_trim ( blade )
        write ( *, 100 ) blade, length_blade
        if ( length_blade < 1 ) stop 'Bad string length; execution halts.'

        ! locate descriptors
        rpos = index ( blade, 'r' )
        write ( *, 110 ) 'r', rpos

        ipos = index ( blade, 'i' )
        write ( *, 110 ) 'i', ipos

        npos = index ( blade, 'n' )
        write ( *, 110 ) 'n', npos

        ! convert to string
        write ( rString, '( g0 )' ) blade ( rpos + 1 : ipos - 1 )
        !write ( *, 120 ) 'rack number', rString
        length = len_trim ( rString )
        if ( length == 1 ) rString = '0' // trim ( rString )
        write ( *, 130 ) rString, 'rack'

        write ( iString, '( g0 )' ) blade ( ipos + 1 : npos - 1 )
        write ( *, '( g0, "." )' ) trim ( blade ( ipos + 1 : npos - 1 ) )
        !write ( *, 120 ) 'icu number', iString
        length = len_trim ( iString )
        if ( length == 1 ) iString = '0' // trim ( iString )
        write ( *, 130 ) iString, 'icu'

        write ( nString, '( g0 )' ) blade ( npos + 1 : length_blade )
        !write ( *, 120 ) 'blade number', nString
        length = len_trim ( nString )
        if ( length == 1 ) nString = '0' // trim ( nString )
        write ( *, 130 ) nString, 'blade'

    100 format ( 'The string ', g0, ' has length ', g0, '.' )
    110 format ( 'The character ', g0, ' is at position ', g0, '.' )
    !120 format ( /, g0, ' = ', g0, '.' )
    130 format ( g0, ' : ', g0 )

    stop

! contains
!
!     function string_field ( string_in ) result ( string_out )
!         character ( len = * ) :: string_in
!         character ( len = 2 ) :: string_out
!         integer               :: length
!             write ( string_out, '( g0 )' )
!             length = len_trim ( string_in )
!             if ( length == 1 ) iString = '0' // trim ( iString )
!     end subroutine string_field

end program parser
