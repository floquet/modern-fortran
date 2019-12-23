module mMatrixWriter

    use mPrecisionDefinitions, only : ip, rp
    implicit none

contains

    subroutine print_matrix ( A, myFormat, spaces, moniker, dims )

        real ( rp ),           intent ( in )           :: A ( : , : )

        integer ( ip ),        intent ( in )           :: spaces
        integer ( ip ),        intent ( in ), optional :: dims ( 2 )

        character ( len = * ), intent ( in ), optional :: moniker
        character ( len = * ), intent ( in )           :: myFormat

        integer ( ip )                                 :: myShape ( 2 ) = 0
        integer ( ip )                                 :: row = 0, col = 0, rows = 0, cols = 0

        character ( len = 25 )                         :: fmt_str

            myShape = shape ( A )
            if ( .not. present ( dims ) ) then
                rows = myShape ( 1 )
                cols = myShape ( 2 )
            else
                rows = dims ( 1 )
                cols = dims ( 2 )
            end if

            if ( present ( moniker ) ) then
                write ( * , 200 ) moniker, rows, cols
            else
                write ( * , 210 ) rows, cols
            end if

            write ( fmt_str, 100 ) cols, myFormat, spaces
            !write ( * , * ) fmt_str

            do row = 1, rows
                !write ( * , fmt = '( 4( F10.4, 2X ) )    ' )  ( A ( row, col ), col = 1, N )
                write ( * , fmt = fmt_str )  ( A ( row, col ), col = 1, cols )
            end do

        return

        100   format ( "( ", g0, "( ", g0, ", ", g0,"X ) )" )

        200   format ( /, g0,' has ', g0, ' rows and ', g0, ' columns.' )
        210   format ( /, 'Target matrix has ', g0, ' rows and ', g0, ' columns.' )

    end subroutine print_matrix

end module mMatrixWriter
