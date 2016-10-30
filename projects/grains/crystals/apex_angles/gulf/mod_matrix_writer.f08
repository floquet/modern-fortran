module mMatrixWriter

    use mPrecisionDefinitions, only : ip, rp
    use mFileHandling,         only : safeopen_writereplace

    implicit none

contains

    !   +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    subroutine print_matrix ( A, myFormat, spaces, moniker, dims, my_io_unit )

        real ( rp ),           intent ( in )           :: A ( : , : )
        integer ( ip ),        intent ( in )           :: spaces, my_io_unit
        character ( len = * ), intent ( in )           :: myFormat

        integer ( ip ),        intent ( in ), optional :: dims ( 2 )
        character ( len = * ), intent ( in ), optional :: moniker

        integer ( ip )                                 :: myShape ( 2 )
        integer ( ip )                                 :: row, col, rows, cols

        character ( len = 25 )                         :: fmt_str = '' ! format descriptor, e.g. 5( E8.3 )

            if ( .not. present ( dims ) ) then ! has user specified a submatrix?
                myShape = shape ( A ) ! measure size of input matrix
                rows = myShape ( 1 )
                cols = myShape ( 2 )
            else
                rows = dims ( 1 )
                cols = dims ( 2 )
            end if

            if ( present ( moniker ) ) then  ! e.g. 'codomain matrix U'
                write ( my_io_unit , 200 ) moniker, rows, cols
            else
                write ( my_io_unit , 210 ) rows, cols
            end if

            write ( fmt_str, 100 ) cols, myFormat, spaces

            do row = 1, rows ! write each row of the matrix
                write ( my_io_unit , fmt = fmt_str )  ( A ( row, col ), col = 1, cols )
            end do

        return

      100   format ( "( ", g0, "( ", g0, ", ", g0,"X ) )" )

      200   format ( /, g0,' has ', g0, ' rows and ', g0, ' columns.' )
      210   format ( /, 'Target matrix has ', g0, ' rows and ', g0, ' columns.' )

    end subroutine print_matrix

    !   +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    subroutine open_file_print_matrix ( A, myFormat, spaces, moniker, dims, myFile )

        real ( rp ),           intent ( in )            :: A ( : , : )

        integer ( ip ),        intent ( in ), optional  :: dims ( 2 )
        integer ( ip ),        intent ( in )            :: spaces

        character ( len = * ), intent ( in ), optional  :: moniker
        character ( len = * ), intent ( in )            :: myFormat, myFile

        integer ( ip )                                  :: my_io_unit = 0

            my_io_unit = safeopen_writereplace ( myFile )

            call print_matrix ( A = A, myFormat = myFormat, spaces = spaces, moniker = moniker, dims = dims, &
                                my_io_unit = my_io_unit )

            close ( unit = my_io_unit )

    end subroutine open_file_print_matrix

end module mMatrixWriter
