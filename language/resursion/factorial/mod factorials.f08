module mFactorial

    use iso_fortran_env
    implicit none

contains

    recursive function fact ( n ) result ( nbang )

        integer ( int64 )                :: nbang
        integer ( int64 ), intent ( in ) :: n

        if ( n > 0 ) then
            nbang = fact ( n - 1 ) * n
        else
            nbang = 1
        end if

    end function fact

end module mFactorial