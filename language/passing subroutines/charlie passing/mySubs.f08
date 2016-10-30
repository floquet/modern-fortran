module mySubs

    implicit none

contains

    subroutine square ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
        y = x ** 2
    end subroutine square

    subroutine double ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
        y = x * 2
    end subroutine double

end module mySubs
