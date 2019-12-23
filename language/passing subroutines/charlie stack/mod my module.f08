module myModule

    implicit none

    type :: intermediates
        real :: z
    contains
        private
        procedure, nopass, public :: square => square_sub
        procedure, nopass, public :: double => double_sub
    end type intermediates

    private :: square_sub
    private :: double_sub

contains

    subroutine square_sub ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
        y = x ** 2
    end subroutine square_sub

    subroutine double_sub ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
        y = x * 2
    end subroutine double_sub

end module myModule
