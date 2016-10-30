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


program casey

    use myModule
    implicit none

    real :: x = 10.0, y

    type ( intermediates ) :: ints

        call selector ( ints % square, x , y )
        call selector ( ints % double ( x, y ), x , y )

contains

    subroutine selector ( sub, x, y )

        interface mySub
            subroutine sub ( x, y )
                real, intent ( in )  :: x
                real, intent ( out ) :: y
            end subroutine sub
        end interface mySub

        real, intent ( in )  :: x
        real, intent ( out ) :: y

            call sub ( x, y )
            print *, 'x = ', x, ', y = ', y

    end subroutine selector

end program casey