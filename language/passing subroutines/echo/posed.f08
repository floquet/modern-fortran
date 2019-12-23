module myModule

    implicit none

    type     :: intermediates
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


program posed

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

end program posed

! dan-topas-pro-2:echo rditldmt$ date
! Thu Oct  8 14:07:26 CDT 2015
! dan-topas-pro-2:echo rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/passing subroutines/echo
! dan-topas-pro-2:echo rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 posed.f08
! posed.f08:40:37:
!
!          call selector ( ints % square, x , y )
!                                      1
! Error: Expected argument list at (1)
! posed.f08:41:24:
!
!          call selector ( ints % double ( x, y ), x , y )
!                         1
! Error: ‘double’ at (1) should be a FUNCTION
