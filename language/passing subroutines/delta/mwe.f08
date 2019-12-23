! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

module newSubs

    implicit none

    type :: myClass
        real :: z
    contains
        private
        procedure, nopass, public :: newSquare     => newSquare_sub
        procedure, nopass, public :: newDouble     => newDouble_sub
        procedure, nopass, public :: classSelector => classSelector_sub
    end type myClass

    private :: newSquare_sub
    private :: newDouble_sub
    private :: classSelector_sub

contains

    subroutine newSquare_sub ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
            y = x ** 2
    end subroutine newSquare_sub

    subroutine newDouble_sub ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
            y = x * 2
    end subroutine newDouble_sub

    subroutine classSelector_sub ( sub, x, y )

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

    end subroutine classSelector_sub

end module newSubs


program working

    use newSubs

    implicit none

    real :: x = 10.0
    real :: y = 1.0

    type ( myClass ) :: dan

        call dan % classSelector ( double, x , y )

        call dan % classSelector ( square, x , y )

end program working

! dan-topas-pro-2:passing subroutines rditldmt$ date
! Tue Oct  6 18:06:09 CDT 2015
! dan-topas-pro-2:passing subroutines rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/passing subroutines
! dan-topas-pro-2:passing subroutines rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 working\ 01.f08 -o working01
! dan-topas-pro-2:passing subroutines rditldmt$ ./working01
!  squaring x =    10.0000000
!  x =    10.0000000     , y =    100.000000
!  doubling x =    10.0000000
!  x =    10.0000000     , y =    20.0000000
