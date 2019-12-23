! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

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

module newSubs

    implicit none

    type :: myClass
        real :: z
    contains
        private
        procedure, nopass, public :: myFunc        => myFunc_fcn
        procedure, nopass, public :: newSquare     => newSquare_sub
        procedure, nopass, public :: newDouble     => newDouble_sub
        procedure, nopass, public :: classSelector => classSelector_sub
    end type myClass

    private :: myFunc_fcn
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

    function myFunc_fcn ( x ) result ( y )
        real, intent ( in )  :: x
        real                 :: y
            y = x / 2
    end function myFunc_fcn

    subroutine classSelector_sub ( sub, x, y )

        interface mySub
            subroutine sub ( x, y )
                import myClass
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

    use mySubs
    use newSubs
    implicit none

    real :: x = 10.0
    real :: y = 1.0

    type ( myClass ) :: dan

!         print *, 'squaring x = ', x
!         call dan % newSquare ( x , y )
!
!         print *, 'doubling x = ', x
!         call selector ( double, x , y )

        print *, 'Hail Mary: double'
        call dan % classSelector ( newDouble, x , y )

        print *, 'Hail Mary: square'
        call dan % classSelector ( square, x , y )

!         call dan % newDouble ( x, y )
!         print *, 'y = ', y

!         x = 3.0
!         print *, 'halving x = ', x
!         y = selection ( dan % myFunc ( x ) )

contains
!     real function selection ( myFunc, x, y )
!
!         type ( myClass ) :: dan
!
!         interface newFcn
!             real function myFunc ( x ) result ( y )
!                 real, intent ( in )  :: x
!                 !real                 :: y
!             end function myFunc
!         end interface newFcn
!
!         real, intent ( in ) :: x
!         real                :: y
!
!             y = dan % myFunc ( x )
!             print *, 'x = ', x, ', y = ', y
!
!     end function selection

!     subroutine newselector ( sub, x, y )
!
!         type ( myClass ) :: dan
!
! !         interface newSub
! !             subroutine sub ( x, y )
! !                 real, intent ( in )  :: x
! !                 real, intent ( out ) :: y
! !             end subroutine sub
! !         end interface newSub
!         interface newFcn
!             function myFunc ( x ) result y
!                 real, intent ( in )  :: x
!                 real                 :: y
!             end function myFunc
!         end interface newFcn
!
!         real, intent ( in )  :: x
!         real, intent ( out ) :: y
!
!             call sub ( x, y )
!             print *, 'x = ', x, ', y = ', y
!
!     end subroutine newselector

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
