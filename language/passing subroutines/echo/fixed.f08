! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! https://stackoverflow.com/questions/32833685/how-to-pass-subroutine-names-as-arguments-in-fortran-classes
module mySubs

    implicit none

    type :: myClass
    contains
        procedure, nopass, public :: square
        procedure, nopass, public :: double
        procedure, nopass, public :: local_selector
    end type myClass

contains

    subroutine square ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
            y = x ** 2
            print *, 'x = ', x, '; x ** 2 = ', y
    end subroutine square

    subroutine double ( x, y )
        real, intent ( in )  :: x
        real, intent ( out ) :: y
            y = x * 2
            print *, 'x = ', x, '; 2 x = ', y
    end subroutine double

    subroutine local_selector ( sub, x, y )

        interface mySub
            subroutine sub ( x, y )
                real, intent ( in )  :: x
                real, intent ( out ) :: y
            end subroutine sub
        end interface mySub

        real, intent ( in )  :: x
        real, intent ( out ) :: y

            call sub ( x, y )

    end subroutine local_selector

end module mySubs

program fixed

    use mySubs
    implicit none

    real :: x = 10.0, y

    type ( myClass ) :: thisClass

        call thisClass % local_selector ( square, x , y )
        call thisClass % local_selector ( double, x , y )

end program fixed

! dan-topas-pro-2:echo rditldmt$ date
! Thu Oct  8 14:19:47 CDT 2015
! dan-topas-pro-2:echo rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/passing subroutines/echo
! dan-topas-pro-2:echo rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 fixed.f08 -o fixed
! dan-topas-pro-2:echo rditldmt$ ./fixed
!  x =    10.0000000     ; x ** 2 =    100.000000
!  x =    10.0000000     ; 2 x =    20.0000000