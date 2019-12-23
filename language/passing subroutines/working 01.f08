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

program working

    use mySubs
    implicit none

    real :: x = 10.0
    real :: y = 1.0

        print *, 'squaring x = ', x
        call selector ( square, x , y )

        print *, 'doubling x = ', x
        call selector ( double, x , y )

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
