! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! http://stackoverflow.com/questions/32809769/how-to-pass-subroutine-names-as-arguments-in-fortran
include 'mySubs.f08'

program vladimir

    use mySubs
    implicit none

    real :: x = 10.0
    real :: y = 0.0

        print *, 'squaring x = ', x
        call action ( square, x , y )

        print *, 'doubling x = ', x
        call action ( double, x , y )

contains

    subroutine action ( sub, x, y )

        implicit none

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

    end subroutine action

end program vladimir

! Muntz-Szasz:passing subroutines dantopa$ date
! Sun Sep 27 19:03:45 CDT 2015
! Muntz-Szasz:passing subroutines dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/passing subroutines
! Muntz-Szasz:passing subroutines dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 alpha.f08
! Muntz-Szasz:passing subroutines dantopa$ ./a.out
!  x =    10.0000000     , y =    100.000000
!  x =    10.0000000     , y =    20.0000000
!
