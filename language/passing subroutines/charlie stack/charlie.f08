! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! http://stackoverflow.com/questions/32809769/how-to-pass-subroutine-names-as-arguments-in-fortran
!include 'mySubs.f08'
include 'mod my module.f08'

program vladimir

    use myModule
    implicit none

    real :: x = 10.0
    real :: y = 1.0

    type ( intermediates ) :: ints

        call selector ( ints % square, x , y )
        call selector ( ints % double, x , y )

contains

    subroutine selector ( sub, x, y )

        implicit none

        type ( intermediates ) :: ints

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

end program vladimir

! dan-topas-pro-2:charlie passing rditldmt$ date
! Mon Oct  5 10:53:37 CDT 2015
! dan-topas-pro-2:charlie passing rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/passing subroutines/charlie passing
! dan-topas-pro-2:charlie passing rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 charlie.f08 -o charlie
! dan-topas-pro-2:charlie passing rditldmt$ ./charlie
!  squaring x =    10.0000000
!  x =    10.0000000     , y =    100.000000
!  doubling x =    10.0000000
!  x =    10.0000000     , y =    20.0000000