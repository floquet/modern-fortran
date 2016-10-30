! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! https://stackoverflow.com/questions/32833685/how-to-pass-subroutine-names-as-arguments-in-fortran-classes
module mySubs

    implicit none

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

end module mySubs

program move_selector

    use mySubs
    implicit none

    real :: x = 10.0
    real :: y = 1.0

        call square ( x, y )

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

    end subroutine selector

end program move_selector

! dan-topas-pro-2:echo rditldmt$ date
! Thu Oct  8 13:55:32 CDT 2015
! dan-topas-pro-2:echo rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/passing subroutines/echo
! dan-topas-pro-2:echo rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 a\ move_selector.f08 -o move_selector
! dan-topas-pro-2:echo rditldmt$ ./move_selector
!  x =    10.0000000     ; x ** 2 =    100.000000
!  x =    10.0000000     ; 2 x =    20.0000000