! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'mySubs.f08'

program vladimir

    implicit none

    real :: x = 10.0
    real :: y = 0.0

        call action ( square, x , y )
        call action ( double, x , y )

contains

    subroutine action ( sub, x, y )

        use mySubs
        implicit none

        interface twoSubs
            subroutine square ( x, y )
                real, intent ( in )  :: x
                real, intent ( out ) :: y
            end subroutine square
            subroutine double ( x, y )
                real, intent ( in )  :: x
                real, intent ( out ) :: y
            end subroutine double
        end interface twoSubs

        real, intent ( in )  :: x
        real, intent ( out ) :: y

            call sub ( x, y )
            print *, 'x = ', x, ', y = ', y

    end subroutine action

end program vladimir
