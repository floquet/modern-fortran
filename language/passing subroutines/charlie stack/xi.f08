module myClass

    implicit none

    type     :: test
        real :: x, y
        contains
            private
            procedure, public :: action => action_sub
            procedure, public :: square => square_sub
            procedure, public :: double => double_sub
            procedure, public :: check  => check_sub
    end type test

contains

    subroutine square_sub ( me )
        class ( test ), target :: me
            me % y = me % x ** 2
    end subroutine square_sub

    subroutine double_sub ( me )
        class ( test ), target :: me
            me % y = me % x * 2
    end subroutine double_sub

    subroutine check_sub ( me )
        class ( test ), target :: me
            me % x = 5.0
            call double_sub ( me )
            print *, 'x = ', me % x, ', y = ', me % y
    end subroutine check_sub

    subroutine action_sub ( me, sub )
        class ( test ), target :: me

        interface mySub
            subroutine sub ( me )
                import test
                class ( test ), target :: me
            end subroutine sub
        end interface mySub

            me % x = 2.0
            call sub ( me )
            print *, 'x = ', me % x, ', y = ', me % y

    end subroutine action_sub

end module myClass

program xi

    use myClass
    implicit none

    type ( test ) :: myTest
    real :: x = 10.0

        print *, 'squaring x = ', x
        myTest % x = x
        call myTest % square ( )
        print *, 'myTest % x = ', myTest % x, '; myTest % y = ', myTest % y

        !call action ( double )
        !call selector ( square )

!         print *, 'doubling x = ', x
!         call selector ( double, x , y )
!
! !        call action ( square, x, y )
!         call selector ( check, x, y )

contains

    subroutine selector ( sub )

        type ( test ) :: thisTest

        interface mySub
            subroutine sub ( thisTest )
            end subroutine sub
        end interface mySub

            call thisTest % sub ( thisTest )
            print *, 'x = ', thisTest % x, ', y = ', thisTest % y

    end subroutine selector

end program xi
