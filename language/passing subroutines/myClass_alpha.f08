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
            print *, 'squaring'
            me % y = me % x ** 2
    end subroutine square_sub

    subroutine double_sub ( me )
        class ( test ), target :: me
            print *, 'doubling'
            me % y = me % x * 2
    end subroutine double_sub

    subroutine check_sub ( me )
        class ( test ), target :: me
            me % x = 5.0
            print *, 'check sub:'
            call double_sub ( me )
            print *, 'x = ', me % x, ', y = ', me % y
    end subroutine check_sub

    subroutine action_sub ( me, sub, x, y )
        class ( test ), target :: me

        real, intent ( in )  :: x
        real, intent ( out ) :: y

        interface mySub
            subroutine sub ( me )
                import test
                class ( test ), target :: me
            end subroutine sub
        end interface mySub

!            me % x = 2.0
            call sub ( me )
            print *, 'x = ', me % x, ', y = ', me % y

    end subroutine action_sub

end module myClass
