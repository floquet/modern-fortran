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

    private :: action_sub
    private :: square_sub
    private :: double_sub
    private :: check_sub

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

    subroutine action_sub ( sub )
        class ( test ), target :: me

        interface mySub
            subroutine sub ( me )
                class ( test ), target :: me
            end subroutine sub
        end interface mySub

            call sub ( me )
            print *, 'x = ', me % x, ', y = ', me % y

    end subroutine action_sub

end module myClass
