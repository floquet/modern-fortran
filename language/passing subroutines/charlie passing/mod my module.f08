module myModule

    implicit none

    type :: intermediates
        real :: z
    contains
        private
        procedure, nopass, public :: normal_a => normal_a_sub
    end type intermediates

    private :: normal_a_sub

contains

!       =============================================================================================                       normal_a

    subroutine normal_a_sub ( x, y )

        !class ( intermediates ), target :: me
        real, intent ( in )  :: x
        real, intent ( out ) :: y

            y = sin ( x )
            print *, 'x = ', x, ', y = ', y

    end subroutine normal_a_sub

end module myModule
