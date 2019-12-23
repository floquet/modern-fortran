! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 03 08

!    V  you are here  V

! data vectors ( 1, x, y ) -> intermediate sums -> matrices ( A, AT, ATAinv ) -> solution ( slope, intercept )

module mMeasurements

    use mPrecisionDefinitions,  only : ip, rp
    use mParameters,            only : one

    use mAllocations,           only : allocator_rank_1_sub

    implicit none

    type                         :: measurements
        integer ( ip )           :: m = 0 ! number of measurements; controls allocation routines
        real ( rp ), allocatable :: x ( : ), y ( : ), ones ( : )
        character ( len = 64 )   :: descriptor_64 = ''
    contains
        private
        procedure, public :: allocate_group      => allocate_group_sub
    end type measurements

    private :: allocate_group_sub

    contains

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +                   allocate_group

        subroutine allocate_group_sub ( me )

            class ( measurements ), target :: me

                call allocator_rank_1_sub ( me % x,    me % m )
                call allocator_rank_1_sub ( me % y,    me % m )
                call allocator_rank_1_sub ( me % ones, me % m )

                me % ones = one

        end subroutine allocate_group_sub

    end module mMeasurements
