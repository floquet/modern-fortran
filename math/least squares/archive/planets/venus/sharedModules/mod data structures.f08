module mDataStructures

    use mPrecisionDefinitions

    implicit none

    type :: measurements
        real ( rp ), dimension ( 1 : 9 ) :: x = zero, y = zero, ones = one
        integer ( ip )                   :: m = 9
    contains
        private
        procedure, public :: load_data
    end type measurements

contains

    subroutine load_data ( me )

        class ( measurements ), target :: me

!           load data
            me % x ( 1 ) = 1.
            me % x ( 2 ) = 2.
            me % x ( 3 ) = 3.
            me % x ( 4 ) = 4.
            me % x ( 5 ) = 5.
            me % x ( 6 ) = 6.
            me % x ( 7 ) = 7.
            me % x ( 8 ) = 8.
            me % x ( 9 ) = 9.

            me % y ( 1 ) = 15.6
            me % y ( 2 ) = 17.5
            me % y ( 3 ) = 36.6
            me % y ( 4 ) = 43.8
            me % y ( 5 ) = 58.2
            me % y ( 6 ) = 61.6
            me % y ( 7 ) = 64.2
            me % y ( 8 ) = 70.4
            me % y ( 9 ) = 98.8

    end subroutine load_data

end module mDataStructures