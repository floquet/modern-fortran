module mMeasurements

    use mPrecisionDefinitions, only : ip, rp, one, zero

    implicit none

    integer ( ip ), parameter :: m = 9

    type :: measurements
        real ( rp ), dimension ( 1 : m ) :: x = zero, y = zero, &
                                            ones = one, residuals = zero
    contains
        private
        procedure, public :: load_data
    end type measurements

contains

    subroutine load_data ( me )

        class ( measurements ), target :: me

!           load data
            me % x ( 1 ) = 1.0_rp
            me % x ( 2 ) = 2.0_rp
            me % x ( 3 ) = 3.0_rp
            me % x ( 4 ) = 4.0_rp
            me % x ( 5 ) = 5.0_rp
            me % x ( 6 ) = 6.0_rp
            me % x ( 7 ) = 7.0_rp
            me % x ( 8 ) = 8.0_rp
            me % x ( 9 ) = 9.0_rp

            me % y ( 1 ) = 15.6_rp
            me % y ( 2 ) = 17.5_rp
            me % y ( 3 ) = 36.6_rp
            me % y ( 4 ) = 43.8_rp
            me % y ( 5 ) = 58.2_rp
            me % y ( 6 ) = 61.6_rp
            me % y ( 7 ) = 64.2_rp
            me % y ( 8 ) = 70.4_rp
            me % y ( 9 ) = 98.8_rp

    end subroutine load_data

end module mMeasurements