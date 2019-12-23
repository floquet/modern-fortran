module mIntermediatesDefinitions

    use mMeasurements

    implicit none

    type :: intermediates
        real ( rp ) :: em = zero, sx = zero, sx2 = zero, sy = zero, sxy = zero
    contains
        private
        procedure, public :: compute_intermediates_dot => compute_intermediates_dot_sub
        procedure, public :: compute_intermediates_sum => compute_intermediates_sum_sub
        procedure, public :: local_selector            => local_selector_sub
    end type intermediates

    private :: local_selector_sub
    private :: compute_intermediates_dot_sub
    private :: compute_intermediates_sum_sub

contains

!     subroutine local_selector_sub ( me, sub, myMeasurements )
!
!         class ( intermediates ), target      :: me
!
!         interface mySub
!             subroutine sub
!                 import measurements
!                 type ( measurements ), intent ( in ) :: myMeasurements
!             end subroutine sub
!         end interface mySub
!
!         type ( measurements ), intent ( in ) :: myMeasurements
!
!         call sub ( me, myMeasurements )
!
!     end subroutine local_selector_sub

!     subroutine local_selector_sub ( me, myMeasurements )
!
!         class ( intermediates ), target      :: me
!
!         type ( measurements ), intent ( in ) :: myMeasurements
!
!         call compute_intermediates_dot_sub ( me, myMeasurements )
!
!     end subroutine local_selector_sub

    subroutine compute_intermediates_dot_sub ( me, myMeasurements )

        class ( intermediates ), target      :: me

        type ( measurements ), intent ( in ) :: myMeasurements

            me % em  = dot_product ( myMeasurements % ones, myMeasurements % ones )
            me % sx  = dot_product ( myMeasurements % ones, myMeasurements % x )
            me % sy  = dot_product ( myMeasurements % ones, myMeasurements % y )
            me % sx2 = dot_product ( myMeasurements % x,    myMeasurements % x )
            me % sxy = dot_product ( myMeasurements % x,    myMeasurements % y )

    end subroutine compute_intermediates_dot_sub

    subroutine compute_intermediates_sum_sub ( me, myMeasurements )

        class ( intermediates ), target      :: me

        type ( measurements ), intent ( in ) :: myMeasurements

            me % em  = sum ( myMeasurements % ones )
            me % sx  = sum ( myMeasurements % x )
            me % sy  = sum ( myMeasurements % y )
            me % sx2 = sum ( myMeasurements % x * myMeasurements % x )
            me % sxy = sum ( myMeasurements % x * myMeasurements % y )

    end subroutine compute_intermediates_sum_sub

end module mIntermediatesDefinitions