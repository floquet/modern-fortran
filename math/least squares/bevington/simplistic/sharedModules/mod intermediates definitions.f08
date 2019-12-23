module mIntermediatesDefinitions

    use mDataStructures

    implicit none

    type :: intermediates
        real ( rp ) :: em = zero, sx = zero, sx2 = zero, sy = zero, sxy = zero
    contains
        private
        procedure, public :: compute_intermediates
    end type intermediates

contains

    subroutine compute_intermediates ( me, myMeasurements )

        class ( intermediates ), target      :: me

        type ( measurements ), intent ( in ) :: myMeasurements

            me % em  = dot_product ( myMeasurements % ones, myMeasurements % ones )
            me % sx  = dot_product ( myMeasurements % ones, myMeasurements % x )
            me % sy  = dot_product ( myMeasurements % ones, myMeasurements % y )
            me % sx2 = dot_product ( myMeasurements % x,    myMeasurements % x )
            me % sxy = dot_product ( myMeasurements % x,    myMeasurements % y )

    end subroutine compute_intermediates

end module mIntermediatesDefinitions