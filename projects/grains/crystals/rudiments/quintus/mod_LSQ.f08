module mLSQ

    use mPrecisionDefinitions, only : rp, zero
    implicit none

    type :: lsq_fit
        real ( rp ) ::     intercept = zero,     gap = zero,     slope = zero
        real ( rp ) :: err_intercept = zero, err_gap = zero, err_slope = zero
    end type lsq_fit

end module mLSQ
