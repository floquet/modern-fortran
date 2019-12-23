module mLSQ

    use mPrecisionDefinitions, only : rp
    use mConstants,            only : zero

    implicit none

    type :: lsq_fit
        real ( rp ) ::     gap = zero,     intercept = zero,     slope = zero
        real ( rp ) :: err_gap = zero, err_intercept = zero, err_slope = zero
    end type lsq_fit

end module mLSQ
