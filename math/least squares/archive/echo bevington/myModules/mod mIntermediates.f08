module mIntermediates

    use mPrecisionDefinitions, only : rp, zero
    implicit none

    type            :: intermediates
        real ( rp ) :: em = zero, sX = zero, sX2 = zero, sY = zero, sXY = zero, det = zero
    end type intermediates 

end module mIntermediates
