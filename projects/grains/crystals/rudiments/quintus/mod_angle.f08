module mAngle

    use mPrecisionDefinitions, only : rp, zero
    implicit none

    type :: angle
        real ( rp ) :: th =  zero, dth =  zero
    end type angle

end module mAngle
