module mAngle

    use mPrecisionDefinitions,  only : rp
    use mConstants,             only : zero

    implicit none

    type :: angle
        real ( rp ) :: th =  zero, dth =  zero  ! angle and uncertainty
    end type angle

end module mAngle
