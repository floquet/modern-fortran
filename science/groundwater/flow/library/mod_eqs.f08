module mEqs  ! shared variables
            ! module initializations in declarations happen only once ( first use )

    use mPrecisionDefinitions,  only : ip, rp, zero
    use mParameters,            only : nmax

    implicit NONE

    ! real variables
    real ( rp ) :: aa ( 1 : 3, 1 : nmax ) = zero  ! rank one
    real ( rp ) :: b  ( 1 : nmax )        = zero  ! rank two

end module mEqs
