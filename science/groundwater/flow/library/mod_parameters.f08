module mParameters  ! shared variables
                    ! module initializations in declarations happen only once ( first use )

    use mPrecisionDefinitions, only : ip, rp, zero

    implicit NONE

    integer ( ip ), parameter :: nmax = 2001

    ! real variables ( alphabetic )
    real ( rp )               :: a     = zero
    real ( rp )               :: alpha = zero
    real ( rp )               :: dz    = zero
    real ( rp )               :: fl    = zero
    real ( rp )               :: fks   = zero
    real ( rp )               :: hr    = zero
    real ( rp )               :: qq    = zero
    real ( rp )               :: thr   = zero
    real ( rp )               :: ths   = zero

end module mParameters
