module mDataSVD

    use mPrecisionDefinitions, only : rp, zero
    implicit none

    integer, parameter               :: MMAX = 10, NB = 64, NMAX = 8
    integer, parameter               :: LDA = MMAX, LDVT = NMAX, LWORK = MMAX + 4 * NMAX + NB * ( MMAX + NMAX )

    ! rank 2
    real ( rp )                      :: A ( LDA, NMAX ) = zero
    integer                          :: M = 0, N = 0

end module mDataSVD
