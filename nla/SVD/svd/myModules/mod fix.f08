module mFix

    use iso_fortran_env
    use mPrecisionDefinitions, only : rp, zero
    implicit none

    integer, parameter               :: MMAX = 10, NB = 64, NMAX = 8
    integer, parameter               :: LDA = MMAX, LDVT = NMAX, LWORK = MMAX + 4 * NMAX + NB * ( MMAX + NMAX )
    integer, parameter               :: io_write = OUTPUT_UNIT

    ! rank 2
    real ( rp )                      :: A ( LDA, NMAX ) = zero

    integer                          :: row = 0, col = 0
    integer                          :: M = 0, N = 0

end module mFix
