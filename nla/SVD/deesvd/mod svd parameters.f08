module mSVDparameters

    use iso_fortran_env
    use mPrecisionDefinitions, only : rp, zero
    implicit none

    integer, parameter               :: MMAX = 10, NMAX = 8, NB = 64
    integer, parameter               :: LDA = MMAX, LDVT = NMAX, LDU = MMAX, LWORK = MMAX + 4 * NMAX + NB * ( MMAX + NMAX )
    integer, parameter               :: io_write = OUTPUT_UNIT

    ! rank 2
    real ( rp )                      :: A ( LDA, NMAX ) = zero
    real ( rp ), dimension ( 4, 6 )  :: Ap = zero, App = zero, Sp = zero, X = zero
    real ( rp )                      :: id  ( 4, 4 ) = zero
    real ( rp )                      :: myU ( 6, 6 ) = zero, myV ( 4, 4 ) = zero, myA ( 6, 4 ) = zero

    integer                          :: row = 0, col = 0
    integer                          :: M = 0, N = 0
    integer                          :: rank = 0

end module mSVDparameters
