module mSVDparameters

    use mSetPrecision, only : rp
    use mFileHandling, only : stdout
    implicit none

    integer, parameter  :: MMAX = 10, NMAX = 8, NB = 64
    integer, parameter  :: LDA = MMAX, LDVT = NMAX, LDU = MMAX, LWORK = MMAX + 4 * NMAX + NB * ( MMAX + NMAX )
    integer, parameter  :: io_write = stdout

    integer, parameter  :: m_row = 3, n_col = 2 ! must agree with matrix in data file

    ! rank 2
    ! rectangular
    real ( rp ) :: A ( LDA, NMAX ) = 0.0_rp
    real ( rp ), dimension ( m_row, n_col ) :: myA = 0.0_rp
    real ( rp ), dimension ( n_col, m_row ) :: Ap  = 0.0_rp, Sp = 0.0_rp, X = 0.0_rp!, App = 0.0_rp
    ! square
    real ( rp ), dimension ( m_row, m_row ) :: myU = 0.0_rp, id_row = 0.0_rp
    real ( rp ), dimension ( n_col, n_col ) :: myV = 0.0_rp, id_col = 0.0_rp

    integer :: row = 0, col = 0
    integer :: M = 0, N = 0
    integer :: rank = 0

end module mSVDparameters
