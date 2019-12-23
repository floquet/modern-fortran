module mMathematicaExample

    use mSetPrecision,  only : rp
    use mSVDparameters, only : m_row, n_col
    implicit none

    ! rank 0
    reap ( rp ), parameter :: oost = 1.0_rp / sqrt ( 2.0_rp )
    ! rank 2
    ! rectangular
    real ( rp ), dimension ( m_row, n_col ) :: myA = 0.0_rp
    real ( rp ), dimension ( n_col, m_row ) :: Ap  = 0.0_rp, Sp = 0.0_rp, X = 0.0_rp !, App = 0.0_rp
    ! square
    real ( rp ), dimension ( m_row, m_row ) :: myU = 0.0_rp, id_row = 0.0_rp
    real ( rp ), dimension ( n_col, n_col ) :: myV = 0.0_rp, id_col = 0.0_rp

    ! rank 1
    real ( rp ), parameter :: s_mm ( 1 : 2 ) = [ sqrt ( 2.0_rp ),  1.0_rp ]

end module mMathematicaExample
