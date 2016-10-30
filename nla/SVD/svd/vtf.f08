include 'mod precision definitions.f08'
include 'myModules/mod data svd.f08'

program vtf

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

        write ( * , * ) 'io_write = ', io_write

        M = 2
        N = 3
        write ( * , * ) 'm + n = ', M + N

        row = 4
        col = 5
        write ( * , * ) 'row + col = ', row + col

end program vtf
