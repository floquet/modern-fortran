module mTriDiagonalSolvers

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero, one

    implicit none

    integer ( ip ) :: i = 0, k = 0

contains

    subroutine thomas ( aa, b ) ! http://www.cfd-online.com/Wiki/Tridiagonal_matrix_algorithm_-_TDMA_(Thomas_algorithm)

        ! rank 2
        real ( rp ), intent ( inout ) :: aa ( : , : )  ! diagonal elements
        ! rank 1
        real ( rp ), intent ( inout ) :: b  ( : )  ! data vector
        ! rank 0
        real ( rp ) :: r = zero

        integer ( ip ) :: n = 0

            n = size ( b )

            ! forward sweep
            do k = 2, n
                r = aa ( 1, k ) / aa ( 2, k - 1 )
                aa ( 2, k ) = aa ( 2, k ) - r * aa ( 3, k - 1 )
                b  ( k )    = b ( k )     - r * b  ( k - 1 )
            end do

            ! backwards sweep
            b ( n ) = b ( n ) / aa ( 2, n )
            do k = n - 1, 1, -1
                b ( k ) = ( b ( k ) - aa ( 3, k ) * b ( k + 1 ) ) / aa ( 2, k )
            end do

    end subroutine thomas

    subroutine tri_diag ( aa, b )

        ! rank 2
        real ( rp ), intent ( inout ) :: aa ( : , : )
        ! rank 1
        real ( rp ), intent ( inout ) :: b  ( : )
        ! rank 0
        real ( rp ) :: r

        integer ( ip ) :: nmax = 0

            nmax = size ( b )

            b(1) = 0.0d0

            do i = 3, nmax
                r = aa(1, i) / aa(2, i - 1)
                aa(2, i) = aa(2, i) - r * aa(3, i - 1)
                b(i)     = b(i)     - r * b(i - 1)
            end do

            b(nmax) = b(nmax) / aa(2, nmax)
            do i = nmax - 1, 2, -1
                b(i) = (b(i) - aa(3, i) * b(i + 1)) / aa(2, i)
            end do

  end subroutine tri_diag

end module mTriDiagonalSolvers
