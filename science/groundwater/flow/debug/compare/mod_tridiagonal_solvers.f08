module mTriDiagonalSolvers

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero, one
    use mParameters,            only : nmax

    implicit none

    integer ( ip ) :: i = 0, k = 0
    integer ( ip ) :: alloc_status = 0

contains

    subroutine tdma ( a, b, c, d, x )

        real ( rp ), intent ( inout )  :: a ( : )
        real ( rp ), intent ( inout )  :: b ( : )
        real ( rp ), intent ( inout )  :: c ( : )
        real ( rp ), intent ( inout )  :: d ( : )
        real ( rp ), intent ( out )    :: x ( : )

        real ( rp )    :: m = zero
        integer ( ip ) :: n = 0

            n = size ( d )

            ! http://www.cfd-online.com/Wiki/Tridiagonal_matrix_algorithm_-_TDMA_(Thomas_algorithm)
            a ( 1 ) = zero  ! space holder: dimension is n - 1
            c ( n ) = zero  ! space holder: dimension is n - 1
            d ( 1 ) = one  ! omitted in source document

            ! forward elimination
            do k = 2, n
                m       = a ( k ) / b ( k - 1 )
                b ( k ) = b ( k ) - m * c ( k - 1 )
                d ( k ) = d ( k ) - m * d ( k - 1 )
            end do

            ! back substitution
            x ( n ) = d ( n ) / b ( n )
            do k = n - 1, 1, -1
                x ( k ) = ( d ( k ) - c ( k ) * x ( k + 1 ) ) / b ( k )
            end do

    end subroutine tdma

    subroutine Dan ( a, b, c, d, x )

        real ( rp ), intent ( in )  :: a ( : )
        real ( rp ), intent ( in )  :: b ( : )
        real ( rp ), intent ( in )  :: c ( : )
        real ( rp ), intent ( in )  :: d ( : )
        real ( rp ), intent ( out ) :: x ( : )

        real ( rp ), allocatable    :: cprime ( : ), dprime ( : )

        real ( rp )                 :: denom = zero

        integer ( ip ) :: n = 0, alloc_status = 0

            n = size ( d )

            ! https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
            allocate ( cprime ( 1 : n     ), stat = alloc_status )
            allocate ( dprime ( 1 : n     ), stat = alloc_status )

            !bprime = a
            cprime ( : ) = c ( : )
            dprime ( : ) = d ( : )

            cprime ( 1 ) = c ( 1 ) / b ( 1 )
            dprime ( 1 ) = d ( 1 ) / b ( 1 )

            ! forward sweep
            do k = 2, n
                denom = b ( k ) - a ( k ) * cprime ( k - 1 )
                cprime ( k ) = c ( k )                                  / denom
                dprime ( k ) = ( d ( k ) - a ( k ) * dprime ( k - 1 ) ) / denom
            end do

            ! back substitution
            x ( n ) = dprime ( n )

            do k = n - 1, 1, -1
                x ( k ) = dprime ( k ) - cprime ( k ) * x ( k + 1 )
            end do

    end subroutine Dan

    subroutine notFred ( aa, b ) ! http://www.cfd-online.com/Wiki/Tridiagonal_matrix_algorithm_-_TDMA_(Thomas_algorithm)

        real ( rp ), intent ( inout ) :: aa ( : , : )  ! diagonal elements
        real ( rp ), intent ( inout ) :: b  ( : )  ! data vector

        ! real ( rp ), allocatable :: x ( : )
        real ( rp ) :: r = zero

        integer ( ip ) :: n = 0

            n = size ( b )
            ! allocate ( x ( n ), stat = alloc_status )

            aa ( 1, 1 ) = zero
            aa ( 3, n ) = zero

            ! print *, 'before a = ', aa ( 1, : )
            ! print *, 'before b = ', aa ( 2, : )
            ! print *, 'before d = ', b ( : )

            b ( 1 ) = one
            do k = 2, n
                r = aa ( 1, k ) / aa ( 2, k - 1 )
                aa ( 2, k ) = aa ( 2, k ) - r * aa ( 3, k - 1 )
                b  ( k )    = b ( k )     - r * b  ( k - 1 )
            end do
            ! print *, 'r = ', r
            ! write ( *,  '( "b = ", 11 ( F10.5, 2X ) )' ) aa ( 2, : )
            ! write ( *,  '( "d = ", 11 ( F10.5, 2X ) )' ) b ( : )

            ! x ( n ) = b ( n ) / aa ( 2, n )
            ! do k = n - 1, 1, -1
            !     x ( k ) = ( b ( k ) - aa ( 3, k ) * x ( k + 1 ) ) / aa ( 2, k )
            ! end do
            !
            ! ! write ( *,  '( "x = ", 11 ( F10.5, 2X ) )' ) x
            ! b ( : ) = x ( : )

            b ( n ) = b ( n ) / aa ( 2, n )
            do k = n - 1, 1, -1
                b ( k ) = ( b ( k ) - aa ( 3, k ) * b ( k + 1 ) ) / aa ( 2, k )
            end do

    end subroutine notFred

    subroutine Fred ( aa, b )

        real ( rp ), intent ( inout ) :: aa ( : , : )
        real ( rp ), intent ( inout ) :: b  ( : )

        real ( rp ) :: r
        integer ( ip ) :: nmax = 0

            nmax = size ( b )

            b(1) = 1.0d0

            do i = 2, nmax
                r = aa(1, i) / aa(2, i - 1)
                aa(2, i) = aa(2, i) - r * aa(3, i - 1)
                b(i)     = b(i)     - r * b(i - 1)
            end do

            write ( *,  '( "aa ( 2, : ) = ", 11 ( F10.5, 2X ) )' ) aa ( 2, : )
            write ( *,  '( "Fred b      = ", 11 ( F10.5, 2X ) )' ) b

            b(nmax) = b(nmax) / aa(2, nmax)
            do i = nmax - 1, 1, -1
                b(i) = (b(i) - aa(3, i) * b(i + 1)) / aa(2, i)
            end do

  end subroutine Fred

end module mTriDiagonalSolvers
