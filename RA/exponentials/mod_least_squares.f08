module mLeastSquares

    use mSetPrecision,  only : rp
    use mData,          only : m, x, w
    implicit none

    integer, private :: k = 0

contains

    pure function astar ( b ) result ( a )

        real ( rp ), intent ( in )       :: b
        real ( rp )                      :: a
        real ( rp ), dimension ( 1 : m ) :: exponential

        exponential = exp ( b * x )
        a = dot_product ( w, exponential ) / dot_product ( exponential, exponential )

    end function astar

    pure function merit ( b ) result ( sse )

        real ( rp ), intent ( in )       :: b
        real ( rp ), dimension ( 1 : m ) :: residual
        real ( rp )                      :: sse

        residual = w - astar ( b ) * exp ( b * x )
        sse = dot_product ( residual, residual )

    end function merit

    subroutine print_data ( )

            write ( *, '( "Raw data from Bevington, table 9.1, p. 184", / )' )
            do k = 1, m
                write ( *, 100 ) k, x ( k ), w ( k )
            end do

        100 format ( g0, '. ', g0, ', ', g0 )

    end subroutine print_data

end module mLeastSquares
