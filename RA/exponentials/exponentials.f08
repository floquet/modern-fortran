program exponentials

    use mSetPrecision, only : rp
    use mLeastSquares, only : astar, merit, print_data
    implicit none

        call print_data ( )
        ! compare with Mathematica
        write ( *, 100 ) astar ( -1.0_rp )
        write ( *, 110 ) astar (  0.0_rp )
        write ( *, 120 ) merit ( -0.008364517181889849_rp )

    stop 'successful execution for program exponentials'

    100 format ( 'astar ( -1 ) = ', g0, ' (expected 106.0000244721849)' )
    110 format ( 'astar (  0 ) = ', g0, ' (expected 65.2)' )
    120 format ( 'merit (  -0.008364517181889849 ) = ', g0, ' (expected 966.2244819057141)' )

end program exponentials
