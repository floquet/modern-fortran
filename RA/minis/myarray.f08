program myarray

    real, dimension ( 1 : 3, 1 : 2 ) :: array

    integer :: k = 0

        array ( 1, : ) = [ 0.0, 1.0 ]
        array ( 2, : ) = [ 1.0, 2.0 ]
        array ( 3, : ) = [ 2.0, 4.0 ]

        do k = 1, 3
            write ( *, 100 ) k, array ( k, : )
        end do

    100 format ( g0, ': (', g0, ', ', g0, ')' )

end program myarray
