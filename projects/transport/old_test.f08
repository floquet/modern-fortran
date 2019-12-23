program test
    implicit none
    integer, parameter :: n = 3
    integer :: k = 0
    real :: origin ( 2 ) = [ 1.0, 2.0 ]
    real :: lista ( n, 2 ), listb ( 2 )

        listb ( : ) = [ -1.0, 1.0 ]

        !lista ( 1, : ) = listb ( : ) + origin
        lista ( 1, : ) =  listb ( : )
        lista ( 2, : ) = -listb ( : )
        lista ( 3, : ) =  2 * listb ( : )

        do k = 1, n
            listb ( : ) = origin + lista ( k, : )
            write ( * , '( I3, ", listb = ", g0, ", ", g0, "." )' ) k, lista ( k, : )
        end do

        do k = 1, n
            lista ( k, : ) = listb ( : ) + origin
            write ( * , '( I3, ", lista = ", g0, ", ", g0, "." )' ) k, lista ( k, : )
        end do

end program test
