module mData

    use mSetPrecision, only : rp
    implicit none

    integer, parameter :: m = 10 ! number of measurements
    integer :: k = 0

    real ( rp ), dimension ( 1 : m ), parameter :: x = [ ( real ( 15 * k, rp ), k = 0, m - 1 ) ]
    real ( rp ), dimension ( 1 : m ), parameter :: w = [ 106.0_rp, 80.0_rp, 98.0_rp, 75.0_rp, 74.0_rp, 73.0_rp, &
                                                          49.0_rp, 38.0_rp, 37.0_rp, 22.0_rp ]

end module mData
