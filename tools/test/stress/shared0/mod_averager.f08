module mAverager

    use, intrinsic :: iso_fortran_env, only : REAL64

    implicit none ! protects all methods
    integer, parameter :: rp = REAL64

contains

    function averager ( rank_one_array ) result ( mean )
        real ( rp ), intent ( in ) :: rank_one_array ( : )
        real ( rp )                :: mean ( 1 : 5 ) ! mean, variance, maximum, minimum, variation

        real ( rp )                :: diff
        integer                    :: length

            length     = size ( rank_one_array ) ! census
            mean ( 1 ) = sum  ( rank_one_array ) / real ( length, rp ) ! mean
            mean ( 2 ) = sum  ( rank_one_array * rank_one_array ) / real ( length, rp ) ! mean of squares
            diff = mean ( 2 ) - mean ( 1 )**2
            if ( diff < 0.0_rp ) then  ! perhaps all elements have the same value
                write ( * , '( "mean of squares - square of mean  = ", g0, " < 0" )' ) diff
                stop 'computational error in function averager'
            end if
            mean ( 2 ) = sqrt ( diff )
            mean ( 3 ) = maxval ( rank_one_array )
            mean ( 4 ) = minval ( rank_one_array )
            mean ( 5 ) = mean ( 3 ) - mean ( 4 )
    end function

end module mAverager
