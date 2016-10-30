module mAverager

    use, intrinsic :: iso_fortran_env, only : REAL64

    implicit none ! protects all methods
    integer, parameter :: rp = REAL64

contains

    function averager ( rank_one_array ) result ( mean )
        real ( rp ), intent ( in ) :: rank_one_array ( : )
        real ( rp )                :: mean ( 1 : 2 ) ! mean, variance

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
    end function

end module mAverager

! rditldmt@ITL-DTOPA-MP:averager $ make debug
! PROGRAM  = driver_averager
! PRG_OBJ  = driver_averager.o
! SRCS     = driver_averager.f08 mod_averager.f08
! OBJS     = driver_averager.o mod_averager.o
! MODS     = mod_averager.f08
! MOD_OBJS = mod_averager.o
! rditldmt@ITL-DTOPA-MP:averager $ make
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_averager.o mod_averager.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o driver_averager.o driver_averager.f08
! gfortran -g -o driver_averager driver_averager.o mod_averager.o
! rditldmt@ITL-DTOPA-MP:averager $ ./driver_averager
!
! expected answer = ( 1, 0 )
! mean value = 1.0000000000000000
! variance = 0.0000000000000000
!
! expected answer = ( 500.5, 288.96366553599779528... )
! mean value = 500.00000000000000
! variance = 288.96366553599779
! rditldmt@ITL-DTOPA-MP:averager $ date
! Wed May 11 14:37:43 CDT 2016
! rditldmt@ITL-DTOPA-MP:averager $ pwd
! /Users/rditldmt/hpc/fortran/library/modules/averager
