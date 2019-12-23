program driver_average

    use, intrinsic :: iso_fortran_env,  only : REAL64
    use mAverager,                      only : averager

    implicit none
    integer, parameter :: rp = REAL64

    real ( rp ) :: ones ( 1 : 10 )  = 1.0_rp
    real ( rp ) :: sequence ( 0 : 1000 )  = 1.0_rp
    real ( rp ) :: mean  ( 1 : 2 )  = 0.0_rp

    integer :: k = 0

        mean = averager ( ones )
        write ( * , '( /, "expected answer = ( 1, 0 )", g0 )' )
        write ( * , '(    "mean value = ", g0, /, "variance = ", g0 )' ) mean ( 1 ), mean ( 2 )

        sequence = [ ( real ( k, rp ), k = 0, 1000 ) ]
        mean = averager ( sequence )
        write ( * , '( /, "expected answer = ( 500.5, 288.96366553599779528... )", g0 )' )
        write ( * , '(    "mean value = ", g0, /, "variance = ", g0 )' ) mean ( 1 ), mean ( 2 )

end program driver_average

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
