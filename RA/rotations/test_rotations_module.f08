program test_rotations_module

    use, intrinsic :: iso_fortran_env,  only : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
    use mSetPrecision,                  only : rp, ip
    use mRotationRoutines,              only : RotatePointCCWTheta, RotatePointCCWThetaAboutQ

    implicit none

    ! parameters
    real ( rp ),    parameter :: one = 1.0_rp, two = 2.0_rp, half = 0.5_rp, zero = 0.0_rp
    real ( rp ),    parameter :: pi = acos ( -one )
    real ( rp ),    parameter :: eps = epsilon ( one )
    real ( rp ),    parameter, dimension ( 1 : 2 ) :: pzero = [ zero, zero ]
    integer ( ip ), parameter :: numPoints = 7

    ! rank 2
    real ( rp ), dimension ( 1 : numPoints, 1 : 2 ) :: vertices, newVertices
    ! rank 1
    real ( rp ), dimension ( 1 : numPoints )        :: radius_oldVertices = zero, radius_newVertices = zero
    real ( rp ), dimension ( 1 : 2 )                :: p = pzero, q = pzero!, pprime = pzero
    ! rank 0
    real    ( rp ) :: theta = zero, phi_const = zero, phi_vary = zero, difference = zero
    integer ( ip ) :: k = 0
    logical :: pass = .true.

        p = [ 2.5_rp, 1.5_rp ]
        theta = pi * 4 / 3

        ! write ( *, '( /, ''test rotation about origin:'' )' )
        ! pprime = RotatePointCCWTheta       ( p,    theta )
        ! write ( *, 100 ) pprime, p, theta
        !
        ! write ( *, '( /, ''test rotation about point q:'' )' )
        ! pprime = RotatePointCCWThetaAboutQ ( p, q, theta )
        ! write ( *, 200 ) pprime, p, q, theta

        ! validation sequence
        q ( : ) = [ -half, half ]

        ! input set of points
        phi_const = 2 * pi / real ( numPoints, rp )
        do k = 1, numPoints
            phi_vary = real ( k - 1, rp ) * phi_const
            vertices ( k, : ) = [ cos ( phi_vary ), sin ( phi_vary ) ] + [ one, two ]
            radius_oldVertices ( k ) = norm2 ( vertices ( k, : ) - q ( : ) )
        end do

        ! output set of rotated points
        do k = 1, numPoints
            phi_vary = real ( k - 1, rp ) * phi_const
            newVertices ( k, : ) = RotatePointCCWThetaAboutQ ( vertices ( k, : ), theta, q ( : ) )
            radius_newVertices ( k ) = norm2 ( newVertices ( k, : ) - q ( : ) )
        end do

        do k = 1, numPoints
            write ( *, 300 ) k, vertices ( k, : ), newVertices ( k, : ) - q ( : ), &
                             radius_oldVertices ( k ), radius_newVertices ( k )
        end do

        write ( *, '( /, ''Is the radius invariant? If so, radius change = 0'' )' )
        do k = 1, numPoints
            difference = abs ( radius_oldVertices ( k ) - radius_newVertices ( k ) )
            write ( *, 310 ) k, difference, difference / eps
            if ( difference >= 5.0_rp * eps ) pass = .false.
        end do

        write ( *, 400 ) pass

        stop 'successful completion for test_rotations_module'

    ! 100 format ( 'pprime = ( ', g0, ', ', g0, ' )', /, &
    !              'p      = ( ', g0, ', ', g0, ' )', /, &
    !              'theta  = ', g0 )
    !
    ! 200 format ( 'pprime = ( ', g0, ', ', g0, ' )', /, &
    !              'p      = ( ', g0, ', ', g0, ' )', /, &
    !              'q      = ( ', g0, ', ', g0, ' )', /, &
    !              'theta  = ', g0 )

    300 format ( g0, ': ( ', g0, ', ', g0, ' ), ( ', g0, ', ', g0, ' ), ', g0, ', ', g0 )
    310 format ( g0, '. radius change, old -> new: ', g0, '( ', g0, ' )' )

    400 format ( /, 'Did the algorithm pass validation? ', g0, / )

end program test_rotations_module

! rditldmt@ITL-DTOPA-MP:rotations $ date
! Tue Aug  9 16:32:32 CDT 2016
! rditldmt@ITL-DTOPA-MP:rotations $ pwd
! /Users/rditldmt/hpc/fortran/RA/rotations
! rditldmt@ITL-DTOPA-MP:rotations $ make debug
! PROGRAM  = test_rotations_module
! PRG_OBJ  = test_rotations_module.o
! SRCS     = mod_rotation_routines.f08 mod_set_precision.f08 test_rotations_module.f08
! OBJS     = mod_rotation_routines.o mod_set_precision.o test_rotations_module.o
! MODS     = mod_rotation_routines.f08 mod_set_precision.f08
! MOD_OBJS = mod_rotation_routines.o mod_set_precision.o
! rditldmt@ITL-DTOPA-MP:rotations $ ./test_rotations_module
! 1: ( 2.0000000000000000, 2.0000000000000000 ), ( 0.49038105676656452E-001, -2.9150635094610964 ), 2.9154759474226499, 2.9154759474226499
! 2: ( 1.6234898018587336, 2.7818314824680295 ), ( 0.91437913004305127, -2.9799118543208625 ), 3.1170440890337572, 3.1170440890337576
! 3: ( 0.77747906604368566, 2.9749279121818235 ), ( 1.5046109114627977, -2.3437932800875627 ), 2.7851787976136340, 2.7851787976136340
! 4: ( 0.99031132097580965E-001, 2.4338837391175581 ), ( 1.3752768799926520, -1.4857180476130367 ), 2.0245356538340875, 2.0245356538340880
! 5: ( 0.99031132097580854E-001, 1.5661162608824419 ), ( 0.62376819926308169, -1.0518343084954780 ), 1.2228827331106069, 1.2228827331106069
! 6: ( 0.77747906604368544, 1.0250720878181765 ), ( -0.18401376615316889, -1.3688653679057383 ), 1.3811782874001408, 1.3811782874001408
! 7: ( 1.6234898018587334, 1.2181685175319701 ), ( -0.43979272054847163, -2.1980803718528321 ), 2.2416455915626159, 2.2416455915626159
!
! Is the radius invariant? If so, radius change = 0
! 1. radius change, old -> new: 0.0000000000000000( 0.0000000000000000 )
! 2. radius change, old -> new: 0.44408920985006262E-015( 2.0000000000000000 )
! 3. radius change, old -> new: 0.0000000000000000( 0.0000000000000000 )
! 4. radius change, old -> new: 0.44408920985006262E-015( 2.0000000000000000 )
! 5. radius change, old -> new: 0.0000000000000000( 0.0000000000000000 )
! 6. radius change, old -> new: 0.0000000000000000( 0.0000000000000000 )
! 7. radius change, old -> new: 0.0000000000000000( 0.0000000000000000 )
!
! Did the algorithm pass validation? T
!
! STOP successful completion for test_rotations_module
