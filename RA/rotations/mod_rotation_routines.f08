module mRotationRoutines

    use mSetPrecision, only : rp
    implicit none

contains

    ! rotate the point p counter clockwise about the origin by theta
    pure function RotatePointCCWTheta ( p, theta ) result ( pprime )
        ! input
        real ( rp ),                      intent ( in ) :: theta  ! 0 <= theta < 2 pi
        real ( rp ), dimension ( 1 : 2 ), intent ( in ) :: p      ! p \in R^2
        ! output
        real ( rp ), dimension ( 1 : 2 )                :: pprime ( 1 : 2 )  ! p' \in R^2
        ! local variables
        real ( rp ), dimension ( 1 : 2, 1 : 2 ) :: RotationMatrix  ! R(theta) \in R^(2x2)

            RotationMatrix ( : , : ) = reshape ( [ [ cos ( theta ), sin ( theta ) ], &
                                                 [ - sin ( theta ), cos ( theta ) ] ], [ 2, 2 ] ) ! column major
            pprime = matmul ( RotationMatrix, p )

    end function RotatePointCCWTheta

    ! rotates the point p counter clockwise about the point q by theta
    pure function RotatePointCCWThetaAboutQ ( p, theta, q ) result ( pprime )
        ! input
        real ( rp ),                      intent ( in ) :: theta  ! 0 <= theta < 2 pi
        real ( rp ), dimension ( 1 : 2 ), intent ( in ) :: p  ! p \in R^2
        real ( rp ), dimension ( 1 : 2 ), intent ( in ) :: q  ! q \in R^2
        ! output
        real ( rp ), dimension ( 1 : 2 )                :: pprime ( 1 : 2 )  ! p' \in R^2

            pprime = RotatePointCCWTheta ( p - q, theta ) + q

    end function RotatePointCCWThetaAboutQ

end module mRotationRoutines
