module mGlobalConstants

    use mSetPrecision, only : rp

    implicit none

    real ( rp ), parameter :: acos ( -1.0_rp ), &
                              rad_to_deg = 180_rp / pi,
                              deg_to_rad = pi / 180_rp
    real ( rp ), parameter :: zero = 0.0_rp, one  = 1.0_rp, half = 0.5_rp

    ! complex constants
    complex ( rp ), parameter :: unit_modulus = ( zero, one )  ! a.k.a i

    ! real unit vectors
    real ( rp ), parameter :: unit_x_2 ( 1 : 2 ) = [ one, zero ], &
                              unit_y_2 ( 1 : 2 ) = [ zero, one ]
    real ( rp ), parameter :: unit_x_3 ( 1 : 3 ) = [ one, zero, zero ], &
                              unit_y_3 ( 1 : 3 ) = [ zero, one, zero ], &
                              unit_z_3 ( 1 : 3 ) = [ zero, zero, one ]

    ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

    ! identity matrix
    real ( rp ), parameter :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ unit_x_2, unit_y_2 ], [ 2, 2 ] )
    real ( rp ), parameter :: id_matrix_3 ( 1 : 3, 1 : 3 ) = reshape ( [ unit_x_3, unit_y_3, unit_z_3 ], [ 3, 3 ] )


    ! physical constants
    real ( rp ), parameter :: grav = 9.80665_rp ! http://physics.nist.gov/cgi-bin/cuu/Value?gn|search_for=gravity
    real ( rp ), parameter :: radius_earth_equitorial      = 6378.137_rp, &
                              radius_earth_polar           = 6356.752_rp, &
                              radius_earth_volumetric mean = 6371.008_rp
    real ( rp ), parameter :: R_earth = radius_earth_volumetric mean

end module mGlobalConstants
