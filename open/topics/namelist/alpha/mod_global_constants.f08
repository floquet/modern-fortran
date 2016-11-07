module mGlobalConstants

    use mSetPrecision, only : rp

    implicit none

    real ( rp ), parameter :: pi = acos ( -1.0_rp ),    &
                              rad_to_deg = 180_rp / pi, &
                              deg_to_rad = pi / 180_rp
    real ( rp ), parameter :: zero = 0.0_rp, one  = 1.0_rp, half = 0.5_rp

    ! complex constants
    complex ( rp ), parameter :: complex_unit_modulus = ( zero, one )  ! a.k.a i

    ! real unit vectors (column vectors)
    real ( rp ), parameter :: unit_x_2 ( 1 : 2 ) = [ one, zero ], &
                              unit_y_2 ( 1 : 2 ) = [ zero, one ]
    real ( rp ), parameter :: unit_x_3 ( 1 : 3 ) = [ one, zero, zero ], &
                              unit_y_3 ( 1 : 3 ) = [ zero, one, zero ], &
                              unit_z_3 ( 1 : 3 ) = [ zero, zero, one ]

    ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )
    ! identity matrices
    real ( rp ), parameter :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ unit_x_2, unit_y_2 ], [ 2, 2 ] )
    real ( rp ), parameter :: id_matrix_3 ( 1 : 3, 1 : 3 ) = reshape ( [ unit_x_3, unit_y_3, unit_z_3 ], [ 3, 3 ] )

    ! physical constants
    real ( rp ), parameter :: grav = 9.80665_rp ! http://physics.nist.gov/cgi-bin/cuu/Value?gn|search_for=gravity
    real ( rp ), parameter :: radius_earth_equitorial      = 6378.137_rp, & ! International Union of Geodesy and Geophysics
                              radius_earth_polar           = 6356.752_rp, & ! pull date: 2016 11 06
                              radius_earth_volumetric_mean = 6371.008_rp
    real ( rp ), parameter :: R_earth = radius_earth_volumetric_mean ! kilometers

    real ( rp ), parameter :: rho_aw = 0.0012041_rp

contains

    subroutine constants_list_maker ( io_handle )

        integer, intent ( in ) :: io_handle

            write ( io_handle, 100 ) 'mod_global_constants'
            write ( io_handle, 110 ) rp

            ! rational constansts
            write ( io_handle, 200 ) zero, 'zero'
            write ( io_handle, 200 ) one,  'one'
            write ( io_handle, 200 ) half, 'half'

            write ( io_handle, 210 ) complex_unit_modulus, 'i, complex unit modulus'

            ! pi
            write ( io_handle, 200 ) pi, 'pi, via acos ( -1 )'
            write ( io_handle, 200 ) rad_to_deg, 'radians to degrees'
            write ( io_handle, 200 ) deg_to_rad, 'degrees to radians'

            ! physical constants in mks units
            write ( io_handle, 200 ) grav, 'acceleration due to gravity'
            write ( io_handle, 200 ) radius_earth_equitorial, 'earth radius, equitorial'
            write ( io_handle, 200 ) radius_earth_polar, 'earth radius, polar'
            write ( io_handle, 200 ) radius_earth_volumetric_mean, 'earth radius, volumetric'
            write ( io_handle, 200 ) R_earth, 'R_earth used in FUNWAVE'
            write ( io_handle, 200 ) rho_aw, 'rho_aw'

            write ( io_handle, * )

        return

    100 format ( /, 'List of parameters in ', g0, ': ' )
    110 format ( 'selected_real_kind = ', g0, / )

    200 format ( g0, ": ", g0 )
    210 format ( '(', g0, ', ', g0, '): ', g0 )

    end subroutine constants_list_maker

end module mGlobalConstants
