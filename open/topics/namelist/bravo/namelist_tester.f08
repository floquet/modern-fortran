program namelist_tester

    use, intrinsic :: iso_fortran_env,  only : OUTPUT_UNIT, compiler_version, compiler_options

    use mGlobalConstants,               only : id_matrix_3, complex_unit_modulus, pi, radius_earth_polar, radius_earth_equitorial
    use mSetPrecision,                  only : rp

    implicit none

        integer :: io_nml = 0, io_status

        real ( rp ) :: myPi = pi  ! namelist variables can't be parameters
        real ( rp ) :: polar_radius = radius_earth_polar
        real ( rp ) :: equator_radius = radius_earth_equitorial
        real ( rp ), dimension ( 1 : 3, 1 : 3 ) :: myID3 = id_matrix_3

        complex ( rp ) :: i = complex_unit_modulus

        character ( len = * ), parameter :: nml_file = 'sample_namelist.txt'
        character ( len = 32 ) :: path_to_input   = '~/dir1/dir2/input_data',   &
                                  path_to_output  = '../dirA/dirB/output_data', &
                                  path_to_modules = '${myModules}/modules'

        namelist / nml_sample / myID3, i, myPi, polar_radius, equator_radius, &
                                path_to_input, path_to_output, path_to_modules

            ! open namelist
            open  ( unit = io_nml, file = nml_file, delim = 'apostrophe', iostat = io_status )

            ! write namelist
            write ( unit = io_nml, nml = nml_sample, iostat = io_status )
            if ( io_status /= 0 ) then                              ! can't write file
                write ( *,  * )
                write ( *,  200 ) 'unable to write file ', nml_file
                write ( *,  200 ) 'trying to write namelist ',  'nml_sample'
                write ( *,  200 ) 'iostat  = ', io_status
                write ( *,  200 ) 'io unit = ', io_nml
                stop  'fatal error during write to ' // nml_file
            end if

            write ( *, * ) 'successful write to ', nml_file

            ! close file
            close ( unit = io_nml, iostat = io_status )

        write ( *, 200 ) 'compiler version: ', compiler_version ()
        write ( *, 200 ) 'compiler options: ', compiler_options ()

        stop '* * * successful completion for namelist_tester...'

    200 format ( g0, g0 )

end program namelist_tester
