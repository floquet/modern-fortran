program namelist_tester

    use, intrinsic :: iso_fortran_env,  only : OUTPUT_UNIT, compiler_version, compiler_options

    use mGlobalConstants,               only : id_matrix_2, complex_unit_modulus, radius_earth_polar, radius_earth_equitorial
    use mSetPrecision,                  only : ip, rp

    implicit none

        integer :: io_nml = 0, io_status

        real ( rp ) :: polar_radius = radius_earth_polar
        real ( rp ) :: equator_radius = radius_earth_equitorial

        character ( len = * ), parameter :: nml_file = 'sample_namelist.txt'
        character ( len = * ) :: path = 'myPath'

        namelist / nml_constants / polar_radius, equator_radius, path

            ! open namelist
            open  ( unit = io_nml, file = nml_file, delim = 'apostrophe', iostat = io_status )

            ! write namelist
            write ( unit = io_nml, nml = nml_constants, iostat = io_status )
            if ( io_status /= 0 ) then                              ! can't write file
                write ( *,  * )
                write ( *,  200 ) 'unable to write file ', nml_file
                write ( *,  200 ) 'trying to write namelist ',  'nml_constants'
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
