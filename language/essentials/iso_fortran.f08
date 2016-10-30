program iso_fortran

    use, intrinsic :: iso_fortran_env, only : compiler_options, compiler_version

    implicit none

    character ( len = * ) :: compiled_by = compiler_version ( )
    character ( len = * ) :: compiled_with = compiler_options ( )

        write ( * , 100 ) compiled_by
        write ( * , 110 ) compiled_with

        stop 'successful run for iso_fortran...'

    100 format ( /, 'compiler version = ', g0 )
    110 format (    'compiler options = ', g0, / )

end program iso_fotran
