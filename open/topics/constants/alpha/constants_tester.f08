program constants_tester

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, compiler_version, compiler_options

    use mGlobalConstants,              only : constants_list_maker

    implicit none

        call constants_list_maker ( OUTPUT_UNIT )

        write ( *, 200 ) 'compiler version: ', compiler_version ()
        write ( *, 200 ) 'compiler options: ', compiler_options ()

        stop '* * * successful completion for constants_tester...'

    200 format ( g0, g0 )

end program constants_tester
