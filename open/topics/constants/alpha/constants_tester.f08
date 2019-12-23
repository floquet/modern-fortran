program constants_tester

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, compiler_version, compiler_options

    use mGlobalConstants,              only : pi, constants_list_maker

    implicit none

        write ( *, 200 ) sin ( pi ), ' = sin pi (ideal =  0)'
        write ( *, 200 ) cos ( pi ), ' = cos pi (ideal = -1) '

        call constants_list_maker ( OUTPUT_UNIT )

        write ( *, 200 ) 'compiler version: ', compiler_version ()
        write ( *, 200 ) 'compiler options: ', compiler_options ()

        stop '* * * successful completion for constants_tester...'

    200 format ( g0, g0 )

end program constants_tester
