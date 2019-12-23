module mValidateLines

    use mPrecisionDefinitions,          only : ip, rp
    use mConstants,                     only : zero, one, stdout, machine_epsilon, &
                                               fmt_fortran, fmt_mathematica, fmt_difference, fmt_difference_eps

    use mLSQ,                           only : lsq_fit
    use mMathematicaOutput,             only : mm_lines

    implicit none

    type :: comparison_lines
        integer ( ip )      :: index = 0
        type ( lsq_fit )    :: result_fortran, result_mathematica, result_difference
    contains
        private
        procedure, public   :: validate_lines       => validate_lines_sub
        procedure, public   :: print_engine_lines   => print_engine_lines_sub
    end type comparison_lines

    private :: validate_lines_sub
    private :: print_engine_lines_sub

contains

    !   +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +    validate_lines

    subroutine validate_lines_sub ( me, result_fortran, index, my_io_unit )

        class ( comparison_lines ), target    :: me

        type ( lsq_fit ), intent ( in )       :: result_fortran

        integer ( ip ),   intent ( in )       :: index, my_io_unit
        integer ( ip )                        :: values ( 1 : 8 ) = 0

        type ( lsq_fit ), pointer             :: p_f => null ( ), p_m => null ( ), p_d => null ( )

            me % index = index
            p_f => me % result_fortran
            p_m => me % result_mathematica
            p_d => me % result_difference
                p_f = result_fortran
                p_m = mm_lines ( index )

                p_d % gap            = p_f % gap           - p_m % gap
                p_d % intercept      = p_f % intercept     - p_m % intercept
                p_d % slope          = p_f % slope         - p_m % slope

                p_d % err_gap        = p_f % err_gap       - p_m % err_gap
                p_d % err_intercept  = p_f % err_intercept - p_m % err_intercept
                p_d % err_slope      = p_f % err_slope     - p_m % err_slope
            p_d => null ( )
            p_f => null ( )
            p_m => null ( )

            !my_io_unit = safeopen_writereplace ( filename = file_out_lines )

            ! write file
            call print_engine_lines_sub ( me, my_io_unit )
            ! time and date stamp
            call date_and_time ( VALUES = values )
            write  ( my_io_unit, '( I5, 2 ( "-", I2.2 ) )', advance = 'no' ) values ( 1 : 3 )
            write  ( my_io_unit, '( I3, 2 ( ":", I2.2 ), / )' )              values ( 5 : 7 )

            ! close file
            ! close ( unit = my_io_unit )

    end subroutine validate_lines_sub

    !   +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   print_engine_lines

    subroutine print_engine_lines_sub ( me, io_unit )

        class ( comparison_lines ), target    :: me
        integer ( ip ), intent ( in )         :: io_unit
        real ( rp )                           :: a = zero, b = zero, c = zero, d = zero, diff1 = zero, diff2 = zero
        type ( lsq_fit ), pointer             :: p_f => null ( ), p_m => null ( ), p_d => null ( )

            write ( io_unit , '( /, "Validation exercise for lines, case ", g0 )' ) me % index

            p_f => me % result_fortran
            p_m => me % result_mathematica
            p_d => me % result_difference

                a = p_f %     intercept
                b = p_f % err_intercept
                c = p_m %     intercept
                d = p_m % err_intercept
                diff1 = a - c
                diff2 = b - d

                write ( io_unit, fmt_fortran )     'intercept, ', a, b
                write ( io_unit, fmt_mathematica ) 'intercept, ', c, d
                write ( io_unit, fmt_difference )   diff1, diff2
                write ( io_unit, fmt_difference_eps ) abs ( diff1 ) / machine_epsilon, abs ( diff2 ) / machine_epsilon

                a = p_f %     gap
                b = p_f % err_gap
                c = p_m %     gap
                d = p_m % err_gap
                diff1 = a - c
                diff2 = b - d

                write ( io_unit, fmt_fortran )     'gap,       ', a, b
                write ( io_unit, fmt_mathematica ) 'gap,       ', c, d
                write ( io_unit, fmt_difference )   diff1, diff2
                write ( io_unit, fmt_difference_eps ) abs ( diff1 ) / machine_epsilon, abs ( diff2 ) / machine_epsilon

                a = p_f %     slope
                b = p_f % err_slope
                c = p_m %     slope
                d = p_m % err_slope
                diff1 = a - c
                diff2 = b - d

                write ( io_unit, fmt_fortran )       'slope,     ', a, b
                write ( io_unit, fmt_mathematica )   'slope,     ', c, d
                write ( io_unit, fmt_difference )     diff1, diff2
                write ( io_unit, fmt_difference_eps ) abs ( diff1 ) / machine_epsilon, abs ( diff2 ) / machine_epsilon

            p_d => null ( )
            p_f => null ( )
            p_m => null ( )

    end subroutine print_engine_lines_sub

end module mValidateLines
