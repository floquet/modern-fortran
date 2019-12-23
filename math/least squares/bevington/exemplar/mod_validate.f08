! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 04 04

module mValidate  ! submodules: smLoaders

    use mPrecisionDefinitions,  only : ip, rp

    use mMeasurements,          only : measurements
    use mResults,               only : results
    use mSolnsLinear,           only : solns_linear, dof, dof_params

    implicit none

    integer ( ip ), private :: kVal

    type            :: comparison
        real ( rp ) :: diff_fit ( 1 : dof )
        real ( rp ) :: diff_err ( 1 : dof )
    contains
        private
        procedure, public, nopass :: compare_solutions               => compare_solutions_sub
        procedure, public, nopass :: load_bevington_6_1              => load_bevington_6_1_sub
        procedure, public, nopass :: validate_bevington_6_1          => validate_bevington_6_1_sub
        procedure, public, nopass :: load_bevington_results_6_1      => load_bevington_results_6_1_sub
        procedure, public, nopass :: load_bevington_measurements_6_1 => load_bevington_measurements_6_1_sub
    end type comparison

    private :: validate_bevington_6_1_sub
    private :: load_bevington_6_1_sub
    private :: load_bevington_results_6_1_sub
    private :: load_bevington_measurements_6_1_sub

    interface ! mod_sub_validate_loaders

        module subroutine load_bevington_6_1_sub ( me, myResults )
            class ( comparison ), target        :: me
            type  ( results ), intent ( inout ) :: myResults
        end subroutine load_bevington_6_1_sub

        module subroutine load_bevington_measurements_6_1_sub ( myResults )
            class ( results ), target :: myResults
        end subroutine load_bevington_measurements_6_1_sub

        module subroutine load_bevington_results_6_1_sub ( myResults )
            class ( results ), target :: myResults
        end subroutine load_bevington_results_6_1_sub

    end interface

    contains                                                                                    ! methods: subroutines and functions

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +             compare_solutions_sub

        subroutine compare_solutions_sub ( a, b )

            type ( comparison ), target          :: compare
            type ( solns_linear ), intent ( in ) :: a, b

                compare % diff_fit = a % solution - b % solution
                compare % diff_err = a % error    - b % error

                write ( * , 100 )
                write ( * , 110 ) trim ( a % descriptor_64 ), trim ( b % descriptor_64 )

                if ( a % status /= 0 ) then
                    write ( * , 900 ) 'Mathematica Results: ', trim ( a % descriptor_64 )
                    write ( * , 910 ) a % status
                    write ( * , 920 ) a % warning
                end if

                if ( b % status /= 0 ) then
                    write ( * , 900 ) 'Fortran Results: ', trim ( b % descriptor_64 )
                    write ( * , 910 ) b % status
                    write ( * , 920 ) b % warning
                end if

                write ( * , 120 )
                write ( * , 130 )
                do kVal = 1, dof
                    write ( * , 200 ) dof_params ( kVal ), a % solution ( kVal ), b % solution ( kVal ), &
                                              compare % diff_fit ( kVal ), &
                                        abs ( compare % diff_fit ( kVal ) ) / epsilon ( a % solution ( kVal ) )
                end do

                write ( * , 140 )
                write ( * , 130 )
                do kVal = 1, dof
                    write ( * , 200 ) dof_params ( kVal ), a % error ( kVal ), b % error ( kVal ), &
                                              compare % diff_err ( kVal ), &
                                        abs ( compare % diff_err ( kVal ) ) / epsilon ( a % solution ( kVal ) )
                end do

                write ( * , 150 ) epsilon ( a % solution ( kVal ) )

                write ( * , 800 ) a % cpu_seconds_compute, 'A'
                write ( * , 800 ) b % cpu_seconds_compute, 'B'

                return

            100 format ( /, 'Comparison of results:' )
            110 format (    'Mathematica: ', g0, /, 'Fortran: ', g0 )
            120 format ( /, 'Fit parameters:' )
            130 format (     T15, 'A', T40, 'B', T65, 'Difference', T90, 'Epsilons')
            140 format ( /, 'Error parameters:' )
            150 format ( /, 'machine epsilon = ', g0 )

            200 format ( A9, 5X, g0, T40, g0, T65, E10.3, T90, E9.3 )

            800 format ( E10.3, ' s: CPU time for ', g0,' solution' )

            900 format ( /, 'Computation ', g0 )
            910 format (    'Warning! status parameter = ', g0 )
            920 format ( g0, / )

        end subroutine compare_solutions_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       + +                                                                                     + +
!       + +  Validate results for Bevington tables 6-1, 6-2                                     + +
!       + +                                                                                     + +
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        subroutine validate_bevington_6_1_sub ( me )

            class ( comparison ), target :: me

            type ( results )             :: validate
            integer ( ip )               :: myM

                call me % load_bevington_6_1 ( me, validate )
                myM = validate % measurement % m

                validate % soln_linear ( 2 ) % descriptor_64 = 'Computation via normal equations, dot product'
                call validate % soln_linear ( 2 ) % normal_a ( validate % measurement )

                validate % numSolnsLinear = 2

                call me % compare_solutions ( validate % soln_linear ( 1 ), validate % soln_linear ( 2 ) )

        end subroutine validate_bevington_6_1_sub

        subroutine validate_bevington_6_2_sub (  )

            !class ( results ), target :: me

                return

        end subroutine validate_bevington_6_2_sub

end module mValidate
