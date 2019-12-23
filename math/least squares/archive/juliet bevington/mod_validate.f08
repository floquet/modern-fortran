! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

!  compare_solutions_sub
!  validate_bevington_6_1_sub
!  validate_bevington_6_2_sub
!  load_bevington_results_6_1_sub
!  load_bevington_results_6_2_sub
!  load_bevington_measurements_6_1_sub
!  load_bevington_measurements_6_2_sub

module mValidate

    use mResults
    implicit none

    integer ( ip ), private :: kVal

        type                :: comparison
            real ( rp )     :: diff_fit ( 1 : dof )
            real ( rp )     :: diff_err ( 1 : dof )
        contains
            private
            procedure, public, nopass :: compare_solutions               => compare_solutions_sub
            procedure, public, nopass :: load_bevington_6_1              => load_bevington_6_1_sub
            procedure, public, nopass :: load_bevington_6_2              => load_bevington_6_2_sub
            procedure, public, nopass :: validate_bevington_6_1          => validate_bevington_6_1_sub
            procedure, public, nopass :: validate_bevington_6_2          => validate_bevington_6_2_sub
            procedure, public, nopass :: load_bevington_results_6_1      => load_bevington_results_6_1_sub
            procedure, public, nopass :: load_bevington_results_6_2      => load_bevington_results_6_2_sub
            procedure, public, nopass :: load_bevington_measurements_6_1 => load_bevington_measurements_6_1_sub
            procedure, public, nopass :: load_bevington_measurements_6_2 => load_bevington_measurements_6_2_sub
        end type comparison

    private :: load_bevington_6_1_sub
    private :: load_bevington_6_2_sub
    private :: validate_bevington_6_1_sub
    private :: validate_bevington_6_2_sub
    private :: load_bevington_results_6_1_sub
    private :: load_bevington_results_6_2_sub
    private :: load_bevington_measurements_6_1_sub
    private :: load_bevington_measurements_6_2_sub

    contains                                                                                    ! methods: subroutines and functions

!       ===========================================================================================            compare_solutions_sub

        subroutine compare_solutions_sub ( a, b )

            type ( comparison ), target          :: compare
            type ( solns_linear ), intent ( in ) :: a, b

                compare % diff_fit = a % solution - b % solution
                compare % diff_err = a % error    - b % error

                write ( * , 100 )
                write ( * , 110 ) trim ( a % descriptor_64 ), trim ( b % descriptor_64 )

                if ( a % status /= 0 ) then
                    write ( * , 900 ) 'A: ', trim ( a % descriptor_64 )
                    write ( * , 910 ) a % status
                    write ( * , 920 ) a % warning
                end if

                if ( b % status /= 0 ) then
                    write ( * , 900 ) 'B: ', trim ( b % descriptor_64 )
                    write ( * , 910 ) b % status
                    write ( * , 920 ) b % warning
                end if

                write ( * , 120 )
                write ( * , 130 )
                do kVal = 1, dof
                    write ( * , 200 ) a % solution ( kVal ), b % solution ( kVal ), compare % diff_fit ( kVal ), &
                                      abs ( compare % diff_fit ( kVal ) ) / epsilon ( a % solution ( kVal ) )
                end do

                write ( * , 140 )
                write ( * , 130 )
                do kVal = 1, dof
                    write ( * , 200 ) a % error ( kVal ), b % error ( kVal ), compare % diff_err ( kVal ), &
                                      abs ( compare % diff_err ( kVal ) ) / epsilon ( a % solution ( kVal ) )
                end do

                write ( * , 150 ) epsilon ( a % solution ( kVal ) )

                write ( * , 800 ) a % cpu_seconds_compute, 'A'
                write ( * , 800 ) b % cpu_seconds_compute, 'B'

            return

  100       format ( /, 'Comparison of results:' )
  110       format (    'A: ', g0, /, 'B: ', g0 )
  120       format ( /, 'Fit parameters:' )
  130       format (    'A', T25, 'B', T50, 'Difference', T75, 'Epsilons')
  140       format ( /, 'Error parameters:' )
  150       format ( /, 'machine epsilon = ', g0 )

  200       format ( g0, T25, g0, T50, E10.3, T75, E9.3 )

  800       format ( E10.3, ' s: CPU time for ', g0,' solution' )

  900       format ( /, 'Computation ', g0 )
  910       format (    'Warning! status parameter = ', g0 )
  920       format ( g0, / )

        end subroutine compare_solutions_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       + +                                                                                     + +
!       + +  Validate results for Bevington tables 6-1, 6-2                                     + +
!       + +                                                                                     + +
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       ===========================================================================================  load_bevington_measurements_6_1

        subroutine validate_bevington_6_1_sub ( me )

            class ( comparison ), target :: me

            type ( results )             :: validate
            integer ( ip )               :: myM
            !type ( intermediates ) :: ints
            !type ( measurements ) :: measures

                call me % load_bevington_6_1 ( me, validate )
                myM = validate % measurement % m

                validate % soln_linear ( 2 ) % descriptor_64 = 'normal equations, dot product'
!                call validate % soln_linear ( 3 ) % normal_a ( validate % measurement, echo = .true. )
                call validate % soln_linear ( 2 ) % normal_a ( validate % measurement, echo = .true. )

                validate % numSolnsLinear = 2

                call me % compare_solutions ( validate % soln_linear ( 1 ), validate % soln_linear ( 2 ) )

        end subroutine validate_bevington_6_1_sub

!       ===========================================================================================  load_bevington_measurements_6_2

        subroutine validate_bevington_6_2_sub (  )

            !class ( results ), target :: me

                return

        end subroutine validate_bevington_6_2_sub

!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        + ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!        + +                                                                                    + +
!        + +  Load data and results for Bevington tables 6-1, 6-2                               + +
!        + +                                                                                    + +
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       ===========================================================================================               load_bevington_6_1

        subroutine load_bevington_6_1_sub ( me, myResults )

            class ( comparison ), target       :: me
            type ( results ), intent ( inout ) :: myResults

                call me % load_bevington_measurements_6_1 ( myResults )
                call me % load_bevington_results_6_1      ( myResults )

        end subroutine load_bevington_6_1_sub

!       ===========================================================================================               load_bevington_6_2

        subroutine load_bevington_6_2_sub ( me, myResults )

            class ( comparison ), target       :: me
            type ( results ), intent ( inout ) :: myResults

                call me % load_bevington_measurements_6_2 ( myResults )
                call me % load_bevington_results_6_2      ( myResults )

        end subroutine load_bevington_6_2_sub

!       ===========================================================================================  load_bevington_measurements_6_1

        subroutine load_bevington_measurements_6_1_sub ( myResults )

            class ( results ), target      :: myResults
            type ( measurements ), pointer :: pntr

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25,
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                pntr => myResults % measurement
                    pntr % descriptor_64 = 'Bevington table 6-1, p. 93'
                    pntr % m = 9
                    call pntr % allocate_group ( echo = .true. )

                    pntr % x ( 1 ) = 1.0_rp
                    pntr % x ( 2 ) = 2.0_rp
                    pntr % x ( 3 ) = 3.0_rp
                    pntr % x ( 4 ) = 4.0_rp
                    pntr % x ( 5 ) = 5.0_rp
                    pntr % x ( 6 ) = 6.0_rp
                    pntr % x ( 7 ) = 7.0_rp
                    pntr % x ( 8 ) = 8.0_rp
                    pntr % x ( 9 ) = 9.0_rp

                    pntr % y ( 1 ) = 15.60_rp
                    pntr % y ( 2 ) = 17.50_rp
                    pntr % y ( 3 ) = 36.60_rp
                    pntr % y ( 4 ) = 43.80_rp
                    pntr % y ( 5 ) = 58.20_rp
                    pntr % y ( 6 ) = 61.60_rp
                    pntr % y ( 7 ) = 64.20_rp
                    pntr % y ( 8 ) = 70.40_rp
                    pntr % y ( 9 ) = 98.80_rp
                pntr => null ( )

        end subroutine load_bevington_measurements_6_1_sub

!       ===========================================================================================  load_bevington_measurements_6_2

        subroutine load_bevington_measurements_6_2_sub ( myResults )

            class ( results ), target      :: myResults
            type ( measurements ), pointer :: pntr

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25,
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                    pntr => myResults % measurement
                        pntr % descriptor_64 = 'Bevington table 6-2, p. 96'
                        pntr % m = 10
                        call pntr % allocate_group ( )

                        pntr % x ( 1 )  =   0.0_rp
                        pntr % x ( 2 )  =  15.0_rp
                        pntr % x ( 3 )  =  30.0_rp
                        pntr % x ( 4 )  =  45.0_rp
                        pntr % x ( 5 )  =  60.0_rp
                        pntr % x ( 6 )  =  75.0_rp
                        pntr % x ( 7 )  =  90.0_rp
                        pntr % x ( 8 )  = 105.0_rp
                        pntr % x ( 9 )  = 120.0_rp
                        pntr % x ( 10 ) = 135.0_rp

                        pntr % y ( 1 )  = 106.0_rp
                        pntr % y ( 2 )  =  80.0_rp
                        pntr % y ( 3 )  =  98.0_rp
                        pntr % y ( 4 )  =  75.0_rp
                        pntr % y ( 5 )  =  74.0_rp
                        pntr % y ( 6 )  =  73.0_rp
                        pntr % y ( 7 )  =  49.0_rp
                        pntr % y ( 8 )  =  38.0_rp
                        pntr % y ( 9 )  =  37.0_rp
                        pntr % y ( 10 ) =  22.0_rp
                    pntr => null ( )

        end subroutine load_bevington_measurements_6_2_sub

!       ===========================================================================================       load_bevington_results_6_1

        subroutine load_bevington_results_6_1_sub ( myResults )

            class ( results ), target      :: myResults
            type ( solns_linear ), pointer :: pntr

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25,
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                pntr => myResults % soln_linear ( 1 )
                    pntr % descriptor_64  = 'Published results'

                    pntr % solution ( 1 ) = 4.813888888888889_rp
                    pntr % solution ( 2 ) = 9.408333333333333_rp

                    pntr % error ( 1 )    = 4.886206312183354_rp
                    pntr % error ( 2 )    = 0.8683016476563611_rp
                pntr => null ( )

        end subroutine load_bevington_results_6_1_sub

!       ===========================================================================================       load_bevington_results_6_2

        subroutine load_bevington_results_6_2_sub ( myResults )

            class ( results ), target      :: myResults
            type ( solns_linear ), pointer :: pntr

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25,
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                pntr => myResults % soln_linear ( 1 )
                    pntr % descriptor_64  = 'Published results'

                    pntr % solution ( 1 ) = 104.36363636363636_rp
                    pntr % solution ( 2 ) =  -0.5802020202020202_rp

                    pntr % error ( 1 )    = 5.127390929219931_rp
                    pntr % error ( 2 )    = 0.06402988792986754_rp
                pntr => null ( )

        end subroutine load_bevington_results_6_2_sub

end module mValidate
