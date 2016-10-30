submodule ( mValidate ) smLoaders

contains

!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        + ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!        + +                                                                                    + +
!        + +  Load data and results for Bevington tables 6-1, 6-2                               + +
!        + +                                                                                    + +
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +               load_bevington_6_1

        module subroutine load_bevington_6_1_sub ( me, myResults )

            class ( comparison ), target        :: me
            type  ( results ), intent ( inout ) :: myResults

                call me % load_bevington_measurements_6_1 ( myResults )
                call me % load_bevington_results_6_1      ( myResults )

        end subroutine load_bevington_6_1_sub

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +       load_bevington_results_6_1

        module subroutine load_bevington_results_6_1_sub ( myResults )

            class ( results ), target       :: myResults
            type  ( solns_linear ), pointer :: pntr => null ( )

                ! user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25,
                ! nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                pntr => myResults % soln_linear ( 1 )
                    pntr % descriptor_64  = 'Mathematica results'

                    pntr % solution ( 1 ) = 4.813888888888889_rp
                    pntr % solution ( 2 ) = 9.408333333333333_rp

                    pntr % error ( 1 )    = 4.886206312183354_rp
                    pntr % error ( 2 )    = 0.8683016476563611_rp
                pntr => null ( )

        end subroutine load_bevington_results_6_1_sub

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  load_bevington_measurements_6_1

        module subroutine load_bevington_measurements_6_1_sub ( myResults )

            class ( results ), target       :: myResults
            type  ( measurements ), pointer :: pntr => null ( )

                ! user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25,
                ! nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                pntr => myResults % measurement
                    pntr % descriptor_64 = 'Bevington table 6-1, p. 93'
                    pntr % m = 9
                    call pntr % allocate_group ( )

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

end submodule smLoaders
