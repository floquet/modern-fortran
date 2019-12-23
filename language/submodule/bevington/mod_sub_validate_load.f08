! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

submodule ( mValidate ) smLoader

    contains                                                                                    ! methods: subroutines and functions

!       ===========================================================================================  load_bevington_measurements_6_1

        module subroutine load_bevington_measurements_6_1_sub ( myResults )

            class ( results ), target      :: myResults
            type ( measurements ), pointer :: pntr

                ! user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25,
                ! nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

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

end submodule smLoader
