! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

!  validate_bevington
!  load_bevington_measurements_sub
!  load_bevington_results_sub

module mValidate

    use mSolnsLinear
    implicit none

    integer ( ip ), parameter         :: nBevington = 9
    type ( solution_linear ), target  :: bevington_exercise
    type ( solution_linear ), pointer :: b

    private :: load_bevington_measurements_sub
    private :: load_bevington_results_sub

    contains                                                                                    ! methods: subroutines and functions

!       ===========================================================================================  load_bevington_measurements_sub

        subroutine validate_bevington_sub ( )

            b => bevington_exercise
                b % m = nBevington
                call b % allocate_group ( )
                call load_bevington_measurements_sub ( )
                call load_bevington_results_sub ( )
                print *, 'b % x ( 1 ) = ', b % x ( 1 )
                print *, 'b % m = ', b % m
                call b % normal_a ( echo = .true. )
            b => null ( )

        end subroutine validate_bevington_sub


!       ===========================================================================================  load_bevington_measurements_sub

        subroutine load_bevington_measurements_sub ( )

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

            b => bevington_exercise
                b % m = 9

                b % x ( 1 ) = 1.0_rp
                b % x ( 2 ) = 2.0_rp
                b % x ( 3 ) = 3.0_rp
                b % x ( 4 ) = 4.0_rp
                b % x ( 5 ) = 5.0_rp
                b % x ( 6 ) = 6.0_rp
                b % x ( 7 ) = 7.0_rp
                b % x ( 8 ) = 8.0_rp
                b % x ( 9 ) = 9.0_rp

                b % y ( 1 ) = 15.60_rp
                b % y ( 2 ) = 17.50_rp
                b % y ( 3 ) = 36.60_rp
                b % y ( 4 ) = 43.80_rp
                b % y ( 5 ) = 58.20_rp
                b % y ( 6 ) = 61.60_rp
                b % y ( 7 ) = 64.20_rp
                b % y ( 8 ) = 70.40_rp
                b % y ( 9 ) = 98.80_rp
                print *, 'b % x ( 1 ) = ', b % x ( 1 )
            b => null ( )

        end subroutine load_bevington_measurements_sub

!       ===========================================================================================       load_bevington_results_sub

        subroutine load_bevington_results_sub ( )

            !type ( solution_linear ), pointer, intent ( inout ) :: b

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

            b => bevington_exercise
                b % solution ( 1 ) = 4.813888888888889_rp
                b % solution ( 2 ) = 9.408333333333333_rp

                b % error ( 1 )    = 4.30922891886373_rp
                b % error ( 2 )    = 0.7657700742287871_rp
            b => null ( )

        end subroutine load_bevington_results_sub

!       ===========================================================================================          validation_exercise_sub

end module mValidate
