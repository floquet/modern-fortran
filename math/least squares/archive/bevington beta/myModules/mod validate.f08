! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

!  validate_bevington
!  load_bevington_measurements_sub
!  load_bevington_results_sub

module mValidate

    use mSolnsLinear
    implicit none

    integer ( ip ), parameter         :: nBevington = 9

        type :: comparison
            type ( solution_linear ) :: bevington_exercise
            real ( rp ) :: diff_slop
            real ( rp ) :: diff_int
        contains
            private
            procedure, public :: validate_bevington          => validate_bevington_sub
            procedure, public :: load_bevington_measurements => load_bevington_measurements_sub
            procedure, public :: load_bevington_results      => load_bevington_results_sub
        end type comparison

    private :: load_bevington_measurements_sub
    private :: load_bevington_results_sub
    private :: validate_bevington_sub

    contains                                                                                    ! methods: subroutines and functions

!       ===========================================================================================  load_bevington_measurements_sub

        subroutine validate_bevington_sub ( me )

            class ( comparison ), target :: me

           ! b => bevington_exercise
                me % bevington_exercise % m = nBevington
                call me % bevington_exercise % allocate_group ( )
                call me % load_bevington_measurements ( )
                call me % load_bevington_results ( )
!                 print *, 'me % bevington_exercise % x ( 1 ) = ', me % bevington_exercise % x ( 1 )
!                 print *, 'me % bevington_exercise % m = ', me % bevington_exercise % m
                call me % bevington_exercise % normal_a ( echo = .true. )
            !b => null ( )

        end subroutine validate_bevington_sub


!       ===========================================================================================  load_bevington_measurements_sub

        subroutine load_bevington_measurements_sub ( me )

            class ( comparison ), target :: me

            !type ( solution_linear ), pointer, intent ( inout ) :: b

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

            !b => bevington_exercise
                me % bevington_exercise % m = 9

                me % bevington_exercise % x ( 1 ) = 1.0_rp
                me % bevington_exercise % x ( 2 ) = 2.0_rp
                me % bevington_exercise % x ( 3 ) = 3.0_rp
                me % bevington_exercise % x ( 4 ) = 4.0_rp
                me % bevington_exercise % x ( 5 ) = 5.0_rp
                me % bevington_exercise % x ( 6 ) = 6.0_rp
                me % bevington_exercise % x ( 7 ) = 7.0_rp
                me % bevington_exercise % x ( 8 ) = 8.0_rp
                me % bevington_exercise % x ( 9 ) = 9.0_rp

                me % bevington_exercise % y ( 1 ) = 15.60_rp
                me % bevington_exercise % y ( 2 ) = 17.50_rp
                me % bevington_exercise % y ( 3 ) = 36.60_rp
                me % bevington_exercise % y ( 4 ) = 43.80_rp
                me % bevington_exercise % y ( 5 ) = 58.20_rp
                me % bevington_exercise % y ( 6 ) = 61.60_rp
                me % bevington_exercise % y ( 7 ) = 64.20_rp
                me % bevington_exercise % y ( 8 ) = 70.40_rp
                me % bevington_exercise % y ( 9 ) = 98.80_rp
                print *, 'me % bevington_exercise % x ( 1 ) = ', me % bevington_exercise % x ( 1 )
            !b => null ( )

        end subroutine load_bevington_measurements_sub

!       ===========================================================================================       load_bevington_results_sub

        subroutine load_bevington_results_sub ( me )

            class ( comparison ), target :: me

            !type ( solution_linear ), pointer, intent ( inout ) :: b

            !type ( solution_linear ), pointer, intent ( inout ) :: b

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

            !b => bevington_exercise
                me % bevington_exercise % solution ( 1 ) = 4.813888888888889_rp
                me % bevington_exercise % solution ( 2 ) = 9.408333333333333_rp

                me % bevington_exercise % error ( 1 )    = 4.30922891886373_rp
                me % bevington_exercise % error ( 2 )    = 0.7657700742287871_rp
            !b => null ( )

        end subroutine load_bevington_results_sub

!       ===========================================================================================          validation_exercise_sub

end module mValidate
