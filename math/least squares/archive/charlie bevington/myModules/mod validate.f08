! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

!  compare_solutions_sub
!  validate_bevington_I_sub
!  validate_bevington_II_sub
!  load_bevington_results_I_sub
!  load_bevington_results_II_sub
!  load_bevington_measurements_I_sub
!  load_bevington_measurements_II_sub

module mValidate

    use mSolnsLinear
    implicit none

    integer ( ip ), private            :: kVal

        type                           :: comparison
            type ( solution_linear )   :: bevington_fiducial
            type ( solution_linear )   :: computed
            real ( rp )                :: diff_fit ( 1 : dof )
            real ( rp )                :: diff_err ( 1 : dof )
        contains
            private
            procedure, public :: load_bevington_I               => load_bevington_I_sub
            procedure, public :: load_bevington_II              => load_bevington_II_sub
            procedure, public :: compare_solutions              => compare_solutions_sub
            procedure, public :: validate_bevington_I           => validate_bevington_I_sub
            procedure, public :: validate_bevington_II          => validate_bevington_II_sub
            procedure, public :: load_bevington_results_I       => load_bevington_results_I_sub
            procedure, public :: load_bevington_results_II      => load_bevington_results_II_sub
            procedure, public :: load_bevington_measurements_I  => load_bevington_measurements_I_sub
            procedure, public :: load_bevington_measurements_II => load_bevington_measurements_II_sub
        end type comparison

    private :: load_bevington_I_sub
    private :: load_bevington_II_sub
    private :: compare_solutions_sub
    private :: validate_bevington_I_sub
    private :: validate_bevington_II_sub
    private :: load_bevington_results_I_sub
    private :: load_bevington_results_II_sub
    private :: load_bevington_measurements_I_sub
    private :: load_bevington_measurements_II_sub

    contains                                                                                    ! methods: subroutines and functions

!       ===========================================================================================             compare_solutions_sub

        subroutine compare_solutions_sub ( me )

            class ( comparison ), target :: me

                me % diff_fit = me % bevington_fiducial % solution - me % computed % solution
                me % diff_err = me % bevington_fiducial % error - me % computed % error
                if ( me % computed % status /= 0 ) then
                    write ( * , 900 ) me % computed % status
                    write ( * , 910 ) me % computed % warning
                end if
                write ( * , 100 )
                write ( * , 110 )
                do kVal = 1, dof
                    write ( * , 200 ) me % bevington_fiducial % solution ( kVal ), &
                                      me % computed           % solution ( kVal ), &
                                      me %                      diff_fit ( kVal ), &
                                      abs ( me % diff_fit ( kVal ) ) / epsilon ( me % bevington_fiducial % solution ( kVal ) )
                end do

                write ( * , 120 )
                write ( * , 110 )
                do kVal = 1, dof
                    write ( * , 200 ) me % bevington_fiducial % error    ( kVal ), &
                                      me % computed           % error    ( kVal ), &
                                      me %                      diff_err ( kVal ), &
                                      abs ( me % diff_err ( kVal ) ) / epsilon ( me % bevington_fiducial % error ( kVal ) )
                end do

                write ( * , 130 ) epsilon ( me % bevington_fiducial % solution )
                write ( * , 800 ) me % computed % cpu_seconds_compute

            return

  100       format ( /, 'Fit parameters:' )
  110       format (    'Fiducial', T25, 'Computed', T50, 'Difference', T75, 'Epsilons')
  120       format ( /, 'Error parameters:' )
  130       format ( /, 'machine epsilon = ', g0 )

  200       format ( g0, T25, g0, T50, E10.3, T75, E9.3 )

  800       format ( /, 'CPU time for normal equations solution = ', E10.3, ' s.')

  900       format ( /, 'Warning! status parameter = ', g0 )
  910       format ( g0, / )
 
        end subroutine compare_solutions_sub

!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        + ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!        + +                                                                                    + +
!        + +  Validate results for Bevington tables 6-1, 6-2                               + +
!        + +                                                                                    + +
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       ===========================================================================================     load_bevington_measurements_I

        subroutine validate_bevington_I_sub ( me )

            class ( comparison ), target :: me

                me % bevington_fiducial % descriptor_64 = 'Bevington exercise 6-1, p 93'
                call me % load_bevington_I ( )
                call me % computed % normal_a ( echo = .true. )
                call me % compare_solutions ( )

        end subroutine validate_bevington_I_sub

!       ===========================================================================================    load_bevington_measurements_II

        subroutine validate_bevington_II_sub ( me )

            class ( comparison ), target :: me

                me % bevington_fiducial % descriptor_64 = 'Bevington exercise 6-2, p 96'
                call me % load_bevington_II ( )
                call me % computed % normal_a ( echo = .true. )
                call me % compare_solutions ( )

        end subroutine validate_bevington_II_sub

!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        + ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!        + +                                                                                    + +
!        + +  Load data and results for Bevington tables 6-1, 6-2                               + +
!        + +                                                                                    + +
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       ===========================================================================================                  load_bevington_I

        subroutine load_bevington_I_sub ( me )

            class ( comparison ), target :: me

            call me % load_bevington_measurements_I ( )
            call me % load_bevington_results_I  ( )

        end subroutine load_bevington_I_sub

!       ===========================================================================================                 load_bevington_II

        subroutine load_bevington_II_sub ( me )

            class ( comparison ), target :: me

            call me % load_bevington_measurements_II ( )
            call me % load_bevington_results_II      ( )

        end subroutine load_bevington_II_sub

!       ===========================================================================================     load_bevington_measurements_I

        subroutine load_bevington_measurements_I_sub ( me )

            class ( comparison ), target :: me

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                me % computed % descriptor_64 = 'Bevington table 6-1, p. 93'

                me % computed % m = 9
                call me % computed % allocate_group ( )

                me % computed % x ( 1 ) = 1.0_rp
                me % computed % x ( 2 ) = 2.0_rp
                me % computed % x ( 3 ) = 3.0_rp
                me % computed % x ( 4 ) = 4.0_rp
                me % computed % x ( 5 ) = 5.0_rp
                me % computed % x ( 6 ) = 6.0_rp
                me % computed % x ( 7 ) = 7.0_rp
                me % computed % x ( 8 ) = 8.0_rp
                me % computed % x ( 9 ) = 9.0_rp

                me % computed % y ( 1 ) = 15.60_rp
                me % computed % y ( 2 ) = 17.50_rp
                me % computed % y ( 3 ) = 36.60_rp
                me % computed % y ( 4 ) = 43.80_rp
                me % computed % y ( 5 ) = 58.20_rp
                me % computed % y ( 6 ) = 61.60_rp
                me % computed % y ( 7 ) = 64.20_rp
                me % computed % y ( 8 ) = 70.40_rp
                me % computed % y ( 9 ) = 98.80_rp

        end subroutine load_bevington_measurements_I_sub

!       ===========================================================================================    load_bevington_measurements_II

        subroutine load_bevington_measurements_II_sub ( me )

            class ( comparison ), target :: me

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                me % computed % descriptor_64 = 'Bevington table 6-2, p. 96'

                me % computed % m = 10
                call me % computed % allocate_group ( )

                me % computed % x ( 1 )  =   0.0_rp
                me % computed % x ( 2 )  =  15.0_rp
                me % computed % x ( 3 )  =  30.0_rp
                me % computed % x ( 4 )  =  45.0_rp
                me % computed % x ( 5 )  =  60.0_rp
                me % computed % x ( 6 )  =  75.0_rp
                me % computed % x ( 7 )  =  90.0_rp
                me % computed % x ( 8 )  = 105.0_rp
                me % computed % x ( 9 )  = 120.0_rp
                me % computed % x ( 10 ) = 135.0_rp

                me % computed % y ( 1 )  = 106.0_rp
                me % computed % y ( 2 )  =  80.0_rp
                me % computed % y ( 3 )  =  98.0_rp
                me % computed % y ( 4 )  =  75.0_rp
                me % computed % y ( 5 )  =  74.0_rp
                me % computed % y ( 6 )  =  73.0_rp
                me % computed % y ( 7 )  =  49.0_rp
                me % computed % y ( 8 )  =  38.0_rp
                me % computed % y ( 9 )  =  37.0_rp
                me % computed % y ( 10 ) =  22.0_rp

        end subroutine load_bevington_measurements_II_sub

!       ===========================================================================================          load_bevington_results_I

        subroutine load_bevington_results_I_sub ( me )

            class ( comparison ), target :: me

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                me % bevington_fiducial % solution ( 1 ) = 4.813888888888889_rp
                me % bevington_fiducial % solution ( 2 ) = 9.408333333333333_rp

                me % bevington_fiducial % error ( 1 )    = 4.886206312183354_rp
                me % bevington_fiducial % error ( 2 )    = 0.8683016476563611_rp

        end subroutine load_bevington_results_I_sub

!       ===========================================================================================          load_bevington_results_I

        subroutine load_bevington_results_II_sub ( me )

            class ( comparison ), target :: me

!               user: rditldmt, CPU: dan-topas-pro-2, MM v. 10.2.0 for Mac OS X x86, date: Sep 25, 2015, time: 16:53:25, 
!               nb: /Users/rditldmt/Dropbox/ nb/fortran/projects/least squares/bevington/writer 02.nb

                me % bevington_fiducial % solution ( 1 ) = 4.813888888888889_rp
                me % bevington_fiducial % solution ( 2 ) = 9.408333333333333_rp

                me % bevington_fiducial % error ( 1 )    = 4.886206312183354_rp
                me % bevington_fiducial % error ( 2 )    = 0.8683016476563611_rp

        end subroutine load_bevington_results_II_sub

end module mValidate
