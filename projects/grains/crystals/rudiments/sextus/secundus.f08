! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! respect load order
! include '../../sharedModules/mod precision definitions.f08'
!
! include 'myModules/mod parameters simulation.f08'
! include 'myModules/mod format descriptors.f08'
! include 'myModules/mod shared.f08'
! include 'myModules/mod matrix writer.f08'
! include 'myModules/mod read LAMMPS.f08'
! include 'myModules/mod axes.f08'
! include 'myModules/mod mathematica results.f08'
! include 'myModules/mod grain.f08'

program secundus

!   order irrelevant
    use iso_fortran_env
    use mPrecisionDefinitions
    use mShared
    use mReadLAMMPS
    use mAxes
    use mValidateLines
    use mValidateApex
    use mGrain

    implicit none

    integer ( ip )              :: k = 0
    !
    type ( LAMMPS_data )        :: myMeasurements
    type ( axis )               :: axisI, axisII, axisIII
    type ( comparison_lines )   :: setI,  setII,  setIII
    type ( comparison_apex )    :: apex_angles
    type ( grain )              :: myGrain

    character ( len = * ), parameter :: myProgram = 'program secundus'  ! self-identification
    character ( len = * ), parameter :: c_version = compiler_version ( )
    character ( len = * ), parameter :: c_options = compiler_options ( )

        ! read LAMMPS data
        call myMeasurements % read_data ( )

        ! analyze three axes for a grain
        call axisI   % least_squares_solution ( 'axis I',   'primus/',   myMeasurements )
        call axisII  % least_squares_solution ( 'axis II',  'secundus/', myMeasurements )
        call axisIII % least_squares_solution ( 'axis III', 'tertius/',  myMeasurements )

        ! measure angles
        call myGrain % compute_apex_angles ( axisI, axisII, axisIII )
        ! compare apex to Mathematica
        do k = 1, 3
            call apex_angles % validate_apex ( myGrain % apex_angle ( k ), k )
        end do
        !call myGrain % compare_mm_apex ( 2 )
        !call myGrain % compare_mm_apex ( 3 )

        ! compare grain axes to Mathematica
        call setI   % validate_lines ( axisI   % results, 1 )
        call setII  % validate_lines ( axisII  % results, 2 )
        call setIII % validate_lines ( axisIII % results, 3 )

        write ( *, '( /, "compiler version  = ", g0, "." )' ) c_version
        write ( *, '( "compiler options  = ", g0, ".", / )' ) c_options

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

end program secundus

!  17:04 dan-topas-pro-2 rditldmt $ date
! Tue Feb  2 17:04:30 CST 2016
!  17:04 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/projects/crystals/rudiments/sextus
!  17:04 dan-topas-pro-2 rditldmt $ scons
! scons: Reading SConscript files ...
! scons: done reading SConscript files.
! scons: Building targets ...
! scons: `.' is up to date.
! scons: done building targets.
!  17:04 dan-topas-pro-2 rditldmt $ aa
!
! Array of 1024 elements read in from ../../data/input/apple/xy_data.txt.
!
! Array of 1024 elements read in from ../../data/input/apple/potentials.txt.
!
! Apex angles (ideal answer = 60 deg):
!   1     1.04032 +/-    0.01801     (  59.60592 +/-    1.03174 ) degrees
!   2     1.03450 +/-    0.00293     (  59.27242 +/-    0.16798 ) degrees
!   3     1.04138 +/-    0.01812     (  59.66650 +/-    1.03828 ) degrees
!
! compiler version  = GCC version 5.1.0.
! compiler options  = -fPIC -mmacosx-version-min=10.9.4 -mtune=core2.
!
! STOP successful completion for program secundus.
