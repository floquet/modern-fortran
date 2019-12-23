! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program apex

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options
    use mPrecisionDefinitions,          only : ip
    use mAxes,                          only : axis
    use mGrain,                         only : grain
    use mReadLAMMPS,                    only : LAMMPS_data
    use mFileHandling,                  only : safeopen_writereplace
    use mParametersSimulation,          only : file_out_lines, file_out_angles
    use mValidateLines,                 only : comparison_lines
    use mValidateApex,                  only : comparison_apex

    implicit none

    integer ( ip )              :: k = 0, io_out_lines = 0, io_out_angles = 0

    type ( LAMMPS_data )        :: myMeasurements
    type ( axis )               :: axisI, axisII, axisIII
    type ( comparison_lines )   :: setI,  setII,  setIII
    type ( comparison_apex )    :: apex_angles
    type ( grain )              :: myGrain

    character ( len = * ), parameter :: myProgram = 'program apex'  ! self-identification
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
        io_out_angles = safeopen_writereplace ( filename = file_out_angles )
            do k = 1, 3
                call apex_angles % validate_apex ( myGrain % apex_angle ( k ), k, io_out_angles )
            end do
        close ( io_out_angles )

        ! compare grain axes to Mathematica
        io_out_lines = safeopen_writereplace ( filename = file_out_lines )
            call setI   % validate_lines ( axisI   % results, 1, io_out_lines )
            call setII  % validate_lines ( axisII  % results, 2, io_out_lines )
            call setIII % validate_lines ( axisIII % results, 3, io_out_lines )
        close ( io_out_lines )

        write ( *, '( /, "compiler version  = ", g0, "." )'    ) c_version
        write ( *, '(    "compiler options  = ", g0, ".", / )' ) c_options

        write ( *, '( /, "Validation for angles written to ", g0 )'    ) file_out_angles
        write ( *, '(    "Validation for lines  written to ", g0, / )' ) file_out_lines

        stop 'successful completion for ' // myProgram // '...'  ! string must reduce to constant expression

end program apex

! dantopa@topaz06.erdc.hpc.mil:class $ ./apex 
!
! Apex angles (ideal answer = pi/3 ≈ 1.04719755 or 60 deg):
!   1     1.04032 +/-    0.01801     (  59.60592 +/-    1.03174 ) degrees
!   2     1.03450 +/-    0.00293     (  59.27242 +/-    0.16798 ) degrees
!   3     1.04138 +/-    0.01812     (  59.66650 +/-    1.03828 ) degrees
!
! Total of all angles (ideal answer = pi or 180 deg ):
!    3.11620 +/-    0.02571     ( 178.54484 +/-    1.47334 ) degrees
!
! compiler version  = GCC version 6.1.0.
! compiler options  = -mtune=generic -march=x86-64 -auxbase-strip apex.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Wpedantic -fbacktrace -fcheck=bounds -fmax-errors=5.
!
!
! Validation for angles written to data/validation apex.txt
! Validation for lines  written to data/validation lines.txt
!
! STOP successful completion for program apex...

