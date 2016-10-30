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

! rditldmt@ITL-DTOPA-MP:foxtrot $ date
! Fri Aug 19 11:07:18 CDT 2016
! rditldmt@ITL-DTOPA-MP:foxtrot $ pwd
! /Users/rditldmt/hpc/fortran/projects/grains/crystals/apex_angles/foxtrot
! rditldmt@ITL-DTOPA-MP:foxtrot $ make
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_precision_definitions.o mod_precision_definitions.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_constants.o mod_constants.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_LSQ.o mod_LSQ.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_allocators.o mod_allocators.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_angle.o mod_angle.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_filehandling.o mod_filehandling.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_matrix_writer.o mod_matrix_writer.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_parameters_simulation.o mod_parameters_simulation.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_read_LAMMPS.o mod_read_LAMMPS.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_axes.o mod_axes.f08
! mod_axes.f08:21:36:
!
!      integer ( ip ), private :: first, last
!                                     1
! Warning: Unused PRIVATE module variable 'first' declared at (1) [-Wunused-value]
! mod_axes.f08:21:42:
!
!      integer ( ip ), private :: first, last
!                                           1
! Warning: Unused PRIVATE module variable 'last' declared at (1) [-Wunused-value]
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_axes_sub_IO.o mod_axes_sub_IO.f08
! mod_axes_sub_IO.f08:2:17:
!
!  submodule ( mAxes ) smAxesIO
!                  1
! Warning: USE statement at (1) has no ONLY qualifier [-Wuse-without-only]
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_axes_sub_aandp.o mod_axes_sub_aandp.f08
! mod_axes_sub_aandp.f08:2:17:
!
!  submodule ( mAxes ) smAllocPop
!                  1
! Warning: USE statement at (1) has no ONLY qualifier [-Wuse-without-only]
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_grain.o mod_grain.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_mathematica_output.o mod_mathematica_output.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_validate_apex.o mod_validate_apex.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_validate_lines.o mod_validate_lines.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o apex.o apex.f08
! gfortran -g -o apex apex.o mod_LSQ.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_IO.o mod_axes_sub_aandp.o mod_constants.o mod_filehandling.o mod_grain.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
! rditldmt@ITL-DTOPA-MP:foxtrot $ ./apex
!
! Apex angles (ideal answer = pi/3 ≈ 1.04719755 or 60 deg):
!   1     1.04032 +/-    0.01801     (  59.60592 +/-    1.03174 ) degrees
!   2     1.03450 +/-    0.00293     (  59.27242 +/-    0.16798 ) degrees
!   3     1.04138 +/-    0.01812     (  59.66650 +/-    1.03828 ) degrees
!
! Total of all angles (ideal answer = pi or 180 deg ):
!    3.11620 +/-    0.02571     ( 178.54484 +/-    1.47334 ) degrees
!
! compiler version  = GCC version 7.0.0 20160731 (experimental).
! compiler options  = -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -mtune=core2 -auxbase-strip apex.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -Wuse-without-only -ffpe-trap=denormal -fbacktrace -fcheck=bounds -fmax-errors=5.
!
!
! Validation for angles written to data/validation apex.txt
! Validation for lines  written to data/validation lines.txt
!
! STOP successful completion for program apex...
! rditldmt@ITL-DTOPA-MP:foxtrot $ cat data/validation\ lines.txt
!
! Validation exercise for lines, case 1
! intercept,  Fortran           0.989893911107748E+00 +/-    0.320024788581389E-02
! intercept,  Mathematica       0.989893911107748E+00 +/-    0.320024788581390E-02
! Difference                    0.222E-15                   -0.260E-17
! Difference, epsilons                  1.0                        0.1E-01
!
! gap,        Fortran           0.343772868880165E+01 +/-    0.130228854930303E-01
! gap,        Mathematica       0.343772868880165E+01 +/-    0.130228854930303E-01
! Difference                   -0.533E-14                   -0.867E-17
! Difference, epsilons                 24.0                        0.4E-01
!
! slope,      Fortran           0.337614377841187E+00 +/-    0.165405427695299E-02
! slope,      Mathematica       0.337614377841188E+00 +/-    0.165405427695300E-02
! Difference                   -0.444E-15                   -0.152E-17
! Difference, epsilons                  2.0                        0.7E-02
!
!  2016-08-19 11:07:31
!
!
! Validation exercise for lines, case 2
! intercept,  Fortran           0.497428148499566E+01 +/-    0.524305283157246E-01
! intercept,  Mathematica       0.497428148499463E+01 +/-    0.524305283157190E-01
! Difference                    0.102E-11                    0.558E-14
! Difference, epsilons               4608.0                        0.3E+02
!
! gap,        Fortran          -0.207514689460427E+01 +/-    0.920777913646986E-01
! gap,        Mathematica      -0.207514689460362E+01 +/-    0.920777913646888E-01
! Difference                   -0.654E-12                    0.985E-14
! Difference, epsilons               2944.0                        0.4E+02
!
! slope,      Fortran           0.516839016591334E+01 +/-    0.521508071691429E-01
! slope,      Mathematica       0.516839016591234E+01 +/-    0.521508071691373E-01
! Difference                    0.995E-12                    0.554E-14
! Difference, epsilons               4480.0                        0.2E+02
!
!  2016-08-19 11:07:31
!
!
! Validation exercise for lines, case 3
! intercept,  Fortran           0.123218059280431E+01 +/-    0.388468384065434E-02
! intercept,  Mathematica       0.123218059280430E+01 +/-    0.388468384065434E-02
! Difference                    0.533E-14                   -0.304E-17
! Difference, epsilons                 24.0                        0.1E-01
!
! gap,        Fortran          -0.750532255668239E+01 +/-    0.434544396699708E-01
! gap,        Mathematica      -0.750532255668240E+01 +/-    0.434544396699708E-01
! Difference                    0.142E-13                   -0.208E-16
! Difference, epsilons                 64.0                        0.9E-01
!
! slope,      Fortran          -0.857618378765638E+00 +/-    0.378896981719881E-02
! slope,      Mathematica      -0.857618378765642E+00 +/-    0.378896981719881E-02
! Difference                    0.355E-14                   -0.217E-17
! Difference, epsilons                 16.0                        0.1E-01
!
!  2016-08-19 11:07:31
!
! rditldmt@ITL-DTOPA-MP:foxtrot $ cat data/validation\ apex.txt
!
! Validation exercise for apex angles, case 1
! apex angle, Fortran           0.104031955595417E+01 +/-    0.180073076816964E-01
! apex angle, Mathematica       0.104031955595421E+01 +/-    0.180073076816950E-01
! Difference                   -0.426E-13                    0.138E-14
! Difference, epsilons                192.0                        0.6E+01
!
!  2016-08-19 11:07:31
!
!
! Validation exercise for apex angles, case 2
! apex angle, Fortran           0.103449886538241E+01 +/-    0.293180909456640E-02
! apex angle, Mathematica       0.103449886538241E+01 +/-    0.293180909456640E-02
! Difference                    0.444E-15                    0.217E-17
! Difference, epsilons                  2.0                        0.1E-01
!
!  2016-08-19 11:07:31
!
!
! Validation exercise for apex angles, case 3
! apex angle, Fortran           0.104137686062484E+01 +/-    0.181214113556190E-01
! apex angle, Mathematica       0.104137686062479E+01 +/-    0.181214113556176E-01
! Difference                    0.431E-13                    0.137E-14
! Difference, epsilons                194.0                        0.6E+01
!
!  2016-08-19 11:07:31
!
! rditldmt@ITL-DTOPA-MP:foxtrot $
