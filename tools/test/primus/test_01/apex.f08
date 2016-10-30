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

! guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ date
! Fri Apr  1 11:42:22 PDT 2016
! guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ pwd
! /home/guest/github/fortran/projects/crystals/apex_angles/delta
! guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ make clean
! rm -rf apex.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_aandp.o mod_axes_sub_IO.o mod_constants.o mod_filehandling.o mod_grain.o mod_LSQ.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o apex mod_allocators.mod mod_angle.mod mod_axes.mod mod_axes_sub_aandp.mod mod_axes_sub_IO.mod mod_constants.mod mod_filehandling.mod mod_grain.mod mod_LSQ.mod mod_mathematica_output.mod mod_matrix_writer.mod mod_parameters_simulation.mod mod_precision_definitions.mod mod_read_LAMMPS.mod mod_validate_apex.mod mod_validate_lines.mod
! rm -f *.mod *.smod *.o
! guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ make
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_allocators.o mod_allocators.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_angle.o mod_angle.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_filehandling.o mod_filehandling.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_LSQ.o mod_LSQ.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_matrix_writer.o mod_matrix_writer.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters_simulation.o mod_parameters_simulation.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_read_LAMMPS.o mod_read_LAMMPS.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_axes.o mod_axes.f08
! mod_axes.f08:21:36:
! 
!      integer ( ip ), private :: first, last
!                                     1
! 
! Warning: Unused PRIVATE module variable ‘first’ declared at (1) [-Wunused-value]
! mod_axes.f08:21:42:
! 
!      integer ( ip ), private :: first, last
!                                           1
! 
! Warning: Unused PRIVATE module variable ‘last’ declared at (1) [-Wunused-value]
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_axes_sub_aandp.o mod_axes_sub_aandp.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_axes_sub_IO.o mod_axes_sub_IO.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_grain.o mod_grain.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_mathematica_output.o mod_mathematica_output.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_validate_apex.o mod_validate_apex.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_validate_lines.o mod_validate_lines.f0
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o apex.o apex.f08
! gfortran -g -o apex apex.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_aandp.o mod_axes_sub_IO.o mod_constants.o mod_filehandling.o mod_grain.o mod_LSQ.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
! guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ ./apex
! 
! Apex angles (ideal answer = pi/3 ≈ 1.04719755 or 60 deg):
!   1     1.04032 +/-    0.01801     (  59.60592 +/-    1.03174 ) degrees
!   2     1.03450 +/-    0.00293     (  59.27242 +/-    0.16798 ) degrees
!   3     1.04138 +/-    0.01812     (  59.66650 +/-    1.03828 ) degrees
! 
! Total of all angles (ideal answer = pi or 180 deg ):
!    3.11620 +/-    0.02571     ( 178.54484 +/-    1.47334 ) degrees
! 
! compiler version  = GCC version 6.0.0 20151214 (experimental).
! compiler options  = -mtune=generic -march=x86-64 -auxbase-strip apex.o -g -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5.
! 
! STOP successful completion for program apex...
! guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ make debug
! PROGRAM  = apex
! PRG_OBJ  = apex.o
! SRCS     = apex.f08 mod_allocators.f08 mod_angle.f08 mod_axes.f08 mod_axes_sub_aandp.f08 mod_axes_sub_IO.f08 mod_constants.f08 mod_filehandling.f08 mod_grain.f08 mod_LSQ.f08 mod_mathematica_output.f08 mod_matrix_writer.f08 mod_parameters_simulation.f08 mod_precision_definitions.f08 mod_read_LAMMPS.f08 mod_validate_apex.f08 mod_validate_lines.f08
! OBJS     = apex.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_aandp.o mod_axes_sub_IO.o mod_constants.o mod_filehandling.o mod_grain.o mod_LSQ.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
! MODS     = mod_allocators.f08 mod_angle.f08 mod_axes.f08 mod_axes_sub_aandp.f08 mod_axes_sub_IO.f08 mod_constants.f08 mod_filehandling.f08 mod_grain.f08 mod_LSQ.f08 mod_mathematica_output.f08 mod_matrix_writer.f08 mod_parameters_simulation.f08 mod_precision_definitions.f08 mod_read_LAMMPS.f08 mod_validate_apex.f08 mod_validate_lines.f08
! MOD_OBJS = mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_aandp.o mod_axes_sub_IO.o mod_constants.o mod_filehandling.o mod_grain.o mod_LSQ.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
