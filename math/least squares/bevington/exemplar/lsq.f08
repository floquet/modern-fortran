! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 02 13
program lsq

    use mPrecisionDefinitions,  only : rp
    use mParameters,            only : one
    use mQueries,               only : qLib_write_system, harvest_command_line_arguments
    use mValidate,              only : comparison

    implicit none

    type ( comparison ) :: myCompare

    real ( rp ) :: cpu_0 = one, cpu_1 = one, t_0 = one, t_1 = one

    character ( len = * ), parameter :: myProgram = 'program lsq'  ! self-identification

        write ( * , '( /, "System identifiers..." )' )
        call qLib_write_system ( )  ! host name, compiler version, compilation options, execution command
        write ( * , '( /, "Command line arguments..." )' )
        call harvest_command_line_arguments ( echo = .false. )  ! echo is a diagnostic print

        call cpu_time ( cpu_0 )   ! global cpu time - start

            call cpu_time ( t_0 ) ! specific task - start

                call myCompare % validate_bevington_6_1 ( myCompare )

            call cpu_time ( t_1 ) ! specific task - stop

        call cpu_time ( cpu_1 )   ! global cpu time - stop
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop 'successful completion for ' // myProgram // '...'  ! string must reduce to constant expression

    100 format ( /, 'CPU time for ', g0, ' = ', E10.3, ' seconds', / )

end program lsq

! guest@rouson-VirtualBox:~/github/fortran/projects/lightning/vm$ date
! Tue Mar  8 11:26:00 PST 2016
! guest@rouson-VirtualBox:~/github/fortran/projects/lightning/vm$ pwd
! /home/guest/github/fortran/projects/lightning/vm
! guest@rouson-VirtualBox:~/github/fortran/projects/lightning/vm$ make clean
! rm -rf lsq.o mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o mod_precision_definitions.o mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o lsq mod_allocators.mod mod_intermediates.mod mod_matrices.mod mod_measurements.mod mod_parameters.mod mod_precision_definitions.mod mod_queries.mod mod_results.mod mod_solns_linear.mod mod_sub_validate_loaders.mod mod_validate.mod
! rm -f *.mod
! guest@rouson-VirtualBox:~/github/fortran/projects/lightning/vm$ make debug
! SRCS = lsq.f08 mod_allocators.f08 mod_intermediates.f08 mod_matrices.f08 mod_measurements.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_queries.f08 mod_results.f08 mod_solns_linear.f08 mod_sub_validate_loaders.f08 mod_validate.f08
! OBJS = lsq.o mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o mod_precision_definitions.o mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o
! MODS = mod_allocators.f08 mod_intermediates.f08 mod_matrices.f08 mod_measurements.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_queries.f08 mod_results.f08 mod_solns_linear.f08 mod_sub_validate_loaders.f08 mod_validate.f08
! MOD_OBJS = mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o mod_precision_definitions.o ! mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o
! PROGRAM  = lsq
! PRG_OBJ  = lsq.o
! guest@rouson-VirtualBox:~/github/fortran/projects/lightning/vm$ make
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_allocators.o mod_allocators.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_measurements.o mod_measurements.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_intermediates.o mod_intermediates.f08
! mod_intermediates.f08:101:17:
! 
!              if ( me % det == zero ) then
!                  1
! 
! Warning: Equality comparison for REAL(8) at (1)
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_matrices.o mod_matrices.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_queries.o mod_queries.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_solns_linear.o mod_solns_linear.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_results.o mod_results.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_validate.o mod_validate.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_sub_validate_loaders.o mod_sub_validate_loaders.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o lsq.o lsq.f08
! gfortran -g -o lsq lsq.o mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o ! mod_precision_definitions.o mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o
! guest@rouson-VirtualBox:~/github/fortran/projects/lightning/vm$ ./lsq
! 
! System identifiers...
! host system       = rouson-VirtualBox.
! compiler version  = GCC version 6.0.0 20151214 (experimental).
! compiler options  = -mtune=generic -march=x86-64 -auxbase-strip mod_queries.o -g -Og -Wall -Wextra -Wconversion -Wpedantic -! fcheck=bounds -fmax-errors=5.
! execution command = ./lsq.
! 
! Command line arguments...
! 
! Comparison of results:
! A: Mathematica results
! B: Computation via normal equations, dot product
! 
! Fit parameters:
!               A                        B                        Difference               Epsilons
! intercept     4.8138888888888891       4.8138888888888971       -0.799E-14               0.360E+02
! slope         9.4083333333333332       9.4083333333333314        0.178E-14               0.800E+01
! 
! Error parameters:
!               A                        B                        Difference               Epsilons
! intercept     4.8862063121833543       4.8862063121833534        0.888E-15               0.400E+01
! slope         0.86830164765636109      0.86830164765636075       0.333E-15               0.150E+01
! 
! machine epsilon = 0.22204460492503131E-015
!  0.000E+00 s: CPU time for A solution
!  0.000E+00 s: CPU time for B solution
! 
! CPU time for all tasks =  0.000E+00 seconds
! 
! STOP successful completion for program lsq...
