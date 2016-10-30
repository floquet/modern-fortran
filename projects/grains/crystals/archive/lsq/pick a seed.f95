! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'myModules/myIncludes.f95'

program picker

    use iso_fortran_env
    use precision_definitions, only : is, wp, zero
    use mQueries

    implicit none

    type ( simulations )             :: mySimulation

    real    ( wp )                   :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero
 
    integer ( is )                   :: numArgsInput
    integer ( is )                   :: c_arg_list ( 1 : numArgsMax )

    character ( len = * ), parameter :: myProgram = 'program picker'  ! self-identification

    call ping_system ( )
    call harvest_command_line_arguments ( c_arg_list, numArgsInput, .true. )

        call cpu_time ( cpu_0 ) ! global cpu time - start

!             call cpu_time ( t_0 ) ! specific task - start
!             call cpu_time ( t_1 ) ! specific task - stop
!             write ( *, 100 ) 'testing simulation', t_1 - t_0
 
        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop "successful completion for " // myProgram // "."  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', g0, ' = ', g0, ' seconds', / )

end program picker

! dan-topas-pro-2:indigo rditldmt$ date
! Thu Sep 17 12:14:21 CDT 2015
! dan-topas-pro-2:indigo rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/transport/indigo
! dan-topas-pro-2:indigo rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 transporter.f95
! myModules/mod simulations.f95:143:42:
!
!          subroutine read_name_list_sub ( me )
!                                           1
! Warning: Unused dummy argument ‘me’ at (1) [-Wunused-dummy-argument]
! dan-topas-pro-2:indigo rditldmt$ ./a.out 10
!
! host system       = dan-topas-pro-2.erdc.dren.mil
! compiler version  = GCC version 5.1.0
! compiler options  = -fPIC -mmacosx-version-min=10.9.4 -mtune=core2 -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5
! execution command = ./a.out 10
!
! 1 command line arguments found:
!         10
!
! Capsule      1 has 100 photons (0.318E+00 photons per unit area).
! Radial zone  1 has 1 photon  (0.318E+00 photons per unit area).
! Radial zone  2 has 3 photons (0.318E+00 photons per unit area).
! Radial zone  3 has 5 photons (0.318E+00 photons per unit area).
! Radial zone  4 has 9 photons (0.409E+00 photons per unit area).
! Radial zone  5 has 14 photons (0.495E+00 photons per unit area).
! Radial zone  6 has 9 photons (0.260E+00 photons per unit area).
! Radial zone  7 has 13 photons (0.318E+00 photons per unit area).
! Radial zone  8 has 14 photons (0.297E+00 photons per unit area).
! Radial zone  9 has 16 photons (0.300E+00 photons per unit area).
! Radial zone 10 has 16 photons (0.268E+00 photons per unit area).
! Total photons in all zones = 100; discrepancy = 0.
!
! CPU time for testing simulation = .28500000000000010E-003 seconds
!
!
! CPU time for all tasks = .31999999999999997E-003 seconds
!
! STOP successful completion for program transporter.
