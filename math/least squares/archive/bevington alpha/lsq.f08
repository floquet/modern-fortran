! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 25

include 'myIncludes.f08'

program lsq

    use mPrecisionDefinitions, only : rp, one
    use mQueries
    use mValidate
    use mData

    implicit none

    real ( rp ) :: cpu_0 = one, cpu_1 = one, t_0 = one, t_1 = one

    character ( len = * ), parameter :: myProgram = 'program lsq'  ! self-identification

        call qLib_write_system ( )  ! host name, compiler version, compilation options, execution command
        call harvest_command_line_arguments ( echo = .true. )  ! command line arguments

        call cpu_time ( cpu_0 )   ! global cpu time - start

            call cpu_time ( t_0 ) ! specific task - start

                call validate_bevington_sub ()

            call cpu_time ( t_1 ) ! specific task - stop

        call cpu_time ( cpu_1 )   ! global cpu time - stop
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', g0, ' = ', g0, ' seconds', / )

end program lsq

! dan-topas-pro-2:bevington rditldmt$ date
! Thu Sep 24 17:37:45 CDT 2015
! dan-topas-pro-2:bevington rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/least squares/bevington
! dan-topas-pro-2:bevington rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 lsq.f08
! lsq.f08:9:8:
!
!      use mPrecisionDefinitions, only : ip, rp, one
!         1
! Warning: Unused parameter ‘ip’ which has been explicitly imported at (1) [-Wunused-parameter]
! dan-topas-pro-2:bevington rditldmt$ ./a.out 10
! host system       = dan-topas-pro-2.erdc.dren.mil.
! compiler version  = GCC version 5.1.0.
! compiler options  = -fPIC -mmacosx-version-min=10.9.4 -mtune=core2 -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5.
! execution command = ./a.out 10.
! 1 command line arguments found:
!         10
!
! CPU time for all tasks = .40000000000000972E-005 seconds
!
! STOP successful completion for program lsq.
