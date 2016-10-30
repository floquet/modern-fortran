! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 25

include 'myIncludes.f08'

program lsq

    use mPrecisionDefinitions, only : rp, one
    use mQueries
    use mValidate
    use mData

    implicit none
    
    type ( comparison ) :: bevington_data

    real ( rp ) :: cpu_0 = one, cpu_1 = one, t_0 = one, t_1 = one

    character ( len = * ), parameter :: myProgram = 'program lsq'  ! self-identification

        call qLib_write_system ( )  ! host name, compiler version, compilation options, execution command
        call harvest_command_line_arguments ( echo = .true. )  ! command line arguments

        call cpu_time ( cpu_0 )   ! global cpu time - start

            call cpu_time ( t_0 ) ! specific task - start

                call bevington_data % validate_bevington_I ()

            call cpu_time ( t_1 ) ! specific task - stop

        call cpu_time ( cpu_1 )   ! global cpu time - stop
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', g0, ' = ', E10.3, ' seconds', / )

end program lsq

! Muntz-Szasz:charlie bevington dantopa$ date
! Sat Sep 26 23:13:47 CDT 2015
! Muntz-Szasz:charlie bevington dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/projects/least squares/charlie bevington
! Muntz-Szasz:charlie bevington dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 lsq.f08
! myModules/mod solns linear.f08:141:21:
!
!                  if ( ints % det == zero ) then
!                      1
! Warning: Equality comparison for REAL(8) at (1)
! Muntz-Szasz:charlie bevington dantopa$ ./a.out
! host system       = Muntz-Szasz.
! compiler version  = GCC version 5.1.0.
! compiler options  = -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.10.5 -mtune=core2 -g -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fbacktrace -fmax-errors=5.
! execution command = ./a.out.
! 0 command line arguments found:
!
! allocating matrices
!
! Successful allocation of 9 x 2 array of  real ( rp ) elements:
! size = 18 elements, sizeof = 144 bytes
!
! Successful allocation of 2 x 9 array of  real ( rp ) elements:
! size = 18 elements, sizeof = 144 bytes
!
! Successful allocation of 9 x 2 array of  real ( rp ) elements:
! size = 18 elements, sizeof = 144 bytes
!
! Successful allocation of 9 elements:
!
! Successful allocation of  real ( rp ) elements:
! size = 9 elements, sizeof = 72 bytes
!  em  =    9.0000000000000000
!  det =    540.00000000000000
!  sX  =    45.000000000000000
!  sX2 =    285.00000000000000
!  matrix % ASAinv =   0.52777777777777779       -8.3333333333333329E-002  -8.3333333333333329E-002   1.6666666666666666E-002
!  me % error =    4.8862063121833534       0.86830164765636075
!
! Fit parameters:
! Fiducial                Computed                 Difference               Epsilons
! 4.8138888888888891      4.8138888888888971       -0.799E-14               0.360E+02
! 9.4083333333333332      9.4083333333333314        0.178E-14               0.800E+01
!
! Error parameters:
! Fiducial                Computed                 Difference               Epsilons
! 4.8862063121833543      4.8862063121833534        0.888E-15               0.400E+01
! .86830164765636109      .86830164765636075        0.333E-15               0.150E+01
!
! machine epsilon = .22204460492503131E-015
!
! CPU time for normal equations solution =  0.830E-04 s.
!
! CPU time for all tasks =  0.141E-03 seconds
!
! STOP successful completion for program lsq.