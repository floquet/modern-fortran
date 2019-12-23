! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 12 01

include 'myIncludes.f08'

program lsq

    use mQueries
    use mMeasurements
    use mSolnsLinear
    use mValidate

    implicit none

    type ( comparison ) :: myCompare

    real ( rp ) :: cpu_0 = one, cpu_1 = one, t_0 = one, t_1 = one

    character ( len = * ), parameter :: myProgram = 'program lsq'  ! self-identification

        write ( * , '( /, "System identifiers..." )' )
        call qLib_write_system ( )  ! host name, compiler version, compilation options, execution command
        write ( * , '( /, "Command line arguments..." )' )
        call harvest_command_line_arguments ( echo = .true. )  ! command line arguments

        call cpu_time ( cpu_0 )   ! global cpu time - start

            call cpu_time ( t_0 ) ! specific task - start

                call myCompare % validate_bevington_6_1 ( myCompare )

            call cpu_time ( t_1 ) ! specific task - stop

        call cpu_time ( cpu_1 )   ! global cpu time - stop
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', g0, ' = ', E10.3, ' seconds', / )

end program lsq

! dan-topas-pro-2:delivery alpha rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/least squares/delivery alpha
! dan-topas-pro-2:delivery alpha rditldmt$ date
! Tue Dec  1 13:22:53 CST 2015
! dan-topas-pro-2:delivery alpha rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 lsq.f08
! myModules/mod intermediates.f08:133:17:
!
!              if ( me % det == zero ) then
!                  1
! Warning: Equality comparison for REAL(8) at (1)
! dan-topas-pro-2:delivery alpha rditldmt$ ./a.out
!
! System identifiers...
! host system       = dan-topas-pro-2.erdc.dren.mil.
! compiler version  = GCC version 5.1.0.
! compiler options  = -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.9.4 -mtune=core2 -g -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fbacktrace -fmax-errors=5.
! execution command = ./a.out.
!
! Command line arguments...
! 0 command line arguments found
! 0 command line arguments returned:
!
! allocating data array x
!
! Successful allocation of 9 real ( rp ) elements:
! size = 9 elements, sizeof = 72 bytes
! allocating data array y
!
! Successful allocation of 9 real ( rp ) elements:
! size = 9 elements, sizeof = 72 bytes
! allocating data array ones
!
! Successful allocation of 9 real ( rp ) elements:
! size = 9 elements, sizeof = 72 bytes
!  normal_a_sub: fecho =  T echo =  T
!
! Successful allocation of 9 real ( rp ) elements:
! size = 9 elements, sizeof = 72 bytes
!
! allocating matrices...
! allocating A
!
! Successful allocation of 9 x 2 array of real ( rp ) elements:
! size = 18 elements, sizeof = 144 bytes
! allocating As
!
! Successful allocation of 2 x 9 array of real ( rp ) elements:
! size = 18 elements, sizeof = 144 bytes
! allocating AsAinv
!
! Successful allocation of 2 x 2 array of real ( rp ) elements:
! size = 4 elements, sizeof = 32 bytes
!
! constructing matrices...
! time for contructing matrices = 0.540E-04s
!  normal_a_sub: fecho =  T echo =  T
!
! Successful allocation of 9 real ( rp ) elements:
! size = 9 elements, sizeof = 72 bytes
!
! allocating matrices...
! allocating A
!
! Successful allocation of 9 x 2 array of real ( rp ) elements:
! size = 18 elements, sizeof = 144 bytes
! allocating As
!
! Successful allocation of 2 x 9 array of real ( rp ) elements:
! size = 18 elements, sizeof = 144 bytes
! allocating AsAinv
!
! Successful allocation of 2 x 2 array of real ( rp ) elements:
! size = 4 elements, sizeof = 32 bytes
!
! constructing matrices...
! time for contructing matrices = 0.370E-04s
!
! Comparison of results:
! A: Published results
! B: normal equations, summation
!
! Fit parameters:
! A                       B                        Difference               Epsilons
! 4.8138888888888891      .0000000000000000         0.481E+01               0.217E+17
! 9.4083333333333332      .0000000000000000         0.941E+01               0.424E+17
!
! Error parameters:
! A                       B                        Difference               Epsilons
! 4.8862063121833543      .0000000000000000         0.489E+01               0.220E+17
! .86830164765636109      .0000000000000000         0.868E+00               0.391E+16
!
! machine epsilon = .22204460492503131E-015
!  0.000E+00 s: CPU time for A solution
!  0.000E+00 s: CPU time for B solution
!
! CPU time for all tasks =  0.329E-03 seconds
!
! STOP successful completion for program lsq.
! dan-topas-pro-2:delivery alpha rditldmt$