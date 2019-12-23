include 'sharedModules/mod precision definitions.f08'
include 'myModules/mod measurements.f08'
include 'myModules/mod intermediates.f08'
include 'myModules/mod matrices.f08'
include 'myModules/mod solns linear.f08'
include 'myModules/mod results.f08'
include 'myModules/mod validate.f08'


program user

    use mQueries
    use mMeasurements
    use mSolnsLinear
    use mValidate

    implicit none

    type ( comparison ) :: myCompare

        call myCompare % validate_bevington_6_1 ( myCompare )

end program user


! Muntz-Szasz:foxtrot bevington dantopa$ date
! Sat Oct  3 19:19:10 CDT 2015
! Muntz-Szasz:foxtrot bevington dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/projects/least squares/foxtrot bevington
! Muntz-Szasz:foxtrot bevington dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 wtf.f08
! myModules/mod intermediates.f08:131:17:
!
!              if ( me % det == zero ) then
!                  1
! Warning: Equality comparison for REAL(8) at (1)
! Muntz-Szasz:foxtrot bevington dantopa$ ./a.out
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
! time for contructing matrices = 0.350E-04s
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
! time for contructing matrices = 0.210E-04s
!
! Comparison of results:
! A: Published results
! B: normal equations, summation
!
! Fit parameters:
! A                       B                        Difference               Epsilons
! 4.8138888888888891      4.8138888888888971       -0.799E-14               0.360E+02
! 9.4083333333333332      9.4083333333333314        0.178E-14               0.800E+01
!
! Error parameters:
! A                       B                        Difference               Epsilons
! 4.8862063121833543      4.8862063121833534        0.888E-15               0.400E+01
! .86830164765636109      .86830164765636075        0.333E-15               0.150E+01
!
! machine epsilon = .22204460492503131E-015
!  0.000E+00 s: CPU time for A solution
!  0.700E-05 s: CPU time for B solution
