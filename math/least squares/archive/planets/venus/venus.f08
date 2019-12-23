! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 12 02

include 'sharedModules/mod precision definitions.f08'
include 'sharedModules/mod parameters.f08'
include 'sharedModules/mod intermediates definitions.f08'
include 'sharedModules/mod data structures.f08'
include 'sharedModules/mod build matrices.f08'
include 'sharedModules/mod compute results.f08'

program venus

    use mPrecisionDefinitions
    use mParameters
    use mIntermediatesDefinitions
    use mDataStructures
    use mBuildMatrices
    use mResults

    implicit none

    type ( measurements )  :: myMeasurements
    type ( intermediates ) :: myIntermediates
    type ( matrices )      :: myMatrices
    type ( results )       :: myResults

    character ( len = * ), parameter :: myProgram = 'program venus'  ! self-identification

!       load data
        call myMeasurements % load_data ( )  ! data available as myMeasurements % x ( 1 ), ...

!       compute intermediates
        call myIntermediates % compute_intermediates ( myMeasurements )

!       compute matrices
        call myMatrices % build_matrices ( myIntermediates )

!       compute results
        call myResults % compute_results ( myMatrices )

        print *, "intercept = ", myResults % a ( 1 )
        print *, "slope     = ", myResults % a ( 2 )

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

end program venus

! dan-topas-pro-2:venus rditldmt$ date
! Wed Dec  2 10:04:40 CST 2015
! dan-topas-pro-2:venus rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/least squares/planets/venus
! dan-topas-pro-2:venus rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 venus.f08
! dan-topas-pro-2:venus rditldmt$ ./a.out
!  intercept =    4.8138878080580128
!  slope     =    9.4083335240681976
! STOP successful completion for program venus.