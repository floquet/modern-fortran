! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 12 07

! respect load order
include 'sharedModules/mod precision definitions.f08'
include 'sharedModules/mod parameters.f08'
include 'sharedModules/mod measurements.f08'
include 'sharedModules/mod intermediates definitions.f08'
include 'sharedModules/mod build matrices.f08'
include 'sharedModules/mod compute results.f08'

program bevington

!   order irrelevant
    use mPrecisionDefinitions, only : ip
    use mParameters
    use mMeasurements
    use mIntermediatesDefinitions
    use mBuildMatrices
    use mResults

    implicit none

    integer ( ip ) :: k = 0

    type ( measurements )  :: myMeasurements
    type ( intermediates ) :: myIntermediates
    type ( matrices )      :: myMatrices
    type ( results )       :: myResults

    character ( len = * ), parameter :: myProgram = 'program bevington'  ! self-identification

        call myMeasurements  % load_data                 ( )
!       call myIntermediates % compute_intermediates_dot ( myMeasurements )
!        call myIntermediates % local_selector ( compute_intermediates_sum, myMeasurements )
        call myIntermediates % local_selector ( myMeasurements )
!        call myIntermediates % compute_intermediates_sum ( myMeasurements )
        call myMatrices      % build_matrices            ( myIntermediates )
        call myResults       % compute_results           ( myMatrices, myMeasurements )

        do k = 1, n
            print *, myResults % descriptor ( k ), " = ", &
                     myResults % a ( k ), "+/-", &
                     myResults % epsilon ( k )
        end do

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

end program bevington

! dan-topas-pro-2:bevington rditldmt$ date
! Thu Dec  3 10:10:05 CST 2015
! dan-topas-pro-2:bevington rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/bevington
! dan-topas-pro-2:bevington rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 bevington.f08
! dan-topas-pro-2:bevington rditldmt$ ./a.out
!  intercept =    4.8138888888888971      +/-   4.8862063121833534
!  slope     =    9.4083333333333314      +/-  0.86830164765636075
! STOP successful completion for program bevington.