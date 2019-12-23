! respect load order
include 'sharedModules/mod precision definitions.f08'
include 'sharedModules/mod parameters.f08'
include 'sharedModules/mod measurements.f08'
include 'sharedModules/mod intermediates definitions.f08'
include 'sharedModules/mod build matrices.f08'
include 'sharedModules/mod compute results.f08'

program bevington

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

        call myMeasurements  % load_data             ( )
        call myIntermediates % compute_intermediates ( myMeasurements )
        call myMatrices      % build_matrices        ( myIntermediates )
        call myResults       % compute_results       ( myMatrices, myMeasurements )

        do k = 1, n
            print *, myResults % descriptor ( k ), " = ", &
                     myResults % a ( k ), "+/-", &
                     myResults % epsilon ( k )
        end do

end program bevington

! dan-topas-pro-2:bevington rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 xbevington.f08
! dan-topas-pro-2:bevington rditldmt$ ./a.out
!  intercept =    4.8138888888888971      +/-   4.8862063121833534
!  slope     =    9.4083333333333314      +/-  0.86830164765636075
