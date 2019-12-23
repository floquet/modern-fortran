module mResults

    use mBuildMatrices
    use mParameters
    use mMeasurements

    implicit none

    integer ( ip ) :: row = 0

    type :: results
        real ( rp ), dimension ( 1 : n )           :: a          = zero
        real ( rp ), dimension ( 1 : n )           :: epsilon    = zero
        real ( rp )                                :: r2         = zero
        character ( len = 9 ), dimension ( 1 : n ) :: descriptor = [ 'intercept', &
                                                                     'slope    ' ]
    contains
        private
        procedure, public :: compute_results
    end type results

contains

    subroutine compute_results ( me, myMatrices, myMeasurements )

        class ( results ), target               :: me

        type ( matrices ),     intent ( in )    :: myMatrices
        type ( measurements ), intent ( inout ) :: myMeasurements

!           fit parameters
            me % a = matmul ( myMatrices % ASAinv, myMatrices % B )
            me % a = matmul ( myMatrices % ASAinv, myMatrices % B )

!           errors
            myMeasurements % residuals = myMeasurements % y - me % a ( 1 ) &
                                       - myMeasurements % x * me % a ( 2 )
            me % r2 = dot_product ( myMeasurements % residuals, &
                                    myMeasurements % residuals )
            me % epsilon = [ ( myMatrices % ASAinv ( row, row ), row = 1, n ) ]
            me % epsilon = sqrt ( me % epsilon * me % r2 / ( m - n ) )

    end subroutine compute_results

end module mResults