module mResults

    use mBuildMatrices
    use mParameters

    implicit none


    type :: results
        real ( rp ), dimension ( 1 : n ) :: a = 0
    contains
        private
        procedure, public :: compute_results
    end type results

contains

    subroutine compute_results ( me, myMatrices )

        class ( results ), target        :: me

        type ( matrices ), intent ( in ) :: myMatrices

            me % a = matmul ( myMatrices % ASAinv, myMatrices % B )

    end subroutine compute_results

end module mResults