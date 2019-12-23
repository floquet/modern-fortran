module mBuildMatrices

    use mIntermediatesDefinitions
    use mParameters

    implicit none

    type :: matrices
        real ( rp ), dimension ( 1 : n, 1 : n ) :: ASAinv = zero
        real ( rp ), dimension ( 1 : n )        :: B      = zero
        real ( rp )                             :: det    = zero
    contains
        private
        procedure, public :: build_matrices
    end type matrices

contains

    subroutine build_matrices ( me, myIntermediates )

        class ( matrices ), target      :: me

        type ( intermediates ), intent ( in ) :: myIntermediates

            me % det = myIntermediates % em * myIntermediates % sx2 - myIntermediates % sx ** 2
            me % ASAinv ( : , 1 ) = [ myIntermediates % sx2, -myIntermediates % sx ]
            me % ASAinv ( : , 2 ) = [-myIntermediates % sx,   myIntermediates % em ]
            me % ASAinv = me % ASAinv / me % det
            me % B = [ myIntermediates % sy, myIntermediates % sxy ]

    end subroutine build_matrices

end module mBuildMatrices