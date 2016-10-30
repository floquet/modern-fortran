! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 03 08

!                                                                                     V  you are here  V

! data vectors ( 1, x, y ) -> intermediate sums -> matrices ( A, AT, ATAinv ) -> solution ( slope, intercept )

module mSolnsLinear

    use mPrecisionDefinitions,  only : ip, rp
    use mParameters,            only : one, zero

    use mAllocations,           only : allocator_rank_1_sub
    use mIntermediates,         only : intermediates
    use mMatrices,              only : matrices
    use mMeasurements,          only : measurements

    implicit none

    integer ( ip ),        parameter :: dof = 2  ! degrees of freedom: intercept, slope
    ! Clerman & Spector 76 (p 101)
    character ( len = * ), parameter :: dof_params ( 2 ) = [ character ( len = 9 ) :: 'intercept', 'slope' ]

    type                             :: solns_linear
        integer ( ip )               :: n = dof
        integer ( ip )               :: status = 0
        real    ( rp )               :: solution ( 1 : dof ) = zero
        real    ( rp )               :: error    ( 1 : dof ) = zero
        real    ( rp )               :: sse = zero
        real    ( rp ), allocatable  :: residuals ( : )
        character ( len = 64 )       :: descriptor_64 = ''
        character ( len = 512 )      :: warning = ''
        real ( rp )                  :: cpu_seconds_compute = zero
    contains
        private
        ! subroutines
        procedure, public :: normal_a => normal_a_sub
    end type solns_linear

    private :: normal_a_sub

    contains

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +                         normal_a

        subroutine normal_a_sub ( me, measures )

            class ( solns_linear ), target           :: me
            type  ( measurements ), intent ( inout ) :: measures

            type ( intermediates )                   :: ints
            type ( matrices )                        :: matrix

            real ( rp )                              :: cpu_start = zero, cpu_stop = zero

                call allocator_rank_1_sub ( me % residuals, measures % m )
                call ints % compute_intermediates ( measures )
                call matrix % construct_matrices ( ints, dof, measures )

                call cpu_time ( cpu_start )
                    ! solve linear system
                    me % solution  ( : ) = matmul ( matrix % ASAinv, matmul ( matrix % AS, measures % y ( : ) ) )
                    me % residuals ( : ) = matmul ( matrix % A, me % solution ) -  measures % y ( : )
                    ! error terms
                    me % sse = dot_product ( me % residuals, me % residuals )
                    me % error = [ ints % sX2, ints % em ]
                    me % error = sqrt ( me % error * me % sse / ( measures % m - dof ) / ints % det )
                call cpu_time ( cpu_stop )
                me % cpu_seconds_compute = cpu_stop - cpu_start

                return

        end subroutine normal_a_sub

end module mSolnsLinear
