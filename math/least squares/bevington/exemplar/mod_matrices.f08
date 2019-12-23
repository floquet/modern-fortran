! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 03 04

!                                                      V  you are here  V

! data vectors ( 1, x, y ) -> intermediate sums -> matrices ( A, AT, ATAinv ) -> solution ( slope, intercept )

module mMatrices

    use mPrecisionDefinitions,  only : ip, rp
    use mParameters,            only : zero, one

    use mAllocations,           only : allocator_rank_2_sub
    use mIntermediates,         only : intermediates
    use mMeasurements,          only : measurements

    implicit none

    type                         :: matrices
        real ( rp ), allocatable :: A ( : , : ), AS ( : , : ), ASAinv ( : , : )
    contains
        private
        procedure, public         :: construct_matrices => construct_matrices_sub
    end type matrices

    private :: construct_matrices_sub

    contains

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +               construct_matrices

        subroutine construct_matrices_sub ( me, ints, mydof, measure )

            class ( matrices ), target                :: me
            type  ( intermediates ), intent ( in )    :: ints
            type  ( measurements ),  intent ( inout ) :: measure
            integer ( ip ),          intent ( in )    :: mydof

            integer ( ip )                            :: m

                m = measure % m
                call allocator_rank_2_sub ( me % A,      m,     mydof )
                call allocator_rank_2_sub ( me % AS,     mydof, m )
                call allocator_rank_2_sub ( me % ASAinv, mydof, mydof )

                ! construct basics
                me % A ( : , 1 ) = measure % ones
                me % A ( : , 2 ) = measure % x
                me % AS = transpose ( me % A )

                ! construct inverse
                call ints % compute_intermediates ( measure )
                me % ASAinv ( : , 1 ) = [  ints % sX2, -ints % sX ]
                me % ASAinv ( : , 2 ) = [ -ints % sX,   ints % em ]
                me % ASAinv           = me % ASAinv / ints % det

        end subroutine construct_matrices_sub

end module mMatrices
