! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 30

!                             V  you are here  V

! data vectors ( 1, x, y ) -> intermediate sums -> matrices ( A, AT, ATAinv ) -> solution ( slope, intercept )

module mIntermediates

    use mPrecisionDefinitions,  only : ip, rp
    use mParameters,            only : zero, one
    use mMeasurements
    implicit none

    type            :: intermediates
        real ( rp ) :: em = zero, sX = zero, sX2 = zero, sY = zero, sXY = zero, det = zero
        contains
            private
            ! subroutines
            procedure, public :: check_determinant             => check_determinant_sub
            procedure, public :: compute_determinant           => compute_determinant_sub
            procedure, public :: compute_intermediates         => compute_intermediates_sub
    end type intermediates

    type ( intermediates )  :: intermediates0 = intermediates ( em = zero, sX = zero, sX2 = zero, sY = zero, sXY = zero, &
                                                                          det = zero )
    integer ( ip )          :: status
    character ( len = 512 ) :: warning

    character ( len = * ), parameter, private :: myModule = 'module mIntermediates'  ! self-identification

    private :: check_determinant_sub
    private :: compute_determinant_sub
    private :: compute_intermediates_sub

    contains

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       + +                                                                                     + +
!       + +  Compute intermediates                                                              + +
!       + +                                                                                     + +
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       =============================================================================================      compute_intermediates_dot

        subroutine compute_intermediates_sub ( me, measures )

            class ( intermediates ), target      :: me
            type ( measurements ), intent ( in ) :: measures

                me % em  = measures % m * one

                me % sX  = dot_product ( measures % ones,     measures % x ( : ) )
                me % sY  = dot_product ( measures % ones,     measures % y ( : ) )
                me % sX2 = dot_product ( measures % x ( : ),  measures % x ( : ) )
                me % sXY = dot_product ( measures % x ( : ),  measures % y ( : ) )

                call me % compute_determinant ( )

        end subroutine compute_intermediates_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       + +                                                                                     + +
!       + +  Determinant routines                                                               + +
!       + +                                                                                     + +
!       + +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ +
!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!       =============================================================================================            compute_determinant

        subroutine compute_determinant_sub ( me )

            class ( intermediates ), target :: me

                me % det = me % em * me % sX2 - me % sX ** 2
                call me % check_determinant ( )

        end subroutine compute_determinant_sub

!       =============================================================================================              check_determinant

        subroutine check_determinant_sub ( me )

            class ( intermediates ), target  :: me

            character ( len = * ), parameter :: mySubroutine    = 'subroutine check_determinant_sub'  ! self-identification
            character ( len = * ), parameter :: callChain       = 'Call chain: ' // myModule // ', ' // mySubroutine // '.'
            character ( len = * ), parameter :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

            ! intializations
            status = 0
            warning = ''

            ! quality checks on determinant
            if ( me % det == zero ) then
                status = -2
                write ( * , 100 )
                stop error_fatal
            end if

            if ( abs ( me % det ) <= epsilon ( one ) ) then
                status = 2
                write ( warning, 110 ) 'Dangerously ', me % det
                write ( *      , 120 ) error_not_fatal
                else if ( abs ( me % det ) <= 5 * epsilon ( one ) ) then
                    status = 1
                    write ( warning, 110 ) 'Suspiciously ', me % det
                    write ( *      , 120 ) error_not_fatal
                end if

            return

  100       format ( /, 'Determinant value = 0.', / )
  110       format ( g0, 'small determinant = ', E9.3, ', machine epsilon = ', E9.3, '. ' )
  120       format ( g0 )

        end subroutine check_determinant_sub

end module mIntermediates
