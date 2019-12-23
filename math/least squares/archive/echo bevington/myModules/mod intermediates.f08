! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

!                             V  you are here  V

! data vectors ( 1, x, y ) -> intermediate sums -> matrices ( A, AT, ATAinv ) -> solution ( slope, intercept )
module mIntermediates

    use mPrecisionDefinitions, only : rp, zero
    implicit none

    type            :: intermediates
        real ( rp ) :: em = zero, sX = zero, sX2 = zero, sY = zero, sXY = zero, det = zero
    end type intermediates 

end module mIntermediates
