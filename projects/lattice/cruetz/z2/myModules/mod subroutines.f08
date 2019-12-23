! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 10 04

module mSubroutines

    use mPrecisionDefinitions, only : rp, one, zero

    implicit none

contains

!   =================================================================================================                         update

    subroutine update ( beta, action )

        real ( rp ), intent ( in )  :: beta
        real ( rp ), intent ( out ) :: action

            action = one

    end subroutine update

!   =================================================================================================                 random_integer

    !subroutine coldstart ( value )

        

    !end subroutine coldstart



!       =============================================================================================                 random_integer
!       =============================================================================================                 random_integer
!       =============================================================================================                 random_integer

end module mSubroutines