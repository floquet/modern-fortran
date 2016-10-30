!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mSuit

    use precision_definitions, only : is

    implicit NONE

    integer ( is ), private                 :: kSuit = 0
    integer ( is ), parameter               :: nofSuits = 4
    integer ( is ), parameter               :: suitIndex ( 1 : nofSuits ) = [ ( kSuit, kSuit = 1, nofSuits ) ]

    character ( len = nofSuits ), parameter :: sDescriptors = "CDHS"

    type, public :: suits
        integer ( is )                      :: index ( 1 : nofSuits ) = suitIndex
        character ( len = nofSuits )        :: suitName = sDescriptors
    end type suits

end module mSuit