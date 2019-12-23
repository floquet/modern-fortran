!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
!implied do
module mDeck

    use mPip
    use mSuit

    implicit NONE

    integer ( is ), private        :: kDeck = 0, j = 0, k = 0
    integer ( is ), parameter      :: ninDeck = nofPip * nofSuits
    integer ( is ), parameter      :: deckTag   ( 1 : ninDeck ) = [ ( pipTag,   kDeck = 1, nofSuits ) ]
    integer ( is ), parameter      :: deckValue ( 1 : ninDeck ) = [ ( pipValue, kDeck = 1, nofSuits ) ]

    type, public :: decks
        integer ( is )             :: index ( 1 : ninDeck ) = deckTag
        integer ( is )             :: value ( 1 : ninDeck ) = deckValue
        integer ( is )             :: suit  ( 1 : ninDeck ) = [ (( k, j = 1, nofPip ), k = 1, nofSuits ) ]
    end type decks

end module mDeck