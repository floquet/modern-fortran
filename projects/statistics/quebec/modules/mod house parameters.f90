!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module house_parameters

    use precision_definitions, only : is, wp, one

    implicit none

    integer ( is ), parameter :: maxPlayers = 6
    integer ( is ), parameter :: maxHandsPP = 4  ! two splits
    integer ( is ), parameter :: maxHandsPT = maxPlayers * maxHandsPP ! maximum hands in play
    integer ( is ), parameter :: nofDecks   = 6
    integer ( is ), parameter :: stopAfter  = 4  ! cut card
    integer ( is ), parameter :: dealerShow = 2

    real ( wp ), parameter    :: bj_payout  = 1.5_wp
    real ( wp ), parameter    :: chip       = one

    logical, parameter        :: fHitOnSoft17 = .false.
    logical, parameter        :: fAllowSplits = .false.

!   secondary derived types
    type, public       :: IDList
        integer ( is ) :: pID! = 0
        integer ( is ) :: hID! = 0
    end type IDList

end module house_parameters
