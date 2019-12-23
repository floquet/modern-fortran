!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mPlayer

    use precision_definitions, only : wp, is, zero
    use house_parameters
    use mHand

    implicit none

    character ( len = * ), private, parameter :: me_module = 'module mPlayer'  ! self-identification

    integer ( is ), private :: kPlay = 0

!   secondary types
    type, private      :: counters
        integer ( is ) :: handsPlayed = 0
        integer ( is ) :: handsBJ     = 0
        integer ( is ) :: handsWon    = 0
        integer ( is ) :: handsLost   = 0
        integer ( is ) :: handsPushed = 0
        integer ( is ) :: handsSplit  = 0
    end type counters

    type            :: banks
        real ( wp ) :: start = zero, now = zero
    end type banks

    type, private   :: performancePlayer
        real ( wp ) :: winningsPH = zero, wagerPH = zero, gain = zero
    end type performancePlayer

!   primary type
    type, public :: players
        type ( hands )    :: myHands ( 1 : maxHandsPP )
        type ( counters ) :: howMany
        type ( banks )    :: myBank

        integer ( is )    :: playerID = 0, rule = 0
        integer ( is )    :: numPlayerHands = 0
        integer ( is )    :: owner

        real ( wp )       :: wagers ( 1 : maxHandsPP )

        contains
            private
            !procedure, public :: play_this_hand       =>  play_this_hand_sub
            procedure, public :: play_my_hands        =>  play_my_hands_sub
            procedure, public :: initialize_counters  =>  initialize_counters_sub
            procedure, public :: initialize_bank      =>  initialize_bank_sub
            procedure, public :: incHandsPlayed       =>  incHandsPlayed_sub
            procedure, public :: incHandsBJ           =>  incHandsBJ_sub
            procedure, public :: incHandsWon          =>  incHandsWon_sub
            procedure, public :: incHandsLost         =>  incHandsLost_sub
            procedure, public :: incHandsPushed       =>  incHandsPushed_sub
            procedure, public :: incHandsSplit        =>  incHandsSplit_sub
            procedure, public :: bankAction           =>  bankAction_sub
            procedure, public :: bankSet              =>  bankSet_sub

    end type players

!   subroutines: initializations
    private                   :: initialize_counters_sub, initialize_bank_sub
!   subroutines: counter actions
    private                   :: incHandsPlayed_sub, incHandsBJ_sub, incHandsWon_sub, incHandsLost_sub, incHandsPushed_sub
    private                   :: incHandsSplit_sub
!   subroutines: bank actions
    private                   :: bankAction_sub, bankSet_sub
!   subroutines: player actions
    private                   :: play_my_hands_sub

    contains

!       ############################################################################################
!       #                                                                                          #
!       #  Initializations                                                                         #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             initialize_bank_sub

        subroutine initialize_bank_sub ( self, amount )

            class ( players ), target  :: self

            real ( wp ), intent ( in ) :: amount
            type ( banks ), pointer    :: bank

                bank => self % myBank
                    bank % start = amount
                    bank % now   = amount
                bank => null ( )

        end subroutine initialize_bank_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             initialize_counters

        subroutine initialize_counters_sub ( self )

            class ( players ), target  :: self

            type ( counters ), pointer :: count

                count => self % howMany
                    count % handsPlayed = 0
                    count % handsBJ     = 0
                    count % handsWon    = 0
                    count % handsLost   = 0
                    count % handsPushed = 0
                    count % handsSplit  = 0
                count => null ( )

        end subroutine initialize_counters_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Incrementers                                                                            #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  incHandsPlayed

        subroutine incHandsPlayed_sub ( self )

            class ( players ), target :: self

                self % howMany % handsPlayed = self % howMany % handsPlayed + 1

        end subroutine incHandsPlayed_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      incHandsBJ

        subroutine incHandsBJ_sub ( self )

            class ( players ), target :: self

                self % howMany % handsBJ = self % howMany % handsBJ + 1

        end subroutine incHandsBJ_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     incHandsWon

        subroutine incHandsWon_sub ( self )

            class ( players ), target :: self

                self % howMany % handsWon = self % howMany % handsWon + 1

        end subroutine incHandsWon_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                    incHandsLost

        subroutine incHandsLost_sub ( self )

            class ( players ), target :: self

                self % howMany % handsLost = self % howMany % handsLost + 1

        end subroutine incHandsLost_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  incHandsPushed

        subroutine incHandsPushed_sub ( self )

            class ( players ), target :: self

                self % howMany % handsPushed = self % howMany % handsPushed + 1

        end subroutine incHandsPushed_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   incHandsSplit

        subroutine incHandsSplit_sub ( self )

            class ( players ), target :: self

                self % howMany % handsSplit = self % howMany % handsSplit + 1

        end subroutine incHandsSplit_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Bank actions                                                                            #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      bankAction

        subroutine bankAction_sub ( self, amount )

            class ( players ), target  :: self

            real ( wp ), intent ( in ) :: amount

                self % myBank % now = self % myBank % now + amount

        end subroutine bankAction_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         bankSet

        subroutine bankSet_sub ( self, amount )

            class ( players ), target  :: self

            real ( wp ), intent ( in ) :: amount

                self % myBank % start = amount

        end subroutine bankSet_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Player actions                                                                          #
!       #                                                                                          #
!       ############################################################################################

        subroutine play_my_hands_sub ( self, pSHoe, BustList, OpenList, nBust, nOpen, show_card )

            class ( players ), target                  :: self

            type ( shoes ),  pointer, intent ( inout ) :: pShoe

            type ( IDList ), pointer, intent ( inout ) :: BustList ( : ), OpenList ( : )
            integer ( is ),           intent ( inout ) :: nBust, nOpen
            integer ( is ),           intent ( in )    :: show_card

!             character ( len = * ), parameter           :: me_subroutine = 'subroutine play_my_hands_sub'  ! self-identification

!               loop over hands

                do kPlay = 1, maxHandsPP
!                     print *, me_module, ', ', me_subroutine, ': size of bust list = ', size ( BustList )
!                     write ( *, '( g0, 2X, g0, 2X, "handID = ", g0 )' ) me_module, &
!                                                                        me_subroutine, self % myHands ( kPlay ) % handID
!                     write ( *, '( g0, 2X, g0, 2X, "best_value = ", g0 )' ) me_module, &
!                                                                            me_subroutine, &
!                                                                            self % myHands ( kPlay ) % best_value
                    call self % myHands ( kPlay ) % play_this_hand ( self % playerID, self % rule, pSHoe, &
                                                                     BustList, OpenList, nBust, nOpen, show_card )
                    if ( kPlay >= self % numPlayerHands ) return
                end do

        end subroutine play_my_hands_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         bankSet

    end module mPlayer
