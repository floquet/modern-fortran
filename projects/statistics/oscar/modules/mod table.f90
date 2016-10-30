!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mTable

    use precision_definitions, only : wp, zero, one, is
    use house_parameters
    use shared_variables
    use mPlayer
    use mShoe
    use mRule

    implicit none

    character ( len = * ), private, parameter :: me_module = 'module mTable'  ! self-identification

    integer ( is ),   private                 :: playerID = 0, handID = 0
    integer ( is ),   private                 :: jTable = 0, kTable = 0, hTable = 0
    integer ( is )                            :: show_card = 0
!    integer ( is )                            :: nRules = 0
!    type ( rules ), allocatable               :: playerRules ( : )

    type, private   :: results
        real ( wp ) :: winningsPP = zero, wagerPP = zero, gain = zero
    end type results

!   pointers
    type ( players ), private, pointer :: player
    type ( players ), private, pointer :: theDealer

    type ( hands ),   private, pointer :: hand

!     type ( IDList ),  private, pointer :: BJList ( : )
!     type ( IDList ),  private, pointer :: BustList ( : )
!     type ( IDList ),  private, pointer :: OpenList ( : )
     type ( IDList ),  private, pointer :: whichList ( : )

        type, public :: superList
            
        end type superList

!   primary derived type
    type, public                 :: tables
        type ( players )         :: myPlayers ( 1 : maxPlayers )
        type ( players )         :: dealer
        type ( IDList )          :: pLBJ   ( 1 : maxHandsPT )
        type ( IDList )          :: pLBust ( 1 : maxHandsPT )
        type ( IDList )          :: pLOpen ( 1 : maxHandsPT )
        integer ( is )           :: numPHandsBJ = 0, numPHandsBust = 0, numPHandsOpen = 0  ! on the table
        type ( IDList ), pointer :: pointerList ( : )
!         type ( IDList ), pointer :: pointerBJList ( : )
         type ( IDList ), pointer :: pointerBustList ( : )
         type ( IDList ), pointer :: pointerOpenList ( : )

        type ( results )         :: performance
        type ( shoes )           :: myShoe
        type ( shoes ), pointer  :: pntShoe

        integer ( is )           :: lPlayersSolvent ( 1 : maxPlayers )
        integer ( is )           :: numDeals = 0, numHandsDelt = 0
        integer ( is )           :: numPlayersTotal = 0, numPlayersSolvent = 0

        logical                  :: fDealerBJ = .false., fDealerBust = .false., fGameOver = .false.

        contains

            private
!           functions
            !procedure, public      :: random_integer   =>    random_integer_fcn

!           subroutines
            procedure, public :: win                      =>  win_sub
            procedure, public :: push                     =>  push_sub
            procedure, public :: lose                     =>  lose_sub
            procedure, public :: settle_win               =>  settle_win_sub
            procedure, public :: settle_push              =>  settle_push_sub
            procedure, public :: settle_lose              =>  settle_lose_sub
            procedure, public :: settle_compare           =>  settle_compare_sub
            procedure, public :: adjudicate               =>  adjudicate_sub

            procedure, public :: Q_player_bj              =>  Q_player_bj_sub

            procedure, public :: wager                    =>  wager_sub
            procedure, public :: shuffle                  =>  shuffle_sub
            procedure, public :: set_table                =>  set_table_sub
            procedure, public :: deal_hands               =>  deal_hands_sub
            procedure, public :: place_bets               =>  place_bets_sub
            procedure, public :: sweep_players            =>  sweep_players_sub
            procedure, public :: who_is_solvent           =>  who_is_solvent_sub

            procedure, public :: print_set_table          =>  print_set_table_sub
            procedure, public :: print_solvent_players    =>  print_solvent_players_sub
            procedure, public :: play_one_round           =>  play_one_round_sub

    end type tables

!   functions
!   private                    :: random_integer_fcn

!   subroutines: initialization
    !private                              :: set_table_sub, initialize_solvent_players_sub  ! initializations
!   subroutines: print
    private                                   :: print_set_table_sub, print_solvent_players_sub!, print_results_sub, print_hands_sub
!   subroutines: query
    private                                   :: who_is_solvent_sub, Q_player_bj_sub
!   subroutines: action
    private                                   :: win_sub, push_sub, lose_sub
    private                                   :: settle_win_sub, settle_push_sub, settle_compare_sub, settle_lose_sub
    private                                   :: adjudicate_sub, shuffle_sub
    private                                   :: wager_sub, place_bets_sub, deal_hands_sub, play_one_round_sub

    contains                                                                                    ! methods: subroutines and functions

!       ############################################################################################
!       #                                                                                          #
!       #  Play                                                                                    #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  play_one_round

        subroutine play_one_round_sub ( self )

            class ( tables ), target                   :: self

                call self % place_bets ( )
                call self % deal_hands ( )  ! catches blackjacks
                call self % sweep_players ( self % pointerBustList, self % pointerOpenList, &
                                            self % numPHandsBust,   self % numPHandsOpen )

        end subroutine play_one_round_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   sweep_players

        subroutine sweep_players_sub ( self, BustList, OpenList, nBust, nOpen )

            class ( tables ), target                   :: self

            type ( IDList ), pointer, intent ( inout ) :: BustList ( : ), OpenList ( : )
            integer ( is ),           intent ( inout ) :: nBust, nOpen

            type ( shoes ), pointer                    :: pShoe

!               sweep solvent players
                pShoe => self % myShoe
                    do kTable = 1, self % numPlayersSolvent
                        playerID = self % lPlayersSolvent ( kTable )
                        player => self % myPlayers ( playerID )
                            player % numPlayerHands = 1
                            call player % play_my_hands ( pSHoe, BustList, OpenList, nBust, nOpen )
                            player => null ( )
                        end do
                pShoe => null ( )

        end subroutine sweep_players_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      deal_hands

        subroutine deal_hands_sub ( self )

            class ( tables ), target  :: self

            type ( shoes ),   pointer :: shoe

!               counters
                self % numDeals = self % numDeals + 1
                self % numHandsDelt = self % numHandsDelt + self % numPlayersSolvent

!               initialize solvent players
                !call self % initialize_solvent_players ( )

!               deal cards
                shoe => self % myShoe
                    do jTable = 1, 2 ! two cards
                        do kTable = 1, self % numPlayersSolvent
                            playerID = self % lPlayersSolvent ( kTable )
                            player => self % myPlayers ( playerID )
                                hand => player % myHands ( 1 )
                                    hand % cards ( jTable ) = shoe % next_card ( )
                                hand => null ( )
                            player => null ( )
                        end do
                        self % dealer % myHands ( 1 ) % cards ( jTable ) = shoe % next_card ( )
                    end do
                    show_card = self % dealer % myHands ( 1 ) % cards ( dealerShow )

!                   check players for blackjack
                    call self % Q_player_bj ( )

!                   did we pass the cut card?
                    if ( shoe % cardCurrent > shoe % cardStop ) then
                        shoe % fPastCutCard = .true.
                        write ( *, '( "cut card" )' )
                    end if
                shoe => null ( )

!               check dealer for blackjack
                theDealer => self % dealer
                    hand => theDealer % myHands ( 1 )
                        if ( hand % Q_bj ( ) ) then
                            self % fDealerBJ = .true.
                            theDealer % howMany % handsBJ = theDealer % howMany % handsBJ + 1
                        else
                            self % fDealerBJ = .false.
                        end if
                    hand => null ( )
                theDealer => null ( )

        end subroutine deal_hands_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                           wager

        subroutine wager_sub ( self, playerID, chips )

            class ( tables ), target      :: self

            integer ( is ), intent ( in ) :: chips
            integer ( is ), intent ( in ) :: playerID

            real    ( wp )                :: bet

                bet = chip * chips

                player => self % myPlayers ( playerID )
                    player % wagers ( 1 ) = bet
                    player % myBank % now = player % myBank % now - bet
                player => null ( )

        end subroutine wager_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      place_bets

        subroutine place_bets_sub ( self )

            class ( tables ), target  :: self

                call who_is_solvent_sub ( self )
                if ( self % fGameOver ) then
                    return
                else
                    do kTable = 1, self % numPlayersSolvent
                        playerID = self % lPlayersSolvent ( kTable )
                        call wager_sub ( self, playerID = playerID, chips = 1 )
                    end do
                end if

        end subroutine place_bets_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Initializations                                                                         #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                       set_table

        subroutine set_table_sub ( self, nPlayers, bank, rule_set )

            class ( tables ), target :: self

            integer ( is ), intent ( in )    :: nPlayers
            integer ( is ), intent ( in )    :: rule_set ( : )
            real    ( wp ), intent ( in )    :: bank     ( : )

            character ( len = * ), parameter :: me_subroutine = 'subroutine set_table_sub'  ! self-identification

                self % myShoe % numCards = nofDecks * ninDeck
                call self % myShoe % initial_spectrum ( )

!               set flags
                self % fGameOver = .false.
                self % myShoe % fPastCutCard = .false.

!               clear table counters
                self % numPHandsBJ   = 0
                self % numPHandsBust = 0
                self % numPHandsOpen = 0

!               verify data
                if ( nPlayers <= 0 ) then
                    write ( *, 200 ) nPlayers
                    stop data_msg // " Look in " // me_module // ", " // me_subroutine // "."
                end if

                if ( nPlayers > maxPlayers ) then
                    write ( *, 210 ) nPlayers, maxPlayers
                    stop data_msg // " Look in " // me_module // ", " // me_subroutine // "."
                else
                    self % numPlayersTotal = nPlayers
                end if

                if ( size ( bank ) /= nPlayers ) then
                    write ( *, 300 ) nPlayers, size ( bank )
                    stop data_msg // " Look in " // me_module // ", " // me_subroutine // "."
                end if

                if ( size ( rule_set ) /= nPlayers ) then
                    write ( *, 310 ) nPlayers, size ( rule_set )
                    stop data_msg // " Look in " // me_module // ", " // me_subroutine // "."
                end if

!               establish players
                do kTable = 1, self % numPlayersTotal
                    player => self % myPlayers ( kTable )
                        player % numPlayerHands = 1
                        player % playerID = k
                        player % rule = rule_set ( kTable )
                        call player % initialize_counters ( )
                        call player % initialize_bank ( bank ( kTable ) )
                    player => null ( )
                end do

!               load different strategies
                call load_rules_sub ( nRules, playerRules )
                stop

            return

!     100     format ( /, "Error allocating memory for ", g0, " array ", g0, "." )
!     110     format (    "  requested size is ", g0, " elements" )
!     120     format (    "  stat = ", g0 )
!     130     format (    "  errmsg = ", g0, "." )

    200     format ( /, "Too few players specified: nPlayers = ", g0 )
    210     format ( /, "Too many players specified: nPlayers = ", g0, " (should be < ", g0,")" )

    300     format ( /, "Data error: number of players != size of bank.", &
                         /, "You specified ", g0, "players but supplied a bank with ", g0, " elements." )
    310     format ( /, "Data error: number of players != size of rules.", &
                         /, "You specified ", g0, "players but supplied a rule list with ", g0, " elements." )

        end subroutine set_table_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         shuffle

        subroutine shuffle_sub ( self )

            class ( tables ), target :: self

                call self % myShoe % shuffle ( )

        end subroutine shuffle_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Query functions                                                                         #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  who_is_solvent

        subroutine who_is_solvent_sub ( self )

            class ( tables ), target :: self

            real ( wp )              :: delta

                self % numPlayersSolvent     = 0
                self % lPlayersSolvent ( : ) = 0

                jTable = 0
                self % fGameOver = .false.

                do kTable = 1, self % numPlayersTotal
                    delta = self % myPlayers ( kTable ) % myBank % now - chip
                    if ( delta >= epsilon ( one ) ) then
                        jTable = jTable + 1
                        self % lPlayersSolvent ( jTable ) = k
                    end if
                end do

            self % numPlayersSolvent = j
            if ( jTable == 0 ) self % fGameOver = .true.

        end subroutine who_is_solvent_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     Q_player_bj

        subroutine Q_player_bj_sub ( self )

            class ( tables ), target :: self

                do kTable = 1, self % numPlayersSolvent
                    playerID = self % lPlayersSolvent ( kTable )
                    player => self % myPlayers ( playerID )
                        do hTable = 1, player % numplayerHands
                            hand => player % myHands ( hTable )
                                hand % fBJ   = .false.
                                if ( hand % Q_bj ( ) ) then
                                    hand % fBJ   = .true.
                                    hand % fBust = .false.
                                    player % howMany % handsBJ = player % howMany % handsBJ + 1
!                                   update the list of BJ hands
                                    self % numPHandsBJ = self % numPHandsBJ + 1
                                    jTable = self % numPHandsBJ
                                    self % pLBJ ( jTable ) % pID = playerID
                                    self % pLBJ ( jTable ) % hID = h
                                end if
                            hand => null ( )
                        end do
                    player => null ( )
                end do

        end subroutine Q_player_bj_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Operations on a single hand                                                             #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                             win

        subroutine win_sub ( self, playerID, handID, bj_multiplier )

            class ( tables ), target          :: self

            integer ( is ), intent ( in )     :: playerID
            integer ( is ), intent ( in )     :: handID
            real ( wp ),    intent ( in )     :: bj_multiplier

            real ( wp )                       :: winnings = zero

                player => self % myPlayers ( playerID )
                    winnings = player % myHands ( handID ) % wager * ( 1 + bj_multiplier )
                    call player % bankAction ( winnings )  ! increment bank
                    call player % incHandsPlayed ( )  ! increment hands won
                player => null ( )

        end subroutine win_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                            push

        subroutine push_sub ( self, pID, hID )

            class ( tables ), target          :: self

            integer ( is ), intent ( in )     :: pID
            integer ( is ), intent ( in )     :: hID

            real ( wp )                       :: winnings = zero

                player => self % myPlayers ( pID )
                    winnings = player % myHands ( hID ) % wager
                    call player % bankAction ( winnings )  ! increment bank
                    call player % incHandsPushed ( )  ! increment hands pushed
                player => null ( )

        end subroutine push_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                            lost

        subroutine lose_sub ( self, pID )

            class ( tables ), target          :: self

            integer ( is ), intent ( in )     :: pID

                call self % myPlayers ( pID ) % incHandsLost ( )  ! increment hands lost

        end subroutine lose_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Operations on a list of hands                                                           #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      settle_win

        subroutine settle_win_sub ( self, pointr, numInList, multiplier )

            class ( tables ), target                :: self

            type ( IDList ), pointer, intent ( in ) :: pointr ( : )
            integer ( is ),  intent ( in )          :: numInList
            real ( wp ),     intent ( in )          :: multiplier

                do kTable = 1, numInList
                    playerID = pointr ( kTable ) % pID
                    handID   = pointr ( kTable ) % hID
                    call self % win ( playerID, handID, multiplier )
                end do

        end subroutine settle_win_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     settle_push

        subroutine settle_push_sub ( self, pointr, numInList )

            class ( tables ), target                :: self

            type ( IDList ), pointer, intent ( in ) :: pointr ( : )
            integer ( is ),  intent ( in )          :: numInList

                do kTable = 1, numInList
                    playerID = pointr ( kTable ) % pID
                    handID   = pointr ( kTable ) % hID
                    call self % lose ( playerID )
                end do

        end subroutine settle_push_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     settle_lose

        subroutine settle_lose_sub ( self, pointr, numInList )

            class ( tables ), target                :: self

            type ( IDList ), pointer, intent ( in ) :: pointr ( : )
            integer ( is ),  intent ( in )          :: numInList

                do kTable = 1, numInList
                    playerID = pointr ( kTable ) % pID
                    handID   = pointr ( kTable ) % hID
                    call self % lose ( playerID )
                end do

        end subroutine settle_lose_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  settle_compare

        subroutine settle_compare_sub ( self, pointr, numInList, houseScore )

            class ( tables ), target                :: self

            type ( IDList ), pointer, intent ( in ) :: pointr ( : )
            integer ( is ),  intent ( in )          :: numInList
            integer ( is ),  intent ( in )          :: houseScore

            integer ( is )                          :: playerScore = 0

                do kTable = 1, numInList
                    playerID = pointr ( kTable ) % pID
                    handID   = pointr ( kTable ) % hID
                    playerScore = self % myPlayers ( playerID ) % myHands ( handID ) % best_value
                    if ( playerScore > houseScore ) then
                            call self % win ( playerID, handID, one )
                        else if ( playerScore == houseScore ) then
                                call self % push ( playerID, handID )
                            else
                                call self % lose ( playerID )
                            endif
                end do

        end subroutine settle_compare_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      adjudicate

!       ############################################################################################
!       #                                                                                          #
!       #  All hands are completed; flip dealer down card                                          #
!       #                                                                                          #
!       ############################################################################################

        subroutine adjudicate_sub ( self )

            class ( tables ), target :: self
            integer ( is )           :: dealer_score

                dealer_score = self % dealer % myHands ( 1 ) % best_value

                if ( self % fDealerBust ) then  ! dealer bust?
                    whichList => self % pLBJ
                    call self % settle_win ( whichList, self % numPHandsBJ, bj_payout )
                    whichList => self % pLOpen
                    call self % settle_win ( whichList, self % numPHandsOpen, one )
                else if ( self % fDealerBJ ) then  ! dealer blackjack?
                        whichList => self % pLBJ
                        call self % settle_push ( whichList, self % numPHandsBJ )
                        whichList => self % pLOpen
                        call self % settle_lose ( whichList, self % numPHandsOpen )
                    else  ! dealer <= 21, no BJ
                        whichList => self % pLBJ
                        call self % settle_win ( whichList, self % numPHandsBJ, bj_payout )
                        whichList => self % pLOpen
                        call self % settle_compare ( whichList, self % numPHandsOpen, dealer_score )
                    endif

        end subroutine adjudicate_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Print routines                                                                          #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 print_set_table

        subroutine print_set_table_sub ( self )

            class ( tables ), target :: self

                write ( *, 100 ) "Number of players = ", self % numPlayersTotal
                write ( *, 110 ) "fDealerBJ",   self % fDealerBJ
                write ( *, 110 ) "fDealerBust", self % fDealerBust
                write ( *, 110 ) "fGameOver",   self % fGameOver

                write ( *, 120 )
                do kTable = 1, self % numPlayersTotal
                    player => self % myPlayers ( kTable )
                        write ( *, 130 ) player % playerID, player % rule, player % myBank % start, player % myBank % now
                    player => null ( )
                end do

                write ( *, 140 ) 'spectrum of cards dealt:        ', self % myShoe % spectrum_dealt
                write ( *, 140 ) 'spectrum of cards held in shoe: ', self % myShoe % spectrum_held

            return

    100     format ( g0, g0 )
    110     format ( "The flag ", g0, " is set to ", g0 )
    120     format ( "Player summary: " )
    130     format ( I2, 2X, "rule = ", I2, ", bank deposit = ", g0, ", account balance = ", g0 )
    140     format ( g0, 15I4 )

        end subroutine print_set_table_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           print_solvent_players

        subroutine print_solvent_players_sub ( self )

            class ( tables ), target :: self

                write ( *, 100 ) "Number of players = ", self % numPlayersTotal
                write ( *, 100 ) "Number of solvent players = ", self % numPlayersSolvent

                write ( *, 120 )
                do kTable = 1, self % numPlayersSolvent
                    playerID = self % lPlayersSolvent ( kTable )
                    player => self % myPlayers ( playerID )
                        write ( *, 130 ) playerID, player % myBank % start, player % myBank % now
                    player => null ( )
                end do

            return

    100     format ( g0, g0 )
    120     format ( "Solvent players: " )
    130     format ( I2, 2X, "bank deposit = ", g0, ", account balance = ", g0 )

        end subroutine print_solvent_players_sub

end module mTable