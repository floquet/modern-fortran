!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mHand

    use precision_definitions, only : is, wp, zero
    use house_parameters
    use mPip
    use mShoe
    use mRule

    implicit none

    character ( len = * ), private, parameter :: me_module = 'module mHand'  ! self-identification

    integer ( is ), private :: jHand = 0, kHand = 0!, hHand = 0

    type, public       :: hands
        integer ( is ) :: handID = 0
        integer ( is ) :: playerID = 0
        integer ( is ) :: cards  ( 1 : 21 ) = 0  ! 21 aces
        integer ( is ) :: values ( 1 :  2 ) = 0  ! <= 21
        integer ( is ) :: best_value = 0
        integer ( is ) :: nCardsHand = 0
        integer ( is ) :: numAces    = 0

        real ( wp )    :: wager = zero

        logical        :: fBJ   = .false.
        logical        :: fBust = .false.

        contains
            private
            procedure, public         :: Q_bj              =>  Q_bj_fcn
            procedure, public         :: action            =>  action_fcn
            procedure, public         :: set_best_value    =>  set_best_value_sub
            procedure, public         :: play_this_hand    =>  play_this_hand_sub
            procedure, public         :: update_bust_list  =>  update_bust_list_sub
            procedure, public         :: update_open_list  =>  update_open_list_sub
            procedure, nopass, public :: test_rules        =>  test_rules_sub

    end type hands

    !type ( hands ), private, pointer :: pntHand

!   subroutines: initializations
    private                   :: Q_bj_fcn
    private                   :: action_fcn
    private                   :: set_best_value_sub
    private                   :: play_this_hand_sub
    private                   :: update_bust_list_sub, update_open_list_sub
    private                   :: test_rules_sub

    contains

!       ############################################################################################
!       #                                                                                          #
!       #  Functions                                                                               #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                            Q_bj

        function Q_bj_fcn ( self ) result ( fIs_bj )

            class ( hands ), target       :: self

            logical                       :: fIs_bj

                fIs_bj = .false.

!               check for A + ?
                if ( self % cards ( 1 ) .eq. 1 ) then
                    if ( self % cards ( 2 ) .eq. 10 ) then
                        fIs_bj = .true.
                        return
                    endif
                endif

!               check for ? + A
                if ( self % cards ( 2 ) .eq. 1 ) then
                    if ( self % cards ( 1 ) .eq. 10 ) then
                        fIs_bj = .true.
                        return
                    endif
                endif

        end function Q_bj_fcn

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                          action

        function action_fcn ( self, rule, show_card ) result ( decision )

            class ( hands ), target       :: self

            integer ( is ), intent ( in ) :: rule, show_card

            character ( len = 1 )         :: decision
            character ( len = nofPip )    :: allChoices

!             write ( *, '( "self % handID = ", g0 )' ) self % handID
!             write ( *, '( "self % best_value = ", g0 )' ) self % best_value
                allChoices = playerRules ( rule ) % handValue ( self % best_value )
                decision   = allChoices ( show_card : show_card )

        end function action_fcn 

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  play_this_hand

!       ############################################################################################
!       #                                                                                          #
!       #  Subroutines: play                                                                       #
!       #                                                                                          #
!       ############################################################################################

        subroutine play_this_hand_sub ( self, pID, rule, pSHoe, BustList, OpenList, nBust, nOpen, show_card )

            class ( hands ), target                    :: self

            integer ( is ),           intent ( in )    :: show_card
            integer ( is ),           intent ( in )    :: pID, rule
            integer ( is ),           intent ( inout ) :: nBust, nOpen

            type ( IDList ), pointer, intent ( inout ) :: BustList ( : ), OpenList ( : )
            type ( shoes ),  pointer, intent ( inout ) :: pShoe

            character ( len = * ), parameter           :: me_subroutine = 'subroutine play_this_hand_sub'  ! self-identification
            character ( len = 1 )                      :: decision = 'X'

                do
                    call self % set_best_value ( )
!             print *, me_subroutine, ': size of bust list = ', size ( BustList )
!             write ( *, '( g0, 2X, g0, 2X, "self % handID = ", g0 )' ) me_module, me_subroutine, self % handID
!             write ( *, '( g0, 2X, g0, 2X, "self % best_value = ", g0 )' ) self % best_value

                    if ( self % best_value > 21 ) then
                        call self % update_bust_list ( pID, BustList, nBust )
                        write ( *, 200 ) pID, self % handID, self % best_value, self % cards ( 1 : self % nCardsHand )
                        return
                    endif

                    if ( self % best_value == 21 ) then
                        call self % update_open_list ( pID, OpenList, nOpen )
                        write ( *, 210 ) pID, self % handID, self % cards ( 1 : self % nCardsHand )
                        return
                    endif

                    decision = self % action ( rule, show_card )
                    select case ( decision )
                    case ( 's' )  ! stand
                        write ( *, 300 ) 'standing on ', self % best_value, rule, self % cards ( 1 : self % nCardsHand )
                        call self % update_open_list ( pID, OpenList, nOpen )
                        write ( *, 220 ) pID, self % handID, self % best_value, self % cards ( 1 : self % nCardsHand )
                        return
                    case ( 'h' )  ! hit
                        write ( *, 300 ) 'hitting on ', self % best_value, rule, self % cards ( 1 : self % nCardsHand )
                        self % nCardsHand = self % nCardsHand + 1
                        self % cards ( self % nCardsHand ) = pShoe % next_card ( )
                        cycle
                    case ( 'j' )  ! double
                        write ( *, 300 ) 'doubling on ', self % best_value, rule, self % cards ( 1 : self % nCardsHand )
                        self % nCardsHand = self % nCardsHand + 1
                        self % cards ( self % nCardsHand ) = pShoe % next_card ( )
                        cycle
                    case ( 'p' )  ! split
                        write ( *, 300 ) 'splitting on ', self % best_value, rule, self % cards ( 1 : self % nCardsHand )
                        call self % update_open_list ( pID, OpenList, nOpen )
                        return
                    case default
                        write ( *, 100 ) decision
                        stop stop_msg // ' Look in ' // me_module // ', ' // me_subroutine // '.'
                    end select
                end do

            return

    100     format ( 'Unknown action for player decision: ', g0 )

    200     format ( 'Bust!      Player ', g0, ', hand ', g0, ', score = ', g0, ': ', 21I3 )
    210     format ( 'Blackjack! Player ', g0, ', hand ', g0, ': ', 21I3 )
    220     format ( 'Open.      Player ', g0, ', hand ', g0, ', score = ', g0, ': ', 21I3 )

    300     format ( g0, g0, ' using rule ', g0, ': cards = ', 21I3 )

        end subroutine play_this_hand_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  set_best_value

        subroutine set_best_value_sub ( self )

            class ( hands ), target          :: self

            integer ( is )                   :: v ! hand value

                self % numAces = count ( mask = self % cards ( 1 : self % nCardsHand ) .eq. 1 )
                v = sum ( pipValue ( self % cards ( 1 : self % nCardsHand ) ) )
                self % values ( 1 ) = v
                self % best_value   = v
                do kHand = 1, self % numAces
                    v = v + 10
                    if ( v <= 21 ) then
                        self % best_value = v
                        self % values ( kHand + 1 ) = v
                    return
                    end if
                end do

        return

        end subroutine set_best_value_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Update census lists                                                                     #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                update_bust_list

        subroutine update_bust_list_sub ( self, pID, BustList, nBust )

            class ( hands ), target                    :: self

            integer ( is ),           intent ( in )    :: pID
            integer ( is ),           intent ( inout ) :: nBust
            type ( IDList ), pointer, intent ( inout ) :: BustList ( : )

!                print *, 'size of bust list = ', size ( BustList )
                nBust = nBust + 1
                BustList ( nBust ) % pID  = pID
                BustList ( nBust ) % hID  = self % handID

        end subroutine update_bust_list_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                update_open_list

        subroutine update_open_list_sub ( self, pID, OpenList, nOpen )

            class ( hands ), target                    :: self

            integer ( is ),           intent ( in )    :: pID
            integer ( is ),           intent ( inout ) :: nOpen
            type ( IDList ), pointer, intent ( inout ) :: OpenList ( : )

!                print *, 'size of open list = ', size ( OpenList )
                nOpen = nOpen + 1
                OpenList ( nOpen ) % pID  = pID
                OpenList ( nOpen ) % hID  = self % handID

        end subroutine update_open_list_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Test functions                                                                          #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      test_rules

        subroutine test_rules_sub ( )!self )

            !class ( hands ), target    :: self

!            character ( len = 1 )      :: decision
            character ( len = nofPip ) :: allChoices

            !type ( rules ), pointer :: pntRules

            do jHand = 1, nRules
                write ( *, 120 ) playerRules ( jHand ) % name
                !pntRules => theRules ( jHand )
                write ( *, 100 ) pDescriptors
                do kHand = 2, 21
                    allChoices = playerRules ( jHand ) % handValue ( kHand )
                    write ( *, 110 ) kHand, allChoices
                end do
                write ( *, * )
                !pntRules => null ( )
            end do

    100    format ( 17X, g0 )
    110    format ( 'hand value = ', I2, ': ', g0 )
    120    format ( g0 )

        end subroutine test_rules_sub


end module mHand
