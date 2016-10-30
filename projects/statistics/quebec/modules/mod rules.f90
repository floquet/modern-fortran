module mRule

    use precision_definitions, only : is
    use mPip

    implicit none

    character ( len = * ), private, parameter :: me_module = 'module mRule'  ! self-identification

    integer ( is ), private                   :: jRule = 0, kRule = 0
    integer ( is )                            :: nRules = 0

    type, public                   :: rules

        character ( len = nofPip ) :: handValue ( 2 : 21 )
        character ( len = 64 )     :: name

!     contains
!
!         private
! !       functions
!
! !       subroutines
!         procedure, nopass, public  :: load_rules_sub
!         procedure, nopass, public  :: print_rules_sub

    end type rules

    type ( rules ), allocatable               :: playerRules ( : )

    contains

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  load_rules_sub

        subroutine load_rules_sub ( nRules, myRules )

            use precision_definitions, only : is
            use shared_variables

            implicit NONE

            integer ( is ), intent ( out )              :: nRules
            type ( rules ), intent ( out ), allocatable :: myRules ( : )
            character ( len = * ), parameter            :: me_subroutine = 'subroutine load_rules_sub'  ! self-identification

                nRules = 2
                allocate ( myRules ( nRules ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "type ( rules )", "myRules"
                    write ( *, 110 ) nRules
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg // "Look in " // me_module // ", " // me_subroutine // "."
                end if

!               http://www.cs.rpi.edu/~szymansk/OOF90/bugs.html
!               Danger with intent(out)
!               when intent(out) is used with a derived type, 
!                   any component not assigned in a procedure could become undefined on exit
                myRules ( 1 ) % name = '1: stand on soft 17, 4+ decks'
                myRules ( 1 ) % handValue (  2 ) = 'hhhhhhhhhhhhh'
                myRules ( 1 ) % handValue (  3 ) = 'hhhhhhhhhhhhh'
                myRules ( 1 ) % handValue (  4 ) = 'hhhhhhhhhhhhh'
                myRules ( 1 ) % handValue (  5 ) = 'hhhhhhhhhhhhh'
                myRules ( 1 ) % handValue (  6 ) = 'hhhhhhhhhhhhh'
                myRules ( 1 ) % handValue (  7 ) = 'hhhhhhhhhhhhh'
                myRules ( 1 ) % handValue (  8 ) = 'hhhjjhhhhhhhh'
                myRules ( 1 ) % handValue (  9 ) = 'hjjjjhhhhhhhh'
                myRules ( 1 ) % handValue ( 10 ) = 'hjjjjhhhhhhhh'
                myRules ( 1 ) % handValue ( 11 ) = 'jjjjjjjjjjjjj'
                myRules ( 1 ) % handValue ( 12 ) = 'hhhssshhhhhhh'
                myRules ( 1 ) % handValue ( 13 ) = 'hhhjjjhhhhhhh'
                myRules ( 1 ) % handValue ( 14 ) = 'hhhjjjhhhhhhh'
                myRules ( 1 ) % handValue ( 15 ) = 'hhhjjjhhhhhhh'
                myRules ( 1 ) % handValue ( 16 ) = 'hhhjjjhhhhhhh'
                myRules ( 1 ) % handValue ( 17 ) = 'hjjjjjhhhhhhh'
                myRules ( 1 ) % handValue ( 18 ) = 'ssjjjjsshhhhh'
                myRules ( 1 ) % handValue ( 19 ) = 'sssssjsssssss'
                myRules ( 1 ) % handValue ( 20 ) = 'sssssssssssss'
                myRules ( 1 ) % handValue ( 21 ) = 'sssssssssssss'

                myRules ( 2 ) % name = '2: hit on soft 17, 4+ decks'
                myRules ( 2 ) % handValue (  2 ) = 'hhhhhhhhhhhhh'
                myRules ( 2 ) % handValue (  3 ) = 'hhhhhhhhhhhhh'
                myRules ( 2 ) % handValue (  4 ) = 'hhhhhhhhhhhhh'
                myRules ( 2 ) % handValue (  5 ) = 'hhhhhhhhhhhhh'
                myRules ( 2 ) % handValue (  6 ) = 'hhhhhhhhhhhhh'
                myRules ( 2 ) % handValue (  7 ) = 'hhhhhhhhhhhhh'
                myRules ( 2 ) % handValue (  8 ) = 'hhhjjhhhhhhhh'
                myRules ( 2 ) % handValue (  9 ) = 'hjjjjhhhhhhhh'
                myRules ( 2 ) % handValue ( 10 ) = 'hjjjjhhhhhhhh'
                myRules ( 2 ) % handValue ( 11 ) = 'jjjjjjjjjjjjj'
                myRules ( 2 ) % handValue ( 12 ) = 'hhhssshhhhhhh'
                myRules ( 2 ) % handValue ( 13 ) = 'hssssshhhhhhh'
                myRules ( 2 ) % handValue ( 14 ) = 'hssssshhhhhhh'
                myRules ( 2 ) % handValue ( 15 ) = 'hssssshhhhhhh'
                myRules ( 2 ) % handValue ( 16 ) = 'hssssshhhhhhh'
                myRules ( 2 ) % handValue ( 17 ) = 'sssssssssssss'
                myRules ( 2 ) % handValue ( 18 ) = 'sssssssssssss'
                myRules ( 2 ) % handValue ( 19 ) = 'sssssssssssss'
                myRules ( 2 ) % handValue ( 20 ) = 'sssssssssssss'
                myRules ( 2 ) % handValue ( 21 ) = 'sssssssssssss'

            return

    100     format ( /, "Error allocating memory for ", g0, " array ", g0, "." )
    110     format (    "  requested size is ", g0, " elements" )
    120     format (    "  stat = ", g0 )
    130     format (    "  errmsg = ", g0, "." )

        end subroutine load_rules_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 print_rules_sub

        subroutine print_rules_sub ( )

            !use mPip
            implicit NONE

                write ( *, 100 )
                write ( *, 110 ) "s", "stand"
                write ( *, 110 ) "h", "hit"
                write ( *, 110 ) "j", "double, then hit"
                write ( *, 110 ) "p", "split"

                do jRule = 1, nRules
                    write ( *, 200 ) jRule, playerRules ( jRule ) % name
                    do kRule = 2, nofPip
                        write ( *, 210 ) kRule, playerRules ( jRule ) % handValue ( kRule )
                    end do
                end do

            return

    100     format ( /, "List of recognized decisions: " )
    110     format ( g0, 2X, g0 )

    200     format ( /, 'Rule set ', g0, ': ', g0 )
    210     format ( 'player hand = ', g0, ': ', g0 )

        end subroutine print_rules_sub

end module mRule
