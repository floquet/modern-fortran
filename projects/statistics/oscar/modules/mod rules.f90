!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mRule

    use precision_definitions, only : is
    use mPip

    implicit NONE

    character ( len = * ), private, parameter :: me_module = 'module mRule'  ! self-identification

    integer ( is )                            :: nRules = 0
    type ( rules ), allocatable               :: playerRules ( : )

    type, public                   :: rules

        character ( len = nofPip ) :: handValue ( 2 : 21 )
        character ( len = 16 )     :: name

    contains

        private
!       functions

!       subroutines
        procedure, nopass, public  :: load_rules_sub
        procedure, nopass, public  :: print_rules_sub

    end type rules

    ! subroutines: initialization
    ! private                        :: load_rules_sub  ! initializations

    contains

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      load_rules

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
                myRules ( 1 ) % name = '1: hard 17'
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

                myRules ( 2 ) % name = '2: soft 17'
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

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     print_rules

        subroutine print_rules_sub ( )

            implicit NONE

                write ( *, 100 )
                write ( *, 110 ) "s", "stand"
                write ( *, 110 ) "h", "hit"
                write ( *, 110 ) "j", "double, then hit"
                write ( *, 110 ) "p", "split"

            return

    100     format ( /, "List of recognized decisions: " )
    110     format ( g0, 2X, g0 )

        end subroutine print_rules_sub

end module mRule

! https://stackoverflow.com/questions/15226252/cannot-assign-initial-value-to-derived-data-type-in-a-module
! http://compgroups.net/comp.lang.fortran/unexpected-assignment-statement-in-module/604269