!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mPip

    use precision_definitions, only : is

    implicit NONE

    integer ( is ), private               :: kPip = 0
    integer ( is ), parameter             :: nofPip = 13
    integer ( is ), parameter             :: pipTag   ( 1 : nofPip ) = [ ( kPip, kPip = 1, nofPip ) ]
    integer ( is ), parameter             :: pipValue ( 1 : nofPip ) = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10 ]

    integer ( is ), parameter             :: nofPipUnique = 10
    integer ( is ), parameter             :: pipValueUnique ( 1 : nofPipUnique ) = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
    integer ( is ), parameter             :: pipDensity     ( 1 : nofPipUnique ) = [ 1, 1, 1, 1, 1, 1, 1, 1, 1,  4 ]

    character ( len = nofPip ), parameter :: pDescriptors = "A23456789TJQK"

    type, public :: pips
        integer ( is )                    :: index ( 1 : nofPip ) = pipTag
        integer ( is )                    :: value ( 1 : nofPip ) = pipValue
        character ( len = nofPip )        :: descriptor = pDescriptors
    end type pips

end module mPip