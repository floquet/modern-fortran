! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 25

module mData

    use mPrecisionDefinitions, only : ip, rp, one
    implicit none

    type                         :: data
        integer ( ip )           :: m
        real ( rp ), allocatable :: x ( : ), y ( : ), residuals ( : )
        character ( len = 64 )   :: descriptor_data = ''
        type ( solution )        :: mySolution
        contains
!           subroutines
            procedure, public    :: allocate               => allocate_sub
    end type data

    integer ( ip ), private          :: kData = 0
    integer ( ip ), private          :: alloc_status  = -1, size_elements = 0, size_bytes = 0
    character ( len = 512 ), private :: alloc_message = 'null'

    character ( len = * ), parameter :: myModule = 'module mData'  ! self-identification

    private :: allocate_sub

    contains

!       =============================================================================================                   allocate_sub

        subroutine allocate_individuals_sub ( me, array, echo, parentSubroutine )

            class ( data ), target                     :: me

            real ( rp ), allocatable, intent ( inout ) :: array
            logical, optional, intent ( in )           :: echo
            character ( len = * ), intent ( in )       :: parentSubroutine

            character ( len = * ), parameter :: mySubroutine    = 'subroutine allocate_individuals_sub'  ! self-identification
            character ( len = * ), parameter :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                   // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

!               allocate array
                allocate ( array ( 1 : me % m ), stat = alloc_status, errmsg = alloc_message )
                if ( status_alloc /= 0 ) then
                    write ( *, 100 ) me % m, ' real ( rp )'
                    write ( *, 110 ) status_alloc
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

!               concurrent population of array
                do concurrent ( kData = 1 : me % m )
                    array ( kData ) = one
                end do

                if ( present ( echo ) ) then
                    if ( echo .eqv. .true. ) then
                        size_elements = size   ( array )
                        size_bytes    = sizeof ( array )
                        write ( * , 200 ) me % m, ' real ( rp )'
                        write ( * , 210 ) size_elements, size_bytes
                    end if
                end if

                return

    100         format ( /, 'Error allocating memory for ', g0, g0, ' elements.' )
    110         format (    '  stat = ', g0 )
    120         format (    '  errmsg = ', g0, '.' )

    200         format ( /, 'Successful allocation of ', g0, g0, ' elements:' )
    210         format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        return

        end subroutine allocate_individuals_sub

!       =============================================================================================                 allocate_group

        subroutine allocate_group_sub ( me, echo )

            class ( data ), target           :: me
            logical, optional, intent ( in ) :: echo

            character ( len = * ), parameter :: mySubroutine    = 'subroutine allocate_group_sub'  ! self-identification

                call allocate_individuals_sub ( me % x,             echo, mySubroutine )
                call allocate_individuals_sub ( me % y,             echo, mySubroutine )
                call allocate_individuals_sub ( me % residuals,     echo, mySubroutine )

        return

        end subroutine allocate_group_sub

end module mData
