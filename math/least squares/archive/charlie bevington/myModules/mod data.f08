! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

module mData

    use mPrecisionDefinitions, only : ip, rp, one
    implicit none

    type                         :: data
        integer ( ip )           :: m
        real ( rp ), allocatable :: x ( : ), y ( : ), residuals ( : )
        character ( len = 64 )   :: descriptor_64 = ''
        real ( rp )              :: cpu_seconds_allocate
    contains
        private
!       subroutines
        procedure, public :: allocate_group => allocate_group_sub
    end type data

    integer ( ip ), private          :: kData = 0
    integer ( ip ), private          :: alloc_status  = -1, size_elements = 0, size_bytes = 0
    character ( len = 512 ), private :: alloc_message = 'null'

    character ( len = * ), parameter, private :: myModule = 'module mData'  ! self-identification

    private :: allocate_group_sub
    private :: allocate_individuals_sub

    contains

!       =============================================================================================           allocate_individuals

        subroutine allocate_individuals_sub ( me, array, echo )

            class ( data ), target                     :: me

            real ( rp ), allocatable, intent ( inout ) :: array ( : )
            logical, optional,        intent ( in )    :: echo

            real ( rp )                                :: cpu_start, cpu_stop

            character ( len = * ), parameter :: mySubroutine     = 'subroutine allocate_individuals_sub'  ! self-identification
            character ( len = * ), parameter :: parentSubroutine = 'subroutine allocate_group_sub'  ! self-identification
            character ( len = * ), parameter :: callChain        = 'Call chain: ' // myModule // ', ' &
                                                                   // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter :: error_fatal      = 'Fatal error; execution halting. ' // callChain

!               deallocate if needed
                call cpu_time ( cpu_start ) 
                if ( allocated ( array ) ) then
                    deallocate ( array, stat = alloc_status, errmsg = alloc_message )
                    if ( alloc_status /= 0 ) then
                        write ( *, 100 ) 'de', me % m, ' real ( rp )'
                        write ( *, 110 ) alloc_status
                        write ( *, 120 ) trim ( alloc_message )
                        stop error_fatal
                    end if
                end if

!               allocate array
                allocate ( array ( 1 : me % m ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) '', me % m, ' real ( rp )'
                    write ( *, 110 ) alloc_status
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

!               concurrent population of array
                do concurrent ( kData = 1 : me % m )
                    array ( kData ) = one
                end do

                call cpu_time ( cpu_stop ) 
                me % cpu_seconds_allocate = cpu_stop - cpu_start

                if ( present ( echo ) ) then
                    if ( echo .eqv. .true. ) then
                        size_elements = size   ( array )
                        size_bytes    = sizeof ( array )
                        write ( * , 200 ) me % m, ' real ( rp )'
                        write ( * , 210 ) size_elements, size_bytes
                    end if
                end if

                return

    100         format ( /, 'Error ', g0,'allocating memory for ', g0, g0, ' elements.' )
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

                call allocate_individuals_sub ( me, me % x,         echo )
                call allocate_individuals_sub ( me, me % y,         echo )
                call allocate_individuals_sub ( me, me % residuals, echo )

                return

        end subroutine allocate_group_sub

end module mData
