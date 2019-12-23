! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 28

!    V  you are here  V

! data vectors ( 1, x, y ) -> intermediate sums -> matrices ( A, AT, ATAinv ) -> solution ( slope, intercept )

module mMeasurements

    use mPrecisionDefinitions,  only : ip, rp, one
    use mParameters,            only : one
    implicit none

    type                         :: measurements
        integer ( ip )           :: m  ! number of measurements; controls allocation routines
        real ( rp ), allocatable :: x ( : ), y ( : )
        real ( rp ), allocatable :: ones ( : )
        real ( rp )              :: cpu_seconds_allocate
        character ( len = 64 )   :: descriptor_64
    contains
        private
!       subroutines
        procedure, public :: allocate_group      => allocate_group_sub
        procedure, public :: allocate_individual => allocate_individual_sub
    end type measurements

    integer ( ip ), private          :: kMeasure = 0
    integer ( ip ), private          :: alloc_status  = -1, size_elements = 0, size_bytes = 0
    character ( len = 512 ), private :: alloc_message = 'null'

    character ( len = * ), parameter, private :: myModule = 'module mMeasurements'  ! self-identification

    !logical, private                 :: lecho

    private :: allocate_group_sub
    private :: allocate_individual_sub

    contains

!       =============================================================================================            allocate_individual

        subroutine allocate_individual_sub ( me, array, echo )

            class ( measurements ), target             :: me

            real ( rp ), allocatable, intent ( inout ) :: array ( : )
            logical, optional,        intent ( in )    :: echo

            logical                                    :: lecho

            real ( rp )                                :: cpu_start, cpu_stop

            character ( len = * ), parameter :: mySubroutine     = 'subroutine allocate_individual_sub'  ! self-identification
            character ( len = * ), parameter :: parentSubroutine = 'subroutine allocate_group_sub'  ! self-identification
            character ( len = * ), parameter :: callChain        = 'Call chain: ' // myModule // ', ' &
                                                                   // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter :: error_fatal      = 'Fatal error; execution halting. ' // callChain

!               stopwatch
                call cpu_time ( cpu_start )
!                print *, 'allocate_individual_sub entry: lecho = ', lecho, ', echo = ', echo
                lecho = .false.
                if ( present ( echo ) ) then
!                    print *, 'a. echo = ', echo
                    lecho = echo
!                    print *, 'b. echo = ', echo
                end if
!                 print *, 'present ( echo ) = ', present ( echo ), ', echo = ', echo
!                 print *, 'allocate_individual_sub transfer: lecho = ', lecho, ', echo = ', echo
!               deallocate if needed
                if ( allocated ( array ) ) then
                    deallocate ( array, stat = alloc_status, errmsg = alloc_message )
                    if ( alloc_status /= 0 ) then
                        write ( *, 100 ) 'de', me % m, 'real ( rp )'
                        write ( *, 110 ) alloc_status
                        write ( *, 120 ) trim ( alloc_message )
                        stop error_fatal
                    end if
                end if

!               allocate array
                allocate ( array ( 1 : me % m ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) '', me % m, 'real ( rp )'
                    write ( *, 110 ) alloc_status
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

!               concurrent population of array
                do concurrent ( kMeasure = 1 : me % m )
                    array ( kMeasure ) = one
                end do

!               stopwatch
                call cpu_time ( cpu_stop )
                me % cpu_seconds_allocate = cpu_stop - cpu_start

!               echo print
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

        end subroutine allocate_individual_sub

!       =============================================================================================                 allocate_group

        subroutine allocate_group_sub ( me, echo )

            class ( measurements ), target   :: me
            logical, optional, intent ( in ) :: echo

            logical                          :: lecho

                lecho = .false.
                if ( present ( echo ) ) lecho = echo
!                print *, 'allocate_group_sub I: lecho = ', lecho, ', echo = ', echo

                if ( lecho ) write ( * , '( "allocating data array x" )' )
                call allocate_individual_sub ( me, me % x, lecho )
!                print *, 'allocate_group_sub II: lecho = ', lecho, ', echo = ', echo
                if ( lecho ) write ( * , '( "allocating data array y" )' )
                call allocate_individual_sub ( me, me % y, lecho )
                if ( lecho ) write ( * , '( "allocating data array ones" )' )
                call allocate_individual_sub ( me, me % ones, lecho )

                return

        end subroutine allocate_group_sub

    end module mMeasurements
