! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 25

module mSolnsLinear

    use mPrecisionDefinitions, only : ip, rp, one
    implicit none

    type, extends ( data )        :: solution_linear
        integer ( ip ), parameter :: n = 2
        real ( rp )               :: solution ( )
        real ( rp )               :: error
        real ( rp )               :: SSE
        character ( len = 64 )    :: descriptor_soln = ''
        integer ( ip )            :: status = 0
        character ( len = 512 )   :: warning = ''
        contains
            private
!           subroutines
            procedure, public     :: allocate_individuals => allocate_individuals_sub
            procedure, public     :: allocate_group       => allocate_group_sub
            procedure, public     :: normal_a             => normal_a_sub
    end type solution

    type :: matrices
!       rank two
        real ( rp ), allocatable :: A      ( : , : ), &
                                    AS     ( : , : ), &
                                    ASAinv ( : , : )
!       rank one
        real ( rp ), allocatable :: ones   ( : )
    end type matrices 

    integer ( ip ), private          :: kSL = 0
    integer ( ip ), private          :: alloc_status  = -1, size_elements = 0, size_bytes = 0
    character ( len = 512 ), private :: alloc_message = 'null'

    character ( len = * ), parameter :: myModule = 'module mSolnsLinear'  ! self-identification

    private :: allocate_individuals_sub
    private :: allocate_group_sub
    private :: normal_a_sub

    contains

!       =============================================================================================                       normal_a

        subroutine normal_a_sub ( me, echo )

            class ( solution_linear ), target :: me
            logical, optional, intent ( in )  :: echo

            type ( matrices )                 :: matrix
            type ( rp )                       :: sX, sX2, sY, sXY, det
            logical                           :: fecho = .false.
            character ( len = * ), parameter  :: mySubroutine    = 'subroutine normal_a_sub'  ! self-identification
            character ( len = * ), parameter  :: callChain       = 'Call chain: ' // myModule // ', ' // mySubroutine // '.'
            character ( len = * ), parameter  :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter  :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                if ( present ( echo ) ) fecho = echo
!               allocations
                allocator2 ( matrix % A,      me % m, me % n, mySubroutine, fecho )
                allocator2 ( matrix % AS,     me % n, me % m, mySubroutine, fecho )
                allocator2 ( matrix % ASAinv, me % m, me % n, mySubroutine, fecho )
                allocator1 ( matrix % ones,   me % m,         mySubroutine, fecho )

!               construct basics
                matrix % ones = [ ( 1, kSL = 1, me % m ) ]
                matrix % A = [ ones, me % x ]
                matrix % AS = transpose ( A )

!               construct intermediate products
                sX  = dot_product ( ones,         me % x ( : ) )
                sX2 = dot_product ( x ( : ),      me % x ( : ) )
                sY  = dot_product ( ones,         me % y ( : ) )
                sXY = dot_product ( me % x ( : ), me % y ( : ) )
                det = me % m * sX2 - sX ** 2

!               quality checks on determinant
                me % status = 0
                if ( det == zero ) then
                    status = -2
                    write ( * , 100 )
                    stop error_fatal
                end if

                if ( abs ( det ) <= epsilon ( one ) ) then
                    status = -1
                    write ( me % warning, 110 ) 'Dangerously ', det
                    write ( *           , 120 ) error_not_fatal
                    else if ( abs ( det ) <= 5 * epsilon ( one ) ) then
                        status = -1
                        write ( me % warning, 110 ) 'Suspiciously ', det
                        write ( *           , 120 ) error_not_fatal
                    end if
                end if

                return

  100           format ( /, 'Determinant value = 0.', / )
  110           format ( /, g0, 'small determinant = ', g0, ', machine epsilon = ', g0, '.', / )
  120           format ( /, g0, / )

        end subroutine normal_a_sub

!       =============================================================================================                   allocate_sub

        subroutine allocator2 ( me, array, m, n, parentSubroutine, echo )

            class ( solution_linear ), target        :: me

            real ( rp ), allocatable, intent ( out ) :: array
            integer ( is ),           intent ( in )  :: m, n
            character ( len = * ),    intent ( in )  :: parentSubroutine
            logical, optional, intent ( in )         :: echo

            logical                                  :: fecho
            character ( len = * ), parameter         :: myType          = ' real ( rp )'
            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator2'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                          // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter         :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                if ( present ( echo ) ) fecho = echo

!               allocate array
                allocate ( array ( 1 : m,  1 : n ), stat = alloc_status, errmsg = alloc_message )
                if ( status_alloc /= 0 ) then
                    write ( *, 100 ) m, n, myType
                    write ( *, 110 ) status_alloc
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

!               populate array
                array = one

                if ( fecho ) then
                    size_elements = size   ( array )
                    size_bytes    = sizeof ( array )
                    write ( * , 200 ) m * n, myType
                    write ( * , 210 ) size_elements, size_bytes
                end if

                return

    100         format ( /, 'Error allocating memory for ', g0, ' x ', g0, ' array of type ', ,'.' )
    110         format (    '  stat = ', g0, '.' )
    120         format (    '  errmsg = ', g0, '.' )

    200         format ( /, 'Successful allocation of ', g0, g0, ' elements:' )
    210         format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        return

        end subroutine allocator2

!       =============================================================================================                     allocator1

        subroutine allocator1 ( me, array, m, parentSubroutine, echo )

            class ( solution_linear ), target        :: me

            real ( rp ), allocatable, intent ( out ) :: array
            integer ( is ),           intent ( in )  :: m
            character ( len = * ),    intent ( in )  :: parentSubroutine
            logical, optional, intent ( in )         :: echo

            logical                                  :: fecho
            character ( len = * ), parameter         :: myType          = ' real ( rp )'
            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator1'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                           // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter         :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                if ( present ( echo ) ) fecho = echo

!               allocate array
                allocate ( array ( 1 : m ), stat = alloc_status, errmsg = alloc_message )
                if ( status_alloc /= 0 ) then
                    write ( *, 100 ) m, myType
                    write ( *, 110 ) status_alloc
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

!               populate array
                do concurrent ( kSL = 1 : m )
                    array ( kSL ) = one
                end do

                if ( fecho ) then
                    size_elements = size   ( array )
                    size_bytes    = sizeof ( array )
                    write ( * , 200 ) m, myType
                    write ( * , 210 ) size_elements, size_bytes
                end if

                return

    100         format ( /, 'Error allocating memory for ', g0, ' array of type ', ,'.' )
    110         format (    '  stat = ', g0, '.' )
    120         format (    '  errmsg = ', g0, '.' )

    200         format ( /, 'Successful allocation of ', g0, ' elements:' )
    210         format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        return

        end subroutine allocator1

end module mSolnsLinear
