! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

module mSolnsLinear

    use mPrecisionDefinitions, only : ip, rp, one, zero
    use mData

    implicit none
    integer ( ip ), parameter     :: dof = 2

    type, extends ( data )        :: solution_linear
        integer ( ip )            :: n = dof
        real    ( rp )            :: solution ( 1 : dof )
        real    ( rp )            :: error    ( 1 : dof )
        real    ( rp )            :: SSE
        character ( len = 64 )    :: descriptor_soln = ''
        integer ( ip )            :: status = 0
        character ( len = 512 )   :: warning = ''
        contains
            private
!           subroutines
            !procedure, public     :: allocator2 => allocator2_sub
            !procedure, public     :: allocator1 => allocator1_sub
            procedure, public     :: normal_a   => normal_a_sub
    end type solution_linear

    type :: matrices
!       rank two
        real ( rp ), allocatable  :: A      ( : , : ), &
                                     AS     ( : , : ), &
                                     ASAinv ( : , : )
!       rank one
        real ( rp ), allocatable  :: ones   ( : )
    end type matrices 

    integer ( ip ), private          :: kSL = 0
    integer ( ip ), private          :: alloc_status  = -1, size_elements = 0, size_bytes = 0
    character ( len = 512 ), private :: alloc_message = 'null'

    logical                          :: fecho

    character ( len = * ), parameter          :: myType = ' real ( rp )'
    character ( len = * ), parameter, private :: myModule = 'module mSolnsLinear'  ! self-identification
    character ( len = * ), parameter, private :: parentSubroutine = 'subroutine normal_a_sub'

    !private :: allocator2_sub
    !private :: allocator1_sub
    private :: normal_a_sub

    contains

!       =============================================================================================                       normal_a

        subroutine normal_a_sub ( me, echo )

            class ( solution_linear ), target :: me
            logical, optional, intent ( in )  :: echo

            type ( matrices )                 :: matrix
            real ( rp )                       :: sX, sX2, sY, sXY, det
            logical                           :: fecho = .false.
            character ( len = * ), parameter  :: mySubroutine    = 'subroutine normal_a_sub'  ! self-identification
            character ( len = * ), parameter  :: callChain       = 'Call chain: ' // myModule // ', ' // mySubroutine // '.'
            character ( len = * ), parameter  :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter  :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                fecho = .false.
                if ( present ( echo ) ) fecho = echo
                print *, 'inside normal_a'
                print *, 'me % sse = ', me % sse
                print *, 'me % n = ', me % n
!               allocations
                if ( fecho ) write ( * , '( "allocating matrices" )' )
                !print *, 'me % m = ', me % m, ', me % n = ', me % n

                call allocator2_sub ( matrix % A,      me % m, me % n, fecho )
                print *, 'A allocated'
                call allocator2_sub ( matrix % AS,     me % n, me % m, fecho )
                print *, 'AS allocated'
                call allocator2_sub ( matrix % ASAinv, me % m, me % n, fecho )
                print *, 'ASAinv allocated'
                call allocator1_sub ( matrix % ones,   me % m,         fecho )
                print *, 'ones allocated'

!               construct basics
                print *, 'construct basics'
                matrix % ones = [ ( 1, kSL = 1, me % m ) ]
                matrix % A    = reshape ( [ matrix % ones, me % x ], [ me % m, me % m ] )
                matrix % AS   = transpose ( matrix % A )

!               construct intermediate products
                sX  = dot_product ( matrix % ones, me % x ( : ) )
                sX2 = dot_product ( me % x ( : ),  me % x ( : ) )
                sY  = dot_product ( matrix % ones, me % y ( : ) )
                sXY = dot_product ( me % x ( : ),  me % y ( : ) )
                det = me % m * sX2 - sX ** 2

!               quality checks on determinant
                me % status = 0
                if ( det == zero ) then
                    me % status = -2
                    write ( * , 100 )
                    stop error_fatal
                end if

                if ( abs ( det ) <= epsilon ( one ) ) then
                    me % status = -1
                    write ( me % warning, 110 ) 'Dangerously ', det
                    write ( *           , 120 ) error_not_fatal
                    else if ( abs ( det ) <= 5 * epsilon ( one ) ) then
                        me % status = -1
                        write ( me % warning, 110 ) 'Suspiciously ', det
                        write ( *           , 120 ) error_not_fatal
                    end if
                    !end if

                return

  100           format ( /, 'Determinant value = 0.', / )
  110           format ( /, g0, 'small determinant = ', g0, ', machine epsilon = ', g0, '.', / )
  120           format ( /, g0, / )

        end subroutine normal_a_sub

!       =============================================================================================                   allocate_sub

        subroutine allocator2_sub ( array, m, n, echo )

            !class ( solution_linear ), target        :: me

            real ( rp ), allocatable, intent ( inout ) :: array ( : , : )
            integer ( ip ),           intent ( in )  :: m, n
            logical, optional,        intent ( in )  :: echo

            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator2'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                          // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            !character ( len = * ), parameter         :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                fecho = .false.
                if ( present ( echo ) ) fecho = echo

                print *, 'inside allocator2'
                print *, 'allocated ( array ) = ', allocated ( array )
                print *, 'm = ', m, 'n = ', m
!               allocate array
                allocate ( array ( 1 : m,  1 : n ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) m, n, myType
                    write ( *, 110 ) alloc_status
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

!               populate array
                array ( : , : )= one

                if ( fecho ) then
                    size_elements = size   ( array )
                    size_bytes    = sizeof ( array )
                    write ( * , 200 ) m * n, myType
                    write ( * , 210 ) size_elements, size_bytes
                end if

                return

    100         format ( /, 'Error allocating memory for ', g0, ' x ', g0, ' array of type ', g0,'.' )
    110         format (    '  stat = ', g0, '.' )
    120         format (    '  errmsg = ', g0, '.' )

    200         format ( /, 'Successful allocation of ', g0, g0, ' elements:' )
    210         format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        return

        end subroutine allocator2_sub

!       =============================================================================================                     allocator1

        subroutine allocator1_sub ( array, m, echo )

            !class ( solution_linear ), target        :: me

            real ( rp ), allocatable, intent ( out ) :: array ( : )
            integer ( ip ),           intent ( in )  :: m
            logical, optional,        intent ( in )  :: echo

            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator1'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                           // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            !character ( len = * ), parameter         :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                fecho = .false.
                if ( present ( echo ) ) fecho = echo

!               allocate array
                allocate ( array ( 1 : m ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) m, myType
                    write ( *, 110 ) alloc_status
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

    100         format ( /, 'Error allocating memory for ', g0, ' array of type ', g0,'.' )
    110         format (    '  stat = ', g0, '.' )
    120         format (    '  errmsg = ', g0, '.' )

    200         format ( /, 'Successful allocation of ', g0, ' elements:' )
    210         format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        return

        end subroutine allocator1_sub

end module mSolnsLinear
