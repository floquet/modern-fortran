! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 26

module mSolnsLinear

    use mPrecisionDefinitions, only : ip, rp, one, zero
    use mData
    use mIntermediates

    implicit none
    integer ( ip ), parameter     :: dof = 2

    type, extends ( data )        :: solution_linear
        integer ( ip )            :: n = dof
        integer ( ip )            :: status = 0
        real    ( rp )            :: solution ( 1 : dof )
        real    ( rp )            :: error    ( 1 : dof )
        real    ( rp )            :: sse
        real ( rp ), allocatable  :: ones ( : )
        character ( len = 64 )    :: descriptor_soln = ''
        character ( len = 512 )   :: warning = ''
        real ( rp )               :: cpu_seconds_compute
        contains
            private
!           subroutines
            procedure, public     :: normal_a                      => normal_a_sub
            procedure, public     :: compute_intermediates_a       => compute_intermediates_a_sub
            procedure, public     :: compute_intermediates_b       => compute_intermediates_b_sub
            procedure, public     :: compute_intermediates_weights => compute_intermediates_weights_sub
    end type solution_linear

    type                         :: matrices
        real ( rp ), allocatable :: A      ( : , : ), &
                                    AS     ( : , : ), &
                                    ASAinv ( : , : )
    end type matrices 

    integer ( ip ), private          :: kSL = 0
    integer ( ip ), private          :: alloc_status  = -1, size_elements = 0, size_bytes = 0
    character ( len = 512 ), private :: alloc_message = 'null'

    logical                          :: fecho

    character ( len = * ), parameter          :: myType = ' real ( rp )'
    character ( len = * ), parameter, private :: myModule = 'module mSolnsLinear'  ! self-identification
    character ( len = * ), parameter, private :: parentSubroutine = 'subroutine normal_a_sub'

    private :: normal_a_sub
    private :: compute_intermediates_a_sub
    private :: compute_intermediates_b_sub
    private :: compute_intermediates_weights_sub

    private :: allocator2_sub
    private :: allocator1_sub

    contains

!       =============================================================================================        compute_intermediates_a

        subroutine compute_intermediates_a_sub ( me, ints )

            class ( solution_linear ), target      :: me
            type ( intermediates ), intent ( out ) :: ints

                me % ones = [ ( one, kSL = 1, me % m ) ]
                ints % em  = dot_product ( me % ones,     me % ones )
                ints % sX  = dot_product ( me % ones,     me % x ( : ) )
                ints % sY  = dot_product ( me % ones,     me % y ( : ) )
                ints % sX2 = dot_product ( me % x ( : ),  me % x ( : ) )
                ints % sXY = dot_product ( me % x ( : ),  me % y ( : ) )
                ints % det = ints % em * ints % sX2 - ints % sX ** 2

!                 print *, 'em  = ', ints % em
!                 print *, 'det = ', ints % det
!                 print *, 'sX  = ', ints % sX
!                 print *, 'sX2 = ', ints % sX2

        end subroutine compute_intermediates_a_sub

!       =============================================================================================        compute_intermediates_b

        subroutine compute_intermediates_b_sub ( me, ints )

            class ( solution_linear ), target      :: me
            type ( intermediates ), intent ( out ) :: ints

                me % ones = [ ( one, kSL = 1, me % m ) ]
                ints % em  = sum ( me % ones )
                ints % sX  = sum ( me % x ( : ) )
                ints % sY  = sum ( me % y ( : ) )
                ints % sX2 = sum ( me % x ( : ) * me % x ( : ) )
                ints % sXY = sum ( me % x ( : ) * me % y ( : ) )
                ints % det = ints % em * ints % sX2 - ints % sX ** 2

                print *, 'em  = ', ints % em
                print *, 'det = ', ints % det
                print *, 'sX  = ', ints % sX
                print *, 'sX2 = ', ints % sX2

        end subroutine compute_intermediates_b_sub

!       =============================================================================================   compute_intermediates_weights

        subroutine compute_intermediates_weights_sub ( me, ints )

            class ( solution_linear ), target      :: me
            type ( intermediates ), intent ( out ) :: ints

                ints % em  = sum ( me % ones / me % y ( : ) )
                ints % sX  = sum ( me % x ( : ) / me % y ( : ) )
                ints % sY  = sum ( me % y ( : ) )
                ints % sX2 = sum ( me % x ( : ) * me % x ( : ) / me % y ( : ) )
                ints % sXY = sum ( me % x ( : ) * me % y ( : ) )
                ints % det = ints % em * ints % sX2 - ints % sX ** 2

                print *, 'em  = ', ints % em
                print *, 'det = ', ints % det
                print *, 'sX  = ', ints % sX
                print *, 'sX2 = ', ints % sX2

        end subroutine compute_intermediates_weights_sub

!       =============================================================================================                       normal_a

        subroutine normal_a_sub ( me, echo, who )

            class ( solution_linear ), target    :: me
            logical, optional, intent ( in )     :: echo
            character ( len = 1 ), intent ( in ) :: who

            type ( matrices )                    :: matrix
            type ( intermediates )               :: ints
            real ( rp )                          :: col1 ( 1 : dof ), col2 ( 1 : dof )  ! column vectors
            real ( rp )                          :: cpu_start, cpu_stop
            logical                              :: fecho = .false.
            character ( len = * ), parameter     :: mySubroutine    = 'subroutine normal_a_sub'  ! self-identification
            character ( len = * ), parameter     :: callChain       = 'Call chain: ' // myModule // ', ' // mySubroutine // '.'
            character ( len = * ), parameter     :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter     :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                call cpu_time ( cpu_start ) 
                fecho = .false.
                if ( present ( echo ) ) fecho = echo
!               allocations
                if ( fecho ) write ( * , '( "allocating matrices" )' )

                call allocator2_sub ( matrix % A,      me % m, dof, fecho )
                call allocator2_sub ( matrix % AS,     dof, me % m, fecho )
                call allocator2_sub ( matrix % ASAinv, me % m, dof, fecho )
                call allocator1_sub ( me     % ones,   me % m,      fecho )

!               construct basics
                matrix % A  = reshape ( [ me % ones, me % x ], [ me % m, dof ] )
                matrix % AS = transpose ( matrix % A )

!               construct intermediate products
                if ( who == 'a' ) then
                    print *, 'intermediates via dot products...'
                    call me % compute_intermediates_a ( ints )
                    else if ( who == 'b' ) then
                        print *, 'intermediates via sums...'
                        call me % compute_intermediates_b ( ints )
                    else
                        print *, 'intermediates with weights...'
                        call me % compute_intermediates_weights ( ints )
                        end if

!               quality checks on determinant
                me % status = 0
                if ( ints % det == zero ) then
                    me % status = -2
                    write ( * , 100 )
                    stop error_fatal
                end if

                if ( abs ( ints % det ) <= epsilon ( one ) ) then
                    me % status = -1
                    write ( me % warning, 110 ) 'Dangerously ', ints % det
                    write ( *           , 120 ) error_not_fatal
                    else if ( abs ( ints % det ) <= 5 * epsilon ( one ) ) then
                        me % status = -1
                        write ( me % warning, 110 ) 'Suspiciously ', ints % det
                        write ( *           , 120 ) error_not_fatal
                    end if

                col1 = [  ints % sX2, -ints % sX ]
                col2 = [ -ints % sX,   ints % em ]
                matrix % ASAinv = reshape ( [ col1, col2 ], [ dof, dof ] ) / ints % det

                print *, 'matrix % ASAinv = ', matrix % ASAinv
!               solve linear system
                me % solution  ( : ) = matmul ( matrix % ASAinv, matmul ( matrix % AS, me % y ( : ) ) )
                me % residuals ( : ) = matmul ( matrix % A, me % solution ) - me % y ( : )
!               error terms
                me % sse = dot_product ( me % residuals, me % residuals )

                me % error = [ ints % sX2, ints % em ]
                me % error = sqrt ( me % error * me % sse / ( me % m - dof ) / ints % det )
                print *, 'me % error = ', me % error
                call cpu_time ( cpu_stop )
                me % cpu_seconds_compute = cpu_stop - cpu_start

                return

  100           format ( /, 'Determinant value = 0.', / )
  110           format ( /, g0, 'small determinant = ', g0, ', machine epsilon = ', g0, '.', / )
  120           format ( /, g0, / )

        end subroutine normal_a_sub

!       =============================================================================================                       normal_a

        subroutine weights ( me, echo )

            class ( solution_linear ), target :: me
            logical, optional, intent ( in )  :: echo

            type ( matrices )                 :: matrix
            type ( intermediates )            :: ints
            real ( rp )                       :: col1 ( 1 : dof ), col2 ( 1 : dof )  ! column vectors
            real ( rp )                       :: cpu_start, cpu_stop
            logical                           :: fecho = .false.
            character ( len = * ), parameter  :: mySubroutine    = 'subroutine weights'  ! self-identification
            character ( len = * ), parameter  :: callChain       = 'Call chain: ' // myModule // ', ' // mySubroutine // '.'
            character ( len = * ), parameter  :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter  :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

                call cpu_time ( cpu_start ) 
                fecho = .false.
                if ( present ( echo ) ) fecho = echo
!               allocations
                if ( fecho ) write ( * , '( "allocating matrices" )' )

                call allocator2_sub ( matrix % A,      me % m, dof, fecho )
                call allocator2_sub ( matrix % AS,     dof, me % m, fecho )
                call allocator2_sub ( matrix % ASAinv, me % m, dof, fecho )
                call allocator1_sub ( me     % ones,   me % m,      fecho )

!               construct basics
                matrix % A  = reshape ( [ me % ones, me % x ], [ me % m, dof ] )
                matrix % AS = transpose ( matrix % A )

!               construct intermediate products
                call me % compute_intermediates_weights ( ints )

!               quality checks on determinant
                me % status = 0
                if ( ints % det == zero ) then
                    me % status = -2
                    write ( * , 100 )
                    stop error_fatal
                end if

                if ( abs ( ints % det ) <= epsilon ( one ) ) then
                    me % status = -1
                    write ( me % warning, 110 ) 'Dangerously ', ints % det
                    write ( *           , 120 ) error_not_fatal
                    else if ( abs ( ints % det ) <= 5 * epsilon ( one ) ) then
                        me % status = -1
                        write ( me % warning, 110 ) 'Suspiciously ', ints % det
                        write ( *           , 120 ) error_not_fatal
                    end if

                col1 = [  ints % sX2, -ints % sX ]
                col2 = [ -ints % sX,   ints % em ]
                matrix % ASAinv = reshape ( [ col1, col2 ], [ dof, dof ] ) / ints % det

                print *, 'matrix % ASAinv = ', matrix % ASAinv
!               solve linear system
                me % solution  ( : ) = matmul ( matrix % ASAinv, matmul ( matrix % AS, me % y ( : ) ) )
                me % residuals ( : ) = matmul ( matrix % A, me % solution ) - me % y ( : )
!               error terms
                me % sse = dot_product ( me % residuals, me % residuals )

                me % error = [ ints % sX2, ints % em ]
                me % error = sqrt ( me % error * me % sse / ( me % m - dof ) / ints % det )
                print *, 'me % error = ', me % error
                call cpu_time ( cpu_stop )
                me % cpu_seconds_compute = cpu_stop - cpu_start

                return

  100           format ( /, 'Determinant value = 0.', / )
  110           format ( /, g0, 'small determinant = ', g0, ', machine epsilon = ', g0, '.', / )
  120           format ( /, g0, / )

        end subroutine weights

!       =============================================================================================                   allocate_sub

        subroutine allocator2_sub ( array, m, n, echo )

            real ( rp ), allocatable, intent ( out ) :: array ( : , : )
            integer ( ip ),           intent ( in )  :: m, n
            logical, optional,        intent ( in )  :: echo

            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator2'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                          // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain

                fecho = .false.
                if ( present ( echo ) ) fecho = echo

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
                    write ( * , 200 ) m, n, myType
                    write ( * , 210 ) size_elements, size_bytes
                end if

                return

    100         format ( /, 'Error allocating memory for ', g0, ' x ', g0, ' array of type ', g0,'.' )
    110         format (    '  stat = ', g0, '.' )
    120         format (    '  errmsg = ', g0, '.' )

    200         format ( /, 'Successful allocation of ', g0, ' x ', g0, ' array of ', g0,' elements:' )
    210         format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        return

        end subroutine allocator2_sub

!       =============================================================================================                     allocator1

        subroutine allocator1_sub ( array, m, echo )

            real ( rp ), allocatable, intent ( out ) :: array ( : )
            integer ( ip ),           intent ( in )  :: m
            logical, optional,        intent ( in )  :: echo

            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator1'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                           // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain

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
