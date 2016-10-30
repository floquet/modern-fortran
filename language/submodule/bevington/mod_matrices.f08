! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 30

!                                                      V  you are here  V

! data vectors ( 1, x, y ) -> intermediate sums -> matrices ( A, AT, ATAinv ) -> solution ( slope, intercept )

module mMatrices

    use mPrecisionDefinitions,  only : rp
    use mParameters,            only : zero, one
    use mIntermediates

    implicit none

    type                         :: matrices
        real ( rp ), allocatable :: A      ( : , : ), &
                                    AS     ( : , : ), &
                                    ASAinv ( : , : )
    contains
        private
        ! subroutines
        procedure, nopass, public :: allocator_rank_1   => allocator_rank_1_sub
        procedure, nopass, public :: allocator_rank_2   => allocator_rank_2_sub
        procedure, public         :: construct_matrices => construct_matrices_sub
    end type matrices

    integer ( ip ), private          :: kMat = 0
    integer ( ip ), private          :: alloc_status  = -1
    character ( len = 512 ), private :: alloc_message = 'null'

    character ( len = * ), parameter, private :: myType = 'real ( rp )'
    character ( len = * ), parameter, private :: myModule = 'module mMatrices'  ! self-identification
    character ( len = * ), parameter, private :: parentSubroutine = 'subroutine construct_matrices'

    private :: allocator_rank_1_sub
    private :: allocator_rank_2_sub
    private :: construct_matrices_sub

    contains

!       =============================================================================================             construct_matrices

        subroutine construct_matrices_sub ( me, ints, mydof, measure, echo )

            class ( matrices ), target               :: me
            type ( intermediates ), intent ( in )    :: ints
            type ( measurements ),  intent ( inout ) :: measure
            integer ( ip ),         intent ( in )    :: mydof
            logical, optional,      intent ( in )    :: echo

            integer ( ip )                           :: m
            logical                                  :: lecho

            real ( rp )                              :: cpu_start, cpu_stop

                call cpu_time ( cpu_start )
                    lecho = .false.
                    if ( present ( echo ) ) lecho = echo
                    if ( lecho ) write ( * , '( /, "allocating matrices..." )' )

                    m = measure % m
                    if ( lecho ) write ( * , '( "allocating A" )' )
                    call allocator_rank_2_sub ( me % A,      m,     mydof, echo = .true. )
                    if ( lecho ) write ( * , '( "allocating As" )' )
                    call allocator_rank_2_sub ( me % AS,     mydof, m,     echo = .true. )
                    if ( lecho ) write ( * , '( "allocating AsAinv" )' )
                    call allocator_rank_2_sub ( me % ASAinv, mydof, mydof, echo = .true. )

                    ! construct basics
                    if ( lecho ) write ( * , '( /, "constructing matrices..." )' )
                    me % A ( : , 1 ) = measure % ones
                    me % A ( : , 2 ) = measure % x
                    me % AS = transpose ( me % A )

                    ! construct inverse
                    call ints % compute_intermediates ( measure )
                    me % ASAinv ( : , 1 ) = [  ints % sX2, -ints % sX ]
                    me % ASAinv ( : , 2 ) = [ -ints % sX,   ints % em ]
                    me % ASAinv           = me % ASAinv / ints % det

                call cpu_time ( cpu_stop )
                if ( lecho ) write ( * , '( "time for contructing matrices = ", E9.3, "s" )' ) cpu_stop - cpu_start

                return

        end subroutine construct_matrices_sub

!       =============================================================================================               allocator_rank_2

        subroutine allocator_rank_2_sub ( array, rows, cols, echo )

            real ( rp ), allocatable, intent ( out ) :: array ( : , : )
            integer ( ip ),           intent ( in )  :: rows, cols
            logical, optional,        intent ( in )  :: echo

            logical                                  :: lecho

            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator_rank_2'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                          // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain

                lecho = .false.
                if ( present ( echo ) ) lecho = echo

                ! allocate array
                allocate ( array ( 1 : rows,  1 : cols ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) rows, cols, myType
                    write ( *, 110 ) alloc_status
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

                array ( : , : )= one ! populate array

                ! echo print
                if ( lecho ) then
                    write ( * , 200 ) rows, cols, myType
                    write ( * , 210 ) size ( array ), sizeof ( array )
                end if

                return

            100 format ( /, 'Error allocating memory for ', g0, ' x ', g0, ' array of type ', g0,'.' )
            110 format (    '  stat = ', g0, '.' )
            120 format (    '  errmsg = ', g0, '.' )

            200 format ( /, 'Successful allocation of ', g0, ' x ', g0, ' array of ', g0,' elements:' )
            210 format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        end subroutine allocator_rank_2_sub

!       =============================================================================================               allocator_rank_1

        subroutine allocator_rank_1_sub ( array, rows, echo )

            real ( rp ), allocatable, intent ( out ) :: array ( : )
            integer ( ip ),           intent ( in )  :: rows
            logical, optional,        intent ( in )  :: echo

            logical                                  :: lecho

            character ( len = * ), parameter         :: mySubroutine    = 'subroutine allocator_rank_1'  ! self-identification
            character ( len = * ), parameter         :: callChain       = 'Call chain: ' // myModule // ', ' &
                                                                           // parentSubroutine // ', ' // mySubroutine // '.'
            character ( len = * ), parameter         :: error_fatal     = 'Fatal error; execution halting. ' // callChain

                lecho = .false.
                if ( present ( echo ) ) lecho = echo

                ! allocate array
                allocate ( array ( 1 : rows ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) rows, myType
                    write ( *, 110 ) alloc_status
                    write ( *, 120 ) trim ( alloc_message )
                    stop error_fatal
                end if

                ! populate array
                do concurrent ( kMat = 1 : rows )
                    array ( kMat ) = one
                end do

                ! echo print
                if ( lecho ) then
                    write ( * , 200 ) rows, myType
                    write ( * , 210 ) size   ( array ), sizeof ( array )
                end if

                return

            100 format ( /, 'Error allocating memory for ', g0, ' array of type ', g0,'.' )
            110 format (    '  stat = ', g0, '.' )
            120 format (    '  errmsg = ', g0, '.' )

            200 format ( /, 'Successful allocation of ', g0, ' elements:' )
            210 format (    'size = ', g0, ' elements, sizeof = ', g0, ' bytes' )

        end subroutine allocator_rank_1_sub

end module mMatrices
