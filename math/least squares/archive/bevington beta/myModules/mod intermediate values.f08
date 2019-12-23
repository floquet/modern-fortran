! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 24

!   subroutine allocator ( numPts, echo )

module mAllocator

    use mPrecisionDefinitions, only : ip, rp, zero, one
    use mFileInquire

    implicit none

    integer ( ip ), private            :: kDemo = 0
    integer ( ip ), private            :: status_alloc = -1
    integer ( ip ), parameter, private :: numPts = 10

    character ( len = 512 ), private   :: msg_alloc = 'null'

    real ( rp ), private, allocatable  :: identity_matrix ( : , : )

    character ( len = * ), parameter, private :: myModule = 'module mAllocator'  ! self-identification
    character ( len = * ), parameter, private :: type_ip  = 'integer ( ip )'
    character ( len = * ), parameter, private :: type_rp  = 'real ( rp )'

    contains                                                                                    ! methods: subroutines and functions

!       ===============================================================================================                    allocator

        subroutine allocator ( numPts, echo )

            integer ( ip ),    intent ( in ) :: numPts
            logical, optional, intent ( in ) :: echo

            integer ( ip )                   :: size_elements = 0
            integer ( ip )                   :: size_bytes    = 0

            character ( len = * ), parameter :: myName          = 'identity_matrix'
            character ( len = * ), parameter :: mySubroutine    = 'subroutine allocator'  ! self-identification
            character ( len = * ), parameter :: callChain       = 'Call chain: ' // myModule // ', ' // mySubroutine // '.'
            character ( len = * ), parameter :: error_fatal     = 'Fatal error; execution halting. ' // callChain
            character ( len = * ), parameter :: error_not_fatal = 'Nonfatal error; execution continuing. ' // callChain

!               allocate identity matrix
!               https://stackoverflow.com/questions/3708307/how-to-initialize-two-dimensional-arrays-in-fortran
                allocate ( identity_matrix ( 1 : numPts, 1 : numPts ), stat = status_alloc, errmsg = msg_alloc )
                if ( status_alloc /= 0 ) then
                    write ( *, 100 ) '', numPts, numPts, ' real ( rp )', myName
                    write ( *, 110 ) numPts * numPts
                    write ( *, 130 ) status_alloc
                    write ( *, 140 ) trim ( msg_alloc )
                    stop error_fatal
                end if
                identity_matrix ( : , : ) = zero

!               concurrent population of identity matrix
                do concurrent ( kDemo = 1 : numPts )
                    identity_matrix ( kDemo, kDemo ) = one
                end do

                if ( present ( echo ) ) then
                    if ( echo .eqv. .true. ) then
                        size_elements = size   ( identity_matrix )
                        size_bytes    = sizeof ( identity_matrix )
                        write ( * , 200 ) '', numPts, numPts, ' real ( rp )', myName
                        write ( * , 210 ) myName, size_elements, myName, size_bytes
                    end if
                end if

!               deallocate identity matrix
                deallocate ( identity_matrix, stat = status_alloc, errmsg = msg_alloc )
                size_elements = size   ( identity_matrix )
                size_bytes    = sizeof ( identity_matrix )
                if ( status_alloc /= 0 ) then
                    write ( *, 100 ) 'de', numPts, numPts, ' real ( rp )', myName
                    write ( *, 110 ) numPts * numPts
                    write ( *, 120 ) size_elements
                    write ( *, 130 ) status_alloc
                    write ( *, 140 ) trim ( msg_alloc )
                    write ( *, 150 ) error_not_fatal
                end if

                if ( present ( echo ) ) then
                    if ( echo .eqv. .true. ) then
                        write ( * , 200 ) 'de', numPts, numPts, ' real ( rp )', myName
                        write ( * , 210 ) myName, size_elements, myName, size_bytes
                    end if
                end if

                return

    100         format ( /, 'Error ', g0, 'allocating memory for ', g0, ' x ', g0, g0, ' array ', g0, '.' )
    110         format (    '  requested size ip ', g0, ' elements' )
    120         format (    '  measured size ip  ', g0, ' elements' )
    130         format (    '  stat = ', g0 )
    140         format (    '  errmsg = ', g0, '.' )
    150         format ( g0 )

    200         format ( /, 'Successful ', g0, 'allocation of ', g0, ' x ', g0, g0, ' array ', g0, '.' )
    210         format (    'size ( ', g0, ' ) = ', g0, ' elements, sizeof ( ', g0, ' ) = ', g0, ' bytes' )

        end subroutine allocator

    end module mAllocator