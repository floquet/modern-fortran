program archeomult

  implicit none

  integer  :: rows = 0, columns = 0, p = 0  ! size variables
  integer  :: r = 0, c = 0, k = 0           ! dummy variables
  integer  :: alloc_status = 0

  integer, allocatable, dimension( : , : ) :: A, B, D

  character ( len = 255 ) :: alloc_msg

!   dimensions( A ) = rows x columns
!   dimensions( B ) = rows x p
!   dimensions( C ) = p    x columns
    rows    = 3;
    columns = 4;
    p       = 5;

!   ALLOCATE
    allocate ( A( 1 : rows, 1 : columns), stat = alloc_status, errmsg = alloc_msg )
    call alloc_alert ( alloc_status, alloc_msg, rows, columns, "" )

    allocate ( B( 1 : rows, 1 : p), stat = alloc_status, errmsg = alloc_msg )
    call alloc_alert ( alloc_status, alloc_msg, rows, p, "" )

    allocate ( D( 1 : p, 1 : columns), stat = alloc_status, errmsg = alloc_msg )
    call alloc_alert ( alloc_status, alloc_msg, p, columns, "" )

!   POPULATE
    B( 1, : ) = [ 0,2,5,0,3 ]
    B( 2, : ) = [ 1,2,5,2,3 ]
    B( 3, : ) = [ 2,5,3,5,0 ]

    D( 1, : ) = [ 0,1,2,1 ]
    D( 2, : ) = [ 4,0,5,4 ]
    D( 3, : ) = [ 4,5,0,0 ]
    D( 4, : ) = [ 4,1,1,3 ]
    D( 5, : ) = [ 2,0,2,3 ]

!   multiply
    A = 0
    do 10 r = 1, rows
      do 10 c = 1, columns
        do 10 k = 1, p
          A( r, c ) = A( r, c ) + B( r, k ) * D( k, c )
 10 end do

    print *, 'A( 1, : ) = ', A( 1, : )
    print *, 'A( 2, : ) = ', A( 2, : )
    print *, 'A( 3, : ) = ', A( 3, : )

!   DEALLOCATE
    deallocate ( A, stat = alloc_status, errmsg = alloc_msg )
    call alloc_alert ( alloc_status, alloc_msg, rows, columns, "de" )

    deallocate ( B, stat = alloc_status, errmsg = alloc_msg )
    call alloc_alert ( alloc_status, alloc_msg, rows, p, "de" )

    deallocate ( D, stat = alloc_status, errmsg = alloc_msg )
    call alloc_alert ( alloc_status, alloc_msg, p, columns, "de" )

end program archeomult

!  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  **  !

subroutine alloc_alert ( alloc_status, alloc_msg, rows, columns, de )  !                                   ALLOCATION ERROR HANDLING

!  use basic_parameters
  implicit none

  integer,               intent ( in ) :: alloc_status, rows, columns
  character ( len = * ), intent ( in ) :: alloc_msg, de

  integer                              :: io_status
  character ( len = 255 )              :: io_msg

    if ( alloc_status /= 0 ) then
      write ( *, 110 ) de, rows, columns
      write ( *, 120 ) de, alloc_status
      write ( *, 130, iostat = io_status, iomsg = io_msg ) alloc_msg
    else
      write ( *, 140, iostat = io_status, iomsg = io_msg ) de, rows, columns
    end if

110 format( "failure to ", A, "allocate real array ( 1 : ", g0, ", 1 : ", g0, " )" )
120 format( A, "allocation status variable = ", g0, "." )
130 format( "error message: ", A, "." )
140 format( "successful memory ", A, "allocation ( 1 : ", g0, ", 1 : ", g0, " )" )

end subroutine alloc_alert