!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

character ( len = 30 ) function timestamp ( ) result ( now ) ! 2013-06-06  19:47:03  UCT-0600                             TIMESTAMP

  use data_types
  implicit none

  integer ( lint ), dimension ( 8 )      :: values  ! DTG ( date time group )

  ! characters
  character ( len =  8 )                 :: date    ! DTG
  character ( len = 10 )                 :: time    ! DTG
  character ( len =  5 )                 :: zone    ! DTG

!     timestamp
  call date_and_time ( date, time, zone, values )

      write  ( now, 100 )  date ( 1 : 4 ), date ( 5 : 6 ), date ( 7 : 8 ), &
                           time ( 1 : 2 ), time ( 3 : 4 ), time ( 5 : 6 ), "UCT", zone
 100  format ( a, "-", a, "-", a, 2X, a, ":", a, ":", a, 2X, a, a )

end function timestamp

!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

subroutine tags ( user_name, uid, pid, gid )  !                                                                                 TAGS

  use data_types
  implicit none

  integer   ( lint )                      :: getuid, getpid, getgid
  integer   ( lint ), intent ( out )      ::    uid,    pid,    gid

  character ( len =  64 ), intent ( out ) :: user_name

  call getlog  ( user_name )  ! moniker
  uid = getuid ( )            ! numerical user ID of the current process
  gid = getgid ( )            ! numerical group ID of the current process
  pid = getpid ( )            ! numerical process identifier of the current process

end subroutine tags

!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

subroutine admin ( dir_home, dir_working ) !                                                                                   ADMIN

  character ( len = 64 ), intent ( out ) :: dir_home, dir_working

  call get_environment_variable ( 'HOME', dir_home )  !  /Users/dantopa
  call getcwd ( dir_working )                         ! ~Dropbox/Fortran/lattice/alpha

end subroutine admin

!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

function pad ( xi, k, pad_str ) result ( string )

  implicit none

  integer,                         intent ( in )  :: xi    ! integer to be formatted
  integer,                         intent ( in )  :: k     ! size of return string
  character ( len = 1 ), optional, intent ( in )  :: pad_str  ! padding character; default = '0'
  character, allocatable                          :: string ( : )

  integer                                         :: i, j    ! counters
  integer                                         :: blanks, str_l, left, right  ! size and location parameters
  integer                                         :: alloc_status  ! allocation status variable

  character ( len = 1 )                           :: pad_char
  character ( len = 20 )                          :: str
  character ( len = 64 )                          :: all_msg  ! allocation error message
  character, allocatable                          :: new ( : )  !

  ! what character shall we pad with?
  pad_char = '0'
  if ( present ( pad_str ) ) pad_char = pad_str

  ! we cannot display error messages in subroutines
  allocate ( new ( 1 : k ), stat = alloc_status, errmsg = all_msg )

  print *, 'allocate to length ', k, '; pad_char = ', pad_char

  ! form a string variable from the integer
  write ( str, '( I20 )' ) abs ( xi )
  str_l = len ( str )
!  print *, 'str = ', str, '; length = ', str_l

  ! find the right-hand boundary ( end )
  do j = 20, 1, -1
    right = j
    if ( str ( j : j ) /= ' ' ) exit
  end do

  ! find the left-hand boundary ( beginning )
  do j = 1, 20
    left = j
    if ( str ( j : j ) /= ' ' ) exit
  end do

  ! length of number in the character string
  str_l = right - left + 1
  ! number of blanks for padding
  blanks = k - str_l

  ! create the output string
  ! blanks first
  do j = 1, blanks
    new ( j : j ) = pad_char
  end do

  ! add the numbers
  i = 0
  do j = blanks + 1, k
    new ( j : j ) = str ( left + i : left + i )
    i = i + 1
  end do

  print *, 'final product = ', new, '.'
  ! exit assignment
  string = new

  deallocate ( new, stat = alloc_status, errmsg = all_msg )

end function pad