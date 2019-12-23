module data_types

  implicit NONE

! parameters
  integer, parameter    :: sint = selected_int_kind   (  8 )
  integer, parameter    :: lint = selected_int_kind   ( 16 )
  integer, parameter    :: sp   = selected_real_kind  (  6,  37 )
  integer, parameter    :: dp   = selected_real_kind  ( 15, 307 )

! constants
  real( dp ), parameter :: pi = 3.141592653589793238462643383279502884197_dp
  real( dp ), parameter :: machine_eps = epsilon ( 1D0 )

end module data_types

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     !

program main

  use data_types
  implicit none

  real ( dp ) :: vector ( 1 : 3 )
  
  interface
    real ( dp ) function L1 ( vector ) result ( norm )
      use data_types
      implicit none
      real ( dp ), intent ( in ) :: vector ( : )
    end function
  end interface
  
  vector  = [ 1.D0, 2.D0, 3.D0 ]
  
  write ( *, * ) 'vector  = ', vector
  write ( *, * ) 'L1 norm = ', L1 ( vector )

end program main

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     !

real ( dp ) function L1 ( v ) result ( norm )

  use data_types
  implicit none
  
  real ( dp ), intent ( in ) :: v ( : )
  real ( dp )                :: absx, max

  integer ( sint )           :: k

  max  = -1.D0

  do k = 1, size ( v )
    absx = abs ( v ( k ) )
    if ( absx > max ) then
      max = absx
    end if
  end do
  
  norm = max

end function L1