program convolve

  implicit none

  integer, parameter    :: n = 3
  integer               :: k
  real, dimension ( n ) :: mesh, y

  interface
    ! *
    elemental function e ( x, z ) result ( y )
      implicit none
      real, intent ( in ) :: x, z
      real                :: y
    end function e
    ! *
    elemental function f ( x ) result ( y )
      implicit none
      real, intent ( in ) :: x
      real                :: y
    end function f
    ! *
    elemental function g ( x ) result ( y )
      implicit none
      real, intent ( in ) :: x
      real                :: y
    end function g
    ! *
    elemental function h ( x ) result ( y )
      implicit none
      real, intent ( in ) :: x
      real                :: y
    end function h
    ! *
    elemental function w ( x ) result ( y )
      implicit none
      real, intent ( in ) :: x
      real                :: y
    end function w
    ! *
  end interface

  interface
  end interface

  !  create mesh
  mesh = [ ( k, k = 1, n ) ]
  print *, 'mesh = ', mesh

  y = e ( mesh, 2.0 )
  print *, 'e ( mesh ) = ', y

  y = f ( mesh )
  print *, 'f ( mesh ) = ', f ( mesh )
  print *, 'g ( y ) = ', g ( y )
  print *, 'f ( g ( mesh ) ) = ', g ( f ( mesh ) )

  print *
  print *, 'h ( mesh )       = ', h ( mesh )
  print *, 'h ( f ( mesh ) ) = ', h ( f ( mesh ) )

  print *
  print *, 'w ( mesh )       = ', w ( mesh )

end program convolve


elemental function e ( x, z ) result ( y )

  implicit none

  real, intent ( in ) :: x, z
  real                :: y

  real, parameter :: m = 2.0 / 3

  y = m * x + z

end function e


elemental function f ( x ) result ( y )

  implicit none

  real, intent ( in ) :: x
  real                :: y

  real, parameter :: m = 1.0
  real, parameter :: b = 2.0 / 3

  y = m * x + b

end function f


elemental function g ( x ) result ( y )

  implicit none

  real, intent ( in ) :: x
  real                :: y

  real, parameter :: m =  2.0
  real, parameter :: b = -2.0 / 5

  y = m * x + b

end function g


elemental function h ( x ) result ( y )

  implicit none

  real, intent ( in ) :: x
  real                :: y

  real, parameter :: m = -3.0
  real, parameter :: b =  1.0 / 5

  if ( x > 2 ) then
    y = 22.
  else
    y = m * x + b
  end if

end function h

function w ( x ) result ( y )

  implicit none

  real, intent ( in ) :: x
  real                :: y

  y = x ** 2 - 2

end function