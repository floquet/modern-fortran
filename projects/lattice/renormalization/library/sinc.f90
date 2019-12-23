program sinc

  implicit none

  integer            :: k
  integer, parameter :: real_kind  = selected_real_kind ( 15, 307 )

  real ( KIND = 16 ) :: x, y
  real ( KIND = 16 ) :: den
  real ( KIND = 16 ) :: a, b, c, d, e

  print *, ' real_kind = ', real_kind
  print *
  print *, ' Taylor definition vs. Fortran'

  x = 1.0
  do k = 1, 20
    y = sin ( x ) / x

    a = 1.0
    b = a - x ** 2 / 6
    c = b + x ** 4 / 120
    d = c - x ** 6 / 5040
    e = d + x ** 8 / 362880

    write  ( *, 100 ) x, y, b, c, d, e
100 format ( 6( 5X, E20.15 ) )

    x = x / 10

  end do

end program sinc