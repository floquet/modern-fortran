program roots

  implicit none

  integer :: nRoots = 0

  real, dimension( 1 : 3 ) :: amplitudes = [ 0.0, 0.0, 0.0 ]
  real :: rootbig = 0.0, rootsmall = 0.0

!   define plotnomial q( x ) = amplitudes( 1 ) x^2  + amplitudes( 2 ) x  + amplitudes( 3 )
    amplitudes = [ 1., 2., -1.]

!   find toorts
    [ rootbig, rootsmall ] = quadratice_roots( amplitudes )

end program roots


  real function zeros ( amps )

    implicit none

    real, intent( IN ) :: a, b, c

    root_1 = - b / ( 2 * a ) + sqrt( b**2 - 4 * a * c ) / ( 2 * a )
    root_2 = - b / ( 2 * a ) - sqrt( b**2 - 4 * a * c ) / ( 2 * a )

  end function zeros
