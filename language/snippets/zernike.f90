program zernike

  implicit none

  integer :: k, n

  real    :: r, z

    print *, 'hello'

    n = 10
    do k = 0, n
      r = 0.0 + k / dble ( n )
      print *, 'r = ', r, '; z(r) = ', z( r )
    end do

end program zernike


real function z( r )  result ( fcn )

  implicit none

  real, intent ( in ) :: r

    fcn =  (-1 + 2 * r**2) * (1 + 26 * (-1 + r) * r**2 * (1 + r) * (5 + (-1 + r) * r**2 * (1 + r) * (150 + 17 * (-1 + r) * &
              r**2 * (1 + r) * (100 + 19 * (-1 + r) * r**2 * (1 + r) * (25 - 42 * r**2 + 42 * r**4)))))

end function z