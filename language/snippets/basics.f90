program ieee_demo

  use, intrinsic :: ieee_arithmetic
  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_exceptions
  
  implicit none
  
  logical :: IEEE_SUPPORT_ALL
ELEMENTAL LOGICAL FUNCTION IEEE_IS_NAN(X)
REAL(kind),INTENT(IN) :: X

!  print *, 'IEEE_SUPPORT_ALL() = ', IEEE_SUPPORT_ALL()

end program ieee_demo