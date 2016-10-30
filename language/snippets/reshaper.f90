program reshaper

  implicit none

  real :: matrix ( 2, 2 )
  real :: vector ( 4 )

  print *, 'shape = ', shape ( matrix )

  matrix = reshape ( vector, [ 2, 2 ] )

  print *, 'shape now = ', shape ( matrix )

  vector = reshape ( matrix, [ 2 * 2 ] )


end program reshaper