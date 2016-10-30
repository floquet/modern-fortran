program binary_out
! unformatted write

  real             :: singles ( 1 : 3 )
  double precision :: doubles ( 1 : 3 ), check ( 1 : 3 )

  integer          :: length

  singles ( 1 ) = 2.E-4
  singles ( 2 ) = 4.E-4
  singles ( 3 ) = 6.E-4

  doubles ( 1 ) = 1.0D-4
  doubles ( 2 ) = 2.0D-4
  doubles ( 3 ) = 3.0D-4

  open  ( 1, file = 'singles.r32', action = 'write', status = 'unknown', form = 'unformatted', access = 'stream' )
  write ( 1 ) singles
  close ( 1 )

  open  ( 1, file = 'doubles.r64', action = 'write', status = 'unknown', form = 'unformatted', access = 'stream' )
  write ( 1 ) doubles
  close ( 1 )

  open  ( 1, file = 'doubles.r64', action = 'read', status = 'unknown', form = 'unformatted', access = 'stream' )
  read  ( 1 ) check
  close ( 1 )

  print *, 'check = ', check

  ! Metcalf, p. 204

  inquire ( iolength = length ) doubles
  print *, ' record length = ', length

end program binary_out