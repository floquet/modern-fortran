program dots

  character ( len = 6 )  :: a = '[**❚  '
  character ( len = 6 )  :: b = '###  ]'

  write ( *, 100, advance = 'no' ) a
  write ( *, 100, advance = 'no' ) b
 100 format ( a )

  write ( 6, * )
  write ( 6, 110, advance = 'no' ) '['
!  backspace ( unit = 6 )
  write ( *, 110, advance = 'no' ) repeat ( '❚', 20 )
!  backspace ( unit = 6 )
  write ( 6, 110, advance = 'no' ) repeat ( '*', 3 )
!  backspace ( unit = 6 )
  write ( 6, 110 ) ']'

  backspace ( unit = 6 )
  write ( 6, 110, advance = 'no' ) '['
  write ( *, 110, advance = 'no' ) repeat ( '❚', 22 )
!  backspace ( unit = 6 )
  write ( 6, 110, advance = 'no' ) repeat ( '*', 1 )
!  backspace ( unit = 6 )
  write ( 6, 110 ) ']'

 110 format ( a )

end program dots