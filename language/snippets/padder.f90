program padder

  implicit none

  integer               :: k
  character ( len = 4 ) :: str

  do k = 1, 5
    write ( str, '( I4 )' ) k
    print *, 'k = ', k, '; str = ', str, '*'
  end do

end program padder