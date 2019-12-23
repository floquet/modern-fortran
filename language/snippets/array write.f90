program array_write

      implicit none

      integer, allocatable, dimension ( : ) :: array, k

      allocate ( array ( 1 : 3 ) )

      write ( *, * ) 'array allocated = ', array

      array = [ 1, 4, 9 ]

      write ( *, * ) 'array brackets = ', array

!      array = ( 1, 4, 9 )

      write ( *, * ) 'array parentheses = ', array

      deallocate ( array )

      write ( *, * ) 'array deallocated = ', array

      allocate ( k ( 1 : 1 ) )
      k = [ 2 ]
      write ( *, * ) 'k = ', k

end program array_write