!  http://fortranwiki.org/fortran/show/Generic+programming
module data_mod
   implicit none
   private

   integer                   , parameter         :: N = 10
   character, dimension ( 1 ), parameter         :: void = [ achar ( 1 ) ]

   type                                          :: data_t
      character, dimension ( : ), allocatable    :: d
   end type

   type ( data_t ), dimension ( N )              :: data

   public                                        :: void
   public                                        :: set, get

contains

   subroutine set ( k, d )  !                       SET

      integer, intent ( in )                     :: k
      character, dimension ( : ), intent ( in )  :: d

      if ( allocated ( data ( k ) % d ) ) deallocate ( data ( k ) % d )
      allocate ( data ( k ) % d ( size ( d ) ) )
      data ( k ) % d = d
      print *, 'd = ', d

   end subroutine set

   function get ( k ) result ( d )  !               GET

      integer, intent ( in )                     :: k
      character, dimension ( : ), allocatable    :: d

      allocate ( d ( size ( data ( k ) % d ) ) )
      d = data ( k ) % d

   end function get

end module data_mod

program main

   use data_mod
   implicit none

   integer               :: x = 2143289344, y = 1082130432, store = 0
   character ( len = 4 ) :: word

   print *, 'x = ', x
   print *, 'transfer( x, 1 ) = ', transfer( x, 1 )
   print *, 'transfer( x, 1.0 ) = ', transfer( x, 1.0 )
   print *, 'transfer( x, 1.0D0 ) = ', transfer( y, 1.0D0 )

!   print *, 'pre: data = ', data

!   write ( *, '( "data = ", g0 )' ) data

   call set ( 1, transfer ( 10, void ) )
   call set ( 2, transfer ( 10.0E0, void ) )
   call set ( 3, transfer ( 10.0D0, void ) )

!   print *, 'set: data = ', data

!   write ( *, '( "data = ", g0 )' ) data

   print *, "void = ", void, "."
   print *, "size ( transfer ( 10, void ) )     = ", size ( transfer ( 10, void ) ), "."
   print *, "size ( transfer ( 10.0, void ) )   = ", size ( transfer ( 10.0, void ) ), "."
   print *, "size ( transfer ( 10.0D0, void ) ) = ", size ( transfer ( 10.0D0, void ) ), "."

   write ( *, * ) transfer ( get ( 1 ) , 0 )
   write ( *, * ) transfer ( get ( 2 ) , ( 0.0, 0.0 ) )
   write ( *, * ) transfer ( get ( 3 ) , 0.0d0 )

!  print *, 'get: data = ', data

end program main