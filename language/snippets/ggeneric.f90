!  http://fortranwiki.org/fortran/show/Generic+programming
module data_mod
   implicit none
   private

   integer                   , parameter         :: N = 10
   character, dimension ( 1 ), parameter         :: char_mold_rank_one = [ achar ( 1 ) ]

   type                                          :: data_t
      character, dimension ( : ), allocatable    :: d
   end type

   type ( data_t ), dimension ( N )              :: data

   public                                        :: char_mold_rank_one
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

   call set ( 1, transfer ( 10,     char_mold_rank_one ) )
   call set ( 2, transfer ( 10.0E0, char_mold_rank_one ) )
   call set ( 3, transfer ( 10.0D0, char_mold_rank_one ) )

   write ( *, * ) transfer ( get ( 1 ) , 0 )
   write ( *, * ) transfer ( get ( 2 ) , ( 0.0, 0.0 ) )
   write ( *, * ) transfer ( get ( 3 ) , 0.0d0 )

   write ( *, * ) 'xfer: ', transfer ( transfer ( 10, char_mold_rank_one ) , ( 0.0, 0.0 ) )

end program main