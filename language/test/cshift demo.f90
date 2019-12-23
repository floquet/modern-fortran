!
!	cshift demo.f90
!	
!
!	Created by Daniel M. Topa on 5/21/13.
!

program cshift_demo

      integer, parameter          :: n = 10
      integer                     :: k
      integer, dimension( 1 : n ) :: array, up, dn
      
      array = [ ( k, k = 1, n ) ]
      
      up = cshift ( array, shift = +1, dim = 1 )  ! push right
      dn = cshift ( array, shift = -1, dim = 1 )  ! push left
      
      write ( *, * ) 'array = ', array      
      write ( *, * ) 'up    = ', up      
      write ( *, * ) 'dn    = ', dn      

end program cshift_demo