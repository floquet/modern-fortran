!
!	recursive function.f90
!	
!
!	Created by Daniel M. Topa on 5/21/13.
!
module globals

      integer ( kind = 4 ), parameter           :: n = 10
      integer ( kind = 4 ), dimension ( 1 : n ) :: dn
      
end module globals

!     ********************************************************************     !

program recursive_function

      use globals
      implicit none
        
      integer ( kind = 4 )                         :: i, k
      integer ( kind = 4 )                         :: rback
      integer ( kind = 4 ), dimension ( 1 : n )    :: array
      integer ( kind = 4 ), dimension ( n, 0 : n ) :: back
            
      array = [ ( k, k = 1, n ) ]      
      dn = cshift ( array, shift = -1, dim = 1 )  ! push left
      
      write ( *, * ) 'dn    = '
      write ( *, * )  dn
      write ( *, * ) 
           
      do i = 1, n ! works up to n=20 even
        back( i, 0)  = i
        back( i, 1)  = dn(i)
        back( i, 2)  = dn(dn(i))
        back( i, 3)  = dn(dn(dn(i)))
        back( i, 4)  = dn(dn(dn(dn(i))))
        back( i, 5)  = dn(dn(dn(dn(dn(i)))))
        back( i, 6)  = dn(dn(dn(dn(dn(dn(i))))))
        back( i, 7)  = dn(dn(dn(dn(dn(dn(dn(i)))))))
        back( i, 8)  = dn(dn(dn(dn(dn(dn(dn(dn(i))))))))
        back( i, 9)  = dn(dn(dn(dn(dn(dn(dn(dn(dn(i)))))))))
        back( i, 10) = dn(dn(dn(dn(dn(dn(dn(dn(dn(dn(i))))))))))
      end do
     
      write ( *, * ) 'baseline case:'
      write ( *, * ) ( back ( 1, k ), k = 1, n )      
      write ( *, * ) 
     
      write ( *, * ) 'using a recursive function:'
      write ( *, * ) ( rback ( 1, k ), k = 1, n )      

end program recursive_function

!     ********************************************************************     !

recursive function rback ( i, k ) result ( xback )
!                                                                       FUNCTION     
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DECLARATIONS            

      use globals
      implicit none

      integer ( kind = 4 ), intent ( in )       :: i, k
      integer ( kind = 4 )                      :: xback, j
!                                                                       FUNCTION     
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       COMPUTE     
      
      if ( k == 0 ) then
        xback = i
      else
        xback = dn( rback ( i, k - 1 ) )
      end if

end function rback