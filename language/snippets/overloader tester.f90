!
!	overloader.f90
!	
!
!	Created by Daniel M. Topa on 6/16/13.
! ftp://ftp.nag.co.uk/sc22wg5/N1551-N1600/N1579.pdf

include '/Users/dantopa/Dropbox/Fortran/demos/overloader.f90'

program tester ! dimensionless variables, careful about energies

      use class_error
      implicit NONE
      
      type ( real_err ) :: a, b, c
      
      c = a + b
      
      print *, c

      stop
      
end program tester