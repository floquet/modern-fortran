!
!	NaN.f90
!	
!
!	Created by Daniel M. Topa on 5/27/13.
!	Copyright 2013 University of New Mexico. All rights reserved.
!  http://gcc.gnu.org/onlinedocs/gfortran/ISNAN.html

program test_nan
      
      implicit none
      real :: x
      
      x = -1.0
      x = sqrt(x)
      print *, x
      if ( isnan(x) ) stop '"x" is a NaN'
      
end program test_nan