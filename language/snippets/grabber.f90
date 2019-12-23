!
!	grabber.f90
!	
!
!	Created by Daniel M. Topa on 6/7/13.
!	Copyright 2013 University of New Mexico. All rights reserved.
!

include '/Users/dantopa/Dropbox/Fortran/demos/include.f95'

program grabber

      use data_types
      integer ( sint ) :: clock_count_start
      real ( wp )      :: cpu_time_start
      
      print *, pi
      
      call timestamp ( ) 
      
      clock_count_start = 0
      cpu_time_start    = 0.D0
      
      call timers ( cpu_time_start, clock_count_start )

end program grabber
      
include '/Users/dantopa/Dropbox/Fortran/demos/time routines.f90'
