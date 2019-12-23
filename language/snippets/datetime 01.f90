!
!	datetime.f90
!	
!
!	Created by Daniel M. Topa on 6/2/13.
!	http://gcc.gnu.org/onlinedocs/gfortran/DATE_005fAND_005fTIME.html
!

program test_time_and_date
      character(8)  :: date
      character(10) :: time
      character(5)  :: zone
      character(30) :: now
      integer,dimension(8) :: values

      call date_and_time ( date, time, zone, values )

      print '(a, "-", a, "-",a, 2X, a, ":", a, ":", a, 2X, a, a)', &
            date( 1 : 4 ), date( 5 : 6 ), date( 7 : 8 ), &
            time( 1 : 2 ), time( 3 : 4 ), time( 5 : 6 ), "GMT", zone
            
      write  ( now, 100 )  date( 1 : 4 ), date( 5 : 6 ), date( 7 : 8 ), &
                           time( 1 : 2 ), time( 3 : 4 ), time( 5 : 6 ), &
                                                                     "GMT", zone
 100  format ( a, "-", a, "-",a, 2X, a, ":", a, ":", a, 2X, a, a)
            
      print *, 'now = ', trim ( now )
      
      stop 'program test_time_and_date'

end program test_time_and_date