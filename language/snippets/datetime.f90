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
              integer,dimension(8) :: values
              print *, 'hi'
              ! using keyword arguments
              !call date_and_time(date,time,zone,values)
              !call date_and_time(DATE=date,ZONE=zone)
              !call date_and_time(TIME=time)
              !call date_and_time(VALUES=values)
              !print '(a,2x,a,2x,a)', date, time, zone
              !print '(a)', date
              !print '(8i5))', values
          end program test_time_and_date