subroutine timestamp ( )

      use data_types
      implicit none
      
      integer ( sint ), dimension ( 8 ) :: values  ! DTG

!     characters
      character (  8 ) :: date  ! DTG
      character ( 10 ) :: time  ! DTG
      character (  5 ) :: zone  ! DTG
      character ( 30 ) :: now   ! DTG
      
!     timestamp
      call date_and_time ( date, time, zone, values )
      write  ( now, 100 )  date ( 1 : 4 ), date ( 5 : 6 ), date ( 7 : 8 ), &
                           time ( 1 : 2 ), time ( 3 : 4 ), time ( 5 : 6 ), &
                                                                     "UCT", zone
 100  format ( a, "-", a, "-", a, 2X, a, ":", a, ":", a, 2X, a, a )

!     2013-06-06  19:47:03  UCT-0600
      write ( *, * ) 
      write ( *, * ) now
 
end subroutine timestamp

!     33333333333333333333333333333333333333333333333333333333333333333333     !

subroutine timers ( cpu_time_start, clock_count_start )

      use data_types
      implicit none

!     integers  
      integer ( sint ), intent ( in ) :: clock_count_start
      integer ( sint )                :: clock_count_stop, clock_count_rate, &
                                         clock_count_max,  clock_count_delta

!     real, working precision
      real ( wp ), intent ( in )      :: cpu_time_start
      real ( wp ), parameter          :: scale = 0.5_wp
      real ( wp )                     :: cpu_time_stop, cpu_time_delta
      real ( wp )                     :: clock_count_sec, efficiency
      real ( wp )                     :: min, hr, day, week, year
      
!                                                                       cpu time
      call cpu_time ( cpu_time_stop )

!     convert to familiar timescales
      cpu_time_delta = cpu_time_stop - cpu_time_start
      min            = cpu_time_delta / 60
      hr             = min            / 60
      day            = hr             / 24
      week           = day            / 7
      year           = day            / 365
      
!     output cpu time
      write ( *, * ) 
      write ( *, * ) 'CPU time (seconds) = ', cpu_time_delta
      if ( min > scale ) then
        write ( *, * ) 'CPU time (minutes) = ', min
        if ( hr > scale ) then
          write ( *, * ) 'CPU time (hours)   = ', hr
          if ( day > scale ) then
            write ( *, * ) 'CPU time (days)    = ', day
            if ( week > scale ) then
              write ( *, * ) 'CPU time (weeks)    = ', week
              if ( year > scale      ) then
                write ( *, * ) 'CPU time (years)    = ', year
              end if
            end if
          end if
        end if
      end if
!                                                                     clock time

      call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )

!     convert to familiar timescales
      clock_count_delta = clock_count_stop - clock_count_start 
      clock_count_sec   = dble ( clock_count_delta ) / clock_count_rate 
      min               = clock_count_sec / 60_wp
      hr                = min               / 60
      day               = hr                / 24
      week              = day               / 7
      year              = day               / 365
      
!     output clock time
      write ( *, * ) 
      write ( *, * ) 'clock time (seconds) = ', clock_count_sec
      if ( min > scale ) then
        write ( *, * ) 'clock time (minutes) = ', min
        if ( hr > scale ) then
          write ( *, * ) 'clock time (hours)   = ', hr
          if ( day > scale ) then
            write ( *, * ) 'clock time (days)    = ', day
            if ( week > scale ) then
              write ( *, * ) 'clock time (weeks)    = ', week
              if ( year > scale ) then
                write ( *, * ) 'clock time (years)    = ', year
              end if
            end if
          end if
        end if
      end if
!                                                           processor efficiency

      efficiency = cpu_time_delta / clock_count_sec

      write ( *, * ) 
      write ( *, * ) 'cpu time / clock time = ', efficiency

end subroutine timers