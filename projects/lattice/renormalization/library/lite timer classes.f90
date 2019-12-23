module cpu_timer_class                            ! time quantum is a second

  use data_types
  implicit none

  ! derived data type
  type, public              :: cpu_timer          ! name to instantiate

    private
    real ( dp )             :: saved_time         ! saved time in seconds
    real ( dp )             :: cum_time           ! cumulative time in seconds

    contains  ! bound procedures

      procedure, public     :: cpu_start_timer    => cpu_start_timer_sub
      procedure, public     :: cpu_elapsed_time   => cpu_elapsed_time_fcn

      ! sequence:  grab  pause, resume, ... pause, resume  stop
      ! stop is equivalent to pause
      procedure, public     :: cpu_timer_grab     => cpu_timer_grab_sub
      procedure, public     :: cpu_timer_pause    => cpu_timer_pause_sub
      procedure, public     :: cpu_timer_resume   => cpu_timer_resume_sub
      procedure, public     :: cpu_timer_stop     => cpu_timer_pause_sub  ! no need for a separate STOP routine

      procedure, public     :: cpu_timer_cum_read => cpu_timer_cum_read_fcn

  end type cpu_timer
  ! end derived data type

  private                   :: cpu_start_timer_sub, cpu_elapsed_time_fcn

  private                   :: cpu_timer_grab_sub, cpu_timer_pause_sub, cpu_timer_resume_sub
  private                   :: cpu_timer_cum_read_fcn

  ! methods
  contains                                              ! subroutines and functions

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    subroutine cpu_timer_grab_sub ( tempus )            ! start a timer

      implicit none

      class ( cpu_timer )         :: tempus             ! timer object

      real ( dp )                 :: cpu_time_t0        ! saved time in seconds

      ! system call for current cpu_time
      call cpu_time ( cpu_time_t0 )

      tempus % saved_time = cpu_time_t0                 ! unique start time for each instantiation
      tempus % cum_time   = zero

    end subroutine cpu_timer_grab_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    subroutine cpu_timer_pause_sub ( tempus )           ! update the cumulative time

      implicit none

      class ( cpu_timer )         :: tempus             ! timer object

      real ( dp )                 :: cpu_time_now

      ! system call for current cpu_time
      call cpu_time ( cpu_time_now )

      ! guard against the erroneous sequence of  pause, pause, ..., pause
      tempus % cum_time   = tempus % cum_time + ( cpu_time_now - tempus % saved_time )
      tempus % saved_time = cpu_time_now                ! unique start time for each instantiation

    end subroutine cpu_timer_pause_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    subroutine cpu_timer_resume_sub ( tempus )          ! restart the time

      implicit none

      class ( cpu_timer )         :: tempus             ! timer object

      real ( dp )                 :: cpu_time_now       ! saved time in seconds

      ! system call for current cpu_time
      call cpu_time ( cpu_time_now )

      tempus % saved_time = cpu_time_now                ! unique start time for each instantiation

    end subroutine cpu_timer_resume_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    function cpu_timer_cum_read_fcn ( tempus ) result ( cpu_cum_time )

      implicit none

      class ( cpu_timer )         :: tempus             ! timer object

      real ( dp )                 :: cpu_cum_time       ! cumulative time buffer value

      cpu_cum_time = tempus % cum_time                  ! read cumulative time buffer

    end function cpu_timer_cum_read_fcn

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !
!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    subroutine cpu_start_timer_sub ( tempus )

      implicit none

      class ( cpu_timer )         :: tempus             ! timer object

      real ( dp )                 :: cpu_time_start     ! saved time in seconds

      ! system call for current cpu_time
      call cpu_time ( cpu_time_start )

      tempus % saved_time = cpu_time_start              ! unique start time for each instantiation

    end subroutine cpu_start_timer_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    function cpu_elapsed_time_fcn ( tempus ) result ( cpu_elapsed_time )

      implicit none

      class ( cpu_timer )         :: tempus             ! cpu timer object
      real ( dp )                 :: cpu_time_stop      ! current cpu time in seconds
!      real ( dp ), intent ( out ) :: cpu_elapsed_time   ! elapsed cpu time in seconds
      real ( dp )                  :: cpu_elapsed_time   ! elapsed cpu time in seconds

      ! system call for current cpu_time
      call cpu_time ( cpu_time_stop )

      cpu_elapsed_time = cpu_time_stop - tempus % saved_time

    end function cpu_elapsed_time_fcn

end module cpu_timer_class

!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

module clock_timer_class                          ! time quantum is processor dependent

  use data_types
  implicit none

  ! derived data type
  type, public              :: clock_timer        ! name to instantiate

    private
    integer ( sint )        :: saved_time         ! saved time in seconds

    contains  ! bound procedures

      procedure, public     :: clock_start_timer  => clock_start_timer_sub
      procedure, public     :: clock_elapsed_time => clock_elapsed_time_fcn

  end type clock_timer
  ! end derived data type

  private :: clock_start_timer_sub, clock_elapsed_time_fcn

  ! methods
  contains                                        ! subroutines and functions

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    subroutine clock_start_timer_sub ( tempus )

      implicit none

      class ( clock_timer ) :: tempus             ! timer object

      integer ( sint )      :: clock_count_start  ! saved time in ticks defined

      ! system call for current clock_time
      call system_clock ( clock_count_start )

      tempus % saved_time = clock_count_start     ! unique start time for each instantiation

    end subroutine clock_start_timer_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    real function clock_elapsed_time_fcn ( tempus )

      implicit none

      class ( clock_timer ) :: tempus             ! clock timer object
      integer ( sint )      :: clock_count_stop, clock_count_rate, clock_count_delta, clock_count_max  ! components

      ! system call for current clock_time
      call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )

      ! units are compiler dependent
      clock_count_delta = clock_count_stop - tempus % saved_time
      ! convert to seconds
      clock_elapsed_time_fcn = dble ( clock_count_delta ) / clock_count_rate

    end function clock_elapsed_time_fcn

end module clock_timer_class