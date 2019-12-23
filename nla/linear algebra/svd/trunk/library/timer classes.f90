module cpu_timer_class

  use basic_parameters
  implicit none

  ! derived data type
  type, public              :: cpu_timer                                            ! name to instantiate

    private
    real ( dp )             :: saved_time                                           ! saved time in seconds

    contains                                                                        ! bound procedures

      procedure, public     :: cpu_start_timer  => cpu_start_timer_sub
      procedure, public     :: cpu_elapsed_time => cpu_elapsed_time_fcn

  end type cpu_timer                                                                ! end derived data type

  private                   :: cpu_start_timer_sub, cpu_elapsed_time_fcn

  ! methods
  contains                                                                          ! subroutines and functions

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    subroutine cpu_start_timer_sub ( tempus )

      implicit none

      class ( cpu_timer )   :: tempus                                               ! timer object

      real ( dp )           :: cpu_time_start                                       ! saved time in seconds

        call cpu_time ( cpu_time_start )                                            ! system call for current cpu_time

        tempus % saved_time = cpu_time_start                                        ! unique start time for each instantiation

    end subroutine cpu_start_timer_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    real ( dp ) function cpu_elapsed_time_fcn  ( tempus )

      implicit none

      class ( cpu_timer )   :: tempus                                               ! cpu timer object
      real ( dp )           :: cpu_time_stop                                        ! current cpu time in seconds

        call cpu_time ( cpu_time_stop )                                             ! system call for current cpu_time

        cpu_elapsed_time_fcn = cpu_time_stop - tempus % saved_time

    end function cpu_elapsed_time_fcn

end module cpu_timer_class

!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

module clock_timer_class

  use basic_parameters
  implicit none

  ! derived data type
  type, public              :: clock_timer                                          ! name to instantiate

    private
    integer ( zint )        :: saved_time                                           ! saved time in seconds

    contains  ! bound procedures

      procedure, public     :: clock_start_timer  => clock_start_timer_sub
      procedure, public     :: clock_elapsed_time => clock_elapsed_time_fcn

  end type clock_timer                                                              ! end derived data type

  private :: clock_start_timer_sub, clock_elapsed_time_fcn

  ! methods
  contains                                                                          ! subroutines and functions

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    subroutine clock_start_timer_sub ( tempus )

      implicit none

      class ( clock_timer ) :: tempus                                               ! timer object

!     must be at least INT32
      integer ( zint )      :: clock_count_start                                    ! saved time in ticks defined

        call system_clock ( clock_count_start )                                     ! system call for current clock_time

        tempus % saved_time = clock_count_start                                     ! unique start time for each instantiation

    end subroutine clock_start_timer_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   !

    real function clock_elapsed_time_fcn ( tempus )

      implicit none

      class ( clock_timer ) :: tempus                                               ! clock timer object

!     must be at least INT32
      integer ( zint )      :: clock_count_stop, clock_count_rate, clock_count_delta, clock_count_max  ! components

        call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )   ! system call for current clock_time

        clock_count_delta      = clock_count_stop - tempus % saved_time             ! units are compiler dependent
        clock_elapsed_time_fcn = dble ( clock_count_delta ) / clock_count_rate      ! convert to seconds

    end function clock_elapsed_time_fcn

end module clock_timer_class