module mTimerClock

    use, intrinsic :: iso_fortran_env, only : REAL64, INT64
    use mParameters,                   only : one

    implicit none ! protects all methods

    integer, parameter, private :: ip = INT64, rp = REAL64

    ! derived data type
    type, public :: timer_clock

        private
        integer ( ip )  :: saved_time ! in seconds

    contains  ! bound procedures

        procedure, public   :: timer_start_clock  => timer_start_clock_sub
        procedure, public   :: time_elapsed_clock => time_elapsed_clock_fcn

    end type timer_clock

    private :: timer_start_clock_sub
    private :: time_elapsed_clock_fcn

    ! methods
    contains ! subroutines and functions

        subroutine timer_start_clock_sub ( me )                                                                  ! timer_start_clock

            class ( timer_clock ), target :: me

            ! must be at least INT32
            integer ( ip )  :: clock_count_start  ! in ticks

                call system_clock ( clock_count_start ) ! current clock_time
                me % saved_time = clock_count_start     ! unique start time for each instantiation

        end subroutine timer_start_clock_sub

        real ( rp ) function time_elapsed_clock_fcn ( me )                                                      ! time_elapsed_clock

            class ( timer_clock ), target :: me

            ! must be at least INT32
            integer ( ip )  :: clock_count_stop, clock_count_rate, clock_count_delta, clock_count_max

                call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )   ! current clock_time

                clock_count_delta      = clock_count_stop - me % saved_time                 ! units are compiler dependent
                time_elapsed_clock_fcn = one * clock_count_delta / clock_count_rate         ! convert to seconds

        end function time_elapsed_clock_fcn

end module mTimerClock
