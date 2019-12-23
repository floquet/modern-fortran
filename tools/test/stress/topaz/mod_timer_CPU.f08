module mTimerCPU

    use, intrinsic :: iso_fortran_env, only : REAL64, INT64

    implicit none ! protects all methods

    ! derived data type
    type, public :: timer_cpu

        private
        real ( REAL64 ) :: saved_time ! seconds

    contains ! type bound procedures

        procedure, public :: timer_start_cpu  => timer_start_cpu_sub
        procedure, public :: time_elapsed_cpu => time_elapsed_cpu_fcn

    end type timer_cpu

    private :: timer_start_cpu_sub
    private :: time_elapsed_cpu_fcn

    contains ! methods

        subroutine timer_start_cpu_sub ( me )                                                                      ! timer_start_cpu

            class ( timer_cpu ), target :: me

            real ( REAL64 ) :: cpu_time_start ! saved time in seconds

                call cpu_time ( cpu_time_start ) ! current cpu_time
                me % saved_time = cpu_time_start

        end subroutine timer_start_cpu_sub

        real ( REAL64 ) function time_elapsed_cpu_fcn  ( me )                                                     ! time_elapsed_cpu

            class ( timer_cpu ), target :: me
            real ( REAL64 ) :: cpu_time_stop ! current cpu time in seconds

                call cpu_time ( cpu_time_stop ) ! current cpu_time
                time_elapsed_cpu_fcn = cpu_time_stop - me % saved_time

    end function time_elapsed_cpu_fcn

end module mTimerCPU
