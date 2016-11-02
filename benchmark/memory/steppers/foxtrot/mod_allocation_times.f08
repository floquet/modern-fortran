module mAllocationTimes

    use mSetPrecision,  only : ip, rp

    implicit none

    integer,        parameter :: measurements = 15 ! repeat measurements
    integer ( ip ), parameter :: gigabytes = 1024 * 1024 * 1024

    ! rank 1
    !real ( rp ) :: ticks_clock = 0.0_rp

    character ( len = * ), parameter :: data_type = 'R64' ! match rp

    type :: ticks
        integer ( ip ) :: array_size
        integer ( ip ) :: start, stop, rate, max, delta
        real ( rp )    :: total_gbytes
    contains
        private
        procedure, public :: record_allocation_times => record_allocation_times_sub
    end type ticks

    type :: seconds
        real ( rp ), dimension ( 1 : measurements ) :: sequence
        real ( rp ) :: mean, variance, max, min
    contains
        private
        procedure, public :: analyze_seconds => analyze_seconds_sub
    end type seconds

    private :: record_allocation_times_sub, analyze_seconds_sub

contains

    subroutine master_loop ( array_size, io_summary, io_sequence )

        integer ( ip ), intent ( in )  :: array_size
        integer,        intent ( in )  :: io_summary, io_sequence

        type ( ticks )   :: myTicks
        type ( seconds ) :: mySeconds

            ! info for ticks -> seconds
            call system_clock ( COUNT = myTicks % stop, COUNT_RATE =  myTicks % rate, COUNT_MAX = myTicks % max )

            call myTicks % record_allocation_times ( array_size, io_summary, mySeconds )

            call mySeconds % analyze_seconds ( )

            call post_results ( myTicks, mySeconds, io_summary, io_sequence )

    end subroutine master_loop

    subroutine post_results ( thoseTicks, thoseSeconds, io_summary, io_sequence )

        integer, intent ( in ) :: io_summary, io_sequence
        integer :: k

        type ( ticks )   :: thoseTicks
        type ( seconds ) :: thoseSeconds

        character ( len = * ), parameter :: fmt_gb = 'E17.8', fmt_time = 'E15.5', fmt_elem = 'I15'
        character ( len = 128 )  :: fmt_str = '' ! format descriptor, e.g. 5( E8.3 )

            ! summary data
            write ( fmt_str, 100 ) fmt_elem, fmt_gb, fmt_time, fmt_time
            write ( unit = io_summary, fmt = trim ( fmt_str ) ) thoseTicks % array_size, thoseTicks % total_gbytes, &
                thoseSeconds % mean, thoseSeconds % variance, thoseSeconds % min, thoseSeconds % max
            flush ( io_summary )

            ! list the sequence of recorded times
            write ( fmt_str, 200 ) fmt_elem, fmt_gb, fmt_time, measurements - 1, fmt_time
            write ( unit = io_sequence, fmt = fmt_str ) thoseTicks % array_size, thoseTicks % total_gbytes, &
                                                      ( thoseSeconds % sequence ( k ), k = 1, measurements )
            flush ( io_sequence )

        100 format ( "( ", g0, ", 2X, ", g0, ", 2X, ", g0, ', ', "3 ( ', ', ", g0, " ) )" )
        200 format ( "( ", g0, ", 2X, ", g0, ", 2X, ", g0, ', ', g0, " ( ', ', ", g0, " ) )" )

    end subroutine post_results

    subroutine analyze_seconds_sub ( me )

        class ( seconds ), target :: me

        !type ( ticks ), intent ( in ) :: thoseTicks

        real ( rp ) :: sum_squares_ave, root

            me % max = maxval ( me % sequence )
            me % min = minval ( me % sequence )

            ! mean and variance
            me % mean       = sum ( me % sequence ) / real ( measurements, rp )

            sum_squares_ave = dot_product ( me % sequence, me % sequence ) / real ( measurements, rp )
            root   = sum_squares_ave - me % mean ** 2
            if ( root < 5.0_rp * epsilon ( 1.0_rp ) ) root = 0.0_rp ! avoid sqrt of a negative number
            me % variance = sqrt ( root )

    end subroutine analyze_seconds_sub

    subroutine record_allocation_times_sub ( me, array_size, io, thoseSeconds )

        class ( ticks ), target :: me

        type ( seconds ), intent ( out ) :: thoseSeconds
        integer ( ip ),   intent ( in )  :: array_size
        integer,          intent ( in )  :: io

        ! rank 1
        real ( rp ), allocatable, dimension ( : )                :: array
        ! rank 0
        integer                 :: k_measurements, stat
        character ( len = 512 ) :: errmsg

            me % array_size   = array_size
            me % total_gbytes = real ( me % array_size * storage_size ( 1.0_rp ), rp ) / real ( 8 * gigabytes, rp )

            do k_measurements = 1, measurements ! repeat measurement
                call system_clock ( COUNT = me % start )

                    allocate ( array ( me % array_size ), stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( io, 100 ) ''
                        write ( io, 110 ) me % array_size, me % total_gbytes, data_type
                        write ( io, 120 ) trim ( errmsg )
                        write ( io, 130 ) stat
                        flush ( io )
                        stop 'fatal program error during allocation'
                    end if

                    array ( : ) = 1.0_rp  ! populate

                    deallocate ( array, stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( io, 100 ) 'de'
                        write ( io, 110 ) me % array_size, me % total_gbytes, data_type
                        write ( io, 120 ) trim ( errmsg )
                        write ( io, 130 ) stat
                        flush ( io )
                        stop 'fatal program error during decallocation'
                    end if

                call system_clock ( COUNT = me % stop )
                thoseSeconds % sequence ( k_measurements ) = real ( me % stop - me % start, rp )  / real ( me % rate, rp ) ! sec
            end do ! k_measurement repeat measurement

        100 format ( 'Mortal error during ', A, 'allocation...' )
        110 format ( 'requested size is ', I15, ' elements (', F15.5,' GB); kind = ', A )
        120 format ( 'errmsg = ', I10, '.' )
        130 format ( 'stat = ', A )

    end subroutine record_allocation_times_sub

end module mAllocationTimes
