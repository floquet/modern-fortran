module mAllocationTimes

    use mSetPrecision,  only : ip, rp

    implicit none

    integer,        parameter :: measurements = 3 ! repeat measurements
    integer ( ip ), parameter :: gigabytes = 1024 * 1024 * 1024

    ! rank 1
    real ( rp ) :: ticks_clock = 0.0_rp

    character ( len = * ), parameter :: data_type = 'R64' ! match rp

    type :: ticks
        integer ( ip ) :: start, stop, rate, max, delta
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

    subroutine master_loop ( array_size, io )

        integer ( ip ), intent ( in )  :: array_size
        integer,        intent ( in )  :: io

        type ( ticks )   :: myTicks
        type ( seconds ) :: mySeconds

            ! info for ticks -> seconds
            call system_clock ( COUNT = myTicks % stop, COUNT_RATE =  myTicks % rate, COUNT_MAX = myTicks % max )

            call myTicks % record_allocation_times ( array_size, io, mySeconds % sequence )

            call mySeconds % analyze_seconds ( myTicks )

    end subroutine master_loop

    subroutine analyze_seconds_sub ( thoseTicks )

        class ( seconds ), target :: me

        type ( ticks ), intent ( in ) :: thoseTicks

            print *, 'inside analyze_seconds_sub'

    end subroutine analyze_seconds_sub

    subroutine record_allocation_times_sub ( me, array_size, io )

        class ( ticks ), target :: me

        integer ( ip ), intent ( in ) :: array_size
        integer,        intent ( in ) :: io

        ! rank 1
        real ( rp ), allocatable, dimension ( : )                :: array
        real ( rp ),              dimension ( 1 : measurements ) :: ticks_clock
        ! rank 0
        real ( rp )             :: total_gbytes
        integer                 :: k_measurements, stat
        character ( len = 512 ) :: errmsg

            total_gbytes = real ( array_size * storage_size ( 1.0_rp ), rp ) / real ( 8 * gigabytes, rp )

            do k_measurements = 1, measurements ! repeat measurement
                call system_clock ( COUNT = me % start )

                    allocate ( array ( array_size ), stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( io, 100 ) ''
                        write ( io, 110 ) array_size, total_gbytes, data_type
                        write ( io, 120 ) trim ( errmsg )
                        write ( io, 130 ) stat
                        flush ( io )
                        stop 'fatal program error during allocation'
                    end if

                    array ( : ) = 1.0_rp  ! populate

                    deallocate ( array, stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( io, 100 ) 'de'
                        write ( io, 110 ) array_size, total_gbytes, data_type
                        write ( io, 120 ) trim ( errmsg )
                        write ( io, 130 ) stat
                        flush ( io )
                        stop 'fatal program error during decallocation'
                    end if

                call system_clock ( COUNT = me % stop )
                me % sequence ( k_measurements ) = real ( me % stop - me % start, rp )
            end do ! k_measurement repeat measurement

        100 format ( 'Mortal error during ', A, 'allocation...' )
        110 format ( 'requested size is ', I15, ' elements (', I10,' GB); kind = ', A )
        120 format ( 'errmsg = ', I10, '.' )
        130 format ( 'stat = ', A )

    end subroutine record_allocation_times_sub

end module mAllocationTimes
