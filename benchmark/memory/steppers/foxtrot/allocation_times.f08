! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program allocation_times

    !use, intrinsic :: iso_fortran_env, only : INT64

    use mSetPrecision,  only : ip, rp
    use mSystemInfo,    only : write_header_sub

    implicit none

    ! independent parameters
    integer, parameter :: measurements = 3 ! repeat measurements
    integer ( ip ), parameter :: power_lo = 3, power_hi = 11 ! decadel range
    integer ( ip ), parameter :: gigabytes = 1024 * 1024 * 1024
    ! derived parameters
    integer ( ip ), parameter :: numElements = ( power_hi - power_lo + 1 ) * 9 ! list length for array sizes

    character ( len = * ), parameter :: data_type = 'R64' ! match rp

    ! variables
    ! rank 1
    real ( rp ) :: ticks_clock = 0.0_rp

    integer ( ip ), dimension ( 1 : numElements ) :: elements
    ! rank 0
    real ( rp ) :: total_gbytes = 0_rp

    integer ( ip ) :: mant = 0_ip, power = 0_ip ! controls elements
    integer ( ip )    :: clock_count_start = 0, clock_count_stop = 0, &
                         global_start      = 0, global_stop      = 0, &
                         clock_count_rate  = 0, clock_count_max  = 0, clock_count_delta = 0
    integer        :: io_summary = 0, io_sequence = 0
    integer        :: k_sizes = 0, k_measurements = 0 ! dummy counters

        elements = [ ( ( 10_ip ** power * mant, mant = 1_ip, 9_ip ), power = power_lo, power_hi ) ] ! sample sizes 1000, 2000, 3000, ...
        print *, 'elements = ', elements
        ! create data files
        call write_header_sub ( data_type, measurements, io_summary, io_sequence )

        do k_sizes = 1, numElements ! loop over sample sizes
            do k_measurements = 1, measurements ! repeat measurement
                call record_allocation_times ( array_size = elements ( k_sizes ), io = io_summary, deltat = clock_count_delta )
                ticks_clock ( k_measurements ) = real ( clock_count_delta, rp )
            end do ! k_measurements: repeat measurements
        end do ! k_sizes array size

        stop 'execution completed for allocation_times ...'

end program allocation_times

subroutine record_allocation_times ( array_size, io, deltat )

    integer ( ip ), intent ( in )  :: array_size
    integer,        intent ( in )  :: io

    integer ( ip ), intent ( out ) :: deltat

    integer :: stat
    character ( len = 512 ) :: errmsg

        total_gbytes = real ( array_size * storage_size ( 1.0_rp ), rp ) / real ( 8 * gigabytes, rp )

        do j = 1, measurements ! repeat measurement
            call system_clock ( clock_count_start )

                allocate ( array ( array_size ), stat = stat, errmsg = errmsg )
                if ( stat /= 0 ) then
                    write ( io, 100 ) ''
                    write ( io, 110 ) array_size, total_gbytes, prec_type
                    write ( io, 120 ) trim ( errmsg )
                    write ( io, 130 ) stat
                    flush ( io )
                    stop 'fatal program error during allocation'
                end if

                array ( : ) = 1.0_rp  ! populate

                deallocate ( array, stat = stat, errmsg = errmsg )
                if ( stat /= 0 ) then
                    write ( io, 100 ) 'de'
                    write ( io, 110 ) array_size, total_gbytes, prec_type
                    write ( io, 120 ) trim ( errmsg )
                    write ( io, 130 ) stat
                    flush ( io )
                    stop 'fatal program error during decallocation'
                end if

            call system_clock ( clock_count_stop )
            deltat = clock_count_stop - clock_count_start
        end do ! repeat measurement

    100 format ( 'Mortal error during ', A, 'allocation...' )
    110 format ( 'requested size is ', I15, ' elements (', I10,' GB); kind = ', A )
    120 format ( 'errmsg = ', I10, '.' )
    130 format ( 'stat = ', A )

end subroutine record_allocation_times
