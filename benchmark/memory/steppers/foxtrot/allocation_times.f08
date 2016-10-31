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

    character ( len = * ), parameter :: data_type = 'R64'

    ! variables
    ! rank 1
    real ( rp ) :: ticks_clock = 0.0_rp

    integer ( ip ), dimension ( 1 : numElements ) :: elements
    ! rank 0
    real ( rp ) :: total_gbytes = 0_rp

    integer ( ip ) :: mant = 0_ip, power = 0_ip ! controls elements
    integer        :: io_summary = 0, io_sequence = 0
    integer        :: k_sizes = 0, k_measurements = 0 ! dummy counters

        elements = [ ( ( 10_ip ** power * mant, mant = 1_ip, 9_ip ), power = power_lo, power_hi ) ] ! sample sizes 1000, 2000, 3000, ...
        print *, 'elements = ', elements
        ! create data files
        call write_header_sub ( data_type, measurements, io_summary, io_sequence )

        do k_sizes = 1, numElements ! loop over sample sizes
            do k_measurements = 1, measurements ! repeat measurement
                call record_allocation_times ( how_many_elements, io_summary, io_sequence, clock_count_delta )
                ticks_clock ( k_measurements ) = real ( clock_count_delta, rp )
            end do ! k_measurements: repeat measurements
        end do ! k_sizes array size

        stop 'execution completed for allocation_times ...'

end program allocation_times

subroutine record_allocation_times ( how_many_elements, io_summary, io_sequence )

    integer ( ip ), intent ( in ) :: how_many_elements
    integer,        intent ( in ) :: io_summary, io_sequence

    real ( rp ),   intent ( out ) :: clock_count_delta

        total_gbytes = real ( elements ( k_sizes ) * storage_size ( 1.0_rp ), rp ) / real ( 8 * gigabytes, rp )

        do j = 1, measurements ! repeat measurement
            call system_clock ( clock_count_start )

                allocate ( array ( how_many_elements ), stat = stat, errmsg = errmsg )
                if ( stat /= 0 ) then
                    write ( myIO, 100 ) ''
                    write ( myIO, 110 ) how_many_elements, total_bytes, prec_type
                    write ( myIO, 120 ) trim ( errmsg )
                    write ( myIO, 130 ) stat
                    flush ( myIO )
                    stop 'fatal program error during allocation'
                end if

                array ( : ) = 1.0_rp  ! populate

                deallocate ( array, stat = stat, errmsg = errmsg )
                if ( stat /= 0 ) then
                    write ( myIO, 100 ) 'de'
                    write ( myIO, 110 ) how_many_elements, total_bytes, prec_type
                    write ( myIO, 120 ) trim ( errmsg )
                    write ( myIO, 130 ) stat
                    flush ( myIO )
                    stop 'fatal program error during decallocation'
                end if

            call system_clock ( clock_count_stop )
            clock_count_delta = clock_count_stop - clock_count_start
        end do ! repeat measurement

end subroutine record_allocation_times
