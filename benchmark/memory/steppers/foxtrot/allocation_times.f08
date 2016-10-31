! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program allocation_times

    !use, intrinsic :: iso_fortran_env, only : INT64

    use mAllocationTimes,   only : record_allocation_times, measurements
    use mSetPrecision,      only : ip, rp
    use mSystemInfo,        only : write_header_sub

    implicit none

    ! independent parameters
    integer ( ip ), parameter :: power_lo = 3, power_hi = 11 ! decadel range
    ! derived parameters
    integer ( ip ), parameter :: numElements = ( power_hi - power_lo + 1 ) * 9 ! list length for array sizes


    ! variables
    ! rank 1
    integer ( ip ), dimension ( 1 : numElements ) :: elements
    ! rank 0
    integer ( ip ) :: mant = 0_ip, power = 0_ip       ! controls elements
    integer        :: io_summary = 0, io_sequence = 0 ! io handles
    integer        :: k_sizes = 0, k_measurements = 0 ! dummy counters

        elements = [ ( ( 10_ip ** power * mant, mant = 1_ip, 9_ip ), power = power_lo, power_hi ) ] ! sample sizes 1000, 2000, 3000, ...
        print *, 'elements = ', elements
        ! create data files
        call write_header_sub ( io_summary, io_sequence )

        do k_sizes = 1, numElements ! loop over sample sizes
            do k_measurements = 1, measurements ! repeat measurement
                call record_allocation_times ( array_size = elements ( k_sizes ), io = io_summary, deltat = clock_count_delta )
            end do ! k_measurements: repeat measurements
        end do ! k_sizes array size

        stop 'execution completed for allocation_times ...'

end program allocation_times
