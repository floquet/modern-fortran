! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program allocation_times

    use mAllocationTimes,   only : master_loop
    use mSetPrecision,      only : ip
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
    integer ( ip ) :: mant = 0, power = 0             ! controls elements
    integer        :: io_summary = 0, io_sequence = 0 ! io handles
    integer        :: k_sizes = 0                     ! dummy counters

        elements = [ ( ( 10_ip ** power * mant, mant = 1, 9 ), power = power_lo, power_hi ) ] ! sample sizes 1000, 2000, 3000, ...

        call write_header_sub ( io_summary, io_sequence ) ! create files for data and results

        do k_sizes = 1, numElements
            call master_loop ( elements ( k_sizes ), io_summary, io_sequence ) ! perform sequence of allocations, analyze times
        end do

        stop 'execution completed for allocation_times ...'

end program allocation_times
