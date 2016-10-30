! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program allocation_times

    use, intrinsic :: iso_fortran_env, only : INT64

    use mSetPrecision,  only : ip, rp, zint
    use mSystemInfo,    only : write_header_sub

    implicit none

    ! independent parameters
    integer ( ip ), parameter :: measurements = 3 ! repeat measurements
    integer ( ip ), parameter :: power_lo = 3, power_hi = 11 ! decadel range
    integer ( ip ), parameter :: gigabytes = 1024 * 1024 * 1024
    ! derived parameters
    integer ( ip ), parameter :: numElements = ( power_hi - power_lo + 1 ) * 9 ! list length for array sizes

    character ( len = * ), parameter :: data_type = 'R64'

    ! variables
    ! rank 1
    integer ( ip ), dimension ( 1 : numElements ) :: elements
    ! rank 0
    integer ( ip ) :: mant = 0_ip, power = 0_ip, j = 0, k = 0 ! controls elements
    integer :: io_summary = 0, io_sequence = 0
    integer :: total_gbytes = 0
    integer :: k_sizes = 0, k_measurements = 0 ! dummy counters


        print *, 'zint                        = ', zint
        print *, 'ip                          = ', ip
        print *, 'selected_int_kind ( INT64 ) = ', selected_int_kind ( INT64 )
        ! sequence of array sizes
        elements = [ ( ( 10_INT64 ** k * j, j = 1, 9 ), k = 3, 11 ) ] ! sample sizes 1000, 2000, 3000, ...
!        elements = [ ( ( 10_ip ** power * mant, mant = 1_ip, 9_ip ), power = power_lo, power_hi ) ] ! sample sizes 1000, 2000, 3000, ...
        !
        ! ! create data files
        ! call write_header_sub ( data_type, measurements, io_summary, io_sequence )
        !
        ! do k_sizes = 1, numElements ! loop over sample sizes
        !     total_gbytes = elements ( k_sizes ) * storage_size ( 1.0, rp ) / 8 / gigabytes
        !     print *, 'k_sizes = ', k_sizes, '; total_gbytes = ', total_gbytes
        !     ! do k_measurements = 1, measurements ! repeat measurement
        !     !     call allocation ( clicks )
        !     !     time ( k_measurements ) = clicks
        !     ! end do ! j repeat measurement
        !     ! time
        !     ! call max-min ()
        ! end do ! k_sizes array size

        stop 'execution completed for allocation_times ...'

end program allocation_times