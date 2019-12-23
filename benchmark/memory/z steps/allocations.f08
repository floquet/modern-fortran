program allocations

    use, intrinsic :: iso_fortran_env, only : REAL32, INT64

    implicit none

    real, allocatable       :: array ( : )
    real                    :: t_clock
    integer ( INT64 )       :: myIO, iostat, stat, j, k
    integer ( INT64 )       :: clock_count_start, clock_count_stop, clock_count_delta, &
                               clock_count_rate, clock_count_max
    character ( len = 512 ) :: iomsg, errmsg

    character ( len = * ), parameter :: myProgram = 'program allocations' ! self-identification

        call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )   ! query rate

        open  ( newunit = myIO, file = 'REAL32 times.txt', iostat = iostat, iomsg = iomsg )

        do k = 1, 14
            j = 2 ** k
            j = j
            call system_clock ( clock_count_start )
                allocate ( array ( 1 : j * 1024 * 1024 ), stat = stat, errmsg = errmsg )
                array ( : ) = 1.0_REAL32
                deallocate ( array, stat = stat, errmsg = errmsg )
            call system_clock ( clock_count_stop )
            clock_count_delta = clock_count_stop - clock_count_start
            t_clock = 1.0_REAL32 * clock_count_delta
            t_clock = t_clock / clock_count_rate ! seconds
            write ( myIO, '( g0, ", ", g0 )' ) j * sizeof ( 1.0_REAL32 ), t_clock
            flush ( myIO )
        end do

        close ( myIO )

        stop "successful completion for " // myProgram // "..."  ! string must reduce to constant expression

end program allocations
