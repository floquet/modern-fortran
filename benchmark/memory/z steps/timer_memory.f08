program timer_memory

    use, intrinsic :: iso_fortran_env, only : REAL32, INT64

    implicit none

    ! parameters
    integer ( INT64 ),     parameter :: rp = REAL32, mega_bytes = 1024 * 1024
    character ( len = * ), parameter :: prec_type = 'REAL64'
    character ( len = * ), parameter :: myProgram = 'program timer_memory' ! self-identification

    ! variables
    real ( rp ), allocatable :: array ( : )
    real                     :: t_clock
    integer ( INT64 )        :: myIO, iostat, stat, k
    integer ( INT64 )        :: clock_count_start, clock_count_stop, clock_count_delta, &
                                clock_count_rate, clock_count_max
    character ( len = 512 )  :: iomsg, errmsg, file_name, host

        call execute_command_line ( 'cp /proc/cpuinfo $HOME/cpuinfo.txt' )
        call execute_command_line ( 'cp /proc/meminfo $HOME/meminfo.txt' )
        stat = hostnm ( host )
        call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )   ! query rate

        file_name = prec_type // '_' // trim ( host ) // '.txt'
        open  ( newunit = myIO, file = file_name, iostat = iostat, iomsg = iomsg )

        do k = 1, 1024
            call system_clock ( clock_count_start )
                allocate ( array ( 1 : k * mega_bytes ), stat = stat, errmsg = errmsg )
                if ( stat /= 0 ) then
                    write ( myIO, 100 ) ''
                    write ( myIO, 110 ) k * mega_bytes, k, prec_type
                    write ( myIO, 120 ) iomsg
                    write ( myIO, 130 ) stat
                    flush ( myIO )
                end if
                array ( : ) = 1.0_rp
                deallocate ( array, stat = stat, errmsg = errmsg )
                if ( stat /= 0 ) then
                    write ( myIO, 100 ) 'de'
                    write ( myIO, 110 ) k * mega_bytes, k, prec_type
                    write ( myIO, 120 ) iomsg
                    write ( myIO, 130 ) stat
                    flush ( myIO )
                end if
            call system_clock ( clock_count_stop )
            clock_count_delta = clock_count_stop - clock_count_start
            t_clock = 1.0_rp * clock_count_delta
            t_clock = t_clock / clock_count_rate ! seconds
            write ( myIO, '( g0, ", ", g0 )' ) k * sizeof ( 1.0_rp ), t_clock
            flush ( myIO )
        end do

        close ( myIO )

        stop "successful completion for " // myProgram // "..."  ! string must reduce to constant expression

    100 format ( g0, 'allocation error' )
    110 format ( 'requested size is ', g0, ' bytes (', g0,' MB); kind = ', g0 )
    120 format ( 'iomsg = ', g0, '.' )
    130 format ( 'stat = ', g0 )

end program timer_memory
