program averager

    use, intrinsic :: iso_fortran_env, only : REAL64, INT64
    !use, intrinsic :: IFLPORT!, only : hostnm
    use :: IFLPORT, only : hostnm

    implicit none

    ! parameters
    integer ( INT64 ),     parameter :: rp = REAL64, ip = INT64, nMeasure = 10
    real ( rp ),           parameter :: one = 1.0_rp, zero = 0.0_rp
    character ( len = * ), parameter :: myType = 'REAL64'
    character ( len = * ), parameter :: myProgram = 'program averager' ! self-identification

    ! variables
    real ( rp ), allocatable :: array ( : )
    real ( rp )              :: t_total, t_total2, t_mean, t_mean2, t_sd
    integer ( ip )           :: myIO, istat, io_stat, host_status, j, k, l, megabytes
    integer ( ip )           :: clock_count_start, clock_count_stop, clock_count_delta, &
                                clock_count_rate, clock_count_max
    character ( len = 512 )  :: iomsg, errmsg, host, file_name

        host_status = hostnm( host )
        call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )   ! query rate

        ! call execute_command_line ( 'cp /proc/cpuinfo $alloc/cpuinfo.txt' )
        ! call execute_command_line ( 'cp /proc/meminfo $alloc/meminfo.txt' )

        file_name = myType // '_' // trim ( host ) // '.txt'
        open  ( newunit = myIO, file = file_name, iostat = io_stat, iomsg = iomsg )

        do j = 1, 10 * 1024 * 1024    ! sweep through array size
            ! clear accumulators for time averages
            t_total  = zero; t_total2 = zero
            t_mean   = zero; t_mean2  = zero
            t_sd     = zero
            averaging: do l = 1, nMeasure   ! average time measurements
                call system_clock ( clock_count_start )
                    megabytes = j * 1024 * 1024
                    allocate ( array ( 1 : megabytes ), stat = istat, errmsg = errmsg )
                    if ( istat /= 0 ) then
                        write ( * , 100 )
                        write ( * , 110 ) k, myType
                        write ( * , 130 ) istat
                        write ( * , 120 ) trim ( errmsg )
                        write ( * , 140 ) l
                    end if
                    array ( : ) = one
                    deallocate ( array, stat = istat, errmsg = errmsg )
                    if ( istat /= 0 ) then
                        write ( * , 100 )
                        write ( * , 110 ) k, myType
                        write ( * , 130 ) istat
                        write ( * , 120 ) trim ( errmsg )
                        write ( * , 140 ) l
                    end if
                call system_clock ( clock_count_stop )
                clock_count_delta = clock_count_stop - clock_count_start
                t_total  = t_total  + clock_count_delta
                t_total2 = t_total2 + clock_count_delta ** 2
            end do averaging
            ! compute mean and standard deviation of allocation times
            t_mean = t_total / nMeasure; t_mean2  = t_total2 / nMeasure
            t_sd   = sqrt ( t_mean2 - t_mean ** 2 )
            t_mean = t_mean / clock_count_rate ! seconds
            t_sd   = t_sd   / clock_count_rate ! seconds
            write ( myIO, '( g0, ", ", g0, ", ", g0 )' ) sizeof ( array ), t_mean, t_sd
            flush ( myIO )
        end do

        close ( myIO )

        stop "successful completion for " // myProgram // "..."  ! string must reduce to constant expression

    100 format ( g0, 'allocation error' )
    110 format ( 'requested size is ', g0, ' bytes (', g0,' MB); kind = ', g0 )
    120 format ( 'iomsg = ', g0, '.' )
    130 format ( 'stat  = ', g0 )
    140 format ( 'sweep = ', g0 )

end program averager
