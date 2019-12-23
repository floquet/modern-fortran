program averager

    use, intrinsic :: iso_fortran_env, only : REAL64, INT64

    implicit none

    ! parameters
    integer ( INT64 ),     parameter :: rp = REAL64, ip = INT64, nMeasure = 5
    real ( rp ),           parameter :: one = 1.0_rp, zero = 0.0_rp
    character ( len = * ), parameter :: myType = 'REAL64'
    character ( len = * ), parameter :: myProgram = 'program averager' ! self-identification

    ! variables
    real ( rp ), allocatable :: array ( : )
    real ( rp )              :: t_total, t_total2, t_mean, t_mean2, t_sd
    integer ( ip )           :: myIO, iostat, stat, j, k, l, status, megabytes
    integer ( ip )           :: clock_count_start, clock_count_stop, clock_count_delta, &
                                clock_count_rate, clock_count_max
    character ( len = 512 )  :: iomsg, errmsg, host, file_name

        status = hostnm ( host )
        call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )   ! query rate

        call execute_command_line ( 'cp /proc/cpuinfo $HOME/cpuinfo.txt' )
        call execute_command_line ( 'cp /proc/meminfo $HOME/meminfo.txt' )

        file_name = trim ( host ) // '_' // myType // '.txt'
        open  ( newunit = myIO, file = file_name, iostat = iostat, iomsg = iomsg )
        write ( myIO , '( g0, / )' ) myProgram

        do j = 1, 1024    ! sweep through array size
            ! clear accumulators for time averages
            t_total  = zero; t_total2 = zero
            t_mean   = zero; t_mean2  = zero
            t_sd     = zero
            averaging: do l = 1, nMeasure   ! average time measurements
                call system_clock ( clock_count_start )
                    megabytes = j * 1024 * 1024
                    allocate ( array ( 1 : megabytes ), stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( * , 100 )
                        write ( * , 110 ) k, myType
                        write ( * , 130 ) stat
                        write ( * , 120 ) trim ( errmsg )
                        write ( * , 140 ) l
                    end if
                    array ( : ) = one
                    deallocate ( array, stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( * , 100 )
                        write ( * , 110 ) k, myType
                        write ( * , 130 ) stat
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
            write ( myIO, '( g0, ", ", g0, ", ", g0 )' ) sizeof ( array ) / 1024 / 1024, t_mean, t_sd
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

! spirit: use module gcc-compilers/4.8.4

! dan-topas-pro-2:memory rditldmt$ date
! Thu Mar 17 17:20:27 CDT 2016
! dan-topas-pro-2:memory rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/benchmark/memory
! dan-topas-pro-2:memory rditldmt$ gfortran -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 averager.f08
! dan-topas-pro-2:memory rditldmt$ ./a.out
! cp: /proc/cpuinfo: No such file or directory
! cp: /proc/meminfo: No such file or directory
! ^Z
! [2]+  Stopped                 ./a.out
