! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program allocation_times

    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64, INT32, INT64, compiler_version, compiler_options

    implicit none

    ! parameters
    integer,        parameter :: ip = INT64, rp = REAL64
    integer ( ip ), parameter :: numElements  = 81, & ! manually match with sequence size
                                 measurements = 15, & ! repeat measurements
                                 mega_bytes   = 1024 * 1024, &
                                 giga_bytes = mega_bytes * 1024, &
                                 myBytes = 8 ! REAL64 needs 8 bytes
    character ( len = * ), parameter :: prec_type = 'R64'
    character ( len = * ), parameter :: myProgram = 'program allocation_times' ! self-identification
    character ( len = * ), parameter :: myPath    = '/transporter/memtimes/'

    ! rank 1
    real ( rp ), allocatable                    :: array ( : ) ! container for REAL64 data
    real ( rp ), dimension ( 1 : measurements ) :: ticks_clock = 0.0_rp ! timer measures ticks, not seconds
    ! rank 0
    real ( rp )                                 :: time_mean = 0.0_rp, time_var = 0.0_rp, gb = 0.0_rp

    ! rank 1
    integer ( ip ), dimension ( 1 : numElements ) :: elements = 0_ip ! sequence of array sizes
    ! rank 0
    integer ( INT32 ) :: host_status = 0
    integer ( ip )    :: j = 0, k = 0, total_bytes = 0, myIO = 0, io_stat = 0, stat = 0
    integer ( ip )    :: clock_count_start = 0, clock_count_stop = 0, &
                         global_start      = 0, global_stop      = 0, &
                         clock_count_rate  = 0, clock_count_max  = 0, clock_count_delta = 0

    character ( len = 512 ) :: file_name = '', mydir = '', myfile = '', &
                               machine_name = '', pbs_jobid = '', pbs_jobname = '', pbs_jobnumber = '', wdhpc = '', host = '', &
                               run_cmd = '', errmsg = '', io_msg = ''
    character ( len =  10 ) :: myTime = ''
    character ( len =   8 ) :: myDate = ''

        call execute_command_line ( 'cp /proc/cpuinfo info_cpu.txt' )
        call execute_command_line ( 'cp /proc/meminfo info_mem.txt' )

        call get_command ( run_cmd )
        call get_environment_variable ( "wdhpc", wdhpc ) ! e.g. root for git repos
        call get_environment_variable ( "BC_HOST", machine_name ) ! e.g. topaz
        call get_environment_variable ( "PBS_JOBID", pbs_jobid ) ! e.g. 123456
        call get_environment_variable ( "PBS_JOBNAME", pbs_jobname ) ! e.g. topaz-big-pbs
        call get_environment_variable ( "JOBID", pbs_jobnumber ) ! e.g. topaz-big-pbs

        call date_and_time ( DATE = myDate, TIME = myTime )
        call system_clock ( clock_count_stop, clock_count_rate, clock_count_max )   ! query rate

        host_status =  hostnm ( host ) ! r23i7n7
        mydir  = trim ( wdhpc ) // myPath // trim ( machine_name ) ! /p/home/dantopa/hpc/transporter/memtimes/topaz
        myfile = trim ( machine_name ) // '_' // trim ( host ) // '_' // prec_type // '_' // trim ( pbs_jobnumber ) // '.txt'
        call execute_command_line ( 'mkdir ' // trim ( mydir ) )
        file_name = trim ( mydir) // '/' // trim ( myfile )

        write ( *, 300 )  myDate ( 1 : 4 ), myDate ( 5 : 6 ), myDate ( 7 : 8 ), myTime ( 1 : 2 ), myTime ( 3 : 4 ), myTime ( 5 : )
        write ( *, 310 )  trim ( run_cmd )
        write ( *, '( "data file: ", A ) ' )     trim ( myfile )
        write ( *, '( "data directory: ", A, / )' ) trim ( mydir )

        open  ( newunit = myIO, file = file_name, iostat = io_stat, iomsg = io_msg )
        if ( io_stat /= 0 ) then
            write ( myIO, 200 ) file_name
            write ( myIO, 210 ) myIO
            write ( myIO, 220 ) trim ( io_msg )
            write ( myIO, 230 ) io_stat
            flush ( myIO )
            stop 'fatal program error attempting to open file'
        end if

        write ( myIO, '( "Fortran compiler version: ", A )'    ) compiler_version ( )
        write ( myIO, '( "Fortran compiler options: ", A, / )' ) compiler_options ( )

        write ( myIO, '( "Machine: ", A, ", node: ", A )' ) trim ( machine_name ), trim ( host )
        write ( myIO, '( "PBS job ID: ", A )' ) trim ( pbs_jobid )
        write ( myIO, '( "PBS job name: ", A )' ) trim ( pbs_jobname )
        write ( myIO, '( "PBS job number: ", A )' ) trim ( pbs_jobnumber )
        write ( myIO, 300 ) myDate ( 1 : 4 ), myDate ( 5 : 6 ), myDate ( 7 : 8 ), myTime ( 1 : 2 ), myTime ( 3 : 4 ), myTime ( 5 :)
        write ( myIO, 310 ) trim ( run_cmd )

        write ( myIO, '( "Number of times measurement is repeated: ", I5 )' ) measurements
        write ( myIO, '( "column order: array size, mean time, variance of time", / )' )

        flush myIO

        call system_clock ( global_start )

        elements = [ ( ( 10_INT64 ** k * j, j = 1, 9 ), k = 3, 11 ) ] ! sample sizes 1000, 2000, 3000, ...

        do k = 1, numElements ! loop over sample sizes
            total_bytes = elements ( k ) * myBytes / mega_bytes
            do j = 1, measurements ! repeat measurement
                call system_clock ( clock_count_start )

                    allocate ( array ( 1 : elements ( k ) ), stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( myIO, 100 ) ''
                        write ( myIO, 110 ) elements ( k ), total_bytes, prec_type
                        write ( myIO, 120 ) trim ( errmsg )
                        write ( myIO, 130 ) stat
                        flush ( myIO )
                        stop 'fatal program error during allocation'
                    end if

                    array ( : ) = 1.0_rp  ! populate

                    deallocate ( array, stat = stat, errmsg = errmsg )
                    if ( stat /= 0 ) then
                        write ( myIO, 100 ) 'de'
                        write ( myIO, 110 ) elements ( k ), total_bytes, prec_type
                        write ( myIO, 120 ) trim ( errmsg )
                        write ( myIO, 130 ) stat
                        flush ( myIO )
                        stop 'fatal program error during decallocation'
                    end if

                call system_clock ( clock_count_stop )
                clock_count_delta = clock_count_stop - clock_count_start
                ticks_clock ( j ) = real ( clock_count_delta, rp )
            end do ! repeat measurement

            time_mean = sum ( ticks_clock ) / real ( clock_count_rate * measurements, rp ) ! conver to seconds
            time_var  = dot_product ( ticks_clock, ticks_clock ) / real ( clock_count_rate ** 2, rp ) ! sum of the squared times
            time_var  = time_var / real ( measurements, rp )  ! mean of the squares
            time_var  = time_var - time_mean ** 2 ! variance ** 2
            if ( abs ( time_var ) < 5.0_rp * epsilon ( 1.0_rp ) ) time_var = 0.0_rp ! safety check
            time_var = sqrt ( time_var ) ! variance

            gb = real ( elements ( k ) * myBytes, rp ) / giga_bytes
            write ( myIO, '( I15, 3 ( ", ", ES10.3E2 ) )' ) elements ( k ), gb, time_mean, time_var
            flush ( myIO )
        end do ! get new allocation number

        call system_clock ( global_stop )
        write ( myIO, '( /, ''total cpu time: '', ES10.3E2, ''s'' )' ) real ( global_stop - global_start, rp ) / &
                                                                       real ( clock_count_rate, rp )
        close ( myIO )

        stop "successful completion for " // myProgram // "..."  ! string must reduce to constant expression

    100 format ( 'Mortal error during ', A, 'allocation...' )
    110 format ( 'requested size is ', I15, ' elements (', I10,' MB); kind = ', A )
    120 format ( 'errmsg = ', I10, '.' )
    130 format ( 'stat = ', A )

    200 format ( 'error attempting to open file ', A )
    210 format ( 'file unit number from NEWUNIT = ', I4 )
    220 format ( 'iomsg = ', I10, '.' )
    230 format ( 'iostat = ', A )

    300 format ( "Date: ", A, " - ", A, " - ", A, ", time: ", A, ":", A, ":", A )
    310 format ( "Program lauched via ", A, ".", / )

end program allocation_times

! rditldmt@ITLDMT-MD-O2034:beta $ pwd
! /Users/rditldmt/hpc/fortran/benchmark/memory/steppers/beta
! rditldmt@ITLDMT-MD-O2034:beta $ gfortran $gflags allocation_times.f08 -o allocation_times
! rditldmt@ITLDMT-MD-O2034:beta $ echo $gflags
! -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5
! rditldmt@ITLDMT-MD-O2034:beta $ gfortran --version
! GNU Fortran (MacPorts gcc7 7-20161002_0) 7.0.0 20161002 (experimental)
! Copyright (C) 2016 Free Software Foundation, Inc.
! This is free software; see the source for copying conditions.  There is NO
! warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!
