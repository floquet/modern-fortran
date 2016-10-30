program stress

    !use, intrinsic :: iso_fortran_env,  only : INT64, REAL64
    use mFileHandling,                  only : safeopen_writereplace
    use mTestSuite,                     only : tangent_test, matmul_test
    use mTimerClock,                    only : timer_clock
    use mTimerCPU,                      only : timer_cpu
    use mTimeStamp,                     only : timestamp

    implicit none
    !integer, parameter :: ip = INT64, rp = REAL64
    integer :: status = 0, io_results = 0, rank = 0, numPE = 0
    character ( len = 128 ) :: host_name = '', machine_name = '', yo = ''

    ! derived types
    type ( timer_clock ) :: global_timer_clock
    type ( timer_cpu )   :: global_timer_cpu

        call MPI_INIT ( ierr )
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        status = hostnm ( host_name )
        call get_environment_variable ( "BC_HOST", machine_name )
        yo = trim ( machine_name ) // "_" // trim ( host_name )
        io_results = safeopen_writereplace ( trim ( yo ) // "_stress.txt" )

        info = 'cp /proc/cpuinfo $HOME/info_' // trim ( yo ) // '_cpu.txt'
        call execute_command_line ( trim ( info ) )
        info = 'cp /proc/cpuinfo $HOME/info_' // trim ( yo ) // '_mem.txt'
        call execute_command_line ( trim ( info ) )

        !   start global timers
        call global_timer_cpu   % timer_start_cpu   ( )
        call global_timer_clock % timer_start_clock ( )

            call tangent_test ( io_results )
            call matmul_test  ( io_results )

        write ( io_results, '( /, "global CPU time  = ", g0, " s" )' ) global_timer_cpu   % time_elapsed_cpu ( )
        write ( io_results, '(    "glocal wall time = ", g0, " s" )' ) global_timer_clock % time_elapsed_clock ( )
        write ( io_results, * ) timestamp ( )

        call MPI_FINALIZE ( ierr )  ! clean up and go home

        stop 'successful completion for cpu.f08...'
    !120 format ( g0, ' utilization', / )

end program stress
