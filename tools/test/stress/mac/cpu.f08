program stress

    !use, intrinsic :: iso_fortran_env,  only : INT64, REAL64
    use mFileHandling,                  only : safeopen_writereplace
    !use mpt_h,                          only : MPI_COMM_WORLD   ! SGI
    !include 'mpif.h'
    use mpi,                            only : MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_COMM_WORLD, MPI_FINALIZE ! Mac
    use mTestSuite,                     only : tangent_test, matmul_test
    use mTimerClock,                    only : timer_clock
    use mTimerCPU,                      only : timer_cpu
    use mTimeStamp,                     only : timestamp

    implicit none

    integer :: status = 0, io_results = 0, rank = 0, numPE = 0, io_stat = 0, ierr = 0
    character ( len = 512 ) :: host_name = '', machine_name = '', yo = ''
<<<<<<< HEAD
    character ( len = 512 ) :: info = '', str_rank = '', myPath = '', myGit = '', io_msg = ''
=======
    character ( len = 512 ) :: info = '', str_rank = '', io_msg = ''
    character ( len = 512 ) :: path_results = '', path_times = '', path_info = ''
>>>>>>> b55ff2259f36705faf34d4f296c30b54e7a97547

    ! derived types
    type ( timer_clock ) :: global_timer_clock
    type ( timer_cpu )   :: global_timer_cpu
<<<<<<< HEAD

    external :: MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_FINALIZE
=======
    ! external :: MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_FINALIZE
>>>>>>> b55ff2259f36705faf34d4f296c30b54e7a97547

        call MPI_INIT ( ierr )
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        status = hostnm ( host_name )
<<<<<<< HEAD
        call get_environment_variable ( "BC_HOST", machine_name )
        call get_environment_variable ( "myGit", myGit )
        call get_environment_variable ( "pwd", myPath )

        write ( unit = str_rank, fmt = '( I5.5 )', iostat = io_stat, iomsg = io_msg ) rank
        if ( io_stat /= 0 ) then
            write ( *, '( /, "Error writing rank = ", g0," to string variable str_rank (len = 512 )" )' ) rank
            write ( *, '( "iomsg = ",  g0, "." )' ) trim ( io_msg )
            write ( *, '( "iostat = ", g0, "." )' ) io_stat
        end if
        yo = trim ( myGit ) // 'transporter/' // trim ( machine_name ) // "_" // trim ( host_name ) // "_" // trim ( str_rank )
        io_results = safeopen_writereplace ( trim ( yo ) // "_stress.txt" )

        call execute_command_line ( 'mkdir ' // trim ( myPath) // 'info' )
        info = 'cp /proc/cpuinfo ' // trim ( myPath ) // '/info/info_' // trim ( yo ) // '_cpu.txt'
        call execute_command_line ( trim ( info ) )
        info = 'cp /proc/cpuinfo ' // trim ( myPath ) // '/info/info_' // trim ( yo ) // '_mem.txt'
        call execute_command_line ( trim ( info ) )
        write ( io_results, '( g0, " processors in use" )' ) numPE
=======
        call get_environment_variable ( "BC_HOST", machine_name ) ! e.g. topaz
        call get_environment_variable ( "results", path_results ) ! e.g. /p/home/dantopa/results
        path_times = trim ( path_results ) // '/times/'
        path_info  = trim ( path_results ) // '/info/'
        write ( *, * ) 'path_times = ', path_times

        yo = trim ( machine_name ) // "_" // trim ( host_name ) // "_"  ! machine and node

        write ( unit = str_rank, fmt = '( I5.5 )', iostat = io_stat, iomsg = io_msg ) rank  ! string version of the rank
        if ( io_stat /= 0 ) then
            write ( *, '( /, "Error writing rank = ", g0," to string variable str_rank (len = 512 )" )' ) rank
            write ( *, '( "iomsg  = ", g0, "." )' ) trim ( io_msg )
            write ( *, '( "iostat = ", g0, "." )' ) io_stat
        end if

        io_results = safeopen_writereplace ( trim ( path_times ) // trim ( yo ) // trim ( str_rank ) // "_stress.txt" )
        write ( io_results, '( g0, " processors in use" )' ) numPE
        flush ( io_results )

        info = 'cp /proc/cpuinfo ' // trim ( path_info ) // trim ( yo ) // trim ( str_rank ) // '_cpu.txt'
        call execute_command_line ( trim ( info ) )
        info = 'cp /proc/meminfo ' // trim ( path_info ) // trim ( yo ) // trim ( str_rank ) // '_mem.txt'
        call execute_command_line ( trim ( info ) )
>>>>>>> b55ff2259f36705faf34d4f296c30b54e7a97547

        !   start global timers
        call global_timer_cpu   % timer_start_cpu   ( )
        call global_timer_clock % timer_start_clock ( )

            call matmul_test  ( io_results, rank, machine_name, host_name )
            call tangent_test ( io_results, rank, machine_name, host_name )

        write ( *, '( /, "global CPU time  = ", g0, " s" )' ) global_timer_cpu   % time_elapsed_cpu ( )
        write ( *, '(    "global wall time = ", g0, " s" )' ) global_timer_clock % time_elapsed_clock ( )
        write ( *, * ) timestamp ( )

        call MPI_FINALIZE ( ierr )  ! clean up and go home

        stop !'successful completion for cpu.f08...'

end program stress