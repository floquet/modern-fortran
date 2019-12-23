program flops

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mFileHandling,                  only : safeopen_writeappend
    use mHarvestBlade,                  only : blade_id
    use mTestSuite,                     only : scalar_tangent_test_bundler

    implicit none

    ! parameters
    integer,     parameter :: ip = INT64, rp = REAL64
    real ( rp ), parameter :: zero = 0.0_rp

    !integer :: dtg ( 1 : 8 )  ! date time group
    integer :: status_hostnm = 0, io_out_results = 0, pid = 0

    character ( len = 512 ) :: host_name = '', machine_name = '', jobid = '', file_results = '', path_results = ''
    character ( len = 512 ) :: descriptor_test = 'test_001'
    character ( len =   2 ) :: rack = '', icu = '', blade = ''
    character ( len =   8 ) :: date = ''
    character ( len =  10 ) :: time_start = '', time_stop = ''

    ! scalar_tangent_test_bundler arguments
    real ( rp )    :: nu = zero, sigma_nu = zero, nu_max = zero, nu_min = zero, alpha = zero
    real ( rp )    :: time_cpu_squared_total = zero, time_wall_squared_total = zero
    integer ( ip ) :: iterations = 0, measures = 0

        status_hostnm = hostnm ( host_name )
        host_name = 'r0i0n0'  ! Mac hack
        call get_environment_variable ( "BC_HOST", machine_name ) ! e.g. topaz
        ! call execute_command_line ( 'export JOBID = ' // trim ( pid_string ) )
        call get_environment_variable ( "JOBID", jobid ) ! e.g. topaz
        pid = getpid ( )
        write ( jobid, '( g0 )' ) pid
        call get_environment_variable ( "results", path_results ) ! e.g. /p/home/dantopa/results

        file_results = trim ( path_results ) // '/' // trim ( machine_name ) // '_' // trim ( descriptor_test ) // '.csv'
        write ( *, '( "file name = ", g0, "." )' ) trim ( file_results )

        ! static data
        call blade_id ( host_name = host_name, rack = rack, icu = icu, blade = blade )
        ! dynamic data
        call date_and_time ( time = time_start )
        ! run test
        iterations = 10_ip ** 8
        measures   = 20_ip

        call scalar_tangent_test_bundler ( nu = nu, sigma_nu = sigma_nu, nu_max = nu_max, nu_min = nu_min, alpha = alpha, &
                                           time_cpu_squared_total = time_cpu_squared_total,                               &
                                           time_wall_squared_total = time_wall_squared_total,                             &
                                           iterations = iterations, measures = measures )
        ! dynamic data
        call date_and_time ( date = date, time = time_stop )
        ! write data
        io_out_results = safeopen_writeappend ( file_results )
        write ( io_out_results, 100 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( blade ),                        &
                                      trim ( jobid ), time_start ( 1 : 6 ), time_stop ( 1 : 6 ), date, trim ( descriptor_test ), &
                                      iterations, measures, nu, sigma_nu, nu_max, nu_min, alpha,                                 &
                                      time_cpu_squared_total, time_wall_squared_total
        close ( io_out_results )

    100 format ( 17( g0, ', '), g0 )

    stop 'successful completion for flops ...'

end program flops
