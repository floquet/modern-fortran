program vector_flops

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mFileHandling,                  only : safeopen_writeappend
    use mHarvestnode,                   only : node_id
    use mTestSuite,                     only : scalar_tangent_test_bundler
    !use mMPI_SGI,                       only : MPI_COMM_WORLD  ! SGI
    include "mpif.h"
    !use mpi,                            only : MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_COMM_WORLD, MPI_FINALIZE ! Mac

    !implicit none

    external :: MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_FINALIZE

    ! parameters
    integer,        parameter :: ip = INT64, rp = REAL64
    integer ( ip ), parameter :: length = 1024 * 1024, iterations = 10_ip ** 9, measures = 20_ip  ! check format descriptor 110 for measures
    real ( rp ),    parameter :: zero = 0.0_rp

    !integer :: dtg ( 1 : 8 )  ! date time group
    integer :: status_hostnm = 0, io_out_sequence = 0, io_out_results = 0!, pid = 0
    integer :: rank = 0, ierr = 0, numPE = 0

    character ( len = 512 ) :: host_name = '', machine_name = '', jobid = '', file_results = '', path_results = '', str_rank = ''
    character ( len = 512 ) :: descriptor_test = 'test_002'  ! atan-tan loop, vector
    character ( len =   2 ) :: rack = '', icu = '', node = ''
    character ( len =   8 ) :: date = ''
    character ( len =  10 ) :: time_start = '', time_stop = ''

    ! scalar_tangent_test_bundler arguments
    real ( rp ) :: frequency_sequence ( 1 : measures ) = zero
    real ( rp ) :: nu_ave = zero, nu_var = zero, nu_max = zero, nu_min = zero, alpha = zero, time_cpu_total = zero

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        write ( str_rank, '( g0 )' ) rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        status_hostnm = hostnm ( host_name )
        call get_environment_variable ( "BC_HOST", machine_name ) ! e.g. topaz
        call get_environment_variable ( "PBS_JOBID", jobid )      ! e.g. 351268
        call get_environment_variable ( "results", path_results ) ! e.g. /p/home/dantopa/results

        ! static data
        !pid = getpid ( )  ! Mac hack
        !write ( jobid, '( g0 )' ) pid  ! Mac hack
        !host_name = 'r1i2n3'  ! Mac hack
        call node_id ( host_name = host_name, rack = rack, icu = icu, node = node )

        ! dynamic data
        call date_and_time ( time = time_start )

        ! run test
        call scalar_tangent_test_bundler ( nu_ave = nu_ave, nu_var = nu_var, nu_max = nu_max, nu_min = nu_min,  & 
                                           time_cpu_total = time_cpu_total, alpha = alpha, length = length,     &
                                           iterations = iterations, measures = measures, frequency_sequence = frequency_sequence )
        ! dynamic data
        call date_and_time ( date = date, time = time_stop )

        ! write summary data
        file_results = trim ( path_results ) // '/summary' // '_' // trim ( machine_name ) // '_' // trim ( descriptor_test ) // &
                       '_' // trim ( rack ) // '_'  // trim ( icu ) // '_'  // trim ( node ) // '_' // trim ( str_rank ) // '.csv'
        !print *, file_results
        io_out_results = safeopen_writeappend ( file_results )
        write ( io_out_results, 100 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( node ), rank,   & ! 5
                                      trim ( jobid ), time_cpu_total, time_start ( 1 : 6 ), time_stop ( 1 : 6 ), & ! 4
                                      date, trim ( descriptor_test ), length, iterations, measures,              & ! 5 
                                      nu_ave, nu_var, nu_max, nu_min, alpha                                      & ! 5
        close ( io_out_results )

        ! write sequence of times
        file_results = trim ( path_results ) // '/rawdata' // '_' // trim ( machine_name ) // '_' // trim ( descriptor_test ) // &
                       '_' // trim ( rack ) // '_'  // trim ( icu ) // '_'  // trim ( node ) // '_' // trim ( str_rank ) // '.csv'
        !print *, file_results
        io_out_sequence = safeopen_writeappend ( file_results )
        write ( io_out_sequence, 110 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( node ), trim ( jobid ),         & 
                                       time_cpu_total, time_start ( 1 : 6 ), time_stop ( 1 : 6 ), date, trim ( descriptor_test ), &
                                       frequency_sequence
        close ( io_out_sequence )

        write ( * , 120 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( node ), rank, numPE, time_stop

        call MPI_FINALIZE ( ierr )  ! clean up and go home

        stop

    100 format (  18 ( g0, ', '), g0 )
    110 format ( 200 ( g0, ', ' ) )
    120 format ( 'completion: machine = ', g0, ', rack = ', g0, ', icu = ', g0, ', node = ', g0, ', rank = ', g0, ' / ', g0, &
                 ', time = ', g0, '.' )

end program vector_flops

