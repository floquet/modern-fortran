program flops

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mFileHandling,                  only : safeopen_writeappend
    use mHarvestnode,                   only : node_id
    use mTestSuite,                     only : scalar_tangent_test_bundler
    !use mMPI_SGI,                       only : MPI_COMM_WORLD  ! SGI
    include "mpif.h"
    !use mpi,                            only : MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_COMM_WORLD, MPI_FINALIZE ! Mac

    !implicit none

    external :: MPI_INIT, MPI_COMM_RANK, MPI_FINALIZE

    ! parameters
    integer,        parameter :: ip = INT64, rp = REAL64
    integer ( ip ), parameter :: iterations = 10_ip ** 9, measures = 10_ip  ! check format descriptor 110 for measures
    real ( rp ),    parameter :: zero = 0.0_rp

    !integer :: dtg ( 1 : 8 )  ! date time group
    integer :: status_hostnm = 0, io_out_sequence = 0, io_out_results = 0!, pid = 0
    integer :: rank = 0, ierr = 0

    character ( len = 256 ) :: host_name = '', machine_name = '', jobid = '', file_results = '', path_results = ''
    character ( len = 256 ) :: descriptor_test = 'test_001'
    character ( len =   2 ) :: rack = '', icu = '', node = ''
    character ( len =   8 ) :: date = ''
    character ( len =  10 ) :: time_start = '', time_stop = ''

    ! scalar_tangent_test_bundler arguments
    real ( rp ) :: frequency_sequence ( 1 : measures )
    real ( rp ) :: nu_ave = zero, nu_var = zero, nu_max = zero, nu_min = zero, alpha = zero

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        !call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        print *, 'a'
        status_hostnm = hostnm ( host_name )
        call get_environment_variable ( "BC_HOST", machine_name ) ! e.g. topaz
        call get_environment_variable ( "JOBID", jobid ) ! e.g. topaz
        call get_environment_variable ( "results", path_results ) ! e.g. /p/home/dantopa/results

        print *, 'b'
        file_results = trim ( path_results ) // '/' // trim ( machine_name ) // '_' // trim ( descriptor_test ) // '.csv'
        write ( *, '( "file name = ", g0, "." )' ) trim ( file_results )

        print *, 'c'
        ! static data
        !pid = getpid ( )  ! Mac hack
        !write ( jobid, '( g0 )' ) pid  ! Mac hack
        !host_name = 'r1i2n3'  ! Mac hack
        call node_id ( host_name = host_name, rack = rack, icu = icu, node = node )

        ! dynamic data
        call date_and_time ( time = time_start )

        ! run test
        call scalar_tangent_test_bundler ( nu_ave = nu_ave, nu_var = nu_var, nu_max = nu_max, nu_min = nu_min, alpha = alpha, &
                                           iterations = iterations, measures = measures, frequency_sequence = frequency_sequence )
        ! dynamic data
        call date_and_time ( date = date, time = time_stop )

        print *, 'd'
        ! write summary data
        io_out_results = safeopen_writeappend ( file_results )
        write ( io_out_results, 100 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( node ), rank,                   & ! 5
                                      trim ( jobid ), time_start ( 1 : 6 ), time_stop ( 1 : 6 ), date, trim ( descriptor_test ), & ! 5
                                      iterations, measures, nu_ave, nu_var, nu_max, nu_min, alpha                                  ! 7
        close ( io_out_results )

        ! write sequence of times
        file_results = trim ( path_results ) // '/' // trim ( machine_name ) // '_' // trim ( descriptor_test ) // '.log'
        io_out_sequence = safeopen_writeappend ( file_results )
        write ( io_out_sequence, 110 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( node ), trim ( jobid ), &
                                       time_start ( 1 : 6 ), time_stop ( 1 : 6 ), date, trim ( descriptor_test ), frequency_sequence
        close ( io_out_sequence )

	write ( * , 120 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( node ), rank

        call MPI_FINALIZE ( ierr )  ! clean up and go home

        stop

    100 format (  16 ( g0, ', '), g0 )
    110 format ( 200 ( g0, ' ' ) )
    120 format ( 'completion: ', 4 ( g0, ', ' ), g0 )

end program flops

! dantopa@r7i4n14:echo_thunder $ mpiexec_mpt ./flops
! 	MPT Environmental Settings
! MPT: MPI_ARRAY (default: ) : 
! MPT: MPI_BUFFER_MAX (default: not set) : not set
! MPT: MPI_BUFS_LIMIT (default: 32) : 32
! MPT: MPI_BUFS_PER_PROC (default: 128) : 128
! MPT: MPI_CHECK_ARGS (default: disabled) : disabled
! MPT: MPI_CLOCKSOURCE (default: ) : 
! MPT: MPI_COLL_A2A_FRAG (default: 2097152) : 2097152
! MPT: MPI_COLL_CLUSTER_OPT (default: enabled) : enabled
! MPT: MPI_COLL_GATHERV (default: 65536) : 65536
! MPT: MPI_COLL_LEADERS (default: enabled) : enabled
! MPT: MPI_COLL_NUMA_THRESHOLD (default: 4) : 4
! MPT: MPI_COLL_OPT (default: enabled) : enabled
! MPT: MPI_COLL_OPT_VERBOSE (default: disabled) : disabled
! MPT: MPI_COLL_PREREG (default: enabled) : enabled
! MPT: MPI_COLL_RED_RB_MIN (default: 16384) : 16384
! MPT: MPI_COLL_REPRODUCIBLE (default: disabled) : disabled
! MPT: MPI_COLL_SYNC (default: not set) : not set
! MPT: MPI_COMM_MAX (default: 256) : 256
! MPT: MPI_COREDUMP (default: FIRST) : FIRST
! MPT: MPI_COREDUMP_DEBUGGER (default: gdb) : gdb
! MPT: MPI_CPR (default: disabled) : disabled
! MPT: MPI_CUDA_BUFFER_MAX (default: 10485760) : 10485760
! MPT: MPI_DEFAULT_SINGLE_COPY_BUFFER_MAX (default: 2000) : 2000
! MPT: MPI_DEFAULT_SINGLE_COPY_OFF (default: 0) : 0
! MPT: MPI_DIR (default: ) : 
! MPT: MPI_DISPLAY_SETTINGS (default: disabled) : enabled
! MPT: MPI_DSM_CPULIST (default: not set) : not set
! MPT: MPI_DSM_DISTRIBUTE (default: enabled) : enabled
! MPT: MPI_DSM_OFF (default: disabled) : disabled
! MPT: MPI_DSM_VERBOSE (default: disabled) : disabled
! MPT: MPI_GATHER_RANKS (default: enabled) : enabled
! MPT: MPI_GROUP_MAX (default: 32) : 32
! MPT: MPI_GRU_BUFFER_MAX (default: 32768) : 32768
! MPT: MPI_GRU_CBS (default: 0) : 0
! MPT: MPI_GRU_ENABLED (default: disabled) : disabled
! MPT: MPI_GRU_GAMIR_GET (default: enabled) : enabled
! MPT: MPI_GRU_SIG_INTERVAL (default: 5) : 5
! MPT: MPI_HUGEPAGE_HEAP_SPACE (default: 0) : 0
! MPT: MPI_HUGEPAGE_MSGS (default: disabled) : disabled
! MPT: MPI_LAUNCH_TIMEOUT (default: 20) : 20
! MPT: MPI_MAP_POWER2 (default: 0) : 0
! MPT: MPI_MAPPED_HEAP_SIZE (default: not set) : not set
! MPT: MPI_MAPPED_STACK_SIZE (default: not set) : not set
! MPT: MPI_MEM_ALIGN (default: 0) : 0
! MPT: MPI_MEMMAP_OFF (default: disabled) : enabled
! MPT: MPI_MSG_RETRIES (default: 200000) : 200000
! MPT: MPI_MSG_MEM (default: not set) : not set
! MPT: MPI_NAP (default: not set) : not set
! MPT: MPI_NUM_QUICKS (default: 8) : 8
! MPT: MPI_PREFAULT_HEAP (default: 0) : 0
! MPT: MPI_QUERYABLE (default: disabled) : disabled
! MPT: MPI_REQUEST_DEBUG (default: disabled) : disabled
! MPT: MPI_REQUEST_MAX (default: 16384) : 16384
! MPT: MPI_RESET_PATH (default: ) : 
! MPT: MPI_SHARED_NEIGHBORHOOD (default: BLADE) : BLADE
! MPT: MPI_SHEPHERD (default: disabled) : disabled
! MPT: MPI_SIGTRAP (default: disabled) : disabled
! MPT: MPI_STATS (default: disabled) : disabled
! MPT: MPI_STATS_FILE (default: ) : 
! MPT: MPI_STATUS_SIGNAL (default: not set) : not set
! MPT: MPI_SUPERPAGE_HEAP_SPACE (default: 0) : 0
! MPT: MPI_SYSLOG_COPY (default: disabled) : disabled
! MPT: MPI_TYPE_DEPTH (default: 14) : 14
! MPT: MPI_TYPE_MAX (default: 8192) : 8192
! MPT: MPI_UNBUFFERED_STDIO (default: disabled) : disabled
! MPT: MPI_UNIVERSE (default: ) : 
! MPT: MPI_UNIVERSE_SIZE (default: 0) : 0
! MPT: MPI_USE_CUDA (default: disabled) : disabled
! MPT: MPI_USE_GRU (default: disabled) : disabled
! MPT: MPI_USE_XPMEM (default: disabled) : disabled
! MPT: MPI_VERBOSE (default: disabled) : disabled
! MPT: MPI_VERBOSE2 (default: disabled) : disabled
! MPT: MPI_WATCHDOG_TIMER (default: 10) : 10
! MPT: MPI_WIN_MODE (default: DEFAULT) : DEFAULT
! MPT: MPI_WORLD_MAP (default: ) : 
! MPT: MPI_XPMEM_ENABLED (default: enabled) : enabled
! MPT: MPI_XPMEM_SHARED (default: enabled) : enabled
! MPT: MPIO_DIRECT_READ (default: disabled) : disabled
! MPT: MPIO_DIRECT_WRITE (default: disabled) : disabled
! MPT: MPIO_DIRECT_READ_CHUNK_SIZE (default: 0) : 0
! MPT: MPIO_DIRECT_WRITE_CHUNK_SIZE (default: 0) : 0
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! file name = /home/dantopa/results//Thunder_test_001.csv.
! Warning: /home/dantopa/results//Thunder_test_001.csv doesn't exist; new empty file will be created.
! Warning: /home/dantopa/results//Thunder_test_001.log doesn't exist; new empty file will be created.
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! dantopa@r7i4n14:echo_thunder $ =>> PBS: job killed: walltime 602 exceeded limit 600
!
! qsub: job 340622.thunder-b01 completed
! dantopa@thunder09:echo_thunder $ pwd
! /home/dantopa/hpc/fortran/tools/test/stress/flopper/delta/echo_thunder

