program flops

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mFileHandling,                  only : safeopen_writeappend
    use mHarvestnode,                   only : node_id
    use mTestSuite,                     only : scalar_tangent_test_bundler
    !use mpt_h,                          only : MPI_COMM_WORLD   ! SGI
    !include 'mpif.h'
    use mpi,                            only : MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_COMM_WORLD, MPI_FINALIZE ! Mac

    implicit none

    ! parameters
    integer,        parameter :: ip = INT64, rp = REAL64
    integer ( ip ), parameter :: iterations = 10_ip ** 9, measures = 20  ! check format descriptor 110 for measures
    real ( rp ),    parameter :: zero = 0.0_rp

    !integer :: dtg ( 1 : 8 )  ! date time group
    integer :: status_hostnm = 0, io_out_sequence = 0, io_out_results = 0, pid = 0
    integer :: rank = 0, ierr = 0

    character ( len = 512 ) :: host_name = '', machine_name = '', jobid = '', file_results = '', path_results = ''
    character ( len = 512 ) :: descriptor_test = 'test_001'
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

        status_hostnm = hostnm ( host_name )
        call get_environment_variable ( "BC_HOST", machine_name ) ! e.g. topaz
        call get_environment_variable ( "JOBID", jobid ) ! e.g. topaz
        call get_environment_variable ( "results", path_results ) ! e.g. /p/home/dantopa/results

        file_results = trim ( path_results ) // '/' // trim ( machine_name ) // '_' // trim ( descriptor_test ) // '.csv'
        !write ( *, '( "file name = ", g0, "." )' ) trim ( file_results )

        ! static data
        pid = getpid ( )  ! Mac hack
        write ( jobid, '( g0 )' ) pid  ! Mac hack
        host_name = 'r1i2n3'  ! Mac hack
        call node_id ( host_name = host_name, rack = rack, icu = icu, node = node )

        ! dynamic data
        call date_and_time ( time = time_start )

        ! run test
        call scalar_tangent_test_bundler ( nu_ave = nu_ave, nu_var = nu_var, nu_max = nu_max, nu_min = nu_min, alpha = alpha, &
                                           iterations = iterations, measures = measures, frequency_sequence = frequency_sequence )
        ! dynamic data
        call date_and_time ( date = date, time = time_stop )

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

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    100 format (  16 ( g0, ', '), g0 )
    110 format ( 200 ( g0, ' ' ) )

    stop 'successful completion for flops ...'

end program flops

! rditldmt@ITL-DTOPA-MP:delta $ pwd
! /Users/rditldmt/hpc/fortran/tools/test/stress/flopper/delta
! rditldmt@ITL-DTOPA-MP:delta $ date
! Mon Jun  6 17:23:34 CDT 2016
! rditldmt@ITL-DTOPA-MP:delta $ make
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_harvest_blade.o mod_harvest_blade.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_CPU.o mod_timer_CPU.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_clock.o mod_timer_clock.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_test_suite.o mod_test_suite.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o flops.o flops.f08
! mpif90 -g -o flops flops.o mod_file_handling.o mod_harvest_blade.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o
! rditldmt@ITL-DTOPA-MP:delta $ mpirun -np 4 ./flops
! file name = /Users/rditldmt/results/ITL-DTOPA-MP_test_001.csv.
! k_measure = 1 of 10: time 172424.
! file name = /Users/rditldmt/results/ITL-DTOPA-MP_test_001.csv.
! k_measure = 1 of 10: time 172424.
! file name = /Users/rditldmt/results/ITL-DTOPA-MP_test_001.csv.
! file name = /Users/rditldmt/results/ITL-DTOPA-MP_test_001.csv.
! k_measure = 1 of 10: time 172424.
! k_measure = 1 of 10: time 172424.
! k_measure = 2 of 10: time 172425.
! k_measure = 2 of 10: time 172425.
! k_measure = 2 of 10: time 172425.
! k_measure = 2 of 10: time 172425.
! k_measure = 3 of 10: time 172426.
! k_measure = 3 of 10: time 172426.
! k_measure = 3 of 10: time 172426.
! k_measure = 3 of 10: time 172426.
! k_measure = 4 of 10: time 172427.
! k_measure = 4 of 10: time 172427.
! k_measure = 4 of 10: time 172427.
! k_measure = 4 of 10: time 172427.
! k_measure = 5 of 10: time 172427.
! k_measure = 5 of 10: time 172427.
! k_measure = 5 of 10: time 172427.
! k_measure = 5 of 10: time 172427.
! k_measure = 6 of 10: time 172428.
! k_measure = 6 of 10: time 172428.
! k_measure = 6 of 10: time 172428.
! k_measure = 6 of 10: time 172428.
! k_measure = 7 of 10: time 172429.
! k_measure = 7 of 10: time 172429.
! k_measure = 7 of 10: time 172429.
! k_measure = 7 of 10: time 172429.
! k_measure = 8 of 10: time 172430.
! k_measure = 8 of 10: time 172430.
! k_measure = 8 of 10: time 172430.
! k_measure = 8 of 10: time 172430.
! k_measure = 9 of 10: time 172431.
! k_measure = 9 of 10: time 172431.
! k_measure = 9 of 10: time 172431.
! k_measure = 9 of 10: time 172431.
! k_measure = 10 of 10: time 172431.
! k_measure = 10 of 10: time 172431.
! k_measure = 10 of 10: time 172431.
! k_measure = 10 of 10: time 172432.
! Warning: /Users/rditldmt/results/ITL-DTOPA-MP_test_001.csv doesn't exist; new empty file will be created.
! Warning: /Users/rditldmt/results/ITL-DTOPA-MP_test_001.log doesn't exist; new empty file will be created.
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
! STOP successful completion for flops ...
