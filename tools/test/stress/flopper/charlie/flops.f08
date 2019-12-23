program flops

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mFileHandling,                  only : safeopen_writeappend
    use mHarvestBlade,                  only : blade_id
    use mTestSuite,                     only : scalar_tangent_test_bundler

    implicit none

    ! parameters
    integer,        parameter :: ip = INT64, rp = REAL64
    integer ( ip ), parameter :: measures = 10  ! check format descriptor 110
    real ( rp ),    parameter :: zero = 0.0_rp

    !integer :: dtg ( 1 : 8 )  ! date time group
    integer :: status_hostnm = 0, io_out_sequence = 0, io_out_results = 0, pid = 0

    character ( len = 512 ) :: host_name = '', machine_name = '', jobid = '', file_results = '', path_results = ''
    character ( len = 512 ) :: descriptor_test = 'test_001'
    character ( len =   2 ) :: rack = '', icu = '', blade = ''
    character ( len =   8 ) :: date = ''
    character ( len =  10 ) :: time_start = '', time_stop = ''

    ! scalar_tangent_test_bundler arguments
    real ( rp )    :: frequency_sequence ( 1 : measures )
    real ( rp )    :: nu_ave = zero, nu_var = zero, nu_max = zero, nu_min = zero, alpha = zero
    integer ( ip ) :: iterations = 10

        status_hostnm = hostnm ( host_name )
        host_name = 'r1i2n3'  ! Mac hack
        call get_environment_variable ( "BC_HOST", machine_name ) ! e.g. topaz
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
        iterations = 10_ip ** 9

        call scalar_tangent_test_bundler ( nu_ave = nu_ave, nu_var = nu_var, nu_max = nu_max, nu_min = nu_min, alpha = alpha, &
                                           iterations = iterations, measures = measures, frequency_sequence = frequency_sequence )
        ! dynamic data
        call date_and_time ( date = date, time = time_stop )

        ! write summary data
        io_out_results = safeopen_writeappend ( file_results )
        write ( io_out_results, 100 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( blade ),                        & ! 4
                                      trim ( jobid ), time_start ( 1 : 6 ), time_stop ( 1 : 6 ), date, trim ( descriptor_test ), & ! 5
                                      iterations, measures, nu_ave, nu_var, nu_max, nu_min, alpha                                  ! 7
        close ( io_out_results )

        ! write sequence of times
        file_results = trim ( path_results ) // '/' // trim ( machine_name ) // '_' // trim ( descriptor_test ) // '.log'
        io_out_sequence = safeopen_writeappend ( file_results )
        write ( io_out_sequence, 110 ) trim ( machine_name ), trim ( rack ), trim ( icu ), trim ( blade ), trim ( jobid ), &
                                       time_start ( 1 : 6 ), time_stop ( 1 : 6 ), date, trim ( descriptor_test ), frequency_sequence
        close ( io_out_sequence )

    100 format ( 16( g0, ', '), g0 )
    110 format ( 200( g0, ' ' ) )

    stop 'successful completion for flops ...'

end program flops

! rditldmt@ITL-DTOPA-MP:charlie $ pwd
! /Users/rditldmt/hpc/fortran/tools/test/stress/flopper/charlie
! rditldmt@ITL-DTOPA-MP:charlie $ date
! Mon Jun  6 15:30:41 CDT 2016
! rditldmt@ITL-DTOPA-MP:charlie $ make
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_harvest_blade.o mod_harvest_blade.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_CPU.o mod_timer_CPU.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_clock.o mod_timer_clock.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_test_suite.o mod_test_suite.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o flops.o flops.f08
! gfortran -g -o flops flops.o mod_file_handling.o mod_harvest_blade.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o
! rditldmt@ITL-DTOPA-MP:charlie $ ./flops
! file name = /Users/rditldmt/results/ITL-DTOPA-MP_test_001.csv.
! k_measure = 1 of 10: time 153107.
! k_measure = 2 of 10: time 153226.
! k_measure = 3 of 10: time 153345.
! k_measure = 4 of 10: time 153504.
! k_measure = 5 of 10: time 153623.
! k_measure = 6 of 10: time 153742.
! k_measure = 7 of 10: time 153901.
! k_measure = 8 of 10: time 154020.
! k_measure = 9 of 10: time 154139.
! k_measure = 10 of 10: time 154257.
! STOP successful completion for flops ...
!
! csv file
! ITL-DTOPA-MP, 1, 2, 3, 5708, 153107, 154416, 20160606, test_001, 1000000000, 10, 12805305.511444326, 15136.361225745441, 12826323.473926604, 12775590.894713722, 0.99300461438044207,
!
! log entry
! ITL-DTOPA-MP 1 2 3 5708 153107 154416 20160606 test_001 12809173.909859564 12803544.676708004 12793182.482351514 12775590.894713722 12790943.929835632 12803190.268223373 12806153.674433181 12822118.873094745 12822832.931296926 12826323.473926604
