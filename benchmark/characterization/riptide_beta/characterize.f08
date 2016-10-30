program characterize

    use mPrecisionDefinitions,  only : zint
    use mParameters,            only : stdout
    use mTimerClock,            only : timer_clock
    use mTimerCPU,              only : timer_cpu
    use mShared,                only : timestamp, alert_io, open_file_output, io_unit, io_status, io_msg
    use mBlocks,                only : nBlock, print_banner
    use mAllocations,           only : allocator
    use mISOVariables,          only : iso_variables
    use mQueries,               only : queries
    use mTimes,                 only : times
    use mVariableTypes,         only : variable_types
    use mConstants,             only : constants
    use mEpsilonTests,          only : epsilon_test
    use mRequestPrecision,      only : request_precision
    use mAccumulator,           only : accumulator
    use mCharacterEncodings,    only : character_sets

    implicit none

    integer ( zint )                    :: MBytes

    type ( timer_cpu )                  :: cpu_global
    type ( timer_clock )                :: clock_global

    character ( len = 255 )             :: sMBytes
    character ( len = * ), parameter    :: myProgram = 'program characterize' ! self-identification
    character ( len = * ), parameter    :: file_name = 'characterization.txt' ! results from all blocks

        !   start global timers
        call cpu_global   % timer_start_cpu   ( )
        call clock_global % timer_start_clock ( )

        !   open target file
        call open_file_output ( file_name, io_unit, 'Fatal error' )
        call alert_io ( task = 'OPEN', file_name = file_name, myIO = stdout, fatal = .true. )
        write ( * , '( "Results posted to ", g0, "." )' ) file_name

        call print_banner ( io_unit )

        nBlock = 0 !  count test blocks

        !   read allocation size in megabytes from command line
        if ( COMMAND_ARGUMENT_COUNT( ) > 0 ) then
            call getarg ( 1, sMBytes )
            !   Convert strings to integers
            read ( sMBytes, * ) MBytes
        else
            MBytes = 10 ! no command arguments; use default value
        end if

        ! DIAGNOSTIC SUITE
        call queries            ( io_unit, 'results system queries.txt' )                   !  INTERROGATE SYSTEM
        call times              ( cpu_global, clock_global, io_unit, 'results times.txt' )  !  TIMERS
        call variable_types     ( io_unit, 'results variable_types.txt' )                   !  CHARACTERIZE DATA TYPES
        call iso_variables      ( io_unit, 'results iso variables.txt' )                    !  ISO FORTRAN ENVIRONMENT VARIABLES
        call constants          ( io_unit, 'results constants.txt' )                        !  CONSTANTS
        call epsilon_test       ( io_unit, 'results epsilon.txt' )                          !  EPSILON
        call request_precision  ( io_unit, 'results precision request.txt' )                !  PRECISION REQUEST RESULTS
        call character_sets     ( io_unit, 'results character sets.txt' )                   !  CHARACTER ENCODINGS
        call accumulator        ( io_unit, 'results accumulators.txt' )                     !  ACCUMULATORS
        call allocator          ( io_unit, MBytes, 'results allocation.txt' )               !  ALLOCATION, POPULATION, DEALLOCATION

        !  close target file
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
        call alert_io ( task = 'CLOSE', file_name = file_name, myIO = stdout )

        write ( * , '( "CPU time   = ", E9.3, " s: global" )' ) cpu_global   % time_elapsed_cpu ( )
        write ( * , '( "clock time = ", E9.3, " s: global" )' ) clock_global % time_elapsed_clock ( )
        write ( * , * ) timestamp ( )

    stop "successful completion for " // myProgram // "."  ! string must reduce to constant expression

end program characterize

<<<<<<< HEAD
! [dantopa@riptide04 riptide_alpha]$ date
! Tue Apr 19 05:46:18 HST 2016
! [dantopa@riptide04 riptide_alpha]$ pwd
! /gpfs/home/dantopa/github/fortran/benchmark/characterization/riptide_alpha
! [dantopa@riptide04 riptide_alpha]$ make
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_shared.o mod_shared.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_blocks.o mod_blocks.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_timer_clock.o mod_timer_clock.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_timer_cpu.o mod_timer_cpu.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_unit_values.o mod_unit_values.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_accumulator.o mod_accumulator.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_allocations.o mod_allocations.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_character_encodings.o mod_character_encodings.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_constants.o mod_constants.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_declaration_precision.o mod_declaration_precision.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_epsilon.o mod_epsilon.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_iso_variables.o mod_iso_variables.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_precision_request.o mod_precision_request.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_queries.o mod_queries.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_times.o mod_times.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o mod_variable_types.o mod_variable_types.f08
! gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds
! -fmax-errors=5 -o characterize.o characterize.f08
! gfortran -g -o characterize characterize.o mod_accumulator.o mod_allocations.o
! mod_blocks.o mod_character_encodings.o mod_constants.o
! mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o
! mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o
! mod_timer_clock.o mod_timer_cpu.o mod_times.o mod_unit_values.o
! mod_variable_types.o
! [dantopa@riptide04 riptide_alpha]$ ./characterize 10
! Results posted to characterization.txt.
! Block I: System Queries.
! cp: cannot create regular file `/gpfs/home/dantopa/cpuinfo.txt': Permission
! denied
! cp: cannot create regular file `/gpfs/home/dantopa/meminfo.txt': Permission
! denied
=======
! dan-topas-pro-2:golf rditldmt$ date
! Thu Feb 11 16:59:45 CST 2016
! dan-topas-pro-2:golf rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/benchmarks/characterization/golf
! dan-topas-pro-2:golf rditldmt$ cn
! dan-topas-pro-2:golf rditldmt$
! dan-topas-pro-2:golf rditldmt$
! dan-topas-pro-2:golf rditldmt$ date
! Thu Feb 11 16:59:52 CST 2016
! dan-topas-pro-2:golf rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/benchmarks/characterization/golf
! dan-topas-pro-2:golf rditldmt$ make
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_shared.o mod_shared.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_blocks.o mod_blocks.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_clock.o mod_timer_clock.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_CPU.o mod_timer_CPU.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_unit_values.o mod_unit_values.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_accumulator.o mod_accumulator.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_allocations.o mod_allocations.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_character_encodings.o mod_character_encodings.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_declaration_precision.o mod_declaration_precision.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_epsilon.o mod_epsilon.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_iso_variables.o mod_iso_variables.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_request.o mod_precision_request.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_queries.o mod_queries.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_times.o mod_times.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_variable_types.o mod_variable_types.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o characterize.o characterize.f08
! gfortran -g -o characterize characterize.o mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o mod_times.o mod_unit_values.o mod_variable_types.o
! dan-topas-pro-2:golf rditldmt$ make debug
! SRCS = characterize.f08 mod_accumulator.f08 mod_allocations.f08 mod_blocks.f08 mod_character_encodings.f08 mod_constants.f08 mod_declaration_precision.f08 mod_epsilon.f08 mod_iso_variables.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_precision_request.f08 mod_queries.f08 mod_shared.f08 mod_timer_CPU.f08 mod_timer_clock.f08 mod_times.f08 mod_unit_values.f08 mod_variable_types.f08
! OBJS = characterize.o mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o mod_times.o mod_unit_values.o mod_variable_types.o
! MODS = mod_accumulator.f08 mod_allocations.f08 mod_blocks.f08 mod_character_encodings.f08 mod_constants.f08 mod_declaration_precision.f08 mod_epsilon.f08 mod_iso_variables.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_precision_request.f08 mod_queries.f08 mod_shared.f08 mod_timer_CPU.f08 mod_timer_clock.f08 mod_times.f08 mod_unit_values.f08 mod_variable_types.f08
! MOD_OBJS = mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o mod_times.o mod_unit_values.o mod_variable_types.o
! PROGRAM = characterize
! PRG_OBJ = characterize.o
! dan-topas-pro-2:golf rditldmt$ cn
! dan-topas-pro-2:golf rditldmt$ ./characterize 100
! Results posted to characterization.txt.
! Block I: System Queries.
! cp: /proc/cpuinfo: No such file or directory
! cp: /proc/meminfo: No such file or directory
>>>>>>> d8754854d237c2124240287e5483133b2c6316e1
! Block II: Check CPU and system clocks.
! Block III: Characterization of data types.
! Block IV: ISO Fortran environment variables.
! Block V: Basic constants.
! Block VI: Machine epsilon.
! Block VII: Requests for precision.
! Block VIII: Character set kind requests.
! Block IX: Accumulation tests.
! Block X: Memory allocations.
<<<<<<< HEAD
! 
! + + + Requesting allocation of 10 megabytes
! 
! attempting to allocate rank one integer array of precision default integer.
!  ... allocation completed
! 
! attempting to allocate rank one integer array of precision INT8.
!  ... allocation completed
! 
! attempting to allocate rank one integer array of precision INT16.
!  ... allocation completed
! 
! attempting to allocate rank one integer array of precision INT32.
!  ... allocation completed
! 
! attempting to allocate rank one integer array of precision INT64.
!  ... allocation completed
! 
! attempting to allocate rank one real array of precision default real.
!  ... allocation completed
! 
! attempting to allocate rank one real array of precision REAL32.
!  ... allocation completed
! 
! attempting to allocate rank one real array of precision REAL64.
!  ... allocation completed
! 
! attempting to allocate rank one real array of precision REAL128.
!  ... allocation completed
! 
! attempting to allocate rank one complex array of precision default complex.
!  ... allocation completed
! 
! attempting to allocate rank one complex array of precision REAL32.
!  ... allocation completed
! 
! attempting to allocate rank one complex array of precision REAL64.
!  ... allocation completed
! 
! attempting to allocate rank one complex array of precision REAL128.
!  ... allocation completed
! CPU time   = 0.732E+01 s: global
! clock time = 0.733E+01 s: global
!  2016-04-19  05:46:46  UCT-1000
=======
!
! + + + Requesting allocation of 100 megabytes
!
! attempting to allocate rank one integer array of precision default integer.
!  ... allocation completed
!
! attempting to allocate rank one integer array of precision INT8.
!  ... allocation completed
!
! attempting to allocate rank one integer array of precision INT16.
!  ... allocation completed
!
! attempting to allocate rank one integer array of precision INT32.
!  ... allocation completed
!
! attempting to allocate rank one integer array of precision INT64.
!  ... allocation completed
!
! attempting to allocate rank one real array of precision default real.
!  ... allocation completed
!
! attempting to allocate rank one real array of precision REAL32.
!  ... allocation completed
!
! attempting to allocate rank one real array of precision REAL64.
!  ... allocation completed
!
! attempting to allocate rank one real array of precision REAL128.
!  ... allocation completed
!
! attempting to allocate rank one complex array of precision default complex.
!  ... allocation completed
!
! attempting to allocate rank one complex array of precision REAL32.
!  ... allocation completed
!
! attempting to allocate rank one complex array of precision REAL64.
!  ... allocation completed
!
! attempting to allocate rank one complex array of precision REAL128.
!  ... allocation completed
! CPU time   = 0.204E+02 s: global
! clock time = 0.205E+02 s: global
!  2016-02-11  17:00:49  UCT-0600
>>>>>>> d8754854d237c2124240287e5483133b2c6316e1
! STOP successful completion for program characterize.