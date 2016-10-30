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
    character ( len = * ), parameter    :: file_name = 'summary.txt'          ! results from all blocks

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

        write ( * , '( /, "CPU time   = ", E9.3, " s: global" )' )    cpu_global   % time_elapsed_cpu ( )
        write ( * , '(    "clock time = ", E9.3, " s: global", / )' ) clock_global % time_elapsed_clock ( )
        write ( * , * ) timestamp ( )

    stop "successful completion for " // myProgram // "."  ! string must reduce to constant expression

end program characterize

! dantopa@topaz03:~/github/fortran/benchmark/characterization/topaz> module switch compiler/intel compiler/gcc
! dantopa@topaz03:~/github/fortran/benchmark/characterization/topaz> make
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_shared.o mod_shared.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_blocks.o mod_blocks.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_clock.o mod_timer_clock.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_CPU.o mod_timer_CPU.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_unit_values.o mod_unit_values.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_accumulator.o mod_accumulator.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_allocations.o mod_allocations.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_character_encodings.o mod_character_encodings.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_declaration_precision.o mod_declaration_precision.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_epsilon.o mod_epsilon.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_iso_variables.o mod_iso_variables.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_request.o mod_precision_request.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_queries.o mod_queries.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_times.o mod_times.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_variable_types.o mod_variable_types.f08
! gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o characterize.o characterize.f08
! gfortran -g -o characterize characterize.o mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_clock.o mod_timer_CPU.o mod_times.o mod_unit_values.o mod_variable_types.o
! dantopa@topaz03:~/github/fortran/benchmark/characterization/topaz> ./characterize 10
! Results posted to summary.txt.
! Block I: System Queries.
! Block I: System Queries.
! cp: cannot create regular file `cpuinfo.txt': Permission denied
! cp: cannot create regular file `meminfo.txt': Permission denied
! Block II: Check CPU and system clocks.
! Block II: Check CPU and system clocks.
! Block III: Characterization of data types.
! Block III: Characterization of data types.
! Block IV: ISO Fortran environment variables.
! Block IV: ISO Fortran environment variables.
! Block V: Basic constants.
! Block V: Basic constants.
! Block VI: Machine epsilon.
! Block VI: Machine epsilon.
! Block VII: Requests for precision.
! Block VII: Requests for precision.
! Block VIII: Character set kind requests.
! Block VIII: Character set kind requests.
! Block IX: Accumulation tests.
! Block X: Memory allocations.
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
! Block X: Memory allocations.
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
!
! CPU time   = 0.162E+02 s: global
! clock time = 0.164E+02 s: global
!
!  2016-04-08  19:09:02  UCT-0500
! STOP successful completion for program characterize.s
! dantopa@topaz03:~/github/fortran/benchmark/characterization/topaz> gfortran -v
! Using built-in specs.
! COLLECT_GCC=gfortran
! COLLECT_LTO_WRAPPER=/p/home/apps/gnu_compiler/5.3.0/libexec/gcc/x86_64-unknown-linux-gnu/5.3.0/lto-wrapper
! Target: x86_64-unknown-linux-gnu
! Configured with: /p/home/u4immtww/objdir/../gcc-5.3.0/configure --prefix=/p/home/apps/gnu_compiler/5.3.0 --enable-languages=c,c++,fortran,go
! Thread model: posix
! gcc version 5.3.0 (GCC)
