program characterize

    !use iso_fortran_env,        only : getarg
    use mPrecisionDefinitions,  only : zint !ip!, ascii
    !use mParameters
    use mTimerCPU
    use mTimerClock
    use mShared,                only : timestamp, open_file_output, io_unit, io_status, io_msg
    use mBlocks!,       only : print_banner
    use mAllocations
    use mISOVariables,          only : iso_variables
    use mQueries,               only : queries
    use mTimes,                 only : times
    use mVariableTypes,         only : variable_types

    !   DECLARATIONS
    implicit none

    integer ( zint )                    :: MBytes

    type ( timer_cpu )                  :: cpu_global
    type ( timer_clock )                :: clock_global

    character ( len = 255 )             :: sMBytes
    character ( len = * ), parameter    :: myProgram = 'program characterize'       ! self-identification
    character ( len = * ), parameter    :: file_name = 'characterization.txt'       ! results from all blocks

        !   start global timers
        call cpu_global   % timer_start_cpu   ( )
        call clock_global % timer_start_clock ( )

        !   open target file
        call open_file_output ( file_name, io_unit, 'Fatal error' )
        call alert_io ( task = 'OPEN', file_name = file_name, myIO = stdout, fatal = .true. )
        write ( * , '( "Results posted to ", g0, "." )' ) file_name

        call print_banner ( io_unit )

        !   Header for output file
        nBlock = 0                                                                      !  count test blocks

        !   read allocation size in megabytes from command line
        call getarg ( 1, sMBytes )

        !   Convert strings to integers
        read ( sMBytes, * ) MBytes

        ! DIAGNOSTIC SUITE
        call queries                ( io_unit, 'results system queries' )                    !  INTERROGATE SYSTEM
        call times                  ( cpu_global, clock_global, io_unit, 'results times' )   !  TIMERS
        call variable_types         ( io_unit, 'results variable_types' )                    !  CHARACTERIZE DATA TYPES
        call allocator              ( io_unit, MBytes, 'results allocation' )                !  ALLOCATION, POPULATION, DEALLOCATION
        call iso_variables          ( io_unit, 'results iso variables.txt' )                 !  ISO FORTRAN ENVIRONMENT VARIABLES
!       call constants         ( io_unit )                             !  CONSTANTS
!       call epsilon_test      ( io_unit )                             !  EPSILON
!       call accumulators      ( io_unit )                             !  ACCUMULATORS
!       call precision_request ( io_unit )                             !  PRECISION REQUEST RESULTS
!       call character_sets    ( io_unit )                             !  CHARACTER ENCODINGS

!       close target file
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
        call alert_io ( task = 'CLOSE', file_name = file_name, myIO = stdout )

        write ( * , '( "CPU time   = ", E9.3, " s: global" )' ) cpu_global   % time_elapsed_cpu ( )
        write ( * , '( "clock time = ", E9.3, " s: global" )' ) clock_global % time_elapsed_clock ( )
        write ( * , * ) timestamp ( )

    stop "successful completion for " // myProgram // "."  ! string must reduce to constant expression

end program characterize

! F O R T R A N   2 0 0 8   C O M P I L E R   T E S T   S U I T E
!
!                           Daniel Topa
!                   daniel.topa@engilitycorp.com
!
!                       HPCMPO PETTT program
!                           ACE on-site
!                            2016 02 04
!
!
! Block I: System Queries.
! home directory    = /Users/rditldmt.
! working directory = /Users/rditldmt/Box Sync/fortran/projects/characterization/stack.
! date and time     = 2016-02-08  15:12:02  UCT-0600.
!
! numerical user ID of the current process            = 595980521
! numerical group ID of the current process           = 2011572450
! numerical process identifier of the current process = 70990
!
! compiler version   = GCC version 5.1.0.
! compiler options   = -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.9.4 -mtune=core2 -auxbase-strip mod_queries.o -g -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5.
! host system        = dan-topas-pro-2.erdc.dren.mil.
! invocation command = ./characterize 10.
! terminal device 6  =  (output file)
! terminal device 5  = /dev/ttys001.
! terminal device 6  = /dev/ttys001.
!
! universally unique identifier (UUID) = BE932200-A12C-462C-8597-D0B275D6B662.
!
!
! Block II: Check CPU and system clocks.
! system clock (implementation not standardized - compiler dependent)
! count rate = 1000: system dependent and can vary depending on the int_kind of the arguments
! count max  = 2147483647: typically [ HUGE ( count max ) = 2147483647 ]
!
! total cpu time   = .10697999999999999E-001 s
! total clock time = .47162999999999997E-001 s
!
! Block III: Characterization of data types.
! REAL variables characterization
! REAL                         default         single                   double                quadruple
! KIND value:                        4              4                        8                       10
! bytes:                             4              4                        8                        8
! digits:                           24             24                       53                       53
! precision:                         6              6                       15                       15
! range:                            37             37                      307                      307
! radix:                             2              2                        2                        2
! maximum exponent:                128            128                     1024                     1024
! minimum exponent:               -125           -125                    -1021                    -1021
! epsilon:               .119209290E-06  .119209290E-06  .22204460492503131E-015  .22204460492503131E-015
! tiny:                  .117549435E-37  .117549435E-37  .22250738585072014E-307  .22250738585072014E-307
! huge:                  .340282347E+39  .340282347E+39  .17976931348623157E+309  .17976931348623157E+309
!
! INTEGER variables characterization
! INTEGER                         INT8          INT16                     INT32                    INT64
! KIND value:                        1              2                        4                        8
! bytes:                             4              4                        4                        4
! digits:                           31             31                       31                       31
! range:                             9              9                        9                        9
! radix:                             2              2                        2                        2
!
!
! Block IV: Memory allocations.
! size = MBytes * mega : 10485760 = 10 * 1048576
!
! + + + Requesting allocation of 10 megabytes
!
! attempting to allocate rank one default integer array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 10485760
! actual total of real elements       = 10485760,
! cpu time                            = 0.560E-01 s
! clock time                          = 0.561E-01 s
!
! attempting to allocate rank one INT8 array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 0
! actual total of real elements       = 0,
! cpu time                            = 0.422E-01 s
! clock time                          = 0.422E-01 s
!
! attempting to allocate rank one INT16 array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 0
! actual total of real elements       = 0,
! cpu time                            = 0.398E-01 s
! clock time                          = 0.398E-01 s
!
! attempting to allocate rank one INT32 array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 0
! actual total of real elements       = 0,
! cpu time                            = 0.443E-01 s
! clock time                          = 0.443E-01 s
!
! attempting to allocate rank one INT64 array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 10485760
! actual total of real elements       = 10485760,
! cpu time                            = 0.547E-01 s
! clock time                          = 0.549E-01 s
!
! attempting to allocate rank one default real array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 10485760.0
! actual total of real elements       = 10485760.0,
! cpu time                            = 0.544E-01 s
! clock time                          = 0.545E-01 s
!
! attempting to allocate rank one REAL32 array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 10485760.0
! actual total of real elements       = 10485760.0,
! cpu time                            = 0.519E-01 s
! clock time                          = 0.520E-01 s
!
! attempting to allocate rank one REAL64 array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 10485760.000000000
! actual total of real elements       = 10485760.000000000,
! cpu time                            = 0.667E-01 s
! clock time                          = 0.668E-01 s
!
! attempting to allocate rank one REAL128 array of precision
! expected total of all unit elements = 10485760
! actual total of all unit elements   = 10485760.000000000
! actual total of real elements       = 10485760.000000000,
! cpu time                            = 0.649E-01 s
! clock time                          = 0.650E-01 s
!
! attempting to allocate rank one default complex array of precision
! expected total of all unit elements = 10485760
! dot product total                   = 10485760.0
! dot product total                   = 10485760.0
! dot product total                   = 20971520.0, .00000000
! cpu time                            = 0.102E+00 s
! clock time                          = 0.102E+00 s
!
! attempting to allocate rank one REAL32 array of precision
! expected total of all unit elements = 10485760
! dot product total                   = 10485760.0
! dot product total                   = 10485760.0
! dot product total                   = 20971520.0, .00000000
! cpu time                            = 0.103E+00 s
! clock time                          = 0.104E+00 s
!
! attempting to allocate rank one REAL64 array of precision
! expected total of all unit elements = 10485760
! dot product total                   = 10485760.000000000
! dot product total                   = 10485760.000000000
! dot product total                   = 20971520.000000000, .0000000000000000
! cpu time                            = 0.213E+00 s
! clock time                          = 0.215E+00 s
!
! attempting to allocate rank one REAL128 array of precision
! expected total of all unit elements = 10485760
! dot product total                   = 10485760.000000000
! dot product total                   = 10485760.000000000
! dot product total                   = 20971520.000000000, .0000000000000000
! cpu time                            = 0.206E+00 s
! clock time                          = 0.207E+00 s
!
! total cpu   time for all allocations = 1.1000019999999999 s
! total clock time for all allocations = 1.1043870000000000 s
!
!
! Block V: ISO Fortran environment variables.
! use ISO_FORTRAN_ENV
!
! IO units
! INPUT_UNIT  = 5: preconnected standard input  unit
! OUTPUT_UNIT = 6: preconnected standard output unit
! ERROR_UNIT  = 0: preconnected output unit for error reporting
! IOSTAT return values
! IOSTAT_END  = -1: end-of-file signal
! IOSTAT_EOR  = -2: end-of-record signal
! storage sizes
! CHARACTER_STORAGE_SIZE =  8: Size in bits of the character storage unit
! NUMERIC_STORAGE_SIZE   = 32: Size in bits of the numeric storage unit
! FILE_STORAGE_SIZE      =  8: Size in bits of the file storage unit
! 
! real kind types REAL(bits) = kind type value
! REAL32 = 4, REAL64 = 8, REAL128 = 10
!
! integer kind types: INT(bits) = kind type value
! INT8 = 1, INT16 = 2, INT32 = 4, INT64 = 8
