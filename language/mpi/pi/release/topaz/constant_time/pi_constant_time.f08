! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! Jerry Morris, loosely based on any of several similar web examples.
! This code approximates the value of pi by approximating
! the area under the curve x^2 + y^2 = r^2 between 0 and 1.
! Assume r = 1 -> y = sqrt(1 - x^2).  Sum all the little
! rectangles (slices) of area y*dx to approximate area under curve.
! Since area = pi * r^2 -> (with r = 1) pi = area.
!                               y
!                               ^
!                               |   top half of a unit circle
!  _    y = sqrt(1 - x^2)     . -  .  use quadrant I & multiply by 4
!  |                \     .     |      .
!  |                 \.         |     |   .
!  dx*sqrt(1-x^2)  .            |     |      .
!  |             .              |     |     |  .
!  |           .                |     |     |    .
!  -     -----.-----------------+-----|-----|-----.------> x
!            -1              x  0  dx    dx    dx 1
! Each PE computes multiple dx sections, ergo we have parallelism.

!!!!!!!!!!!!!
! Linux/OS X (assumes gcc with Fortran and OpenMPI)
! change to source directory, e.g., cd ~/mpiprim/pi
! compile: mpif90 piMPI -O3 -lm -o piMPIf
! run: mpirun -np <n> ./piMPIf <m> # m=# of slices, e.g., 50000000

!!!!!!!!!!!!!
! Garnet (interactive)
! a)  qsub -q standard -l select=1:ncpus=32:mpiprocs=32 \
!       -l walltime=1:00:00 -A ERDCS97290STA  -l ccm=1 -X -I
! b) change to source directory, e.g., cd ~/mpiprim/pi
! c) compile:  ftn piMPI -O3 -lm -o piMPIf
! d) run: aprun -n <n> ./piMPIf <m>  # m=# of slices, e.g.,50000000

!!!!!!!!!!!!!
! Garnet (batch)
! qsub -v CC=ftn,SFX=f,EXT=f90,M=50000000,N=8 piMPI.pbs # any m/n value
! rank    ! MPI rank (PE number [0,n-1])
! ierr    ! Fortran MPI call error var.
! n       ! number of PE's
! m       ! number of intervals
! dpi     ! my delta pi
! pi      ! sum of all dpi's
! argv1   ! command line version of m
! x       ! current x value
! dx      ! delta x
! x0, xm  ! my starting & ending x value
! t0, t1  ! start and finish time

!  adding MPI processes increases precision, but not the run time
!  each MPI process does the same number of integrations
!  more processors = more integrations = more precision
program pi_constant_time

    use, intrinsic :: iso_fortran_env,  only : REAL64, INT64
    use mpt,                            only : MPI_COMM_WORLD, MPI_INT, MPI_DOUBLE, MPI_SUM, MPI_WTIME
    use mFileHandling,                  only : safeopen_writeappend

    implicit none

    integer,     parameter :: ip = INT64, rp = REAL64 ! control precision in one place
    real ( rp ), parameter :: M_PI = acos ( -1.0_rp ), zero = 0.0_rp, one = 1.0_rp

    integer        :: io_out = 0
    integer ( ip ) :: rank = 0_ip, ierr = 0_ip
    integer ( ip ) :: j = 0_ip, k = 0_ip
    integer ( ip ) :: numIntDomain = 0_ip, numPE = 0_ip

    real ( rp ) :: dpi = zero, pi = zero
    real ( rp ) :: x0 = zero, x = zero, dx = zero
    real ( rp ) :: t0 = zero, t1 = zero                     ! start and finish time

    character ( len = 128 ) :: host_name = 'nemo'
    character ( len = 256 ) :: results_file = ''

    external :: MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_BCAST, MPI_REDUCE, MPI_FINALIZE

        call MPI_INIT ( ierr )                              ! initialize mpi runtime
        ! default communicator is MPI_COMM_WORLD (all PE's)
        call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr )  ! PE's grab rank
        call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr )  ! number of PE's

        !   read machine name
        if ( COMMAND_ARGUMENT_COUNT( ) > 0 ) call getarg ( 1, host_name )
        results_file = 'results/' // trim( host_name ) // '_constant_time_gnu_i8.csv'
        !   create a log file to hold results of runs
        if ( rank == 0 ) then
            io_out = safeopen_writeappend ( trim ( results_file ) )
                write ( unit = io_out, fmt = 200 ) 'integration intervals', 'MPI procs', 'cpu time', 'precision'
            close ( io_out )
        end if

        ! refine the mesh by a factor of 10 to see how the time increases and the precision remains static
        refine_mesh: do j = 0, 4                            ! larger j = finer mesh
            if ( rank == 0 ) then                           ! rank 0 grabs start time & # of intervals
                t0 = MPI_WTIME ( )                          ! start time
                numIntDomain = 10 ** j * 1441440            ! scale up: 720720 = LCM( Range[16] )
                if ( numIntDomain .lt. 0 ) exit refine_mesh
            end if
            ! maybe all ranks can see argv, but want to illustrate
            ! broadcast: sender sends m value, all other ranks receive m
            call MPI_BCAST ( numIntDomain, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )
            !                ^             ^     ^     ^        ^           ^
            !                |             |     |     |        |           +--- error var.
            !                |             |     |     |        +--- communicator
            !                |             |     |     +--- rank of sender
            !                |             |     +--- datatype of buffer
            !                |             +--- count of items in buffer
            !                +--- buffer being broadcast

            dpi = zero                                      ! my piece of the pi
            dx = one / real ( numIntDomain, rp ) / real ( numPE, rp ) ! integration measure
            x0 = real ( rank, rp ) / real ( numPE, rp )     ! starting position for this partition (for this PE)

            do k = 0, numIntDomain - 1                      ! sum all slices for this PE
                x = x0 + dx * real ( k, rp )
                dpi = dpi + dx * sqrt ( one - x * x )
            end do

            ! use a summing reduction to add up all the pieces
            ! rank 0 will have the final result, other ranks simply send
            call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )

            if ( rank == 0 ) then               ! rank 0 finalizes and reports
                pi = 4 * pi                     ! need 4x the area since only quadrant i used
                t1 = MPI_WTIME ( )              ! finish time
                io_out = safeopen_writeappend ( trim ( results_file ) )
                    write ( unit = io_out, fmt = 200 ) numIntDomain, numPE, t1 - t0, pi - M_PI
                close ( io_out )
            end if

        end do refine_mesh

        call MPI_FINALIZE ( ierr )  ! clean up and go home

    200 format ( g0, 3( ',  ', g0 ) )

end program pi_constant_time

! dantopa@topaz03.erdc.hpc.mil:constant_time $ date
! Mon Sep 12 17:34:08 CDT 2016
! dantopa@topaz03.erdc.hpc.mil:constant_time $ pwd
! /p/home/dantopa/hpc/fortran/language/mpi/pi/release/topaz/constant_time
! dantopa@topaz03.erdc.hpc.mil:constant_time $ module list
! Currently Loaded Modulefiles:
!   1) java/1.8                2) compiler/intel/15.0.3   3) mpi/sgimpt/2.12-11218   4) pbs/13.1.0.160576
! dantopa@topaz03.erdc.hpc.mil:constant_time $ module switch compiler/intel/15.0.3 compiler/gcc/6.1.0
! dantopa@topaz03.erdc.hpc.mil:constant_time $ make
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_mpt.o mod_mpt.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o pi_constant_time.o pi_constant_time.f08
! mpif90 -g -o pi_constant_time mod_file_handling.o mod_mpt.o pi_constant_time.o
! dantopa@topaz03.erdc.hpc.mil:constant_time $ qsub -l select=1:ncpus=36:mpiprocs=36 -A ERDCS97270PET -q standard -l walltime=000:10:00 -I
! qsub: waiting for job 544737.topaz10 to start
! qsub: job 544737.topaz10 ready
!
! Start Prologue v2.5.3 Mon Sep 12 17:35:48 CDT 2016
! r15i5n10: MemFree:        123907260 kB
! End Prologue v2.5.3 Mon Sep 12 17:35:49 CDT 2016
! Welcome dantopa to ~/.bashrc on topaz
! Welcome dantopa to ~/.bash_profile on topaz
! dantopa@r15i5n10:~ $ cd ~/hpc/fortran/language/mpi/pi/release/topaz/constant_time
! dantopa@r15i5n10:constant_time $ mpiexec_mpt -np 4 ./pi_constant_time $BC_HOST
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
! MPT: MPI_MEMMAP_OFF (default: disabled) : disabled
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
! Warning: results/topaz_constant_time_gnu_i8.csv doesn't exist; new empty file will be created.
! dantopa@r15i5n10:constant_time $ mpiexec_mpt -np 32 ./pi_constant_time $BC_HOST
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
! MPT: MPI_MEMMAP_OFF (default: disabled) : disabled
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
! dantopa@r15i5n10:constant_time $ cat results/topaz_constant_time_gnu_i8.csv
! integration intervals,  MPI procs,  cpu time,  precision
! 1441440,  4,  .66500911489129066E-002,  .34679043636742790E-006
! 14414400,  4,  .63638462219387293E-001,  .34684989991262682E-007
! 144144000,  4,  .62078049592673779,  .34687146488465714E-008
! 1441440000,  4,  6.2101000919938087,  .34677150040351989E-009
! 14414400000,  4,  57.363976321183145,  .32076297173944113E-009
! integration intervals,  MPI procs,  cpu time,  precision
! 1441440,  32,  .78214453533291817E-002,  .43355670165112770E-007
! 14414400,  32,  .72603954002261162E-001,  .43358787671365917E-008
! 144144000,  32,  .72495865682139993,  .43359360546446624E-009
! 1441440000,  32,  7.2467443337664008,  .43566039664710843E-010
! 14414400000,  32,  58.879744758829474,  .65428551465629425E-010
! dantopa@r15i5n10:constant_time $ exit
! logout
!
! qsub: job 544737.topaz10 completed
! dantopa@topaz03.erdc.hpc.mil:constant_time $
