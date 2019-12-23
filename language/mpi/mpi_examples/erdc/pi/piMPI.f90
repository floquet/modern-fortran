! Jerry Morris, loosely based on any of several similar web examples.
! This code approximates the value of pi by approximating
! the area under the curve x^2 + y^2 = r^2 between 0 and 1.
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
program piMPI

    implicit none

    include 'mpif.h'

    real(kind=8),parameter :: M_PI = acos ( -1.0 )
    integer(kind=4)        :: rank    ! MPI rank (PE number [0,n-1])
    integer(kind=4)        :: ierr    ! Fortran MPI call error var.
    integer(kind=4)        :: n       ! number of PE's
    integer(kind=4)        :: m       ! number of intervals
    real(kind=8)           :: dpi     ! my delta pi
    real(kind=8)           :: pi      ! sum of all dpi's
    character(len=10)      :: argv1   ! command line version of m
    real(kind=8)           :: x       ! current x value
    real(kind=8)           :: dx      ! delta x
    real(kind=8)           :: x0, xm  ! my starting & ending x value
    real(kind=8)           :: t0, t1  ! start and finish time
    call MPI_INIT(ierr)               ! initialize mpi runtime
    ! default communicator is MPI_COMM_WORLD (all PE's)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr) ! PE's grab rank
    call MPI_COMM_SIZE(MPI_COMM_WORLD, n, ierr)    ! number of PE's

    if (rank == 0) then ! rank 0 grabs start time & # of intervals
        t0 = mpi_wtime ( )              ! start time
        call getarg(1,argv1)              ! number of intervals
        read(argv1,'(i10)') m             ! convert to integer
        write(*,"(3a)") 'using ', trim(argv1), " intervals (dx's)."
    end if
    ! maybe all ranks can see argv, but want to illustrate
    ! broadcast: sender sends m value, all other ranks receive m
    call MPI_BCAST(m, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
    !              ^  ^     ^     ^        ^           ^
    !              |  |     |     |        |           +--- error var.
    !              |  |     |     |        +--- communicator
    !              |  |     |     +--- rank of sender
    !              |  |     +--- datatype of buffer
    !              |  +--- count of items in buffer
    !              +--- buffer being broadcast
    dpi = 0.0                ! my piece of the pi
    dx = 1.0/m               ! width of each interval
    x0 = rank/n+dx           ! start x value for this PE
    xm = (rank+1)/n          ! end x value for this PE

    x = x0                   ! start here for this PE
    do while(x <= xm)        ! sum all slices for this PE
        dpi = dpi + dx * sqrt(1.0-x*x)
        x = x + dx           ! next x
    end do

    ! use a summing reduction to add up all the pieces
    ! rank 0 will have the final result, other ranks simply send
    call MPI_REDUCE(dpi,pi,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)
    !                ^  ^  ^    ^          ^    ^        ^
    !                |  |  |    |          |    |        `--- comm
    !                |  |  |    |          |    +--- destination rank
    !                |  |  |    |          +--- summing reduction
    !                |  |  |    +--- datatype of send buffer
    !                |  |  +--- count of items in send buffer
    !                |  +--- receive buffer (destination rank)
    !                +--- send buffer (all ranks)

    if (rank == 0) then ! rank 0 finalizes and reports
        pi = 4.0 * pi   ! need 4x the area since only quadrant i used
        t1 = MPI_WTIME ( )              ! finish time
        write(*,"(a,f10.8,a)") 'runtime: ', t1-t0, ' s'
        write(*,"(a,f17.14)") 'pi (to 14 decimal places): ', M_PI
        write(*,"(a,f17.14)") 'pi was approximated as:    ', pi
        write(*,"(a,f17.14)") 'error:                     ', pi-M_PI
    end if

    call MPI_FINALIZE(ierr)  ! clean up and go home
    return

end program piMPI

! dan-topas-pro-2:pi rditldmt$ date
! Mon Mar 21 13:44:00 CDT 2016
! dan-topas-pro-2:pi rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/mpi/mpi examples/erdc/pi
! dan-topas-pro-2:pi rditldmt$ echo $flags
! -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5
! dan-topas-pro-2:pi rditldmt$ mpifort $flags piMPI.f90
! mpif.h:16:18: Warning: Obsolescent feature: Old-style character length at (1)
! mpif.h:17:18: Warning: Obsolescent feature: Old-style character length at (1)
! mpif.h:528:16: Warning: GNU Extension: Nonstandard type declaration INTEGER*8 at (1)
! mpif.h:546:13: Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
! mpif.h:547:13: Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
! piMPI.f90:93:10:
!
!      return
!           1
! Warning: GNU Extension: RETURN statement in main program at (1)
! mpif.h:296:36: Warning: Unused parameter 'mpi_2double_precision' declared at (1) [-Wunused-parameter]
! mpif.h:380:23: Warning: Unused parameter 'mpi_2int' declared at (1) [-Wunused-parameter]
! mpif.h:294:27: Warning: Unused parameter 'mpi_2integer' declared at (1) [-Wunused-parameter]
! mpif.h:298:24: Warning: Unused parameter 'mpi_2real' declared at (1) [-Wunused-parameter]
! mpif.h:332:31: Warning: Unused parameter 'mpi_address_kind' declared at (1) [-Wunused-parameter]
! mpif.h:410:23: Warning: Unused parameter 'mpi_aint' declared at (1) [-Wunused-parameter]
! mpif.h:262:29: Warning: Unused parameter 'mpi_any_source' declared at (1) [-Wunused-parameter]
! mpif.h:264:26: Warning: Unused parameter 'mpi_any_tag' declared at (1) [-Wunused-parameter]
! mpif.h:226:25: Warning: Unused parameter 'mpi_appnum' declared at (1) [-Wunused-parameter]
! mpif.h:532:45: Warning: Unused parameter 'mpi_async_protects_nonblocking' declared at (1) [-Wunused-parameter]
! mpif.h:170:23: Warning: Unused parameter 'mpi_band' declared at (1) [-Wunused-parameter]
! mpif.h:174:22: Warning: Unused parameter 'mpi_bor' declared at (1) [-Wunused-parameter]
! mpif.h:258:33: Warning: Unused parameter 'mpi_bsend_overhead' declared at (1) [-Wunused-parameter]
! mpif.h:178:23: Warning: Unused parameter 'mpi_bxor' declared at (1) [-Wunused-parameter]
! mpif.h:302:23: Warning: Unused parameter 'mpi_byte' declared at (1) [-Wunused-parameter]
! mpif.h:400:25: Warning: Unused parameter 'mpi_c_bool' declared at (1) [-Wunused-parameter]
! mpif.h:404:28: Warning: Unused parameter 'mpi_c_complex' declared at (1) [-Wunused-parameter]
! mpif.h:406:35: Warning: Unused parameter 'mpi_c_double_complex' declared at (1) [-Wunused-parameter]
! mpif.h:402:34: Warning: Unused parameter 'mpi_c_float_complex' declared at (1) [-Wunused-parameter]
! mpif.h:408:40: Warning: Unused parameter 'mpi_c_long_double_complex' declared at (1) [-Wunused-parameter]
! mpif.h:270:23: Warning: Unused parameter 'mpi_cart' declared at (1) [-Wunused-parameter]
! mpif.h:340:23: Warning: Unused parameter 'mpi_char' declared at (1) [-Wunused-parameter]
! mpif.h:300:28: Warning: Unused parameter 'mpi_character' declared at (1) [-Wunused-parameter]
! mpif.h:428:38: Warning: Unused parameter 'mpi_combiner_contiguous' declared at (1) [-Wunused-parameter]
! mpif.h:450:34: Warning: Unused parameter 'mpi_combiner_darray' declared at (1) [-Wunused-parameter]
! mpif.h:426:31: Warning: Unused parameter 'mpi_combiner_dup' declared at (1) [-Wunused-parameter]
! mpif.h:454:39: Warning: Unused parameter 'mpi_combiner_f90_complex' declared at (1) [-Wunused-parameter]
! mpif.h:456:39: Warning: Unused parameter 'mpi_combiner_f90_integer' declared at (1) [-Wunused-parameter]
! mpif.h:452:36: Warning: Unused parameter 'mpi_combiner_f90_real' declared at (1) [-Wunused-parameter]
! mpif.h:440:36: Warning: Unused parameter 'mpi_combiner_hindexed' declared at (1) [-Wunused-parameter]
! mpif.h:460:42: Warning: Unused parameter 'mpi_combiner_hindexed_block' declared at (1) [-Wunused-parameter]
! mpif.h:438:44: Warning: Unused parameter 'mpi_combiner_hindexed_integer' declared at (1) [-Wunused-parameter]
! mpif.h:434:35: Warning: Unused parameter 'mpi_combiner_hvector' declared at (1) [-Wunused-parameter]
! mpif.h:432:43: Warning: Unused parameter 'mpi_combiner_hvector_integer' declared at (1) [-Wunused-parameter]
! mpif.h:436:35: Warning: Unused parameter 'mpi_combiner_indexed' declared at (1) [-Wunused-parameter]
! mpif.h:442:41: Warning: Unused parameter 'mpi_combiner_indexed_block' declared at (1) [-Wunused-parameter]
! mpif.h:424:33: Warning: Unused parameter 'mpi_combiner_named' declared at (1) [-Wunused-parameter]
! mpif.h:458:35: Warning: Unused parameter 'mpi_combiner_resized' declared at (1) [-Wunused-parameter]
! mpif.h:446:34: Warning: Unused parameter 'mpi_combiner_struct' declared at (1) [-Wunused-parameter]
! mpif.h:444:42: Warning: Unused parameter 'mpi_combiner_struct_integer' declared at (1) [-Wunused-parameter]
! mpif.h:448:36: Warning: Unused parameter 'mpi_combiner_subarray' declared at (1) [-Wunused-parameter]
! mpif.h:430:34: Warning: Unused parameter 'mpi_combiner_vector' declared at (1) [-Wunused-parameter]
! mpif.h:194:28: Warning: Unused parameter 'mpi_comm_null' declared at (1) [-Wunused-parameter]
! mpif.h:190:28: Warning: Unused parameter 'mpi_comm_self' declared at (1) [-Wunused-parameter]
! mpif.h:478:35: Warning: Unused parameter 'mpi_comm_type_shared' declared at (1) [-Wunused-parameter]
! mpif.h:282:26: Warning: Unused parameter 'mpi_complex' declared at (1) [-Wunused-parameter]
! mpif.h:328:28: Warning: Unused parameter 'mpi_complex16' declared at (1) [-Wunused-parameter]
! mpif.h:330:28: Warning: Unused parameter 'mpi_complex32' declared at (1) [-Wunused-parameter]
! mpif.h:326:27: Warning: Unused parameter 'mpi_complex8' declared at (1) [-Wunused-parameter]
! mpif.h:142:28: Warning: Unused parameter 'mpi_congruent' declared at (1) [-Wunused-parameter]
! mpif.h:414:24: Warning: Unused parameter 'mpi_count' declared at (1) [-Wunused-parameter]
! mpif.h:336:29: Warning: Unused parameter 'mpi_count_kind' declared at (1) [-Wunused-parameter]
! mpif.h:416:27: Warning: Unused parameter 'mpi_cxx_bool' declared at (1) [-Wunused-parameter]
! mpif.h:420:37: Warning: Unused parameter 'mpi_cxx_double_complex' declared at (1) [-Wunused-parameter]
! mpif.h:418:36: Warning: Unused parameter 'mpi_cxx_float_complex' declared at (1) [-Wunused-parameter]
! mpif.h:422:42: Warning: Unused parameter 'mpi_cxx_long_double_complex' declared at (1) [-Wunused-parameter]
! mpif.h:528:41: Warning: Unused parameter 'mpi_displacement_current' declared at (1) [-Wunused-parameter]
! mpif.h:272:29: Warning: Unused parameter 'mpi_dist_graph' declared at (1) [-Wunused-parameter]
! mpif.h:520:35: Warning: Unused parameter 'mpi_distribute_block' declared at (1) [-Wunused-parameter]
! mpif.h:522:36: Warning: Unused parameter 'mpi_distribute_cyclic' declared at (1) [-Wunused-parameter]
! mpif.h:526:39: Warning: Unused parameter 'mpi_distribute_dflt_darg' declared at (1) [-Wunused-parameter]
! mpif.h:524:34: Warning: Unused parameter 'mpi_distribute_none' declared at (1) [-Wunused-parameter]
! mpif.h:284:33: Warning: Unused parameter 'mpi_double_complex' declared at (1) [-Wunused-parameter]
! mpif.h:374:29: Warning: Unused parameter 'mpi_double_int' declared at (1) [-Wunused-parameter]
! mpif.h:290:35: Warning: Unused parameter 'mpi_double_precision' declared at (1) [-Wunused-parameter]
! mpif.h:56:29: Warning: Unused parameter 'mpi_err_access' declared at (1) [-Wunused-parameter]
! mpif.h:26:28: Warning: Unused parameter 'mpi_err_amode' declared at (1) [-Wunused-parameter]
! mpif.h:110:26: Warning: Unused parameter 'mpi_err_arg' declared at (1) [-Wunused-parameter]
! mpif.h:34:29: Warning: Unused parameter 'mpi_err_assert' declared at (1) [-Wunused-parameter]
! mpif.h:86:31: Warning: Unused parameter 'mpi_err_bad_file' declared at (1) [-Wunused-parameter]
! mpif.h:30:27: Warning: Unused parameter 'mpi_err_base' declared at (1) [-Wunused-parameter]
! mpif.h:84:29: Warning: Unused parameter 'mpi_err_buffer' declared at (1) [-Wunused-parameter]
! mpif.h:134:27: Warning: Unused parameter 'mpi_err_comm' declared at (1) [-Wunused-parameter]
! mpif.h:46:33: Warning: Unused parameter 'mpi_err_conversion' declared at (1) [-Wunused-parameter]
! mpif.h:42:28: Warning: Unused parameter 'mpi_err_count' declared at (1) [-Wunused-parameter]
! mpif.h:58:27: Warning: Unused parameter 'mpi_err_dims' declared at (1) [-Wunused-parameter]
! mpif.h:126:27: Warning: Unused parameter 'mpi_err_disp' declared at (1) [-Wunused-parameter]
! mpif.h:80:34: Warning: Unused parameter 'mpi_err_dup_datarep' declared at (1) [-Wunused-parameter]
! mpif.h:24:27: Warning: Unused parameter 'mpi_err_file' declared at (1) [-Wunused-parameter]
! mpif.h:130:34: Warning: Unused parameter 'mpi_err_file_exists' declared at (1) [-Wunused-parameter]
! mpif.h:60:34: Warning: Unused parameter 'mpi_err_file_in_use' declared at (1) [-Wunused-parameter]
! mpif.h:48:28: Warning: Unused parameter 'mpi_err_group' declared at (1) [-Wunused-parameter]
! mpif.h:100:32: Warning: Unused parameter 'mpi_err_in_status' declared at (1) [-Wunused-parameter]
! mpif.h:40:27: Warning: Unused parameter 'mpi_err_info' declared at (1) [-Wunused-parameter]
! mpif.h:22:31: Warning: Unused parameter 'mpi_err_info_key' declared at (1) [-Wunused-parameter]
! mpif.h:96:33: Warning: Unused parameter 'mpi_err_info_nokey' declared at (1) [-Wunused-parameter]
! mpif.h:116:33: Warning: Unused parameter 'mpi_err_info_value' declared at (1) [-Wunused-parameter]
! mpif.h:120:29: Warning: Unused parameter 'mpi_err_intern' declared at (1) [-Wunused-parameter]
! mpif.h:104:25: Warning: Unused parameter 'mpi_err_io' declared at (1) [-Wunused-parameter]
! mpif.h:54:29: Warning: Unused parameter 'mpi_err_keyval' declared at (1) [-Wunused-parameter]
! mpif.h:106:31: Warning: Unused parameter 'mpi_err_lastcode' declared at (1) [-Wunused-parameter]
! mpif.h:74:31: Warning: Unused parameter 'mpi_err_locktype' declared at (1) [-Wunused-parameter]
! mpif.h:36:27: Warning: Unused parameter 'mpi_err_name' declared at (1) [-Wunused-parameter]
! mpif.h:38:29: Warning: Unused parameter 'mpi_err_no_mem' declared at (1) [-Wunused-parameter]
! mpif.h:68:31: Warning: Unused parameter 'mpi_err_no_space' declared at (1) [-Wunused-parameter]
! mpif.h:132:35: Warning: Unused parameter 'mpi_err_no_such_file' declared at (1) [-Wunused-parameter]
! mpif.h:52:31: Warning: Unused parameter 'mpi_err_not_same' declared at (1) [-Wunused-parameter]
! mpif.h:32:25: Warning: Unused parameter 'mpi_err_op' declared at (1) [-Wunused-parameter]
! mpif.h:76:28: Warning: Unused parameter 'mpi_err_other' declared at (1) [-Wunused-parameter]
! mpif.h:66:30: Warning: Unused parameter 'mpi_err_pending' declared at (1) [-Wunused-parameter]
! mpif.h:128:27: Warning: Unused parameter 'mpi_err_port' declared at (1) [-Wunused-parameter]
! mpif.h:70:28: Warning: Unused parameter 'mpi_err_quota' declared at (1) [-Wunused-parameter]
! mpif.h:64:27: Warning: Unused parameter 'mpi_err_rank' declared at (1) [-Wunused-parameter]
! mpif.h:62:32: Warning: Unused parameter 'mpi_err_read_only' declared at (1) [-Wunused-parameter]
! mpif.h:124:30: Warning: Unused parameter 'mpi_err_request' declared at (1) [-Wunused-parameter]
! mpif.h:88:33: Warning: Unused parameter 'mpi_err_rma_attach' declared at (1) [-Wunused-parameter]
! mpif.h:114:35: Warning: Unused parameter 'mpi_err_rma_conflict' declared at (1) [-Wunused-parameter]
! mpif.h:98:33: Warning: Unused parameter 'mpi_err_rma_flavor' declared at (1) [-Wunused-parameter]
! mpif.h:122:32: Warning: Unused parameter 'mpi_err_rma_range' declared at (1) [-Wunused-parameter]
! mpif.h:94:33: Warning: Unused parameter 'mpi_err_rma_shared' declared at (1) [-Wunused-parameter]
! mpif.h:50:31: Warning: Unused parameter 'mpi_err_rma_sync' declared at (1) [-Wunused-parameter]
! mpif.h:92:27: Warning: Unused parameter 'mpi_err_root' declared at (1) [-Wunused-parameter]
! mpif.h:78:30: Warning: Unused parameter 'mpi_err_service' declared at (1) [-Wunused-parameter]
! mpif.h:20:27: Warning: Unused parameter 'mpi_err_size' declared at (1) [-Wunused-parameter]
! mpif.h:44:28: Warning: Unused parameter 'mpi_err_spawn' declared at (1) [-Wunused-parameter]
! mpif.h:72:26: Warning: Unused parameter 'mpi_err_tag' declared at (1) [-Wunused-parameter]
! mpif.h:108:31: Warning: Unused parameter 'mpi_err_topology' declared at (1) [-Wunused-parameter]
! mpif.h:28:31: Warning: Unused parameter 'mpi_err_truncate' declared at (1) [-Wunused-parameter]
! mpif.h:102:27: Warning: Unused parameter 'mpi_err_type' declared at (1) [-Wunused-parameter]
! mpif.h:118:30: Warning: Unused parameter 'mpi_err_unknown' declared at (1) [-Wunused-parameter]
! mpif.h:82:42: Warning: Unused parameter 'mpi_err_unsupported_datarep' declared at (1) [-Wunused-parameter]
! mpif.h:90:44: Warning: Unused parameter 'mpi_err_unsupported_operation' declared at (1) [-Wunused-parameter]
! mpif.h:112:26: Warning: Unused parameter 'mpi_err_win' declared at (1) [-Wunused-parameter]
! mpif.h:208:34: Warning: Unused parameter 'mpi_errhandler_null' declared at (1) [-Wunused-parameter]
! mpif.h:9:45: Warning: Unused parameter 'mpi_error' declared at (1) [-Wunused-parameter]
! mpif.h:136:35: Warning: Unused parameter 'mpi_errors_are_fatal' declared at (1) [-Wunused-parameter]
! mpif.h:138:32: Warning: Unused parameter 'mpi_errors_return' declared at (1) [-Wunused-parameter]
! mpif.h:198:28: Warning: Unused parameter 'mpi_file_null' declared at (1) [-Wunused-parameter]
! mpif.h:360:24: Warning: Unused parameter 'mpi_float' declared at (1) [-Wunused-parameter]
! mpif.h:372:28: Warning: Unused parameter 'mpi_float_int' declared at (1) [-Wunused-parameter]
! mpif.h:268:24: Warning: Unused parameter 'mpi_graph' declared at (1) [-Wunused-parameter]
! mpif.h:192:30: Warning: Unused parameter 'mpi_group_empty' declared at (1) [-Wunused-parameter]
! mpif.h:200:29: Warning: Unused parameter 'mpi_group_null' declared at (1) [-Wunused-parameter]
! mpif.h:216:23: Warning: Unused parameter 'mpi_host' declared at (1) [-Wunused-parameter]
! mpif.h:140:24: Warning: Unused parameter 'mpi_ident' declared at (1) [-Wunused-parameter]
! mpif.h:212:27: Warning: Unused parameter 'mpi_info_env' declared at (1) [-Wunused-parameter]
! mpif.h:210:28: Warning: Unused parameter 'mpi_info_null' declared at (1) [-Wunused-parameter]
! mpif.h:386:26: Warning: Unused parameter 'mpi_int16_t' declared at (1) [-Wunused-parameter]
! mpif.h:388:26: Warning: Unused parameter 'mpi_int32_t' declared at (1) [-Wunused-parameter]
! mpif.h:390:26: Warning: Unused parameter 'mpi_int64_t' declared at (1) [-Wunused-parameter]
! mpif.h:384:25: Warning: Unused parameter 'mpi_int8_t' declared at (1) [-Wunused-parameter]
! mpif.h:292:26: Warning: Unused parameter 'mpi_integer' declared at (1) [-Wunused-parameter]
! mpif.h:310:27: Warning: Unused parameter 'mpi_integer1' declared at (1) [-Wunused-parameter]
! mpif.h:318:28: Warning: Unused parameter 'mpi_integer16' declared at (1) [-Wunused-parameter]
! mpif.h:312:27: Warning: Unused parameter 'mpi_integer2' declared at (1) [-Wunused-parameter]
! mpif.h:314:27: Warning: Unused parameter 'mpi_integer4' declared at (1) [-Wunused-parameter]
! mpif.h:316:27: Warning: Unused parameter 'mpi_integer8' declared at (1) [-Wunused-parameter]
! mpif.h:338:31: Warning: Unused parameter 'mpi_integer_kind' declared at (1) [-Wunused-parameter]
! mpif.h:218:21: Warning: Unused parameter 'mpi_io' declared at (1) [-Wunused-parameter]
! mpif.h:256:33: Warning: Unused parameter 'mpi_keyval_invalid' declared at (1) [-Wunused-parameter]
! mpif.h:168:23: Warning: Unused parameter 'mpi_land' declared at (1) [-Wunused-parameter]
! mpif.h:224:31: Warning: Unused parameter 'mpi_lastusedcode' declared at (1) [-Wunused-parameter]
! mpif.h:306:21: Warning: Unused parameter 'mpi_lb' declared at (1) [-Wunused-parameter]
! mpif.h:278:33: Warning: Unused parameter 'mpi_lock_exclusive' declared at (1) [-Wunused-parameter]
! mpif.h:280:30: Warning: Unused parameter 'mpi_lock_shared' declared at (1) [-Wunused-parameter]
! mpif.h:286:26: Warning: Unused parameter 'mpi_logical' declared at (1) [-Wunused-parameter]
! mpif.h:356:23: Warning: Unused parameter 'mpi_long' declared at (1) [-Wunused-parameter]
! mpif.h:364:30: Warning: Unused parameter 'mpi_long_double' declared at (1) [-Wunused-parameter]
! mpif.h:382:34: Warning: Unused parameter 'mpi_long_double_int' declared at (1) [-Wunused-parameter]
! mpif.h:376:27: Warning: Unused parameter 'mpi_long_int' declared at (1) [-Wunused-parameter]
! mpif.h:370:28: Warning: Unused parameter 'mpi_long_long' declared at (1) [-Wunused-parameter]
! mpif.h:366:32: Warning: Unused parameter 'mpi_long_long_int' declared at (1) [-Wunused-parameter]
! mpif.h:172:22: Warning: Unused parameter 'mpi_lor' declared at (1) [-Wunused-parameter]
! mpif.h:176:23: Warning: Unused parameter 'mpi_lxor' declared at (1) [-Wunused-parameter]
! mpif.h:160:22: Warning: Unused parameter 'mpi_max' declared at (1) [-Wunused-parameter]
! mpif.h:250:37: Warning: Unused parameter 'mpi_max_datarep_string' declared at (1) [-Wunused-parameter]
! mpif.h:238:35: Warning: Unused parameter 'mpi_max_error_string' declared at (1) [-Wunused-parameter]
! mpif.h:244:31: Warning: Unused parameter 'mpi_max_info_key' declared at (1) [-Wunused-parameter]
! mpif.h:246:31: Warning: Unused parameter 'mpi_max_info_val' declared at (1) [-Wunused-parameter]
! mpif.h:252:45: Warning: Unused parameter 'mpi_max_library_version_string' declared at (1) [-Wunused-parameter]
! mpif.h:242:34: Warning: Unused parameter 'mpi_max_object_name' declared at (1) [-Wunused-parameter]
! mpif.h:240:32: Warning: Unused parameter 'mpi_max_port_name' declared at (1) [-Wunused-parameter]
! mpif.h:248:37: Warning: Unused parameter 'mpi_max_processor_name' declared at (1) [-Wunused-parameter]
! mpif.h:182:25: Warning: Unused parameter 'mpi_maxloc' declared at (1) [-Wunused-parameter]
! mpif.h:482:34: Warning: Unused parameter 'mpi_message_no_proc' declared at (1) [-Wunused-parameter]
! mpif.h:480:31: Warning: Unused parameter 'mpi_message_null' declared at (1) [-Wunused-parameter]
! mpif.h:162:22: Warning: Unused parameter 'mpi_min' declared at (1) [-Wunused-parameter]
! mpif.h:180:25: Warning: Unused parameter 'mpi_minloc' declared at (1) [-Wunused-parameter]
! mpif.h:506:30: Warning: Unused parameter 'mpi_mode_append' declared at (1) [-Wunused-parameter]
! mpif.h:502:30: Warning: Unused parameter 'mpi_mode_create' declared at (1) [-Wunused-parameter]
! mpif.h:498:39: Warning: Unused parameter 'mpi_mode_delete_on_close' declared at (1) [-Wunused-parameter]
! mpif.h:504:28: Warning: Unused parameter 'mpi_mode_excl' declared at (1) [-Wunused-parameter]
! mpif.h:468:31: Warning: Unused parameter 'mpi_mode_nocheck' declared at (1) [-Wunused-parameter]
! mpif.h:474:33: Warning: Unused parameter 'mpi_mode_noprecede' declared at (1) [-Wunused-parameter]
! mpif.h:472:29: Warning: Unused parameter 'mpi_mode_noput' declared at (1) [-Wunused-parameter]
! mpif.h:470:31: Warning: Unused parameter 'mpi_mode_nostore' declared at (1) [-Wunused-parameter]
! mpif.h:476:33: Warning: Unused parameter 'mpi_mode_nosucceed' declared at (1) [-Wunused-parameter]
! mpif.h:492:30: Warning: Unused parameter 'mpi_mode_rdonly' declared at (1) [-Wunused-parameter]
! mpif.h:494:28: Warning: Unused parameter 'mpi_mode_rdwr' declared at (1) [-Wunused-parameter]
! mpif.h:508:34: Warning: Unused parameter 'mpi_mode_sequential' declared at (1) [-Wunused-parameter]
! mpif.h:500:35: Warning: Unused parameter 'mpi_mode_unique_open' declared at (1) [-Wunused-parameter]
! mpif.h:496:30: Warning: Unused parameter 'mpi_mode_wronly' declared at (1) [-Wunused-parameter]
! mpif.h:186:24: Warning: Unused parameter 'mpi_no_op' declared at (1) [-Wunused-parameter]
! mpif.h:412:25: Warning: Unused parameter 'mpi_offset' declared at (1) [-Wunused-parameter]
! mpif.h:334:30: Warning: Unused parameter 'mpi_offset_kind' declared at (1) [-Wunused-parameter]
! mpif.h:202:26: Warning: Unused parameter 'mpi_op_null' declared at (1) [-Wunused-parameter]
! mpif.h:516:26: Warning: Unused parameter 'mpi_order_c' declared at (1) [-Wunused-parameter]
! mpif.h:518:32: Warning: Unused parameter 'mpi_order_fortran' declared at (1) [-Wunused-parameter]
! mpif.h:308:25: Warning: Unused parameter 'mpi_packed' declared at (1) [-Wunused-parameter]
! mpif.h:260:28: Warning: Unused parameter 'mpi_proc_null' declared at (1) [-Wunused-parameter]
! mpif.h:166:23: Warning: Unused parameter 'mpi_prod' declared at (1) [-Wunused-parameter]
! mpif.h:288:23: Warning: Unused parameter 'mpi_real' declared at (1) [-Wunused-parameter]
! mpif.h:324:25: Warning: Unused parameter 'mpi_real16' declared at (1) [-Wunused-parameter]
! mpif.h:320:24: Warning: Unused parameter 'mpi_real4' declared at (1) [-Wunused-parameter]
! mpif.h:322:24: Warning: Unused parameter 'mpi_real8' declared at (1) [-Wunused-parameter]
! mpif.h:184:26: Warning: Unused parameter 'mpi_replace' declared at (1) [-Wunused-parameter]
! mpif.h:206:31: Warning: Unused parameter 'mpi_request_null' declared at (1) [-Wunused-parameter]
! mpif.h:266:23: Warning: Unused parameter 'mpi_root' declared at (1) [-Wunused-parameter]
! mpif.h:512:27: Warning: Unused parameter 'mpi_seek_cur' declared at (1) [-Wunused-parameter]
! mpif.h:514:27: Warning: Unused parameter 'mpi_seek_end' declared at (1) [-Wunused-parameter]
! mpif.h:510:27: Warning: Unused parameter 'mpi_seek_set' declared at (1) [-Wunused-parameter]
! mpif.h:348:24: Warning: Unused parameter 'mpi_short' declared at (1) [-Wunused-parameter]
! mpif.h:378:28: Warning: Unused parameter 'mpi_short_int' declared at (1) [-Wunused-parameter]
! mpif.h:342:30: Warning: Unused parameter 'mpi_signed_char' declared at (1) [-Wunused-parameter]
! mpif.h:144:26: Warning: Unused parameter 'mpi_similar' declared at (1) [-Wunused-parameter]
! mpif.h:9:25: Warning: Unused parameter 'mpi_source' declared at (1) [-Wunused-parameter]
! mpif.h:530:38: Warning: Unused parameter 'mpi_subarrays_supported' declared at (1) [-Wunused-parameter]
! mpif.h:276:29: Warning: Unused parameter 'mpi_subversion' declared at (1) [-Wunused-parameter]
! mpif.h:18:26: Warning: Unused parameter 'mpi_success' declared at (1) [-Wunused-parameter]
! mpif.h:9:34: Warning: Unused parameter 'mpi_tag' declared at (1) [-Wunused-parameter]
! mpif.h:214:25: Warning: Unused parameter 'mpi_tag_ub' declared at (1) [-Wunused-parameter]
! mpif.h:486:34: Warning: Unused parameter 'mpi_thread_funneled' declared at (1) [-Wunused-parameter]
! mpif.h:490:34: Warning: Unused parameter 'mpi_thread_multiple' declared at (1) [-Wunused-parameter]
! mpif.h:488:36: Warning: Unused parameter 'mpi_thread_serialized' declared at (1) [-Wunused-parameter]
! mpif.h:484:32: Warning: Unused parameter 'mpi_thread_single' declared at (1) [-Wunused-parameter]
! mpif.h:466:36: Warning: Unused parameter 'mpi_typeclass_complex' declared at (1) [-Wunused-parameter]
! mpif.h:464:36: Warning: Unused parameter 'mpi_typeclass_integer' declared at (1) [-Wunused-parameter]
! mpif.h:462:33: Warning: Unused parameter 'mpi_typeclass_real' declared at (1) [-Wunused-parameter]
! mpif.h:304:21: Warning: Unused parameter 'mpi_ub' declared at (1) [-Wunused-parameter]
! mpif.h:394:27: Warning: Unused parameter 'mpi_uint16_t' declared at (1) [-Wunused-parameter]
! mpif.h:396:27: Warning: Unused parameter 'mpi_uint32_t' declared at (1) [-Wunused-parameter]
! mpif.h:398:27: Warning: Unused parameter 'mpi_uint64_t' declared at (1) [-Wunused-parameter]
! mpif.h:392:26: Warning: Unused parameter 'mpi_uint8_t' declared at (1) [-Wunused-parameter]
! mpif.h:254:28: Warning: Unused parameter 'mpi_undefined' declared at (1) [-Wunused-parameter]
! mpif.h:146:26: Warning: Unused parameter 'mpi_unequal' declared at (1) [-Wunused-parameter]
! mpif.h:222:32: Warning: Unused parameter 'mpi_universe_size' declared at (1) [-Wunused-parameter]
! mpif.h:354:27: Warning: Unused parameter 'mpi_unsigned' declared at (1) [-Wunused-parameter]
! mpif.h:344:32: Warning: Unused parameter 'mpi_unsigned_char' declared at (1) [-Wunused-parameter]
! mpif.h:358:32: Warning: Unused parameter 'mpi_unsigned_long' declared at (1) [-Wunused-parameter]
! mpif.h:368:37: Warning: Unused parameter 'mpi_unsigned_long_long' declared at (1) [-Wunused-parameter]
! mpif.h:350:33: Warning: Unused parameter 'mpi_unsigned_short' declared at (1) [-Wunused-parameter]
! mpif.h:274:26: Warning: Unused parameter 'mpi_version' declared at (1) [-Wunused-parameter]
! mpif.h:346:24: Warning: Unused parameter 'mpi_wchar' declared at (1) [-Wunused-parameter]
! mpif.h:228:27: Warning: Unused parameter 'mpi_win_base' declared at (1) [-Wunused-parameter]
! mpif.h:234:36: Warning: Unused parameter 'mpi_win_create_flavor' declared at (1) [-Wunused-parameter]
! mpif.h:232:32: Warning: Unused parameter 'mpi_win_disp_unit' declared at (1) [-Wunused-parameter]
! mpif.h:150:38: Warning: Unused parameter 'mpi_win_flavor_allocate' declared at (1) [-Wunused-parameter]
! mpif.h:148:36: Warning: Unused parameter 'mpi_win_flavor_create' declared at (1) [-Wunused-parameter]
! mpif.h:152:37: Warning: Unused parameter 'mpi_win_flavor_dynamic' declared at (1) [-Wunused-parameter]
! mpif.h:154:36: Warning: Unused parameter 'mpi_win_flavor_shared' declared at (1) [-Wunused-parameter]
! mpif.h:236:28: Warning: Unused parameter 'mpi_win_model' declared at (1) [-Wunused-parameter]
! mpif.h:196:27: Warning: Unused parameter 'mpi_win_null' declared at (1) [-Wunused-parameter]
! mpif.h:156:31: Warning: Unused parameter 'mpi_win_separate' declared at (1) [-Wunused-parameter]
! mpif.h:230:27: Warning: Unused parameter 'mpi_win_size' declared at (1) [-Wunused-parameter]
! mpif.h:158:30: Warning: Unused parameter 'mpi_win_unified' declared at (1) [-Wunused-parameter]
! mpif.h:220:34: Warning: Unused parameter 'mpi_wtime_is_global' declared at (1) [-Wunused-parameter]
! piMPI.f90:86:0:
!
!          write(*,"(a,f10.8,a)") 'runtime: ', t1-t0, ' s'
!  ^
! Warning: 't0' may be used uninitialized in this function [-Wmaybe-uninitialized]
! dan-topas-pro-2:pi rditldmt$ mpirun -np 6 ./a.out 20000
! using 20000 intervals (dx's).
! runtime: 0.00279107 s
! pi (to 14 decimal places):  3.14159274101257
! pi was approximated as:     3.14149228203771
! error:                     -0.00010045897486
