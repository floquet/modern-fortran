! A simple example that gets lots of Flops (Floating Point Operations) on 
! Intel(r) Xeon Phi(tm) co-processors using openmp to scale

! Fortran version by James Reinders, September 27, 2012

! Main program - pedal to the metal...calculate using tons o'flops!
 
program helloflops
  use ISO_FORTRAN_ENV
  use omp_lib
  implicit none
  integer, parameter :: sp = REAL32
  integer, parameter :: dp = REAL64
  integer, parameter :: FLOPS_ARRAY_SIZE = 1024*512
  integer, parameter :: MAXFLOPS_ITERS = 100000000
  integer, parameter :: LOOP_COUNT = 128
  integer, parameter :: FLOPSPERCALC = 2

  ! ... define arrays that are 64 byte aligned for best cache access 
  real (sp), allocatable :: fa(:)
  real (sp), allocatable :: fb(:)

  integer   :: i,j,k
  integer   :: numthreads, offset
  real (dp) :: tstart, tstop, ttime
  real (dp) :: gflops = 0.0_dp
  real (sp) :: a = 1.1_sp

  allocate( fa(FLOPS_ARRAY_SIZE) )
  allocate( fb(FLOPS_ARRAY_SIZE) )

  ! initialize the compute arrays 

  !$OMP PARALLEL
  !$OMP MASTER
  numthreads = omp_get_num_threads()
  !$OMP end MASTER
  !$OMP end PARALLEL

  write(*,*) "Initializing"

  !$OMP PARALLEL DO
  do i = 1, FLOPS_ARRAY_SIZE
     fa(i) = i + 0.1_sp
     fb(i) = i + 0.2_sp
  end do

  write(*,*) 'Starting Compute on ', numthreads, ' threads'
  tstart = mytime()

  ! scale the calculation across threads requested 
  ! need to set environment variables OMP_NUM_THREADS and KMP_AFFINITY
  !$OMP PARALLEL do PRIVATE(i,j,k,offset)
   do i=1, numthreads
      ! each thread will work it's own array section
      ! calc offset into the right section
      offset = i*LOOP_COUNT

      ! loop many times to get lots of calculations
      do j=1, MAXFLOPS_ITERS
          ! scale 1st array and add in the 2nd array 
          do k=1, LOOP_COUNT
              fa(k+offset) = a * fa(k+offset) + fb(k+offset)
          end do
      end do
  end do

  tstop = mytime()

  ! # of gigaflops we just calculated  
  gflops = 1.0e-9 * numthreads * LOOP_COUNT * MAXFLOPS_ITERS * FLOPSPERCALC

  !elasped time
  ttime = tstop - tstart

  ! Print the results

  if (ttime > 0.0) THEN
      write (*,'(A,F10.3)') 'GFlops per second = ',gflops/ttime
  end if

contains
  ! --------------------------------------------------
  ! mytime: returns the current wall clock time
  ! --------------------------------------------------

  function mytime()  result (tseconds)
    real (dp)       :: tseconds
    integer (INT64) ::  count, count_rate, count_max
    real (dp)       :: tsec, rate

    CALL SYSTEM_CLOCK(count, count_rate, count_max)

    tsec = count
    rate = count_rate
    tseconds = tsec / rate
  end function mytime 

end program helloflops
