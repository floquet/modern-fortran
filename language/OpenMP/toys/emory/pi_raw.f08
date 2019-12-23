! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/91-pthreads/Progs/F90/openMP_compute_pi2.f90

  FUNCTION f(a)
    IMPLICIT NONE

    double precision a
    double precision f

    f = 2.d0 / SQRT(1.d0 - a*a)
  END

! ===================================================



  PROGRAM Compute_PI
   IMPLICIT NONE

   interface
     FUNCTION f(a)
     double precision a
     double precision f
     END FUNCTION
   end interface

   INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS

   INTEGER           N, i
   INTEGER           id, num_threads
   DOUBLE PRECISION  w, x, sum
   DOUBLE PRECISION  pi, mypi


   N = 50000000		!! Number of intervals
   w = 1.0d0/N  	!! width of each interval

   sum = 0.0d0

!$OMP    PARALLEL PRIVATE(i, id, num_threads, x, mypi)

   num_threads = omp_get_num_threads()
   id = omp_get_thread_num()

   mypi = 0.0d0;

   DO i = id, N-1, num_threads
     x = w * (i + 0.5d0)
     mypi = mypi + w*f(x)
   END DO

!$OMP CRITICAL
   pi = pi + mypi
!$OMP END CRITICAL

!$OMP    END PARALLEL

   PRINT *, "Pi = ", pi

   END PROGRAM

! rditldmt@ITL-DTOPA-MP:emory $ date
! Thu May 26 17:30:31 CDT 2016
! rditldmt@ITL-DTOPA-MP:emory $ pwd
! /Users/rditldmt/hpc/fortran/language/OpenMP/toys/emory
! rditldmt@ITL-DTOPA-MP:emory $ gfortran -fopenmp $gflags pi_raw.f08
! pi_raw.f08:34:16:
!
!     N = 50000000  !! Number of intervals
!                 1
! Warning: Nonconforming tab character at (1) [-Wtabs]
! pi_raw.f08:35:17:
!
!     w = 1.0d0/N   !! width of each interval
!                  1
! Warning: Nonconforming tab character at (1) [-Wtabs]
! pi_raw.f08:35:14:
!
!     w = 1.0d0/N   !! width of each interval
!               1
! Warning: Conversion from INTEGER(4) to REAL(8) at (1) [-Wconversion-extra]
! pi_raw.f08:47:14:
!
!       x = w * (i + 0.5d0)
!               1
! Warning: Conversion from INTEGER(4) to REAL(8) at (1) [-Wconversion-extra]
! rditldmt@ITL-DTOPA-MP:emory $ aa
!  Pi =    3.1414716738623545
! rditldmt@ITL-DTOPA-MP:emory $
