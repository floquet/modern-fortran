! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/91-pthreads/openMP-F90.html
PROGRAM  omp_hello

   !$OMP PARALLEL

   print *, "Hello World !"

   !$OMP END PARALLEL

   END PROGRAM  omp_hello

! rditldmt@ITL-DTOPA-MP:emory $ date
! Thu May 26 09:05:28 CDT 2016
! rditldmt@ITL-DTOPA-MP:emory $ pwd
! /Users/rditldmt/hpc/fortran/language/OpenMP/toys/emory
! rditldmt@ITL-DTOPA-MP:emory $
! rditldmt@ITL-DTOPA-MP:emory $ gfortran -fopenmp $gflags omp_hello.f08
! rditldmt@ITL-DTOPA-MP:emory $ export OMP_NUM_THREADS=16
! rditldmt@ITL-DTOPA-MP:emory $ aa
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
!  Hello World !
! rditldmt@ITL-DTOPA-MP:emory $
