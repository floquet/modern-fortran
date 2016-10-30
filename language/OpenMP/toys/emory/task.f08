! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/91-pthreads/openMP-F90.html
PROGRAM  task

    real :: x ( 1 : 1024 )

        !$OMP PARALLEL

        print *, "Hello World !"
        call system_clock ( t0 )

        !$OMP END PARALLEL

END PROGRAM  task

! rditldmt@ITL-DTOPA-MP:emory $ date
! Thu May 26 09:05:28 CDT 2016
! rditldmt@ITL-DTOPA-MP:emory $ pwd
! /Users/rditldmt/hpc/fortran/language/OpenMP/toys/emory
! rditldmt@ITL-DTOPA-MP:emory $
! rditldmt@ITL-DTOPA-MP:emory $ gfortran -fopenmp $gflags task.f08
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
