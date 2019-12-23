! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/91-pthreads/openMP-F90.html
program bravo

    IMPLICIT NONE

    INTEGER              :: nthreads, myid
    INTEGER, EXTERNAL    :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS

        !$OMP PARALLEL private(nthreads, myid)

        myid = OMP_GET_THREAD_NUM()

        print *, "I am thread ", myid

        if ( myid == 0 ) then
            nthreads = OMP_GET_NUM_THREADS()
            print *, "Number of threads = ", nthreads
        end if

        !$OMP END PARALLEL

end program bravo

! rditldmt@ITL-DTOPA-MP:emory $ date
! Thu May 26 12:59:30 CDT 2016
! rditldmt@ITL-DTOPA-MP:emory $ pwd
! /Users/rditldmt/hpc/fortran/language/OpenMP/toys/emory
! rditldmt@ITL-DTOPA-MP:emory $ gfortranexport OMP_NUM_THREADS=8
! rditldmt@ITL-DTOPA-MP:emory $ gfortran -fopenmp $gflags bravo.f08
! rditldmt@ITL-DTOPA-MP:emory $ ./a.out
!  I am thread            3
!  I am thread            4
!  I am thread            1
!  I am thread            6
!  I am thread            5
!  I am thread            2
!  I am thread            7
!  I am thread            0
!  Number of threads =            8
