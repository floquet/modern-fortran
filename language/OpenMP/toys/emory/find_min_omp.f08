! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/91-pthreads/openMP-F90.html
program find_max_omp

    use, intrinsic :: iso_fortran_env,  only : REAL64

    implicit none

    integer, parameter :: MAX = 1024 * 1024
    integer, parameter :: rp = REAL64

    real ( rp ) :: x ( 1 : MAX ) = 1.0_rp
    real ( rp ) :: my_max ( 1 : 10 ) = 0.0_rp

    real ( rp ) :: rmax

    integer :: num_threads = 0
    integer :: i = 0, n = 0
    integer :: id = 0, start = 0, halt = 0

    ! ===========================================================
    ! Declare the OpenMP functions
    ! ===========================================================
    integer, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS

        do concurrent ( i = 1 : MAX )
            x ( i ) = real ( i, rp )
        end do

        print *, 'expected answer: maximum value = ', MAX

        ! ===================================
        ! Parallel section: Find local maxima
        ! ===================================
        !$OMP  PARALLEL  PRIVATE ( i, id, start, halt, n )

        num_threads = omp_get_num_threads ( )
        n = MAX / num_threads

        id = omp_get_thread_num ( )

        ! ----------------------------------
        ! Find my own starting index
        ! ----------------------------------
        start = id * n + 1          !! Array start at 1
        halt = start + n - 1
        !print *, 'id = ', id, ', start = ', start, ', halt = ', halt

        ! ! ----------------------------------
        ! ! Find my own stopping index
        ! ! ----------------------------------
        ! if ( id /= ( num_threads - 1 ) ) then
        !     stop = start + n
        ! else
        !     stop = MAX
        ! end if

        ! ----------------------------------
        ! Find my own max
        ! ----------------------------------
        my_max ( id + 1 ) = x ( start )

        DO i = start + 1, halt
            IF ( x ( i ) > my_max ( id + 1 ) ) my_max ( id + 1 ) = x ( i )
        END DO
        !print *, 'id = ', id, '; max = ', my_max ( id + 1 )

        !$OMP END PARALLEL

        ! ===================================
        ! Find max over the local maxima
        ! ===================================
        rmax = my_max ( 1 )

        DO i = 2, num_threads
            IF ( rmax < my_max ( i ) ) rmax = my_max ( i )
        END DO

        print *, "global max = ", rmax

end program find_max_omp

! rditldmt@ITL-DTOPA-MP:emory $ date
! Thu May 26 16:56:41 CDT 2016
! rditldmt@ITL-DTOPA-MP:emory $ pwd
! /Users/rditldmt/hpc/fortran/language/OpenMP/toys/emory
! rditldmt@ITL-DTOPA-MP:emory $
! rditldmt@ITL-DTOPA-MP:emory $ export OMP_NUM_THREADS=4
! rditldmt@ITL-DTOPA-MP:emory $
! rditldmt@ITL-DTOPA-MP:emory $ gfortran -fopenmp $gflags find_min_omp.f08
! rditldmt@ITL-DTOPA-MP:emory $ ./a.out
!  expected answer: maximum value =      1048576
!  global max =    1048576.0000000000
! rditldmt@ITL-DTOPA-MP:emory $ 
