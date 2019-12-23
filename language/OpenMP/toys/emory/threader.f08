! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/91-pthreads/openMP-F90.html
program tanatan

    use, intrinsic :: iso_fortran_env,  only : REAL64

    implicit none

    integer, parameter :: MAX = 1024 * 1024
    integer, parameter :: rp = REAL64

    real ( rp ) :: x ( 1 : MAX ) = 1.0_rp

    real ( rp ) :: t0 = 0.0_rp, t1 = 0.0_rp, dt = 0.0_rp

    integer :: num_threads = 0
    integer :: i = 0, k = 0, n = 0
    integer :: id = 0, start = 0, halt = 0, loops = 0

    ! ===========================================================
    ! Declare the OpenMP functions
    ! ===========================================================
    integer, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS

        do concurrent ( i = 1 : MAX )
            x ( i ) = real ( i, rp )
        end do

        loops = 1024 ! numerical tan - atan cycles

        call cpu_time ( t0 )

        !$OMP  PARALLEL  PRIVATE ( i, id, start, halt, n )

        num_threads = omp_get_num_threads ( )
        n = MAX / num_threads

        id = omp_get_thread_num ( )

        ! paritition the array : give each thread an equal share
        start = id * n + 1
        halt = start + n - 1

        ! work loop
        do i = start + 1, halt
            do k = 1, loops
                x ( i ) = atan ( x ( i ) )
                x ( i ) = tan  ( x ( i ) )
            end do
        end do

        !$OMP end PARALLEL

        call cpu_time ( t1 )
        dt = t1 - t0

        write ( *, '( g0, ", ", g0 )' ) num_threads, dt

end program tanatan

! rditldmt@ITL-DTOPA-MP:emory $ date
! Thu May 26 16:52:39 CDT 2016
! rditldmt@ITL-DTOPA-MP:emory $ pwd
! /Users/rditldmt/hpc/fortran/language/OpenMP/toys/emory
! rditldmt@ITL-DTOPA-MP:emory $ export OMP_NUM_THREADS=4
! rditldmt@ITL-DTOPA-MP:emory $ gfortran -fopenmp $gflags tanatan.f08
! rditldmt@ITL-DTOPA-MP:emory $ ./a.out
! 1 vector size, MB
! 1024 tan - atan cycles
! 4 OMP threads
! 97.279949000000002 time used
! 24.319987250000001 efficiency, sec / thread
! rditldmt@ITL-DTOPA-MP:emory $ 
