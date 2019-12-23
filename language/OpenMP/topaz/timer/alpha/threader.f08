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
