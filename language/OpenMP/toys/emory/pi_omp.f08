program pi_omp

    use, intrinsic :: iso_fortran_env,  only : REAL64

    implicit none

    ! parameters
    integer,     parameter :: rp = REAL64
    real ( rp ), parameter :: pi_actual = acos ( -1.0_rp )

    integer, EXTERNAL :: omp_get_thread_num, omp_get_num_threads

    integer :: N = 0, w = 0, i = 0
    integer :: id = 0, num_threads = 0

    real ( rp ) :: x = 0.0_rp, sum = 0.0_rp
    real ( rp ) :: t0 = 0.0_rp, t1 = 0.0_rp
    real ( rp ) :: pi_computed = 0.0_rp, mypi = 0.0_rp

        N = 1024 * 1024             !! Number of intervals

        sum = 0.0_rp
        call cpu_time ( t0 )

        !$OMP    PARALLEL PRIVATE ( i, id, x, mypi )

        num_threads = omp_get_num_threads ( )
        id = omp_get_thread_num ( )

        w = N / num_threads  !! width of each interval

        mypi = 0.0_rp;

        ! area of a circle = pi r ** 2 = 4 int_0^1 sqrt ( 1 - x**2 ) dx
        ! print *, 'thread ', id, ': ', id * w, ' to ', ( id + 1 ) * w - 1
        do i = id * w, ( id + 1 ) * w - 1
            x = real ( i , rp ) / real ( N, rp )
            mypi = mypi + sqrt ( 1.0_rp - x ** 2 )
        end do
        ! print *, 'thread ', id, ': x = ', x, ': mypi = ', mypi

        !$OMP CRITICAL
        pi_computed = pi_computed + mypi / real ( N, rp )
        !$OMP END CRITICAL

        !$OMP    END PARALLEL
        pi_computed = 4.0_rp * pi_computed  ! integration was over quarter circle

        call cpu_time ( t1 )
        write ( *, 100 ) num_threads, N, w
        write ( *, 110 ) pi_computed, 'pi computed'
        write ( *, 110 ) pi_actual, 'pi actual'
        write ( *, 110 ) pi_actual - pi_computed, 'error'
        write ( *, 120 ) t1 - t0

    100 format ( 'Results using ', g0, ' threads over ', g0, ' intervals (', g0,' integrations per thread):' )
    110 format ( g0, 2X, g0 )
    120 format ( 'CPU time = ', g0, ' s' )

end program pi_omp
