module mTestSuite

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mAverager,                      only : averager
    use mTimerClock,                    only : timer_clock
    use mTimerCPU,                      only : timer_cpu

    implicit none ! protects all methods in this scope

    ! parameters
    integer, parameter :: measurements = 2!0
    integer, parameter :: ip = INT64, rp = REAL64

    ! rank 1
    real ( rp ), dimension ( 1 : measurements ) :: array_cpu = 0.0_rp, array_wall = 0.0_rp, array_util = 0.0_rp
    real ( rp ) :: stats ( 1 : 5 ) = 0.0_rp

    ! rank 0
    integer ( ip ) :: io_stat = 0
    integer ( ip ) :: k = 0_ip, m = 0_ip
    real    ( rp ) :: time_cpu = 0.0_rp, time_wall = 0.0_rp, cpu_utilization = 0.0_rp

    ! strings
    character ( len = 256 ) :: io_msg = ''
    !character ( len = 128 ) :: iterations = ''

    ! derived types
    type ( timer_clock ) :: local_timer_clock
    type ( timer_cpu )   :: local_timer_cpu

contains

    subroutine matmul_test ( io_handle )

        integer, intent ( in ) :: io_handle

        integer, parameter :: rows = 100!1024
        real ( rp ), dimension ( 1 : rows, 1 : rows ) :: A = 0.0_rp, B = 0.0_rp, C = 0.0_rp
        integer ( ip ) :: lambda = 20

            write ( io_handle, 100 ) 'matrix multiplcation, A = BC, B = CA, C = AB', rows, lambda

            do k = 1, rows
                A ( k, k ) = 1.0_rp
                B ( k, k ) = 1.0_rp
                C ( k, k ) = 1.0_rp
            end do

            do m = 1, measurements
                !   start local timers
                call local_timer_cpu   % timer_start_cpu   ( )
                call local_timer_clock % timer_start_clock ( )
                do k = 1, lambda
                    C = matmul ( A, B )
                    A = matmul ( B, C )
                    B = matmul ( C, A )
                end do

                time_cpu  = local_timer_cpu   % time_elapsed_cpu ( );
                time_wall = local_timer_clock % time_elapsed_clock ( );
                cpu_utilization  = time_cpu / time_wall

                array_cpu ( m )  = time_cpu
                array_wall ( m ) = time_wall
                array_util ( m ) = cpu_utilization
            end do
            stats = averager ( array_cpu )
            write ( io_handle, 110 ) 'CPU time:    ', stats ( 1 ), stats ( 2 ), stats ( 5 )
            stats = averager ( array_wall )
            write ( io_handle, 110 ) 'wall time:   ', stats ( 1 ), stats ( 2 ), stats ( 5 )
            stats = averager ( array_util )
            write ( io_handle, 110 ) 'utilization: ', stats ( 1 ), stats ( 2 ), stats ( 5 )
            flush ( io_handle )

        100 format ( /, 'Process = ', g0, ', A = B = C = I_', g0, ': ', g0,' iterations.' )
        110 format ( g0, G9.4, ' +/- ', G9.4, ' s, spread = ', G9.4 )

    end subroutine matmul_test

    subroutine tangent_test ( io_handle )

        integer, intent ( in ) :: io_handle

        real    ( rp ) :: x = 1.0_rp
        integer ( ip ) :: lambda = 2**3!0

            write ( io_handle, 100 ) 'tan - arctan', lambda

            do m = 1, measurements
                !   start local timers
                call local_timer_cpu   % timer_start_cpu   ( )
                call local_timer_clock % timer_start_clock ( )
                do k = 1, lambda
                    x = atan ( x )
                    x = tan ( x )
                end do

                time_cpu  = local_timer_cpu   % time_elapsed_cpu ( );
                time_wall = local_timer_clock % time_elapsed_clock ( );
                cpu_utilization  = time_cpu / time_wall

                array_cpu  ( m ) = time_cpu
                array_wall ( m ) = time_wall
                array_util ( m ) = cpu_utilization
            end do
            stats = averager ( array_cpu )
            write ( io_handle, 110 ) 'CPU time:    ', stats ( 1 ), stats ( 2 )
            stats = averager ( array_wall )
            write ( io_handle, 110 ) 'wall time:   ', stats ( 1 ), stats ( 2 )
            stats = averager ( array_util )
            write ( io_handle, 110 ) 'utilization: ', stats ( 1 ), stats ( 2 )
            flush ( io_handle )

        100 format ( /, 'Process = ', g0, ', cycle of ', g0, '.' )
        110 format ( g0, g0, ' +/- ', g0, ' s')

    end subroutine tangent_test

end module mTestSuite
