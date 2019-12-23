module mTestSuite

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mAverager,                      only : averager
    use mTimerClock,                    only : timer_clock
    use mTimerCPU,                      only : timer_cpu

    implicit none ! protects all methods in this scope

    ! parameters
    integer, parameter :: measurements = 50
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

    subroutine matmul_test ( io_handle, rank, hpc, rack )

        integer,               intent ( in ) :: io_handle, rank
        character ( len = * ), intent ( in ) :: hpc, rack

        integer, parameter :: rows = 1024
        real ( rp ), dimension ( 1 : rows, 1 : rows ) :: A = 0.0_rp, B = 0.0_rp, C = 0.0_rp
        integer ( ip ) :: lambda = 50

            write ( io_handle, 100 ) 'identity matrices: C = AB, B = CA, C = AB', rows

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

                time_cpu  = local_timer_cpu   % time_elapsed_cpu ( )
                time_wall = local_timer_clock % time_elapsed_clock ( )
                cpu_utilization  = time_cpu / time_wall

                array_cpu ( m )  = time_cpu
                array_wall ( m ) = time_wall
                array_util ( m ) = cpu_utilization
            end do

            stats = averager ( array_cpu )
            write ( io_handle, 110 ) trim ( hpc ), trim ( rack ), rank, 'matmul_1024', 'cpu',         [ ( stats ( k ), k = 1, 5 ) ]

            stats = averager ( array_wall )
            write ( io_handle, 110 ) trim ( hpc ), trim ( rack ), rank, 'matmul_1024', 'wall',        [ ( stats ( k ), k = 1, 5 ) ]

            stats = averager ( array_util )
            write ( io_handle, 110 ) trim ( hpc ), trim ( rack ), rank, 'matmul_1024', 'utilization', [ ( stats ( k ), k = 1, 5 ) ]
            flush ( io_handle )

        100 format ( g0, ': size = ', g0 )
        110 format ( 9( g0, ', ' ), g0 )

    end subroutine matmul_test

    subroutine tangent_test ( io_handle, rank, hpc, rack )

        integer,               intent ( in ) :: io_handle, rank
        character ( len = * ), intent ( in ) :: hpc, rack

        real    ( rp ) :: x = 1.0_rp
        integer ( ip ) :: lambda = 2**30

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
            write ( io_handle, 110 ) trim ( hpc ), trim ( rack ), rank, 'tan-atan', 'cpu',         [ ( stats ( k ), k = 1, 5 ) ]

            stats = averager ( array_wall )
            write ( io_handle, 110 ) trim ( hpc ), trim ( rack ), rank, 'tan-atan', 'wall',        [ ( stats ( k ), k = 1, 5 ) ]

            stats = averager ( array_util )
            write ( io_handle, 110 ) trim ( hpc ), trim ( rack ), rank, 'tan-atan', 'utilization', [ ( stats ( k ), k = 1, 5 ) ]
            flush ( io_handle )

        100 format ( g0, ': ', g0, ' cycles' )
        110 format ( 9( g0, ', ' ), g0 )

    end subroutine tangent_test

end module mTestSuite
