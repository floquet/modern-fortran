module mTestSuite

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    use mTimerClock,                    only : timer_clock
    use mTimerCPU,                      only : timer_cpu

    implicit none ! protects all methods in this scope

    ! parameters
    integer,     parameter :: ip = INT64, rp = REAL64
    real ( rp ), parameter :: tolerance = 5.0_rp * epsilon ( 1.0_rp ), zero = 0.0_rp

    integer ( ip ) :: k_measure = 0_ip, k_iteration = 0_ip

    ! derived types
    type ( timer_clock ) :: local_timer_clock
    type ( timer_cpu )   :: local_timer_cpu

contains

    subroutine scalar_tangent_test_bundler ( nu_ave, nu_var, nu_max, nu_min, time_cpu_total, alpha, &
                                             length, iterations, measures, frequency_sequence )
        real ( rp ),    intent ( out ) :: frequency_sequence ( : )
        real ( rp ),    intent ( out ) :: nu_ave, nu_var, nu_max, nu_min, alpha
        !real ( rp ),    intent ( out ) :: time_cpu_squared_total, time_wall_squared_total
        integer ( ip ), intent ( in )  :: length, iterations, measures

        ! rank 1
        real ( rp ), allocatable :: x_in ( : ), x_out ( : ), dx ( : )
        ! rank 0
        real ( rp ) :: time_cpu, time_wall, time_cpu_total
        real ( rp ) :: ave_dx
        real ( rp ) :: nu, nu_sq_ave, root

        character ( len =  10 ) :: time
            
            time_cpu_total = 0.0_rp
            ! initialize extremization variables
            nu_max = zero
            nu_min = huge ( 1.0_rp )

            do k_measure = 1, measures  ! repeat measurements to resolve clearer picture of performance
                call date_and_time ( time = time )
                !write ( *, '( ''k_measure = '', g0, '' of '', g0, '': time '', g0, ''.'' )' ) k_measure, measures, time ( 1 : 6 )

                call scalar_tangent_test_kernel ( time_cpu, time_wall, length, iterations, x_in, x_out )

                ! verify arithmetic integrity
                dx ( : ) = abs ( x_in ( : ) - x_out ( : ) )  ! atan - tan should return x_out = 
                ave_dx = norm2 ( dx ) / real ( length, rp )
                if ( ave_dx > tolerance ) then
                    write ( *, 100 ) ave_dx
                    write ( *, 110 ) tolerance
                    stop '... fatal arithmetic error ...'
                end if

                nu = real ( iterations, rp ) / time_cpu ! convert to frequency
                frequency_sequence ( k_measure ) = nu   ! watch time evolution

                alpha = time_cpu / time_wall  ! efficiency

                ! bound variation
                if ( nu > nu_max ) nu_max = nu
                if ( nu < nu_min ) nu_min = nu

                time_cpu_total = time_cpu_total + time_cpu
            end do

            ! extremal values
            nu_max = maxval ( frequency_sequence )
            nu_min = minval ( frequency_sequence )

            ! mean and variance
            nu_ave    = sum ( frequency_sequence )                      / real ( measures, rp )
            nu_sq_ave = sum ( frequency_sequence * frequency_sequence ) / real ( measures, rp )
            root      = nu_sq_ave - nu_ave ** 2

            if ( root < 0.0_rp ) then ! avoid sqrt of a negative number
                root = abs ( root )
                if ( root < tolerance ) write ( *, 200 ) root
            end if
            nu_var = sqrt ( root )

            return

        100 format ( /, 'Consistency error in atan - tan test', /, 'dx = | x_in - x_out | = ', g0 )
        110 format ( /, 'Tolerance = 5 epsilon = ', g0 )

        200 format ( /, 'Arithmetic error computing variance: calling for square root of -', g0, '.', &
                     /, 'Possible causes: 1) pathological data, 2) large magnitude data, 3) nearly constant data.' &
                     /, 'Computation continues...')

    end subroutine scalar_tangent_test_bundler

    subroutine scalar_tangent_test_kernel ( time_cpu, time_wall, length, iterations, x_in, x_out )

        real    ( rp ), intent ( out ) :: time_cpu, time_wall, x_in ( : ), x_out ( : )
        integer ( ip ), intent ( in )  :: length, iterations

        real    ( rp ), allocatable :: x ( : )

        integer ( ip ) :: alloc_status
        character ( len = 512 ) :: alloc_message

        character ( len = * ), parameter :: fmt_allocproblem = '( "Error ", g0, " memory." )'
        character ( len = * ), parameter :: fmt_allocstat    = '( "stat = ", g0 )'
        character ( len = * ), parameter :: fmt_allocmsg     = '( "errmsg  = ", g0, "." )'

            call local_timer_clock % timer_start_clock ( )

            ! deallocate array
            if ( allocated ( x ) ) then
                deallocate ( x, stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, fmt = fmt_allocproblem ) 'deallocating'
                    write ( *, fmt = fmt_allocstat ) alloc_status
                    write ( *, fmt = fmt_allocmsg ) trim ( alloc_message )
                end if
            end if

            ! allocate array
            allocate ( x ( 1 : length ), stat = alloc_status, errmsg = alloc_message )
            if ( alloc_status /= 0 ) then
                write ( *, fmt = fmt_allocproblem ) 'allocating'
                write ( *, fmt = fmt_allocstat ) alloc_status
                write ( *, fmt = fmt_allocmsg ) trim ( alloc_message )
            end if

            x ( : ) = 1.0_rp

            !   start local timer
            call local_timer_cpu   % timer_start_cpu   ( )

                x_in ( : ) = x ( : )
                do k_iteration = 1, iterations
                    x ( : ) = atan ( x ( : ) )
                    x ( : ) =  tan ( x ( : ) )
                end do
                x_out ( : ) = x ( : )

            time_cpu  = local_timer_cpu   % time_elapsed_cpu ( )
            time_wall = local_timer_clock % time_elapsed_clock ( )
 
    end subroutine scalar_tangent_test_kernel

end module mTestSuite
