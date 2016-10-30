program matrix_multiplication

    use mPrecisionDefinitions,  only : ip, rp
    use mParameters,            only : zero, one
    use mAllocator,             only : allocator

    implicit none

    integer ( ip ), parameter :: m_default = 1024_ip, n_default = 1024_ip, p_default = 1024_ip
    integer ( ip )            :: m = 0, n = 0, p = 0  ! dimension parameters
    integer ( ip )            :: c = 0, r = 0, d = 0  ! indices
    integer ( ip )            :: census_1 = 0, census_2 = 0, census_3 = 0
    integer ( ip )            :: narg = 0

    real ( rp ), allocatable  :: A ( : , : ), B ( : , : ), F ( : , : )
    real ( rp ), allocatable  :: flat_1 ( : ), flat_2 ( : )

    real ( rp )               :: t0 = zero, t1 = zero, delta_t = zero
    real ( rp )               :: u0 = zero, u1 = zero
    real ( rp )               :: best_time = zero, ref_time = zero

    character ( len = * ),   parameter :: me   = 'program matrix_multiplication' ! Metcalf, Reid, Cohen: p. 309
    character ( len = 255 )            :: best = "", argument = "", cmd = ""

        call cpu_time ( u0 ) ! global cpu time

        call srand ( 1 )

        m = m_default !   default shape parameters
        n = n_default
        p = p_default

        !   check for command line shape parameters
        call get_command ( cmd )
        write ( *, '( "command line = ", g0, / )' ) trim ( cmd )

        narg = command_argument_count ( )

        if ( narg >= 1 ) then
            call getarg ( 1, argument )
            read  ( argument, * ) m
            write ( *, '( "m set to ", g0 )' ) m
            if ( narg >= 2 ) then
                call getarg ( 2, argument )
                read  ( argument, * ) n
                write ( *, '( "n set to ", g0 )' ) n
                if ( narg >= 3 ) then
                    call getarg ( 3, argument )
                    read  ( argument, * ) p
                    write ( *, '( "p set to ", g0 )' ) p
                end if
            end if
        end if
        write ( *, * )

        call allocator ( A, B, F, flat_1, flat_2, m, n, p, census_1, census_2, census_3 ) ! allocate memory for matrices and lists

        ! load matrices
        write ( *, '( /, "matrix loading..." )' )
        best_time = huge ( one )

        ! load matrices columns inside
        call cpu_time ( t0 )

        write ( *, '( g0, " n (rows)", /, g0, " p (cols) for A", / )' ) n, p
        do c = 1, n
            do r = 1, m
                A ( r, c ) = rand ( )
            end do
        end do

        write ( *, '( g0, " n (rows)", /, g0, " p (cols) for B", / )' ) n, p
        do c = 1, p
            do r = 1, n
                B ( r, c ) = rand ( )
            end do
        end do

        call cpu_time ( t1 )
        delta_t = t1 - t0
        ref_time = delta_t
        write ( *, 100 ) one, delta_t, 'load matrices columns inside'
        if ( delta_t < best_time ) then
          best_time = delta_t
          best = 'load matrices columns inside'
        end if

        ! load matrices columns outside
        call cpu_time ( t0 )

        do r = 1, m
            do c = 1, n
                A ( r, c ) = rand ( )
            end do
        end do

        do r = 1, n
            do c = 1, p
                B ( r, c ) = rand ( )
            end do
        end do

        call cpu_time ( t1 )
        delta_t = t1 - t0
        write ( *, 100 ) delta_t / ref_time, delta_t, 'load matrices columns outside'
        if ( delta_t < best_time ) then
          best_time = delta_t
          best = 'load matrices columns outside'
        end if

        ! load matrices via reshape
        call cpu_time ( t0 )

        flat_1 = [ ( rand ( ), r = 1, census_1 ) ]
        A = reshape ( A, [ m, n ] )

        flat_2 = [ ( rand ( ), r = 1, census_2 ) ]
        B = reshape ( B, [ n, p ] )

        call cpu_time ( t1 )
        delta_t = t1 - t0
        write ( *, 100 ) delta_t / ref_time, delta_t, 'load matrices via reshape'
        if ( delta_t < best_time ) then
          best_time = delta_t
          best = 'load matrices via reshape'
        end if

        write ( *, 110 ) best_time, trim ( best )

        ! matrix multiplications
        write ( *, '( /, "matrix multiplications..." )' )
        best_time = huge ( one )

        ! intrinsic matrix multiplications
        call cpu_time ( t0 )
        F = matmul ( A, B )
        call cpu_time ( t1 )

        delta_t = t1 - t0
        ref_time  = delta_t
        write ( *, 100 ) one, delta_t, 'intrinsic matrix multiplication'
        if ( delta_t < best_time ) then
          best_time = delta_t
          best = 'intrinsic matrix multiplication'
        end if

        ! stone age matrix multiplications
        call cpu_time ( t0 )
        do r = 1, m
            do c = 1, n
                F ( r, c ) = 0
            end do
        end do

        do r = 1, m
            do c = 1, n
                do d = 1, p
                    F ( r, c ) = F ( r, c ) + A ( r, n ) * B ( n, c )
                end do
            end do
        end do

        call cpu_time ( t1 )
        delta_t = t1 - t0
        write ( *, 100 ) delta_t / ref_time, delta_t, 'do loop multiplication'
        if ( delta_t < best_time ) then
          best_time = delta_t
          best = 'do loop multiplication'
        end if

        ! dot product matrix multiplication
        call cpu_time ( t0 )
        do r = 1, m
            do c = 1, n
                F ( r, c ) = dot_product ( A ( r, : ), B ( : , c ) )
            end do
        end do

        call cpu_time ( t1 )
        delta_t = t1 - t0
        write ( *, 100 ) delta_t / ref_time, delta_t, 'dot product matrix multiplication rc'
        if ( delta_t < best_time ) then
          best_time = delta_t
          best = 'dot product matrix multiplication rc'
        end if

        ! dot product matrix multiplication
        call cpu_time ( t0 )
        do c = 1, n
            do r = 1, m
                F ( r, c ) = dot_product ( A ( r, : ), B ( : , c ) )
            end do
        end do

        call cpu_time ( t1 )
        delta_t = t1 - t0
        write ( *, 100 ) delta_t / ref_time, delta_t, 'dot product matrix multiplication cr'
        if ( delta_t < best_time ) then
          best_time = delta_t
          best = 'dot product matrix multiplication cr'
        end if

        write ( *, 110 ) best_time, trim ( best )

        ! end of computation
        call cpu_time ( u1 )
        write ( *, 120 ) u1 - u0, 'global cpu time'

        stop "successful completion for " // me  ! must reduce to constant expression

    100 format ( F10.2, ': ', E9.3, ' seconds for ', A, / )
    110 format ( 'winner:', /, E9.3, ' seconds for ', A, / )
    120 format ( E9.3, ' seconds for ', A, '.', / )


end program matrix_multiplication
