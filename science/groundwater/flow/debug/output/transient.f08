! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program transient

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero, one, half, pi
    use mParameters,            only : nmax, alpha, dz, fl, fks, hr, qq, thr, ths
    use mEqs,                   only : aa, b
    use lSubroutines,           only : fkr, dfkr, analytic, tri_diag

    implicit NONE

    ! rank 1
    integer ( ip ) :: node ( 1 : 2 )
    ! rank 0
    integer ( ip ) :: iquit, nonitr, notime, nt
    integer ( ip ) :: k, kk, m, n, nn, i
    integer ( ip ) :: myIO, worst_at = 0, io_status, io_tri

    ! rank 2
    real ( rp ) :: aa_save ( 1 : 3, 1 : nmax ) = zero

    ! rank 1
    real ( rp ) :: bsave  ( 1 : nmax ), hlast  ( 1 : nmax ), thead  ( 1 : nmax ), theada ( 1 : nmax ), z_vec ( 1 : nmax )

    real ( rp ) :: zz ( 1 : 2 )

    ! rank 0
    real ( rp ) :: cpu_0 = zero, cpu_1 = zero, time = zero
    real ( rp ) :: dt = zero, dth = zero, epsnon = zero, ph = zero, rlast = zero, rmax = zero, rr = zero
    real ( rp ) :: om = zero, omnon = zero, ommin = zero, ommax = zero, omred = zero, omadd = zero
    real ( rp ) :: sum1 = zero, sum2 = zero, sum3 = zero, tt1 = zero, tt2 = zero, tt3 = zero
    real ( rp ) :: abs_err = zero, err = zero, worst_error = zero, worst = zero, z = zero

    character ( len = * ), parameter :: me = 'program transient'  ! self-identification
    character ( len = 512 )          :: io_msg = ''
    character ( len = 1 )            :: method = ''

        call cpu_time ( cpu_0 ) ! global cpu time

        !   initialization
        fl  = 50.0_rp
        fks = 0.1_rp
        thr = 0.15_rp
        ths = 0.45_rp
        dz  = fl / ( nmax - 1 )
        dth = 2 * pi / ( nmax - 1 )
        dt  = 0.01_rp

        alpha  = 0.1_rp
        epsnon = one / 10000_ip
        hr     = -50.0_rp
        qq     = 10.0_rp

        om     = one
        omnon  = one / 10
        ommin  = one / 100
        ommax  = one
        omred  = 2 * one / 3
        omadd  = one / 200

        nonitr = 20000_ip
        notime = 2_ip

        aa ( : , : ) = zero
        b  ( : )     = zero
        !
        !       Initial guess.
        !
        ! vector assignments
        z_vec ( : ) = dz * [ ( k - 1, k = 1, nmax ) ]
        thead ( : ) = hr + z_vec
        hlast ( : ) = thead ( : )

        ! boundary conditions ( vector assignments handle first term for thead and hlast )
        thead ( nmax ) = zero
        hlast ( nmax ) = thead ( nmax )
        ! open ( newunit = myIO, file = 'step 1.txt', iostat = io_status, iomsg = io_msg )
        ! do i = 1, nmax
        !     write ( myIO, '( I4, ". ", g0, ", ", g0 )' ) i, thead ( i ), z_vec ( i )
        ! end do
        ! close ( myIO )
        !
        ! stop
        !
        !       Transient solution.
        !
        open ( newunit = myIO, file = 'debug.txt', iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , '( "Failure to open file debug.txt" )' )
            write ( * , '( "iostat = " )' ) io_status
            write ( * , '( "iomsg  = " )' ) trim ( io_msg )
        end if

        open ( newunit = io_tri, file = 'tri_diag.txt', iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , '( "Failure to open file tri_diag.txt" )' )
            write ( * , '( "iostat = " )' ) io_status
            write ( * , '( "iomsg  = " )' ) trim ( io_msg )
        end if

        advance_time: do nt = 1, notime
            time = dt * nt
            iquit = 0
            rlast = 1.0e20_rp  ! Reid, Metcalf, Cohen: section B.6, p. 408

            ! do while ( ( iquit .eq. 0 ) .and. ( m .lt. nonitr ) )  ! Reid p. 408
            nonlinear_iteration: do m = 1, nonitr

                ! Set nonlinear solver to Newton.

                method = 'n'
                if ( m .le. 100 ) method = 'p'

                !print *, ' m = ', m, ', method = ', method
                do n = 2, nmax - 1

                    z        = ( n - 1 ) * dz
                    zz ( 1 ) = z - dz * half
                    zz ( 2 ) = zz ( 1 ) + dz

                    node ( 1 ) = n - 1
                    node ( 2 ) = n + 1

                    sum1 = zero
                    sum2 = zero
                    sum3 = zero

                    do k = 1, 2
                        kk = k * 2 - 1
                        nn = node ( k )
                        ph = ( thead ( nn ) + thead ( n ) ) * half - zz ( k )
                        tt1 = fkr ( ph, alpha )
                        sum1 = sum1 + tt1
                        sum3 = sum3 + tt1 * ( thead ( nn ) - thead ( n ) )
                        aa ( kk, n ) = - tt1
                        if ( method .eq. 'n' ) then
                            tt2  = dfkr ( ph, alpha ) * half * ( thead ( n ) - thead ( nn ) )
                            sum2 = sum2 + tt2
                            aa ( kk, n ) = aa ( kk, n ) + tt2
                        end if
                    end do  ! k = 1, 2

                    tt3 = exp ( alpha * ( thead ( n ) - z ) ) * dz * dz * ( ths - thr ) * alpha / ( fks * dt )
                    aa ( 2, n ) = sum1 + tt3
                    if ( method .eq. 'n' ) aa ( 2, n ) = aa ( 2, n ) + sum2
                    b ( n ) = sum3 + tt3 * ( hlast ( n ) - thead ( n ) )
                end do  ! n = 2, nmax - 1

        ! open ( newunit = myIO, file = 'step 2.txt', iostat = io_status, iomsg = io_msg )
        ! write ( myIO, '( "tt1, tt2, tt3, ph, sum1, sum3 = ", 6( g0, 2X ) )' ) tt1, tt2, tt3, ph, sum1, sum3
        ! do i = 1, nmax
        !     write ( myIO, '( I4, ". ", g0, ", ", g0, ", ", g0, ", ", g0 )' ) i, b ( i ), aa ( 1, i ), aa ( 2, i ), aa ( 3, i )
        ! end do
        ! close ( myIO )
        !
        ! stop

                ! q term.

                ph = ( thead( nmax ) + thead ( nmax - 1 ) ) * half - fl + dz * half
                tt1 = fkr ( ph, alpha )
                aa ( 1, nmax ) = - tt1
                aa ( 2, nmax ) =   tt1
                b ( nmax ) = qq * dz / fks + tt1 * dz + ( thead ( nmax - 1 ) - thead ( nmax ) ) * tt1

                ! Compute a new delta head.

                bsave   = b
                aa_save = aa
                call tri_diag ( )
                open ( newunit = myIO, file = 'step 3.txt', iostat = io_status, iomsg = io_msg )
                write ( myIO, '( /, "input system tri_diag = " )' )
                do i = 1, 3
                    write ( myIO, '( i4, ". ", 30( g0, 2X ) )' ) i, aa ( i , 1 : nmax )
                end do
                write ( myIO, '( /, "solution vector b  = " )' )
                do i = 1, nmax
                    write ( myIO, '( I4, ". ", g0 )' ) i, b ( i )
                end do
                close ( myIO )

                stop

                ! Compute maximum residual from linear solver.

                rmax = - 1.0e20_rp
                ! n = 2
                rr = bsave ( 2 ) - aa_save ( 2, 2 ) * b ( 2 ) - aa_save ( 3, 2 ) * b ( 2 + 1 )
                rmax = abs ( rr )
                do n = 3, nmax - 1
                    rr = bsave ( n ) - aa_save ( 1, n ) * b ( n - 1 ) - aa_save ( 2, n ) * b ( n ) &
                                     - aa_save ( 3, n ) * b ( n + 1 )
                    rmax = max ( abs ( rr ), rmax )
                end do
                ! nmax - 2
                rr = bsave ( nmax - 1 ) - aa_save ( 1, nmax - 1 ) * b ( nmax - 2 ) - aa_save ( 2, nmax - 1 ) * b ( nmax - 1 )
                rmax = max ( abs ( rr ), rmax )

                write ( myIO, '( /, "Max linear solver residual = ", g0 )' ) rmax
                write ( myIO, '( "thead ( nmax - 1 ) = ", g0 )' ) thead ( nmax - 1 )
                write ( myIO, '( "aa_save ( 2, nmax - 1 ), aa_save ( 3, nmax - 1 ), bsave ( nmax - 1 ) ", 3( g0, 2X ) )' ) &
                    aa_save ( 2, nmax - 1 ), aa_save ( 3, nmax - 1 ), bsave ( nmax - 1 )

                !  Compute maximum difference in the nonlinear solution and
                !  update head.

                rmax = - 1.0e20_rp
                do n = 1, nmax
                    rmax = max ( abs ( b ( n ) ), rmax )
                    thead ( n ) = omnon * b ( n ) + thead ( n )
                end do  ! n = 1, nmax
                write ( myIO, '( /, "thead = ", 30( g0, 2X ) )' ) thead

                ! print *, 'method, m, rmax, omnon ', method, m, rmax, omnon
                write ( myIO, '( "method = ", g0, ", m = ", g0, ", rmax = ", g0, ", omnon = ", g0 )' ) method, m, rmax, omnon
                flush ( myIO )

                ! Adjust omnon.

                if ( rmax .le. rlast ) then
                    omnon = omnon + omadd
                    if ( omnon .gt. ommax ) omnon = ommax
                else
                    omnon = omnon * omred
                    if ( omnon .lt. ommin ) omnon = ommin
                end if

                rlast = rmax
                hlast = thead

                ! Check for convergence.

                if ( rmax .le. epsnon ) exit nonlinear_iteration

            end do nonlinear_iteration  ! exit on ( ( iquit .eq. 0 ) .and. ( m .lt. nonitr ) )

        end do advance_time  ! nt = 1, notime
        close ( myIO )

        call cpu_time ( cpu_1 ) ! global cpu time
        write ( *, '( /, "cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me // "..." ! must reduce to constant expression

end program transient

!  18:03 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/projects/groundwater/flow/modern
!  18:03 dan-topas-pro-2 rditldmt $
!  18:03 dan-topas-pro-2 rditldmt $
!  18:03 dan-topas-pro-2 rditldmt $ date
! Tue Feb 16 18:03:08 CST 2016
!  18:03 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/projects/groundwater/flow/modern
!  18:03 dan-topas-pro-2 rditldmt $ make
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_eqs.o mod_eqs.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_subroutines.o mod_subroutines.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o transient.o transient.f08
! gfortran -g -o transient mod_constants.o mod_eqs.o mod_parameters.o mod_precision_definitions.o mod_subroutines.o transient.o
!  18:03 dan-topas-pro-2 rditldmt $ ./transient
!  data written to 'head pressures.txt'
! worst error = 157.14013345097405 at step 2000
!
! cpu time used = .50889999999999998E-001 seconds
!
! STOP successful completion for program transient...