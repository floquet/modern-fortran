! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'library/mod_precision_definitions.f08'
include 'library/mod_parameters.f08'
include 'library/mod_eqs.f08'

program transient

    use mPrecisionDefinitions, only : ip, rp, zero, one, half, pi
    use mParameters
    use mEqs

    implicit NONE

    ! populate at compilation to detect memory issues

    integer ( ip ) :: iquit = 0, nonitr = 0, notime = 0, nt = 0
    integer ( ip ) :: k = 0, kk = 0, m = 0, n = 0, nn = 0

    ! rank 2
    real ( rp ) :: aa_save ( 1 : 3, 1 : nmax ) = zero

    ! rank 1
    real ( rp ) :: bsave  ( 1 : nmax ) = zero
    real ( rp ) :: hlast  ( 1 : nmax ) = zero
    real ( rp ) :: thead  ( 1 : nmax ) = zero
    real ( rp ) :: theada ( 1 : nmax ) = zero
    real ( rp ) :: z      ( 1 : nmax ) = zero

    real ( rp ) :: zz   ( 1 : 2 ) = zero
    real ( rp ) :: node ( 1 : 2 ) = zero

    ! rank 0
    real ( rp ) :: cpu_0 = zero, cpu_1 = zero, time = zero
    real ( rp ) :: dt = zero, dth = zero, epsnon = zero, ph = zero, rlast = zero, rmax = zero, rr = zero
    real ( rp ) :: om = zero, omnon = zero, ommin = zero, ommax = zero, omred = zero, omadd = zero
    real ( rp ) :: sum1 = zero, sum2 = zero, sum3 = zero, tt1 = zero, tt2 = zero, tt3 = zero
    real ( rp ) :: abs_err = zero, err = zero, worst_error = zero, worst = zero

    character ( len = * ), parameter :: me = 'program transient'  ! self-identification
    character ( len = 1 )            :: method = " "

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
        notime = 100_ip

        aa ( : , : ) = zero
        b  ( : )     = zero
!
!       Initial guess.
!
        ! vector assignments
        z     ( : ) = dz * [ ( k - 1, k = 1, nmax ) ]
        thead ( : ) = hr + z
        hlast ( : ) = thead ( : )

        ! boundary conditions ( vector assignments handle first term for thead and hlast )
        thead ( nmax ) = zero
        hlast ( nmax ) = thead ( nmax )
!
!       Transient solution.
!
        advance_time: do nt = 1, notime
            time = dt * nt
            print *, 'Time =', time
!
!           Nonlinear iteration.
!
            m     = 0
            iquit = 0
            rlast = 1.0e20_rp  ! Reid, Metcalf, Cohen: section B.6, p. 408

!            do while ( ( iquit .eq. 0 ) .and. ( m .lt. nonitr ) )  ! Reid p. 408
            nonlinear_iteration: do ! do while is deprecated: Reid, Metcalf, Cohen p. 408
                m = m + 1
!
!               Set nonlinear solver to Newton.
!
                if ( m .le. 100 ) then
                    method = 'p'
                else
                    method = 'n'
                end if

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
                        tt1 = fkr ( ph )
                        sum1 = sum1 + tt1
                        sum3 = sum3 + tt1 * ( thead ( nn ) - thead ( n ) )
                        if ( method .eq. 'p' ) then
                            aa ( kk, n ) = - tt1
                        else  !  method = 'n' cf. line 96
                            tt2  = dfkr ( ph ) * half * ( thead ( n ) - thead ( nn ) )
                            sum2 = sum2 + tt2
                            aa ( kk, n ) = - tt1 + tt2
                        end if
                    end do  ! k = 1, 2

                    tt3 = dexp (alpha * (thead ( n ) - z)) * dz * dz * (ths - thr) * alpha / (fks * dt)
                    aa ( 2, n ) = sum1 + tt3
                    if ( method .eq. 'n' ) aa ( 2, n ) = aa ( 2, n ) + sum2
                    b ( n ) = sum3 + tt3 * ( hlast ( n ) - thead ( n ) )
                end do  ! n = 2, nmax - 1

!               q term.

                ph = ( thead( nmax ) + thead ( nmax - 1 ) ) * half - fl + dz * half
                tt1 = fkr ( ph )
                aa ( 1, nmax ) = - tt1
                aa ( 2, nmax ) =   tt1
                b ( nmax ) = qq * dz / fks + tt1 * dz + ( thead ( nmax - 1 ) - thead ( nmax ) ) * tt1

!               Compute a new delta head.

                bsave   = b
                aa_save = aa
                call tri_diag

!               Compute maximum residual from linear solver.

                rmax = - 1.0e20_rp
                do n = 2, nmax - 1
                    if ( n .eq. 2 ) then
                        rr = bsave ( n ) - aa_save ( 2, n ) * b ( n ) - aa_save ( 3, n ) * b ( n + 1 )
                    else if ( n .eq. nmax - 1 ) then
                        rr = bsave ( n ) - aa_save ( 1, n ) * b ( n - 1 ) - aa_save ( 2, n ) * b ( n )
                    else
                        rr = bsave ( n ) - aa_save ( 1, n ) * b ( n - 1 ) - aa_save ( 2, n ) * b ( n ) &
                                         - aa_save ( 3, n ) * b ( n + 1 )
                    end if  ! ( n .eq. 2 )
                    rmax = max ( abs ( real ( rr, rp ) ), rmax )
                end do  ! n = 2, nmax - 1

                print *, 'Max linear solver residual =', rmax
                print *, 'aa_save ( 2, nmax - 1 ), aa_save ( 3, nmax - 1 ), bsave ( nmax - 1 ) ', &
                          aa_save ( 2, nmax - 1 ), aa_save ( 3, nmax - 1 ), bsave ( nmax - 1 )
                print *, 'thead ( nmax - 1 )', thead ( nmax - 1 )

!               Compute maximum difference in the nonlinear solution and
!               update head.

                rmax = - 1.0e20_rp
                do n = 1, nmax
                    rmax = max ( abs ( b ( n ) ), rmax )
                    thead ( n ) = omnon * b ( n ) + thead ( n )
                end do  ! n = 1, nmax

                print *, 'method, m, rmax, omnon ', method, m, rmax, omnon

!               Adjust omnon.

                if (rmax .le. rlast) then
                    omnon = omnon + omadd
                    if ( omnon .gt. ommax ) omnon = ommax
                else
                    omnon = omnon * omred
                    if ( omnon .lt. ommin ) omnon = ommin
                end if

!               Check for convergence.

                if ( rmax .le. epsnon ) iquit = 1
                if ( ( iquit .eq. 0 ) .and. ( m .lt. nonitr ) ) exit nonlinear_iteration

               rlast = rmax

            end do nonlinear_iteration  ! exit on ( ( iquit .eq. 0 ) .and. ( m .lt. nonitr ) )

            do n = 1, nmax
                hlast ( n ) = thead ( n )
            end do

        end do advance_time  ! nt = 1, notime

!         Compute the analytic solution.

        call analytic ( time, theada )

        print *, '( n, z, thead ( n ), theada( n ), n = 191, 200 )'
        do n = nmax - 20, nmax - 1
            write ( *, '( i5, 4f10.3 )' ) n, ( n - 1 ) * dz, thead ( n ), theada ( n )
        end do

!         Compute the worst h.

        worst = zero
        do n = 1, nmax - 1
            err = thead ( n ) - theada ( n )
            abs_err = abs (err)
            if ( worst .lt. abs_err) then
                worst       = abs_err
                worst_error = err
            end if
        end do

        print *, 'worst error =', worst_error

        call cpu_time ( cpu_1 ) ! global cpu time
        write ( *, '( /, "cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me // "." ! must reduce to constant expression

end program transient

!       ====================================================================================================================       !

real ( rp ) function fkr ( ph ) result ( rhc )

!         This subroutine computes the relative hydraulic conductivity.

    use mPrecisionDefinitions, only : rp, zero, one
    use mParameters

    implicit NONE

    real ( rp ), intent ( in )  :: ph

        if ( ph .ge. zero ) then
            rhc = one
        else
            rhc = exp ( alpha * ph )
        end if

end function fkr

!       ====================================================================================================================       !

    real ( rp ) function dfkr ( ph ) result ( srhc )

!         This subroutine computes the slope of the relative hydraulic
!         conductivity curve.

    ! equivalently alpha * fkr ( ph )
    use mPrecisionDefinitions, only : rp, zero, one
    use mParameters

    implicit NONE

    real ( rp ), intent ( in ) :: ph

        if ( ph .ge. zero ) then
            srhc = zero
        else
            srhc = alpha * exp ( alpha * ph )
        end if

end function dfkr

!       ====================================================================================================================       !

subroutine analytic ( time, theada )

!         This subroutine computes the slope of the relative hydraulic
!         conductivity curve.

    use mPrecisionDefinitions, only : ip, rp, zero, one, half, pi
    use mParameters

    implicit NONE

    real ( rp ), intent ( in )  :: time
    real ( rp ), intent ( out ) :: theada ( 1 : nmax ) = zero

    real ( rp )                 :: c = zero, flamk = zero, flip = zero, gamma = zero, sum = 0, term = zero
    real ( rp )                 :: t1 = zero, t2 = zero, t3 = zero, t4 = zero, t5 = 0, t6 = zero

    integer ( ip )              :: k = 0, n = 0

        t1 = exp ( alpha * hr )
        c  = ( ths - thr ) * alpha / fks

        do n = 2, nmax - 1

            z  = ( n - 1 ) * dz
            t3 = exp (  alpha * half * ( fl * 3 - z ) )
            t4 = exp (- alpha * ( fl - z ) * half ) &
               - exp (- alpha * ( fl + z ) * half )
            t5 = one - exp ( - alpha * fl )
            t6 = qq / fks

!         Compute the time-dependent term.

            sum  = zero
            flip = -one
            k = 0
            do
                k = k + 1
                flamk = pi * k / fl
                gamma = ( alpha * alpha * 0.25e0_rp + flamk * flamk ) / c
                term  = flamk / gamma * exp ( - gamma * time )
                sum = sum + term * flip * sin ( flamk * z )
                if ( term .lt. 1.0e-30_rp ) exit
                flip = - flip
            end do  ! term .lt. 1.0e-30_rp

            theada ( n ) = log ( t1 + t6 * ( t4 + t5 * sum * 2 / ( fl * c ) ) ) / alpha + z

        end do  ! n = 2, nmax - 1

        theada ( 1 ) = hr

end subroutine analytic

!       ====================================================================================================================       !

subroutine tri_diag (  )

!         This subroutine solves a tri-diagonal system of equations,

!         aa * x = b

!         with unknowns starting at 2 and ending at nmax - 1.

    use mPrecisionDefinitions, only : ip, rp, zero, one
    use mParameters, only : nmax
    use mEqs

    implicit NONE

    real ( rp ), intent ( in )  :: time
    real ( rp ), intent ( out ) :: theada ( 1 : nmax ) = zero

    real ( rp )                 :: r = zero

    integer ( ip )              :: i = 0

        b ( 1 ) = zero

        do i = 3, nmax
            r = aa ( 1, i ) / aa ( 2, i - 1 )
            aa ( 2, i ) = aa ( 2, i ) - r * aa ( 3, i - 1 )
            b ( i ) = b ( i ) - r * b ( i - 1 )
        end do

        b ( nmax ) = b ( nmax ) / aa ( 2, nmax )
        do i = nmax - 1, 2, -1
            b ( i ) = ( b ( i ) - aa ( 3, i ) * b ( i + 1 ) ) / aa ( 2, i )
        end do

end subroutine tri_diag
