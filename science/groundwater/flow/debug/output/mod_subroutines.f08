module lSubroutines

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : one, zero

    implicit none

contains

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    real ( rp ) function fkr ( ph, alpha ) result ( rhc ) ! computes the relative hydraulic conductivity.

        real ( rp ), intent ( in )  :: ph, alpha

            if ( ph .ge. zero ) then
                rhc = one
            else
                rhc = exp ( alpha * ph )
            end if

    end function fkr

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    real ( rp ) function dfkr ( ph, alpha ) result ( srhc ) ! computes the slope of the relative hydraulic conductivity curve.

        ! equivalently alpha * fkr ( ph, alpha )

        real ( rp ), intent ( in ) :: ph, alpha

            if ( ph .ge. zero ) then
                srhc = zero
            else
                srhc = alpha * exp ( alpha * ph )
            end if

    end function dfkr

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    subroutine analytic ( time, theada ) ! computes the slope of the relative hydraulic conductivity curve.

        use mPrecisionDefinitions,  only : ip, rp
        use mConstants,             only : half, pi
        use mParameters,            only : nmax, alpha, hr, dz, fks, fl, qq, thr, ths

        real ( rp ), intent ( in )  :: time
        real ( rp ), intent ( out ) :: theada ( 1 : nmax )

        real ( rp )                 :: c = zero, flamk = zero, flip = zero, gamma = zero, sum = 0, term = zero, z = zero
        real ( rp )                 :: t1 = zero, t3 = zero, t4 = zero, t5 = 0, t6 = zero

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

                ! Compute the time-dependent term.

                sum  = zero
                flip = -one
                k = 0
                do
                    k = k + 1
                    flamk = pi * k / fl
                    gamma = ( alpha * alpha * 0.25e0_rp + flamk * flamk ) / c
                    term  = flamk / gamma * exp ( - gamma * time )
                    sum = sum + term * flip * sin ( flamk * z )
                    flip = - flip
                    if ( term .lt. 1.0e-30_rp ) exit
                end do  ! term .lt. 1.0e-30_rp

                theada ( n ) = log ( t1 + t6 * ( t4 + t5 * sum * 2 / ( fl * c ) ) ) / alpha + z

            end do  ! n = 2, nmax - 1

            theada ( 1 ) = hr

    end subroutine analytic

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    subroutine tri_diag ( )

        !         This subroutine solves a tri-diagonal system of equations,
        !         aa * x = b
        !         with unknowns starting at 2 and ending at nmax - 1.

        use mParameters,    only : nmax
        use mEqs,           only : aa, b

        real ( rp )     :: r = zero

        integer ( ip )  :: i = 0

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

end module lSubroutines
