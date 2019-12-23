c         1-D transient nsaturated flow test.
c         Specified head at the top and bottom.
c
c
    !   use, intrinsic :: iso_fortran_env, only : REAL64

      implicit real * 8 (a-h, o-z)
c
    !   real ( selected_real_kind ( REAL64 ) ) :: cpu_start, cpu_stop
      parameter (nmax = 11)
c
      common / params / alpha, hr, a, fl, dz, fks, thr, ths,
     &  qq
c
      common / eqs / aa(3, nmax), b(nmax)
c
      dimension thead(nmax)
      dimension hlast(nmax)
      dimension theada(nmax)
      dimension bsave(nmax)
      dimension aa_save(3, nmax)
      character method * 1

      integer :: io_status = 0, myIO, io_debug
      character ( len = 255 ) :: io_msg

c
      dimension zz(2), node(2)
c
      call cpu_time ( cpu_start ) ! global cpu time

      fl = 50.0d0
      fks = 0.1d0
      thr = 0.15d0
      ths = 0.45d0
      dz = fl / dble (nmax - 1)
      dth = datan2 (1.0d0, 0.0d0) * 2.0d0 / dble (nmax - 1)

      alpha = 0.1d0

      epsnon = 1.0d-5
      om = 1.0d0
      hr = -50.0d0
      nonitr = 20000
      omnon = 0.1d0
      ommin = 0.01d0
      ommax = 1.0d0
      omred = 0.667d0
      omadd = 0.005d0
      dt = 0.01
      notime = 2
      qq = 10.0d0
c
      do i = 1, nmax
        do j = 1, 3
          aa(j, i) = 0.0d0
        end do
        b(i) = 0.0d0
      end do
c
c         Initial guess.
c
      thead(1) = hr
      hlast(1) = thead(1)
      thead(nmax) = 0.0d0
      hlast(nmax) = thead(nmax)
      do n = 2, nmax - 1
        z = dble (n - 1) * dz
        thead(n) = hr + z
        hlast(n) = thead(n)
      end do
    !   print *, 'thead = ', thead
    !     open ( newunit = myIO, file = 'step 1.txt', iostat = io_status, iomsg = io_msg )
    !     do i = 1, nmax
    !         write ( myIO, '( I4, ". ", g0 )' ) i, thead ( i )
    !     end do
    !     close ( myIO )
      !
    !     stop
c         Transient solution.
c
      open ( newunit = io_debug, file = 'debug.txt',
     & iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
          print *, 'iostat = ', io_status
          print *, 'iomsg = ', trim ( io_msg )
      end if

        open ( newunit = io_tri, file = 'tri_diag.txt', iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , '( "Failure to open file tri_diag.txt" )' )
            write ( * , '( "iostat = " )' ) io_status
            write ( * , '( "iomsg  = " )' ) trim ( io_msg )
        end if

      do nt = 1, notime
c
        time = dble (nt) * dt
        write ( io_debug, '( /, "time = ", g0 )' ) time
        write ( io_debug, '(    "nt = ", g0, "; dt = ", g0 )' ) nt, dt

c
c         Nonlinear iteration.
c
        iquit = 0
        m = 0
        rlast = 1.0d20
c
        do while ((iquit .eq. 0) .and. (m .lt. nonitr))
c
          m = m + 1
c
c         Set nonlinear solver to Newton.
c
          if (m .le. 100) then
            method = 'p'
          else
            method = 'n'
          end if
c
          !print *, ' m = ', m, ', method = ', method
          do n = 2, nmax - 1
c
            z = dble (n - 1) * dz
            zz(1) = z - dz * 0.5d0
            zz(2) = zz(1) + dz
c
            node(1) = n - 1
            node(2) = n + 1
c
            sum1 = 0.0d0
            sum2 = 0.0d0
            sum3 = 0.0d0
c
            do k = 1, 2
              kk = k * 2 - 1
              nn = node(k)
              ph = (thead(nn) + thead(n)) * 0.5d0 - zz(k)
              tt1 = fkr (ph)
              sum1 = sum1 + tt1
              sum3 = sum3 + tt1 * (thead(nn) - thead(n))
              if (method .eq. 'p') then
                aa(kk, n) = - tt1
              else if (method .eq. 'n') then
                tt2 = dfkr (ph) * 0.5d0 * (thead(n) - thead(nn))
                sum2 = sum2 + tt2
                aa(kk, n) = - tt1 + tt2
              end if
            end do
c
            tt3 = dexp (alpha * (thead(n) - z)) * dz * dz *
     &        (ths - thr) * alpha / (fks * dt)
            if (method .eq. 'p') then
              aa(2, n) = sum1 + tt3
            else if (method .eq. 'n') then
              aa(2, n) = sum1 + sum2 + tt3
            end if
c
            b(n) = sum3 + tt3 * (hlast(n) - thead(n))
c
          end do
        ! open ( newunit = myIO, file = 'step 2.txt', iostat = io_status, iomsg = io_msg )
        ! write ( myIO, '( "tt1, tt2, tt3, ph, sum1, sum3 = ", 6( g0, 2X ) )' ) tt1, tt2, tt3, ph, sum1, sum3
        ! do i = 1, nmax
        !     write ( myIO, '( I4, ". ", g0, ", ", g0, ", ", g0, ", ", g0 )' ) i, b ( i ), aa ( 1, i ), aa ( 2, i ), aa ( 3, i )
        ! end do
        ! close ( myIO )
        !
        ! stop

c
c         q term.
c
          ph = (thead(nmax) + thead(nmax - 1)) * 0.5d0 - fl +
     &      dz * 0.5d0
          tt1 = fkr (ph)
          aa(1, nmax) = - tt1
          aa(2, nmax) = tt1
          b(nmax) = qq * dz / fks + tt1 * dz + (thead(nmax - 1)
     &      - thead(nmax)) * tt1
c
c         Compute a new delta head.
c
          bsave = b
          aa_save = aa
          call tri_diag

                open ( newunit = myIO, file = 'step 3.txt', iostat = io_status, iomsg = io_msg )
                write ( myIO, '( /, "input system tri_diag = " )' )
                do i = 1, 3
                    write ( myIO, '( i4, ". ", 30( g0, 2X ) )' ) i, aa ( i , : )
                end do
                write ( myIO, '( /, "solution vector b  = " )' )
                do i = 1, nmax
                    write ( myIO, '( I4, ". ", g0 )' ) i, b ( i )
                end do
                close ( myIO )

                stop

c
c         Compute maximum residual from linear solver.
c
          rmax = - 1.0d20
          do n = 2, nmax - 1
            if (n .eq. 2) then
              rr = bsave(n) - aa_save(2, n) * b(n) - aa_save(3, n)
     &          * b(n + 1)
            else if (n .eq. nmax - 1) then
              rr = bsave(n) - aa_save(1, n) * b(n - 1) - aa_save(2, n)
     &          * b(n)
            else
              rr = bsave(n) - aa_save(1, n) * b(n - 1) - aa_save(2, n)
     &          * b(n) - aa_save(3, n) * b(n + 1)
            end if
            rmax = dmax1 (dabs (rr), rmax)
          end do
    !       print*, 'thead(nmax - 1)', thead(nmax - 1)
          write ( io_debug, '( /, "Max linear solver residual = ", g0 )' ) rmax
          write ( io_debug, '( "solution vector from tri_diag = ", 30( g0, 2X ) )' ) b
          write ( io_debug, 110 ) aa_save(2, nmax - 1), aa_save(3, nmax - 1),
     &      bsave(nmax - 1)
          write ( io_debug, '( "thead( nmax - 1 ) = ", g0 )' ) thead(nmax - 1)
 110  format ( 'aa_save(2, nmax - 1), aa_save(3, nmax - 1), bsave(nmax - 1) = ', g0, ", ", g0, ", ", g0 )
c
c         Compute maximum difference in the nonlinear solution and
c         update head.
c
          rmax = - 1.0d20
          do n = 1, nmax
            rmax = dmax1 (dabs (b(n)), rmax)
            thead(n) = omnon * b(n) + thead(n)
          end do
          write ( io_debug, '( "thead = ", 30( g0, 2X ) )' ) thead

c
        !   print*, 'method, m, rmax, omnon ', method, m, rmax, omnon
          write ( io_debug, 120 ) method, m, rmax, omnon
 120      format ( 'method, m, rmax, omnon = ', 4( g0, 2X ) )
          flush ( io_debug )
c
c         Adjust omnon.
c
          if (rmax .le. rlast) then
            omnon = omnon + omadd
            if (omnon .gt. ommax) omnon = ommax
          else
            omnon = omnon * omred
            if (omnon .lt. ommin) omnon = ommin
          end if
c
c         Check for convergence.
c
          if (rmax .le. epsnon) iquit = 1
c
          rlast = rmax
c
        end do
c
        do n = 1, nmax
          hlast(n) = thead(n)
        end do
c
        if ( ( iquit .eq. 0 ) .and. ( m .lt. nonitr ) ) then
            print *, 'rmax = ', rmax, ', iquit = ', iquit, ', m = ', m
        end if
      end do

      close ( io_debug )
c
c         Compute the analytic solution.
c
      call analytic (time, theada)

      open ( newunit = myIO, file = 'head pressures.txt',
     & iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
          print *, 'iostat = ', io_status
          print *, 'iomsg = ', trim ( io_msg )
      end if
      do n = 1, nmax
        write ( myIO, '(i5, 3f10.3, 2X, E9.2)') n, dble (n - 1) * dz, thead(n),
     ^    theada(n), thead ( n ) - theada ( n )
      end do
      flush ( myIO )
      close ( myIO )
      write ( * , * ) "data written to 'head pressures.txt'"
c
c         Compute the worst h.
c
      worst = 0.0d0
      do n = 1, nmax - 1
        err = thead(n) - theada(n)
        abs_err = dabs (err)
        if (worst .lt. abs_err) then
          worst = abs_err
          worst_error = err
          worst_at    = n
        end if
      end do
      print*, 'worst error =', worst_error, ' at step ', worst_at, ', z = ', dz * ( worst_at - 1 )
c
      call cpu_time ( cpu_stop ) ! global cpu time
      write ( *, '( /, "cpu time used = ", g0, " seconds", / )' )
     &cpu_stop - cpu_start

      end
c
      function fkr (ph)
c
c
c         This subroutine computes the relative hydraulic conductivity.
c
c
      implicit real * 8 (a-h, o-z)
c
      common / params / alpha, hr, a, fl, dz, fks, thr, ths,
     &  qq
c
      if (ph .ge. 0.0d0) then
c
        fkr = 1.0d0
c
      else
c
        fkr = dexp (alpha * ph)
c
      end if
c
      return
      end
c
      function dfkr (ph)
c
c
c         This subroutine computes the slope of the relative hydraulic
c         conductivity curve.
c
c
      implicit real * 8 (a-h, o-z)
c
      common / params / alpha, hr, a, fl, dz, fks, thr, ths,
     &  qq
c
      if (ph .ge. 0.0d0) then
c
        dfkr = 0.0d0
c
      else
c
        dfkr = alpha * dexp (alpha * ph)
c
      end if
c
      return
      end
c
      subroutine analytic (time, theada)
c
c
c         This subroutine computes the analytic solution.
c
c
      implicit real * 8 (a-h, o-z)
c
      parameter (nmax = 11)
c
      common / params / alpha, hr, a, fl, dz, fks, thr, ths,
     &  qq
c
      dimension theada(nmax)
c
      pi = datan2 (1.0d0, 0.0d0) * 2.0d0
c
      t1 = dexp (alpha * hr)
      c = (ths - thr) * alpha / fks
c
      do n = 2, nmax - 1
c
        z = dble (n - 1) * dz
        t3 = dexp (alpha * 0.5d0 * (fl * 3.0d0 - z))
        t4 = dexp (- alpha * (fl - z) * 0.5d0) - dexp (- alpha *
     &    (fl + z) * 0.5d0)
        t5 = 1.0d0 - dexp (- alpha * fl)
        t6 = qq / fks
c
c         Compute the time-dependent term.
c
        sum = 0.0d0
        flip = - 1.0d0
        kquit = 0
        k = 0
        do while (kquit .eq. 0)
          k = k + 1
          flamk = pi * dble (k) / fl
          gamma = (alpha * alpha * 0.25d0 + flamk * flamk) / c
          term = flamk / gamma * dexp (- gamma * time)
          if (term .lt. 1.0d-30) kquit = 1
          sum = sum + term * flip * dsin (flamk * z)
          flip = - flip
        end do
c
        theada(n) = dlog (t1 + t6 * (t4 +
     &    t5 * sum * 2.0d0 / (fl * c))) / alpha + z
c
      end do
c
      theada(1) = hr
c
      return
      end
c
      subroutine tri_diag
c
c
c         This subroutine solves a tri-diagonal system of equations,
c
c         aa * x = b
c
c         with unknowns starting at 2 and ending at nmax - 1.
c
c
      implicit real * 8 (a-h, o-z)
c
      parameter (nmax = 11)
      common / eqs / aa(3, nmax), b(nmax)
c
      b(1) = 0.0d0
c
      do i = 3, nmax
        r = aa(1, i) / aa(2, i - 1)
        aa(2, i) = aa(2, i) - r * aa(3, i - 1)
        b(i) = b(i) - r * b(i - 1)
      end do
c
      b(nmax) = b(nmax) / aa(2, nmax)
      do i = nmax - 1, 2, -1
        b(i) = (b(i) - aa(3, i) * b(i + 1)) / aa(2, i)
      end do
c
      return
      end

!  17:42 dan-topas-pro-2 rditldmt $ date
! Tue Feb 16 17:42:13 CST 2016
!  17:42 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/projects/groundwater/flow/archive
!  17:42 dan-topas-pro-2 rditldmt $ gfortran -Wall -Wextra -pedantic -fcheck=bounds -fmax-errors=5 -ffixed-line-length-132 fiducial.f
! fiducial.f:7:23:
!
!        implicit real * 8 (a-h, o-z)
!                        1
! Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
! fiducial.f:290:23:
!
!        implicit real * 8 (a-h, o-z)
!                        1
! Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
! fiducial.f:315:23:
!
!        implicit real * 8 (a-h, o-z)
!                        1
! Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
! fiducial.f:339:23:
!
!        implicit real * 8 (a-h, o-z)
!                        1
! Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
! fiducial.f:398:23:
!
!        implicit real * 8 (a-h, o-z)
!                        1
! Warning: GNU Extension: Nonstandard type declaration REAL*8 at (1)
!  17:42 dan-topas-pro-2 rditldmt $ aa
!  data written to 'head pressures.txt'
!  worst error =  -33.901682987697207
!
! cpu time used = .28651499999999996 seconds
!
