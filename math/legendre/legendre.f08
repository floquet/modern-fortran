! http://ww2.odu.edu/~agodunov/computing/programs/f90/Legendre.f90

program Legendre
!=======================================
! Fortran 90 demo program for students
! calculates Legendre polynomials Pn(x)
! using the recurrence relation
! written by: Alex Godunov (2008)
!=======================================
    implicit none
    double precision x, xmin, xmax, dx
    double precision pl, plg
    integer n

        xmin = -1.0          ! left point
        dx   =  0.1          ! step
        xmax =  1.0          ! right point

        n = 10                ! order of Pn(x)

        x = xmin

        !open (unit=7,file="Legendre.dat")
        write(*,100) n

        do while (x < xmax+0.01)
            plg = pl(x,n)
            write(*,101) x, plg
            x = x + dx
        end do

  100   format(' Legendre polynomials for n=',i3,/,&
               '        x       Pn(x)')
  101     format(f12.2,2f10.4)

    stop
end

    function pl(x,n)
!======================================
! calculates Legendre polynomials Pn(x)
! using the recurrence relation
! if n > 100 the function retuns 0.0
!======================================
        double precision pl
        double precision x
        double precision pln(0:n)
        integer n, k

            pln(0) = 1.0
            pln(1) = x

            if (n <= 1) then
                pl = pln(n)
            else
                do k=1,n-1
                    pln(k+1) = ((2.0*k+1.0)*x*pln(k) - float(k)*pln(k-1))/(float(k+1))
                end do
                pl = pln(n)
            end if
        return
    end

! dan-topas-pro-2:legendre functions rditldmt$ date
! Wed Jan 20 16:21:07 CST 2016
! dan-topas-pro-2:legendre functions rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/orthogonality/legendre functions
! dan-topas-pro-2:legendre functions rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fintrinsic-modules-path=/myModules legendre.f08
! dan-topas-pro-2:legendre functions rditldmt$ ./a.out
!  Legendre polynomials for n= 10
!         x       Pn(x)
!        -1.00    1.0000
!        -0.90   -0.2631
!        -0.80    0.3005
!        -0.70    0.0858
!        -0.60   -0.2437
!        -0.50   -0.1882
!        -0.40    0.0968
!        -0.30    0.2515
!        -0.20    0.1291
!        -0.10   -0.1221
!         0.00   -0.2461
!         0.10   -0.1221
!         0.20    0.1291
!         0.30    0.2515
!         0.40    0.0968
!         0.50   -0.1882
!         0.60   -0.2437
!         0.70    0.0858
!         0.80    0.3005
!         0.90   -0.2631
!         1.00    1.0000
