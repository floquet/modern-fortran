program tridiagonal

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero, one
    use mParameters,            only : nmax
    use mTriDiagonalSolvers,    only : Fred, notFred, Dan, tdma

    implicit NONE

    ! rank 2
    real ( rp ) :: aa ( 1 : 3, 1 : nmax ) = zero

    ! rank 1
    real ( rp ) :: bF ( 1 : nmax ) = one, bNF ( 1 : nmax ) = one, diff  ( 1 : nmax ) = zero
    real ( rp ) :: a ( nmax ), b ( nmax ), c ( nmax ), d ( nmax ), x ( nmax ), z ( nmax )

    integer ( ip ) :: k = 0

        aa ( 1, : ) = -one
        aa ( 2, : ) =  one
        aa ( 3, : ) = 2 * one

        d ( : ) = one

        a ( : ) = aa ( 1 , : )
        b ( : ) = aa ( 2 , : )
        c ( : ) = aa ( 3 , : )

        do k = 1, 3
            write ( * , '( I3, 11 ( 2X, F5.1 ) )' ) k, aa ( k, 1 : nmax )
        end do

        write ( *,  '( "data vector = ", 11 ( F10.5, 2X ) )' ) aa ( 2, : )

        call Fred    ( aa, bF )  ! overwrites b
        aa ( 2, : ) =  one
        call notFred ( aa, bNF )

        diff ( : ) = bF ( : ) - bNF ( : )

        do k = 1, nmax
            write ( * , '( I3, 3X, E10.2, 2 ( 2X, F9.3 ) )' ) k, diff ( k ), bF ( k ), bNF ( k )
        end do

        print *, 'norm diff = ', norm2 ( diff )

        print *, ''
        ! print *, 'a = ', a
        ! print *, 'b = ', b
        ! print *, 'c = ', c
        ! print *, 'd = ', d

        call Dan ( a, b, c, d, x )

        print *, ''
        ! print *, 'a = ', a
        ! print *, 'b = ', b
        ! print *, 'c = ', c
        ! print *, 'd = ', d

        call tdma ( a, b, c, d, z )

        diff ( : ) = x ( : ) - z ( : )

        do k = 1, nmax
            write ( * , '( I3, 3X, E10.2, 4 ( 2X, F9.3 ) )' ) k, diff ( k ), x ( k ), z ( k )
        end do

        print *, 'norm diff = ', norm2 ( diff )

        stop 'program tridiagonal ...'

end program tridiagonal

!  18:20 dan-topas-pro-2 rditldmt $ date
! Tue Feb 23 18:20:17 CST 2016
!  18:20 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/projects/groundwater/flow/debug/compare
!  18:20 dan-topas-pro-2 rditldmt $ make clean
! rm -rf mod_constants.o mod_parameters.o mod_precision_definitions.o mod_tridiagonal_solvers.o tridiagonal.o tridiagonal mod_constants.mod mod_parameters.mod mod_precision_definitions.mod mod_tridiagonal_solvers.mod
! rm -f *.mod
!  18:20 dan-topas-pro-2 rditldmt $ make
! /usr/local/bin/gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! /usr/local/bin/gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
! /usr/local/bin/gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
! /usr/local/bin/gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_tridiagonal_solvers.o mod_tridiagonal_solvers.f08
! /usr/local/bin/gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o tridiagonal.o tridiagonal.f08
! /usr/local/bin/gfortran -g -o tridiagonal mod_constants.o mod_parameters.o mod_precision_definitions.o mod_tridiagonal_solvers.o tridiagonal.o
!  18:20 dan-topas-pro-2 rditldmt $ ./tridiagonal
!   1   -1.0   -1.0   -1.0   -1.0   -1.0   -1.0   -1.0   -1.0   -1.0   -1.0   -1.0
!   2    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0    1.0
!   3    2.0    2.0    2.0    2.0    2.0    2.0    2.0    2.0    2.0    2.0    2.0
! data vector =    1.00000     1.00000     1.00000     1.00000     1.00000     1.00000     1.00000     1.00000     1.00000     1.00000     1.00000
! aa ( 2, : ) =    1.00000     3.00000     1.66667     2.20000     1.90909     2.04762     1.97674     2.01176     1.99415     2.00293     1.99854
! Fred b      =    1.00000     2.00000     1.66667     2.00000     1.90909     2.00000     1.97674     2.00000     1.99415     2.00000     1.99854
!   1     0.00E+00      1.000      1.000
!   2     0.00E+00      0.000      0.000
!   3     0.00E+00      1.000      1.000
!   4     0.00E+00      0.000      0.000
!   5     0.00E+00      1.000      1.000
!   6     0.00E+00      0.000      0.000
!   7     0.00E+00      1.000      1.000
!   8     0.00E+00      0.000      0.000
!   9     0.00E+00      1.000      1.000
!  10     0.00E+00      0.000      0.000
!  11     0.00E+00      1.000      1.000
!  norm diff =    0.0000000000000000
!
!
!   1     0.00E+00      1.000      1.000
!   2     0.00E+00      0.000      0.000
!   3     0.00E+00      1.000      1.000
!   4     0.00E+00      0.000      0.000
!   5     0.00E+00      1.000      1.000
!   6     0.00E+00      0.000      0.000
!   7     0.00E+00      1.000      1.000
!   8     0.00E+00      0.000      0.000
!   9     0.00E+00      1.000      1.000
!  10     0.00E+00      0.000      0.000
!  11     0.00E+00      1.000      1.000
!  norm diff =    0.0000000000000000
! STOP program tridiagonal ...
!  18:20 dan-topas-pro-2 rditldmt $
