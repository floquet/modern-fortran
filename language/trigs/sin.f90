!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

include 'precision definitions.f90'

program trigs

!    use precision_definitions, only : is, wp, zero
    use precision_definitions

    implicit NONE

    real ( qp ), parameter :: x      = 2646693125139304345_qp
    real ( qp ), parameter :: twopi  = 6.283185307179586476925286766559005768394_qp
    real ( qp ), parameter :: mm     = 1.188488579586842482197671275324864157187e-20_qp

    real ( qp ), parameter :: nPi    = x / twopi
    real ( qp ), parameter :: rem    = modulo( x, nPi )
    real ( qp ), parameter :: qem    = fraction( x / twopi )
    real ( qp ), parameter :: w      = rem * twopi

    !integer ( zint )       :: flr = floor( nPi, qp )

    character ( len = * ), parameter :: me   = 'program trigs' ! Metcalf, Reid, Cohen: p. 309

        write ( *, '( "x     = ", g0 )' ) x
        write ( *, '( "twopi = ", g0 )' ) twopi
        write ( *, '( "mm    = ", g0 )' ) mm
        write ( *, '( "rem   = ", g0 )' ) rem
        write ( *, '( "qem   = ", g0 )' ) qem
        write ( *, '( "modulo( x, nPi )   = ", g0 )' ) modulo( x, nPi )
        write ( *, '( "fraction( x / twopi )   = ", g0 )' ) fraction( x / twopi )
        !write ( *, '( "flr   = ", g0 )' ) flr

        write ( *, '( /, "sin ( ", g0, " ) = ", g0, ", expected = ", g0, / )' ) x, sin( x ), mm
        write ( *, '( "nPi = ", g0, ", remainder = ", g0, ", expected = ", g0, / )' ) nPi, rem, 0.5
        write ( *, '( "reduced argument: sin ( ", g0, " ) = ", g0, / )' ) w, sin( w )

        stop "successful completion for " // me  ! must reduce to constant expression

end program trigs

! dan-topas-pro-2:trigs rditldmt$ date
! Mon Jan 11 14:42:13 CST 2016
! dan-topas-pro-2:trigs rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/trigs
! dan-topas-pro-2:trigs rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 sin.f90
! dan-topas-pro-2:trigs rditldmt$ ./a.out
! x     = .26466931251393044E+019
! twopi = 6.2831853071795862
! mm    = .11884885795868425E-019
! rem   = .11928736285976461E+018
! qem   = .73072501819089586
! modulo( x, nPi )   = .11928736285976461E+018
! fraction( x / twopi )   = .73072501819089586
!
! sin ( .26466931251393044E+019 ) = -.62298863144234884, expected = .11884885795868425E-019
!
! nPi = .42123429371325664E+018, remainder = .11928736285976461E+018, expected = .500000000
!
! reduced argument: sin ( .74950460565267290E+018 ) = -.26507210040521306
!
! STOP successful completion for program trigs
