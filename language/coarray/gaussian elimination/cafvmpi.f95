!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
!           Co-Array FORTRAN: Fred Tracy
!           Compare MPI and Coarray Fortran
!include 'mpif.h'
module sh

    use iso_fortran_env

    implicit none

    integer ( kind = int64 ), parameter :: nRows = 200, nRowsp1 = nRows + 1, iband = 15
    integer ( kind = int64 ), parameter :: wp = selected_real_kind ( REAL64 )

    real ( wp ) :: zero = 0.0_wp, one = 1.0_wp


end module sh


program cafvmpi

    use sh

    implicit none

    real ( wp ) :: A ( 1 : nRows, 1 : nRows ), b ( nRows ), row ( 2 * nRows )
    !real ( wp ), dimension ( n * 2 ), codimension [ * ] :: buff
    real ( wp ) :: cpu_0, cpu_1

    integer ( kind = int64 ) :: r = 0, k = 0

    character ( len = * ), parameter :: me_program = 'program cafvmpi'  ! self-identification

        call cpu_time ( cpu_0 ) ! cpu time - start

!           build Toepliz matrix LHS
            A ( : , : ) = zero
            do concurrent ( r = 1 : nRows )
                A ( r, r ) = 2 * iband * one
            end do

!           nb: /Users/rditldmt/Dropbox/ nb/fortran/coarrays/test/test cases 01.nb
!           data vector b RHS
            b ( : ) = 2.0_wp
            do concurrent ( k = 1 : iband - 1 )
                b ( k )             = iband - k + 2
                b ( nRows - k + 1 ) = iband - k + 2
            end do

            write ( *, 100 ) b

        call cpu_time ( cpu_1 ) ! cpu time - end
        write ( *, '( /, "cpu time to BUILD system with ", g0," rows = ", g0, " seconds", / )' ) nRows, cpu_1 - cpu_0

        call cpu_time ( cpu_0 ) ! cpu time - start

        do k = 2, nRows
        end do

        call cpu_time ( cpu_1 ) ! cpu time - end
        write ( *, '( /, "cpu time to SOLVE system with ", g0," rows = ", g0, " seconds", / )' ) nRows, cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

  100   format ( 'b = ', /, 200(2X, g0) )

end program cafvmpi

! dan-topas-pro-2:gaussian elimination rditldmt$ date
! Tue Nov 24 10:37:13 CST 2015
! dan-topas-pro-2:gaussian elimination rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/gaussian elimination
! dan-topas-pro-2:gaussian elimination rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 cafvmpi.f95
! cafvmpi.f95:26:77:
!
!      real ( wp ) :: A ( 1 : nRows, 1 : nRows ), b ( nRows ), row ( 2 * nRows )
!                                                                              1
! Warning: Unused variable ‘row’ declared at (1) [-Wunused-variable]
! dan-topas-pro-2:gaussian elimination rditldmt$ ./a.out
! b =
!   16.000000000000000  15.000000000000000  14.000000000000000  13.000000000000000  12.000000000000000  11.000000000000000  10.000000000000000  9.0000000000000000  8.0000000000000000  7.0000000000000000  6.0000000000000000  5.0000000000000000  4.0000000000000000  3.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  2.0000000000000000  3.0000000000000000  4.0000000000000000  5.0000000000000000  6.0000000000000000  7.0000000000000000  8.0000000000000000  9.0000000000000000  10.000000000000000  11.000000000000000  12.000000000000000  13.000000000000000  14.000000000000000  15.000000000000000  16.000000000000000
!
! cpu time to BUILD system with 200 rows = .40400000000000028E-003 seconds
!
!
! cpu time to SOLVE system with 200 rows = .19999999999998318E-005 seconds
!
! STOP successful completion for program cafvmpi.
! dan-topas-pro-2:gaussian elimination rditldmt$
