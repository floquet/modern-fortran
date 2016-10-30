! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 01 18

include '../../sharedModules/mod precision definitions.f08'
!include 'mod precision definitions.f08'

program basic_svd

    use mPrecisionDefinitions, only : ip, rp, one, zero

    implicit none

    integer ( ip ), parameter        :: m = 2, n = 3, rank = 2, lworkmax = 1000

    ! rank 2
    real ( rp )                      :: A ( 1 : m, 1 : n ) = zero
    real ( rp )                      :: U ( 1 : m, 1 : m ) = zero  ! codomain
    real ( rp )                      :: VT ( 1 : n, 1 : n ) = zero  ! domain
    ! rank 1
    real ( rp )                      :: s ( 1 : rank ) = zero, work ( 1 : lworkmax ) = zero
    ! rank 0
    real ( rp )                      :: cpu_0 = zero, cpu_1 = zero

    integer ( ip )                   :: row = 0, col = 0
    integer ( ip )                   :: lda = 0, ldu = 0, ldvt = 0, lwork = 0, info = 0
    integer ( ip )                   :: ios = 0

    character ( len = 1 )            :: jobu = 'A', jobvt = 'A'
                                        ! A:  all columns returned in domain matrix
                                        ! S:  first min( m, n ) columns are returned in the domain matrix
                                        ! O:  first min( m, n ) columns are overwritten in A
                                        ! N:  no columns are computed
    character ( len = * ), parameter :: myProgram = 'program basic_svd'  ! self-identification

        call cpu_time ( cpu_0 )   ! global cpu time - start
            A ( : , 1 ) = [  one, -one ]
            A ( : , 2 ) = [ -one,  one ]
            A ( : , 3 ) = [  one, -one ]

            write ( unit = *, fmt = '( /, "Input matrix A:" )', iostat = ios )
            do row = 1, m
                write ( * , 200 ) [ ( A ( row, col), col = 1, n ) ]
            end do

            lda = n
            ldu = m
            ldvt = n

            ! query
            lwork = -1
            call dgesvd ( jobu, jobvt, m, n, A, lda, s, U, ldu, VT, ldvt, work, lwork, info )
            write ( unit = *, fmt = '( /, "Optimal value for LWORK = ", g0, "." )', iostat = ios ) floor ( work ( 1 ) )
            if ( ios /= 0 ) stop "Write error: work( 1 )."

            ! SVD
            call dgesvd ( jobu, jobvt, m, n, A, lda, s, U, ldu, VT, ldvt, work, lwork, info )

            write ( * , 300 ) info
            if ( info > 0 ) then
                write ( unit = *, fmt = '( "Error in slot ", g0, " in call to DGESVD." )', iostat = ios ) -info
                if ( ios /= 0 ) stop "Write error: info > 0."
            else if ( info < 0 ) then
                write ( unit = *, fmt = '( "Number of superdiagonals which did not converge = ", g0,  "." )', iostat = ios ) info
                if ( ios /= 0 ) stop "Write error: info < 0."
            end if

            write ( unit = *, fmt = '( /, "SVD results:", //, "codomain matrix U:" )', iostat = ios )
            do row = 1, m
                write ( * , 210 ) [ ( U ( row, col), col = 1, m ) ]
            end do

            write ( unit = *, fmt = '( /, "domain matrix V:" )', iostat = ios )
            do row = 1, n
                write ( * , 200 ) [ ( VT ( col, row), col = 1, n ) ]
            end do

            write ( unit = *, fmt = '( /, "singular values:" )', iostat = ios )
            do row = 1, rank
                write ( unit = *, fmt = 220 ) row, s ( row )
            end do

            write ( unit = *, fmt = '( /, "Returned matrix A:" )', iostat = ios )
            do row = 1, m
                write ( * , 200 ) [ ( A ( row, col), col = 1, n ) ]
            end do


        call cpu_time ( cpu_1 )   ! global cpu time - stop
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', A, ' = ', g0, ' seconds', / )

  200   format ( 3( g0, 2X ) )
  210   format ( 2( g0, 2X ) )
  220   format ( 'sigma ', g0, ' = ', g0 )

  300   format ( 'DGESVD info = ', g0 )

end program basic_svd

! dan-topas-pro-2:svd rditldmt$ date
! Wed Jan 20 16:08:21 CST 2016
! dan-topas-pro-2:svd rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/nla/svd
! dan-topas-pro-2:svd rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate basic_svd.f08
! dan-topas-pro-2:svd rditldmt$ ./a.out
!
! Input matrix A:
! 1.0000000000000000  -1.0000000000000000  1.0000000000000000
! -1.0000000000000000  1.0000000000000000  -1.0000000000000000
! Optimal value for LWORK = 138.
! DGESVD info = 0
!
! SVD results:
!
! codomain matrix U:
! .0000000000000000  .0000000000000000
! .0000000000000000  .0000000000000000
!
! domain matrix V:
! .0000000000000000  .0000000000000000
! .0000000000000000
! .0000000000000000  .0000000000000000
! .0000000000000000
! .0000000000000000  .0000000000000000
! .0000000000000000
! sigma 1 = .69532193193987908E-309
! sigma 2 = .69532633607073946E-309
!
! CPU time for all tasks = .18899999999999993E-003 seconds
!
! Note: The following floating-point exceptions are signalling: IEEE_DENORMAL
! STOP successful completion for program basic_svd.
