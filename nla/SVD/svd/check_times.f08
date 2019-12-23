! http://www.nag.com/lapack-ex/examples/source/dgesvd-ex.f
! assignment at declaration precludes
! Note: The following floating-point exceptions are signalling: IEEE_DENORMAL

!include '../../sharedModules/mod precision definitions.f08'
include 'mod precision definitions.f08'
include 'myModules/mod svd parameters.f08'
include 'myModules/mod read matrix file.f08'
include 'myModules/mod matrix writer.f08'

program check_times

    !use iso_fortran_env
    use mPrecisionDefinitions, only : rp, zero
    use mSVDparameters
    use mReadMatrix
    use mMatrixWriter

    implicit none

    ! parameters
    character ( len = * ), parameter :: myProgram = "check_times", myFile = "dgesvd-ex.d"

    ! rank 2
    ! real ( rp )                      :: DUMMY ( 1, 1 ) = zero
    real ( rp )                      :: U  ( LDU,  MMAX ) = zero
    real ( rp )                      :: VT ( LDVT, NMAX ) = zero
    ! rank 1
    real ( rp )                      :: S ( NMAX ) = zero, WORK ( LWORK )  = zero
    ! rank 0
    real ( rp )                      :: cpu_0 = zero, cpu_1 = zero

    integer                          :: INFO = 0, LWKOPT = 0, k = 0
    integer ( ip )                   :: j = 0, jmax = 10000000

        call read_matrix_file ( myFile )
        call print_matrix ( A, 'E20.13', 3, dims = [ 6, 4 ] )

        ! Compute the singular values and left and right singular vectors of A (A = U S (V'), m .ge. n )
        !call DGESVD ( 'Overwrite A by U', 'Singular vectors ( V )', M, N, A, LDA, S, DUMMY, 1, VT, LDVT, WORK, LWORK, INFO )
        call DGESVD ( 'A', 'Singular vectors ( V )', M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )

        LWKOPT = floor( WORK ( 1 ) )
        !write ( io_write, 200 ) LWKOPT, LWORK

        write ( * , 300 ) info
        if ( info > 0 ) then
            write ( unit = *, fmt = '( "Error in slot ", g0, " in call to sgesvd." )', iostat = io_status ) info
        else if ( info < 0 ) then
            write ( unit = *, fmt = '( "Number of superdiagonals which did not converge = ", g0,  "." )', iostat = io_status ) -info
        end if

        ! assemble the pseudoinverse I
        call cpu_time ( cpu_0 )
        do j = 1, jmax
            do row = 1, rank
                Sp ( row, row ) = 1 / S ( row )
            end do
            myU = U ( 1 : m, 1 : m )
            myV = transpose ( Vt ( 1 : n, 1 : n ) )
            Ap = matmul ( myV, matmul ( Sp, transpose ( myU ) ) )
        end do
        call cpu_time ( cpu_1 )
        write ( * , 100 ) 'matrix manipulation', jmax, cpu_1 - cpu_0
        call print_matrix ( Ap, 'E24.17', 1, moniker = 'pseudoinverse Ap' )

        ! assemble the pseudoinverse II
        call cpu_time ( cpu_0 )
        do j = 1, jmax
            App = zero
            do row = 1, n
                do col = 1, m
                    do k = 1, rank
                        App ( row, col ) = App ( row, col ) + VT ( k, row ) * U ( col, k ) / S ( k )
                    end do
                end do
            end do
        end do
        call cpu_time ( cpu_1 )
        write ( * , 100 ) 'do loops', jmax, cpu_1 - cpu_0
        call print_matrix ( App, 'E20.13', 3, moniker = 'pseudoinverse App' )

        id = matmul ( Ap, A ( 1 : m, 1 : n ) )
        call print_matrix ( id, 'F7.3', 3, moniker = 'identity' )

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  100   format ( /, 'Method: ', g0, /, 'Time for ', g0, ' iterations = ', g0 )

  300   format ( ' sgesvd info = ', g0, ': no errors' )

end program check_times

! Muntz-Szasz:svd dantopa$ date
! Sat Jan 23 18:42:29 CST 2016
! Muntz-Szasz:svd dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/nla/svd
! Muntz-Szasz:svd dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate check_times.f08
! Muntz-Szasz:svd dantopa$ ./a.out
!  Attempting to open dgesvd-ex.d
!
! Target matrix has 6 rows and 4 columns.
!  0.2270000000000E+01   -0.1540000000000E+01    0.1150000000000E+01   -0.1940000000000E+01
!  0.2800000000000E+00   -0.1670000000000E+01    0.9400000000000E+00   -0.7800000000000E+00
! -0.4800000000000E+00   -0.3090000000000E+01    0.9900000000000E+00   -0.2100000000000E+00
!  0.1070000000000E+01    0.1220000000000E+01    0.7900000000000E+00    0.6300000000000E+00
! -0.2350000000000E+01    0.2930000000000E+01   -0.1450000000000E+01    0.2300000000000E+01
!  0.6200000000000E+00   -0.7390000000000E+01    0.1030000000000E+01   -0.2570000000000E+01
!  sgesvd info = 0: no errors
!
! Method: matrix manipulation
! Time for 10000000 iterations = 3.4405809999999999
!
! Target matrix pseudoinverse Ap has 4 rows and 6 columns.
! -0.13412781004176838E-01 -0.78242619353861131E+00 -0.28048708073518797E+00  0.69005677566325574E+00 -0.57898976288381782E-01  0.38785355496777540E+00
!  0.11510840334211880E+00  0.33388491041180207E+00  0.15068106214832511E-02 -0.26928052460342777E+00 -0.51122815707764654E-01 -0.30011134998577643E+00
!  0.11381440485695521E+00  0.87573028565356603E+00  0.54297994590480858E+00 -0.12365389596669285E+00 -0.85638424788358244E-01 -0.50302166861654685E+00
! -0.28244900292470265E+00 -0.72912863064408240E+00  0.83843843395431911E-01  0.92828238344630154E+00  0.15190216913706006E+00  0.40204555682866727E+00
!
! Method: do loops
! Time for 10000000 iterations = 7.6544910000000002
!
! Target matrix pseudoinverse App has 4 rows and 6 columns.
! -0.1341278100418E-01   -0.7824261935386E+00   -0.2804870807352E+00    0.6900567756633E+00   -0.5789897628838E-01    0.3878535549678E+00
!  0.1151084033421E+00    0.3338849104118E+00    0.1506810621483E-02   -0.2692805246034E+00   -0.5112281570776E-01   -0.3001113499858E+00
!  0.1138144048570E+00    0.8757302856536E+00    0.5429799459048E+00   -0.1236538959667E+00   -0.8563842478836E-01   -0.5030216686165E+00
! -0.2824490029247E+00   -0.7291286306441E+00    0.8384384339543E-01    0.9282823834463E+00    0.1519021691371E+00    0.4020455568287E+00
!
! Target matrix identity has 4 rows and 4 columns.
!  -0.019    -0.218     0.003    -1.128
!   0.088    -0.107    -0.055     0.517
!  -0.021    -0.081     0.577     1.005
!   0.038     0.091     0.455    -1.203
! STOP successful completion for check_times.
