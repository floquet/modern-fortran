! http://www.nag.com/lapack-ex/examples/source/dgesvd-ex.f

! include '../../sharedModules/mod precision definitions.f08'
! !include 'mod precision definitions.f08'
! include 'myModules/mod svd parameters.f08'
! include 'myModules/mod read matrix file.f08'  ! looking for dgesvd-ex.d
! include 'myModules/mod matrix writer.f08'

program solver_svd

    !use iso_fortran_env
    use, intrinsic :: iso_fortran_env,  only : stdout => output_unit
    use mPrecisionDefinitions,          only : rp, one, zero
    use mSVDparameters
    use mReadMatrix
    use mMatrixWriter

    implicit none

    ! parameters
    real ( rp ),            parameter   :: s_mm ( 1 : 4 ) = [ 9.996627661356914,  3.683101373968635, &
                                                              1.3569287262747172, 0.5000440991298909 ]
    character ( len = * ),  parameter   :: myProgram = "solver_svd", myFile = "dgesvd-ex.d"

    ! rank 2
    real ( rp ) :: U  ( LDU,  MMAX ) = zero
    real ( rp ) :: VT ( LDVT, NMAX ) = zero
    ! rank 1
    real ( rp ) :: RCONDU ( NMAX ) = zero, RCONDV ( NMAX ) = zero, S ( NMAX ) = zero, UERRBD ( NMAX ) = zero, &
                   VERRBD ( NMAX ) = zero, WORK ( LWORK )  = zero
    ! rank 0
    real ( rp )                      :: EPS = zero, SERRBD = zero

    integer                          :: INFO = 0, LWKOPT = 0!, k = 0

        call read_matrix_file ( myFile )
        call print_matrix ( A = A, myFormat = 'E20.13', spaces = 3, dims = [ 6, 4 ], my_io_unit = stdout )
        myA = A ( 1 : m, 1 : n )

        ! Compute the singular values and left and right singular vectors of A (A = U S (V'), m >= n )
        call DGESVD ( 'A', 'Singular vectors ( V )', M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )

        LWKOPT = floor( WORK ( 1 ) )
        write ( io_write, 200 ) LWKOPT, LWORK

        write ( * , 300 ) info
        if ( info > 0 ) then
            write ( unit = *, fmt = '( "Error in slot ", g0, " in call to sgesvd." )', iostat = io_status ) info
        else if ( info < 0 ) then
            write ( unit = *, fmt = '( "Number of superdiagonals which did not converge = ", g0,  "." )', iostat = io_status ) -info
        end if

        write ( io_write, * )
        write ( io_write, * ) 'DGESVD Example Program Results'
        write ( io_write, * )
        write ( io_write, * ) 'Singular values, LAPACK:'
        write ( io_write, * ) 'S = ', S
        write ( io_write, * ) 'Singular values, Mathematica:'
        write ( io_write, * ) 'S = ', s_mm
        write ( io_write, * ) 'Singular values, differenced:'
        write ( io_write, * ) 'S = ', ( ( s ( row ) - s_mm ( row ) ), row = 1, 4 )
        write ( io_write, * )
        call print_matrix ( A = U,                myFormat = 'F7.3', spaces = 3, moniker = 'codomain U', dims = [ 6, 6 ], &
                                                my_io_unit = stdout )
        call print_matrix ( A = transpose ( VT ), myFormat = 'F7.3', spaces = 3, moniker = 'domain V',   dims = [ 4, 4 ], &
                                                my_io_unit = stdout )

        EPS = epsilon ( one )
        SERRBD = EPS * S ( 1 )

        !Call DDISNA to estimate reciprocal condition numbers for the singular vectors
        call DDISNA ( 'Left',  M, N, S, RCONDU, INFO )
        call DDISNA ( 'Right', M, N, S, RCONDV, INFO )

        ! Compute the error estimates for the singular vectors
        UERRBD ( 1 : rank ) = SERRBD / RCONDU ( 1 : rank )
        VERRBD ( 1 : rank ) = SERRBD / RCONDV ( 1 : rank )

        write ( io_write, * ) 'Error estimate for the singular values:'
        write ( io_write, 310 ) SERRBD
        write ( io_write, * )
        write ( io_write, * ) 'Error estimates for the left singular vectors:'
        write ( io_write, 320 ) ( UERRBD ( row ), row = 1, rank )
        write ( io_write, * )
        write ( io_write, * ) 'Error estimates for the right singular vectors:'
        write ( io_write, 320 ) ( VERRBD ( row ), row = 1, rank )

        ! assemble the pseudoinverse I
        do row = 1, rank
          Sp ( row, row ) = 1 / S ( row )
        end do
        myU = U ( 1 : m, 1 : m )
        myV = transpose ( Vt ( 1 : n, 1 : n ) )
        Ap = matmul ( myV, matmul ( Sp, transpose ( myU ) ) )

        id = matmul ( Ap, myA )
        call print_matrix ( A = id, myFormat = 'F8.3', spaces = 1, moniker = 'Identity matrix Ap A', my_io_unit = stdout )

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  200   format ( /, ' Optimum workspace required = ', g0, /, &
                    ' Workspace provided         = ', g0 )

  300   format ( ' sgesvd info = ', g0, ': no errors' )
  310   format ( G10.3 )
  320   format ( 4( G10.3, 2X ) )

end program solver_svd

! Muntz-Szasz:svd dantopa$ date
! Sun Jan 24 19:09:10 CST 2016
! Muntz-Szasz:svd dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/nla/svd
! Muntz-Szasz:svd dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate solver_svd.f90
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
!
!  Optimum workspace required = 284
!  Workspace provided         = 1194
!  sgesvd info = 0: no errors
!
!  DGESVD Example Program Results
!
!  Singular values, LAPACK:
!  S =    9.9966276613569178        3.6831013739686371        1.3569287262747156       0.50004409912989145        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000
!  Singular values, Mathematica:
!  S =    9.9966278076171875        3.6831014156341553        1.3569287061691284       0.50004410743713379
!  Singular values, differenced:
!  S =   -1.4626026967334838E-007  -4.1665518146771774E-008   2.0105587195118346E-008  -8.3072423384678018E-009
!
!
! codomain U has 6 rows and 6 columns.
!  -0.277    -0.600    -0.128     0.132     0.636     0.352
!  -0.202    -0.030     0.281     0.703     0.129    -0.607
!  -0.292     0.335     0.645     0.191    -0.048     0.590
!   0.094    -0.370     0.678    -0.540     0.108    -0.302
!   0.421     0.527     0.041    -0.058     0.733    -0.058
!  -0.782     0.335    -0.164    -0.396     0.166    -0.256
!
! domain V has 4 rows and 4 columns.
!  -0.192    -0.803     0.004    -0.564
!   0.879    -0.393    -0.075     0.259
!  -0.214    -0.298     0.783     0.503
!   0.379     0.335     0.618    -0.602
!  Error estimate for the singular values:
!  0.222E-14
!
!  Error estimates for the left singular vectors:
!  0.352E-15   0.954E-15   0.259E-14   0.444E-14
!
!  Error estimates for the right singular vectors:
!  0.352E-15   0.954E-15   0.259E-14   0.259E-14
!
! Identity matrix Ap A has 4 rows and 4 columns.
!    1.000   -0.000    0.000   -0.000
!    0.000    1.000    0.000   -0.000
!    0.000    0.000    1.000    0.000
!   -0.000   -0.000    0.000    1.000
! STOP successful completion for test_dgesvd.
