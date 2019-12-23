! http://www.nag.com/lapack-ex/examples/source/dgesvd-ex.f
! assignment at declaration precludes
! Note: The following floating-point exceptions are signalling: IEEE_DENORMAL

!include '../../sharedModules/mod precision definitions.f08'
include 'mod precision definitions.f08'
include 'myModules/mod svd parameters.f08'
include 'myModules/mod get data.f08'
include 'myModules/mod matrix writer.f08'

program nag_svd

    !use iso_fortran_env
    use mPrecisionDefinitions, only : rp, one, zero
    use mGetData
    use mMatrixWriter

    implicit none

    ! parameters
    real ( rp ), parameter           :: s_mm ( 1 : 4 ) = [ 9.996627661356914,  3.683101373968635, &
                                                           1.3569287262747172, 0.5000440991298909 ]
    character ( len = * ), parameter :: myProgram = "nag_DGESVD", myFile = "dgesvd-ex.d"

    ! rank 2
    ! real ( rp )                      :: DUMMY ( 1, 1 ) = zero
    real ( rp )                      :: U  ( LDU,  MMAX ) = zero
    real ( rp )                      :: VT ( LDVT, NMAX ) = zero
    ! rank 1
    real ( rp )                      :: RCONDU ( NMAX ) = zero, RCONDV ( NMAX ) = zero, S ( NMAX ) = zero, UERRBD ( NMAX ) = zero, &
                                        VERRBD ( NMAX ) = zero, WORK ( LWORK )  = zero
    ! rank 0
    real ( rp )                      :: EPS = zero, SERRBD = zero

    integer                          :: INFO = 0, LWKOPT = 0, k = 0

        call read_myfile ( myFile )
        !call print_matrix ( A, 'F7.3', 3, dims = [ 6, 4 ] )

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

        ! write ( io_write, * )
        ! write ( io_write, * ) 'DGESVD Example Program Results'
        ! write ( io_write, * )
        ! write ( io_write, * ) 'Singular values, computed:'
        ! write ( io_write, * ) 'S = ', S
        ! write ( io_write, * ) 'Singular values, Mathematica:'
        ! write ( io_write, * ) 'S = ', s_mm
        ! write ( io_write, * ) 'Singular values, differenced:'
        ! write ( io_write, * ) 'S = ', ( ( s ( row ) - s_mm ( row ) ), row = 1, 4 )
        ! write ( io_write, * )
        ! call print_matrix ( U, 'F7.3', 3, moniker = 'codomain U', dims = [ 6, 6 ] )
        ! call print_matrix ( transpose ( VT ), 'F7.3', 3, moniker = 'domain V', dims = [ 4, 4 ] )

        EPS = epsilon ( one )
        SERRBD = EPS * S ( 1 )

        !Call DDISNA to estimate reciprocal condition numbers for the singular vectors
        call DDISNA ( 'Left',  M, N, S, RCONDU, INFO )
        call DDISNA ( 'Right', M, N, S, RCONDV, INFO )

        ! Compute the error estimates for the singular vectors
        UERRBD = SERRBD / RCONDU
        VERRBD = SERRBD / RCONDV

        ! write ( io_write, * ) 'Error estimate for the singular values:'
        ! write ( io_write, 310 ) SERRBD
        ! write ( io_write, * )
        ! write ( io_write, * ) 'Error estimates for the left singular vectors:'
        ! write ( io_write, 320 ) ( UERRBD ( row ), row = 1, N )
        ! write ( io_write, * )
        ! write ( io_write, * ) 'Error estimates for the right singular vectors:'
        ! write ( io_write, 320 ) ( VERRBD ( row ), row = 1, N )

        ! assemble the pseudoinverse I
        do row = 1, rank
          Sp ( row, row ) = 1 / S ( row )
        end do
        myU = U ( 1 : m, 1 : m )
        myV = transpose ( Vt ( 1 : n, 1 : n ) )
        X = matmul ( Sp, transpose ( myU ) )
        !write ( * , * ) 'dimensions myU   = ', shape ( myU )
        !write ( * , * ) 'dimensions Sp    = ', shape ( Sp )
        !write ( * , * ) 'dimensions myV   = ', shape ( myV )
        !write ( * , * ) 'dimensions Sp.Ut = ', shape ( matmul ( Sp, transpose ( myU ) ) )
        !write ( * , * ) 'Sp . Ut = ', matmul ( Sp, transpose ( myU ) )
        write ( * , * ) 'dimensions myV     = ', shape ( myV )
        write ( * , * ) 'dimensions X       = ', shape ( X )
        write ( * , * ) 'dimensions myV . X = ', shape ( matmul ( myV, X ) )
        write ( * , * ) 'dimensions id      = ', shape ( id )
        Ap = matmul ( myV, matmul ( Sp, transpose ( myU ) ) )
        !id = matmul ( myV, X )
        call print_matrix ( Ap, 'F7.3', 3, moniker = 'identity via matmul' )

        ! assemble the pseudoinverse II
        do row = 1, n
            do col = 1, m
                do k = 1, rank
                    Ap ( row, col ) = Ap ( row, col ) + VT ( k, row ) * U ( col, k ) / S ( k )
                end do
            end do
        end do
        !call print_matrix ( Ap, 'F7.3', 3, moniker = 'pseudoinverse Ap' )

        id = matmul ( Ap, A ( 1 : m, 1 : n ) )
        call print_matrix ( id, 'F7.3', 3, moniker = 'identity' )

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  200   format ( ' Optimum workspace required = ', g0, /, &
                 ' Workspace provided         = ', g0 )

  300   format ( ' sgesvd info = ', g0 )
  310   format ( G10.3 )
  320   format ( 4( G10.3, 2X ) )

end program nag_svd

! dan-topas-pro-2:svd rditldmt$ date
! Fri Jan 22 17:47:18 CST 2016
! dan-topas-pro-2:svd rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/nla/svd
! dan-topas-pro-2:svd rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate nag_svd.f08
! dan-topas-pro-2:svd rditldmt$ ./a.out
!  Attempting to open dgesvd-ex.d
!  Attempting to read dgesvd-ex.d
!
! Target matrix has 6 rows and 4 columns.
!   2.270    -1.540     1.150    -1.940
!   0.280    -1.670     0.940    -0.780
!  -0.480    -3.090     0.990    -0.210
!   1.070     1.220     0.790     0.630
!  -2.350     2.930    -1.450     2.300
!   0.620    -7.390     1.030    -2.570
!  Optimum workspace required = 284
!  Workspace provided         = 1194
!  sgesvd info = 0
!
!  DGESVD Example Program Results
!
!  Singular values, computed:
!  S =    9.9966276613569143        3.6831013739686362        1.3569287262747163       0.50004409912989134        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000
!  Singular values, Mathematica:
!  S =    9.9966278076171875        3.6831014156341553        1.3569287061691284       0.50004410743713379
!  Singular values, differenced:
!  S =   -1.4626027322606205E-007  -4.1665519034950194E-008   2.0105587861252161E-008  -8.3072424494901043E-009
!
!
! Target matrix codomain U has 6 rows and 6 columns.
!  -0.277    -0.600    -0.128     0.132     0.636     0.352
!  -0.202    -0.030     0.281     0.703     0.129    -0.607
!  -0.292     0.335     0.645     0.191    -0.048     0.590
!   0.094    -0.370     0.678    -0.540     0.108    -0.302
!   0.421     0.527     0.041    -0.058     0.733    -0.058
!  -0.782     0.335    -0.164    -0.396     0.166    -0.256
!
! Target matrix domain V has 4 rows and 4 columns.
!  -0.192    -0.803     0.004    -0.564
!   0.879    -0.393    -0.075     0.259
!  -0.214    -0.298     0.783     0.503
!   0.379     0.335     0.618    -0.602
! STOP successful completion for nag_DGESVD.
