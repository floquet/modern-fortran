! http://www.nag.com/lapack-ex/examples/source/dgesvd-ex.f
! assignment at declaration precludes
! Note: The following floating-point exceptions are signalling: IEEE_DENORMAL

!include '../../sharedModules/mod precision definitions.f08'
include 'mod precision definitions.f08'
include 'myModules/mod svd parameters.f08'
include 'myModules/mod read matrix file.f08'
include 'myModules/mod matrix writer.f08'

program test_dgesvd

    !use iso_fortran_env
    use mPrecisionDefinitions, only : rp, one, zero
    use mSVDparameters
    use mReadMatrix
    use mMatrixWriter

    implicit none

    ! parameters
    real ( rp ), parameter           :: s_mm ( 1 : 4 ) = [ 9.996627661356914,  3.683101373968635, &
                                                           1.3569287262747172, 0.5000440991298909 ]
    character ( len = * ), parameter :: myProgram = "test_dgesvd", myFile = "dgesvd-ex.d"

    ! rank 2
    ! real ( rp )                      :: DUMMY ( 1, 1 ) = zero
    real ( rp )                      :: U  ( LDU,  MMAX ) = zero
    real ( rp )                      :: VT ( LDVT, NMAX ) = zero
    ! rank 1
    real ( rp )                      :: RCONDU ( NMAX ) = zero, RCONDV ( NMAX ) = zero, S ( NMAX ) = zero, UERRBD ( NMAX ) = zero, &
                                        VERRBD ( NMAX ) = zero, WORK ( LWORK )  = zero
    ! rank 0
    real ( rp )                      :: EPS = zero, SERRBD = zero

    integer                          :: INFO = 0, LWKOPT = 0!, k = 0

        call read_matrix_file ( myFile )
        call print_matrix ( A, 'E20.13', 3, dims = [ 6, 4 ] )
        myA = A ( 1 : m, 1 : n )

        ! Compute the singular values and left and right singular vectors of A (A = U S (V'), m .ge. n )
        !call DGESVD ( 'Overwrite A by U', 'Singular vectors ( V )', M, N, A, LDA, S, DUMMY, 1, VT, LDVT, WORK, LWORK, INFO )
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
        write ( io_write, * ) 'Singular values, computed:'
        write ( io_write, * ) 'S = ', S
        write ( io_write, * ) 'Singular values, Mathematica:'
        write ( io_write, * ) 'S = ', s_mm
        write ( io_write, * ) 'Singular values, differenced:'
        write ( io_write, * ) 'S = ', ( ( s ( row ) - s_mm ( row ) ), row = 1, 4 )
        write ( io_write, * )
        call print_matrix ( U, 'F7.3', 3, moniker = 'codomain U', dims = [ 6, 6 ] )
        call print_matrix ( transpose ( VT ), 'F7.3', 3, moniker = 'domain V', dims = [ 4, 4 ] )

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

        write ( * , * ) 'Multiplying Ap A:'
        call print_matrix ( A ( 1 : m, 1 : n ),  'E24.17', 1, moniker = 'input A' )
        call print_matrix ( Ap, 'E24.17', 1, moniker = 'pseudoinverse Ap' )

        id = matmul ( Ap, myA )
        call print_matrix ( id, 'E24.17', 1, moniker = 'identity' )

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  200   format ( /, ' Optimum workspace required = ', g0, /, &
                    ' Workspace provided         = ', g0 )

  300   format ( ' sgesvd info = ', g0, ': no errors' )
  310   format ( G10.3 )
  320   format ( 4( G10.3, 2X ) )

end program test_dgesvd

! Muntz-Szasz:svd dantopa$ date
! Sat Jan 23 19:27:58 CST 2016
! Muntz-Szasz:svd dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/nla/svd
! Muntz-Szasz:svd dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate test_dgesvd.f08
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
!  Singular values, computed:
!  S =    9.9966276613569178        3.6831013739686371        1.3569287262747156       0.50004409912989145        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000
!  Singular values, Mathematica:
!  S =    9.9966278076171875        3.6831014156341553        1.3569287061691284       0.50004410743713379
!  Singular values, differenced:
!  S =   -1.4626026967334838E-007  -4.1665518146771774E-008   2.0105587195118346E-008  -8.3072423384678018E-009
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
!  Error estimate for the singular values:
!  0.222E-14
!
!  Error estimates for the left singular vectors:
!  0.352E-15   0.954E-15   0.259E-14   0.444E-14
!
!  Error estimates for the right singular vectors:
!  0.352E-15   0.954E-15   0.259E-14   0.259E-14
!  Multiplying Ap A:
!
! Target matrix input A has 6 rows and 4 columns.
! -0.27735458428469750E+00 -0.60032802225360848E+00 -0.12771001476702332E+00  0.13226688789949517E+00
! -0.20201880126640046E+00 -0.30083977578151913E-01  0.28051390895151995E+00  0.70344507315846438E+00
! -0.29175908666436295E+00  0.33477621193821283E+00  0.64528945061935672E+00  0.19058788380606145E+00
!  0.93759693326717131E-01 -0.36991402685393293E+00  0.67812221677720286E+00 -0.53990075519315994E+00
!  0.42125386459293679E+00  0.52664748163967656E+00  0.41276984833062677E-01 -0.57508880909801337E-01
! -0.78159693917819351E+00  0.33530903764996089E+00 -0.16446912649058726E+00 -0.39565977950652176E+00
!
! Target matrix pseudoinverse Ap has 4 rows and 6 columns.
! -0.13412781004176838E-01 -0.78242619353861131E+00 -0.28048708073518797E+00  0.69005677566325574E+00 -0.57898976288381782E-01  0.38785355496777540E+00
!  0.11510840334211880E+00  0.33388491041180207E+00  0.15068106214832511E-02 -0.26928052460342777E+00 -0.51122815707764654E-01 -0.30011134998577643E+00
!  0.11381440485695521E+00  0.87573028565356603E+00  0.54297994590480858E+00 -0.12365389596669285E+00 -0.85638424788358244E-01 -0.50302166861654685E+00
! -0.28244900292470265E+00 -0.72912863064408240E+00  0.83843843395431911E-01  0.92828238344630154E+00  0.15190216913706006E+00  0.40204555682866727E+00
!
! Target matrix identity has 4 rows and 4 columns.
!  0.99999999999999933E+00 -0.26645352591003757E-14  0.33306690738754696E-15 -0.33306690738754696E-15
!  0.16653345369377348E-15  0.99999999999999978E+00  0.00000000000000000E+00 -0.33306690738754696E-15
!  0.22204460492503131E-15  0.13322676295501878E-14  0.99999999999999989E+00  0.44408920985006262E-15
! -0.13877787807814457E-15 -0.22204460492503131E-14  0.55511151231257827E-15  0.10000000000000000E+01
! STOP successful completion for test_dgesvd.
