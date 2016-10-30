! http://www.nag.com/lapack-ex/examples/source/dgesvd-ex.f

program lapack_demo_svd

    use mMatrixReader,  only : read_matrix_file
    use mMatrixWriter,  only : print_matrix
    use mSetPrecision,  only : rp
    use mSVDparameters, only : A, LDA, LDU, LDVT, LWORK, MMAX, NMAX, &
                               M, N, Ap, Sp, id_col, id_row, myA, myU, myV, &
                               m_row, n_col, rank, io_write
    implicit none

    external DGESVD, DDISNA ! LAPACK routines

    ! parameters
    real ( rp ),           parameter :: s_mm ( 1 : 2 ) = [ sqrt ( 2.0_rp ),  1.0_rp ] ! exact singular values
    character ( len = * ), parameter :: inputMatrix = "sample_matrix.txt"
    ! rank 2
    ! real ( rp )
    real ( rp ) :: U  ( LDU,  MMAX ) = 0.0_rp
    real ( rp ) :: VT ( LDVT, NMAX ) = 0.0_rp
    ! rank 1
    real ( rp ) :: RCONDU ( NMAX ) = 0.0_rp, VERRBD ( NMAX ) = 0.0_rp, S ( NMAX )     = 0.0_rp, &
                   RCONDV ( NMAX ) = 0.0_rp, UERRBD ( NMAX ) = 0.0_rp, WORK ( LWORK ) = 0.0_rp
    ! rank 0
    real ( rp ) :: EPS = 0.0_rp, SERRBD = 0.0_rp

    integer     :: INFO = 0, LWKOPT = 0
    integer     :: io_status = 0, row = 0

        call read_matrix_file ( inputMatrix )
        call print_matrix ( A = A, myFormat = 'E20.13', spaces = 3, dims = [ m_row, n_col ], my_io_unit = io_write )
        myA = A ( 1 : m_row, 1 : n_col )

        ! Compute the singular values and left and right singular vectors of A (A = U S (V'), m >= n )
        ! Overwrite A by U
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
        write ( io_write, * ) 'S = ', ( ( s ( row ) - s_mm ( row ) ), row = 1, 2 )
        write ( io_write, * )
        call print_matrix ( A = U, myFormat = 'F7.3', spaces = 3, moniker = 'codomain U', dims = [ 6, 6 ], my_io_unit = io_write )
        call print_matrix ( A = transpose ( VT ), myFormat = 'F7.3', spaces = 3, moniker = 'domain V', dims = [ 4, 4 ], &
                            my_io_unit = io_write )

        EPS = epsilon ( 1.0_rp )
        SERRBD = EPS * S ( 1 )

        ! estimate reciprocal condition numbers for the singular vectors
        call DDISNA ( 'Left',  M, N, S, RCONDU, INFO )
        call DDISNA ( 'Right', M, N, S, RCONDV, INFO )

        ! compute the error estimates for the singular vectors
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

        ! assemble the pseudoinverse matrix
        do row = 1, rank
          Sp ( row, row ) = 1 / S ( row )
        end do
        !( Sp ( 1 : row, 1 : row ) = 1 / S ( 1 : row ), row = 1, rank )
        myU = U ( 1 : m, 1 : m )
        myV = transpose ( Vt ( 1 : n, 1 : n ) )
        Ap = matmul ( myV, matmul ( Sp, transpose ( myU ) ) )

        id_col = matmul ( Ap, myA )
        call print_matrix ( A = id_col, myFormat = 'F8.3', spaces = 1, moniker = 'Identity matrix Ap A', my_io_unit = io_write )

        id_row = matmul ( myA, Ap )
        call print_matrix ( A = id_row, myFormat = 'F8.3', spaces = 1, moniker = 'Identity matrix A Ap', my_io_unit = io_write )

        stop 'successful completion for lapack_demo_svd ...'

  200   format ( /, ' Optimum workspace required = ', g0, /, &
                    ' Workspace provided         = ', g0 )

  300   format ( ' dgesvd info = ', g0, ': no errors' )
  310   format ( G10.3 )
  320   format ( 4( G10.3, 2X ) )

end program lapack_demo_svd

! rditldmt@ITL-DTOPA-MP:bravo $ date
! Mon Sep 19 15:26:20 CDT 2016
! rditldmt@ITL-DTOPA-MP:bravo $ pwd
! /Users/rditldmt/hpc/fortran/nla/SVD/lapack/bravo
! rditldmt@ITL-DTOPA-MP:bravo $ make
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_svd_parameters.o mod_svd_parameters.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_matrix_reader.o mod_matrix_reader.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_matrix_writer.o mod_matrix_writer.f08
! gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o lapack_demo_svd.o lapack_demo_svd.f08
! gfortran -g -framework Accelerate -o lapack_demo_svd lapack_demo_svd.o mod_file_handling.o mod_matrix_reader.o mod_matrix_writer.o mod_set_precision.o mod_svd_parameters.o
! rditldmt@ITL-DTOPA-MP:bravo $ ./lapack_demo_svd
!
! Target matrix has 3 rows and 2 columns.
!  0.1000000000000E+01    0.1000000000000E+01
! -0.7071067811865E+00    0.7071067811865E+00
!  0.0000000000000E+00    0.0000000000000E+00
!
!  Optimum workspace required = 138
!  Workspace provided         = 1194
!  dgesvd info = 0: no errors
!
!  DGESVD Example Program Results
!
!  Singular values, LAPACK:
!  S =    1.4142135623730949        1.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000        0.0000000000000000
!  Singular values, Mathematica:
!  S =    1.4142135623730951        1.0000000000000000
!  Singular values, differenced:
!  S =   -2.2204460492503131E-016   0.0000000000000000
!
!
! codomain U has 6 rows and 6 columns.
!  -1.000     0.000     0.000     0.000     0.000     0.000
!   0.000     1.000     0.000     0.000     0.000     0.000
!   0.000     0.000     1.000     0.000     0.000     0.000
!   0.000     0.000     0.000     0.000     0.000     0.000
!   0.000     0.000     0.000     0.000     0.000     0.000
!   0.000     0.000     0.000     0.000     0.000     0.000
!
! domain V has 4 rows and 4 columns.
!  -0.707    -0.707     0.000     0.000
!  -0.707     0.707     0.000     0.000
!   0.000     0.000     0.000     0.000
!   0.000     0.000     0.000     0.000
!  Error estimate for the singular values:
!  0.314E-15
!
!  Error estimates for the left singular vectors:
!  0.758E-15   0.758E-15
!
!  Error estimates for the right singular vectors:
!  0.758E-15   0.758E-15
!
! Identity matrix Ap A has 2 rows and 2 columns.
!    1.000    0.000
!    0.000    1.000
!
! Identity matrix A Ap has 3 rows and 3 columns.
!    1.000    0.000    0.000
!    0.000    1.000    0.000
!    0.000    0.000    0.000
! STOP successful completion for lapack_demo_svd ...
