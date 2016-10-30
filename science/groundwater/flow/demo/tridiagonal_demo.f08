program tridiagonal_demo

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero
    use mTriDiagonalSolvers,    only : thomas, tri_diag

    implicit none

    ! rank 2
    real ( rp ), allocatable :: aa ( : , : )

    ! rank 1
    real ( rp ), allocatable :: solution_old ( : ), solution_new ( : ), solution_ideal ( : )
    real ( rp ), allocatable :: accuracy_old ( : ), accuracy_new ( : )
    real ( rp ), allocatable :: vector_a ( : ), vector_b ( : ), vector_c ( : )  ! Toeplitz diagonals
    real ( rp ), allocatable :: vector_d ( : )  ! data vector:  A solution = data
    real ( rp ), allocatable :: b ( : )  ! match Fred's notation

    ! rank 0
    real ( rp ) :: subdiagonal = zero, diagonal = zero, superdiagonal = zero, solution_value = zero
    real ( rp ) :: norm_error_new = zero, norm_error_old = zero

    integer ( ip ) :: nDim = 0, alloc_status = 0
    integer ( ip ) :: j = 0, k = 0

        write ( * , 100 ) huge ( 1_ip )  ! banner

        ! input data
        do  ! dimension
            write ( * , 110 )
            read  ( * , * ) nDim
            if ( nDim .ge. 3 .and. nDim .lt. huge ( 1_ip ) ) exit
        end do

        do
            write ( * , 120 )
            read  ( * , * ) diagonal
            if ( diagonal .ne. zero ) exit
        end do

        do
            write ( * , 130 )
            read  ( * , * ) subdiagonal
            if ( subdiagonal .ne. zero ) exit
        end do

        do
            write ( * , 140 )
            read  ( * , * ) superdiagonal
            if ( superdiagonal .ne. zero ) exit
        end do

        do
            write ( * , 150 )
            read  ( * , * ) solution_value
            if ( solution_value .ne. zero ) exit
        end do

        write ( * , 200 )  ! Allocating vectors...
        allocate ( aa ( 1 : 3, 1 : nDim ), stat = alloc_status )
        allocate ( b ( 1 : nDim ),         stat = alloc_status )

        allocate ( solution_old   ( 1 : nDim ), stat = alloc_status )
        allocate ( solution_new   ( 1 : nDim ), stat = alloc_status )
        allocate ( solution_ideal ( 1 : nDim ), stat = alloc_status )

        allocate ( accuracy_old ( 1 : nDim ), stat = alloc_status )
        allocate ( accuracy_new ( 1 : nDim ), stat = alloc_status )

        allocate ( vector_a ( 1 : nDim ), stat = alloc_status )
        allocate ( vector_b ( 1 : nDim ), stat = alloc_status )
        allocate ( vector_c ( 1 : nDim ), stat = alloc_status )
        allocate ( vector_d ( 1 : nDim ), stat = alloc_status )

        ! load diagonals for Toeplitz matrix
        vector_a = subdiagonal
        vector_b = diagonal
        vector_c = superdiagonal

        write ( * , 210 )  ! Constructing data vector....

        ! construct data_vector = A x
        vector_d ( : )    = ( subdiagonal + diagonal + superdiagonal ) * solution_value
        vector_d ( 1 )    = (               diagonal + superdiagonal ) * solution_value
        vector_d ( nDim ) = ( subdiagonal + diagonal                 ) * solution_value

        write ( * , 220 )  ! Computing solutions...

        ! recreate data structures of tridiag
        aa ( 1, : ) = vector_a
        aa ( 2, : ) = vector_b
        aa ( 3, : ) = vector_c

        b ( : ) = vector_d

        call thomas ( aa, b )  ! updated method
        solution_new = b

        aa ( 1, : ) = vector_a
        aa ( 2, : ) = vector_b
        aa ( 3, : ) = vector_c

        b ( : ) = vector_d

        call tri_diag ( aa, b )  ! current method
        solution_old = b

        write ( * , 230 )  ! Comparing solutions...
        solution_ideal ( : ) = solution_value
        accuracy_old   ( : ) = solution_ideal ( : ) - solution_old ( : )
        accuracy_new   ( : ) = solution_ideal ( : ) - solution_new ( : )

        write ( *, 300 ) 'first'
        do k = 1, 3
            write ( * , 310 ) k, solution_ideal ( k ), accuracy_new ( k ), accuracy_old ( k )
        end do

        write ( *, 300 ) 'last'
        do k = 1, 3
            j = nDim - k + 1
            write ( * , 310 ) j, solution_ideal ( j ), accuracy_new ( j ), accuracy_old ( j )
        end do

        norm_error_new = norm2 ( accuracy_new )
        norm_error_old = norm2 ( accuracy_old )

        write ( *, 320 ) norm_error_old, norm_error_new

        stop 'program tridiagonal_demo complete ...'

    100 format ( 'Program to test the Thomas Algorithm for solving tridiagonal systems Ax = b.', /, &
                 'For simplicity test program is restricted to Toeplitz matrices and constant data vectors.', /, &
                 'User inputs information to build matrix A and solution vector x. The program constructs the data vector b.', / &
                 'A and b are then passed to the tridiagnonal solvers to find x_computed.' // &
                 'Requested inputs:', /, &
                 '1. Dimension n (3 <= n < ', g0, ')', /, &
                 '2. Diagonal value      (REAL64, nonzero)', /, &
                 '3. Subdiagonal value   (REAL64, nonzero)', /, &
                 '4. Superdiagonal value (REAL64, nonzero)', /, &
                 '5. Solution value      (REAL64, nonzero)', /, &
                 'The algorithm is stable for strictly diagonally dominant matrices', // )
    110 format ( 'Input the dimension...' )
    120 format ( 'Input the diagonal value...' )
    130 format ( 'Input the subdiagonal value...' )
    140 format ( 'Input the superdiagonal value...' )
    150 format ( 'Input the solution value...' )

    200 format ( 'Allocating vectors...' )
    210 format ( 'Constructing data vector...' )
    220 format ( 'Computing solutions...' )
    230 format ( 'Comparing solutions...' )

    300 format ( /, 'Error in ', g0, ' three terms...', /, T35, 'error in', T50, 'error in', &
                                          /, T10, 'ideal', T35, 'updated',  T50, 'current' )
    310 format ( I3, T10, g0, T35, E10.3, T50, E10.3 )
    320 format ( /, '2-Norm of the residual error vectors:', /, 'Current method: ', E10.2, /, 'Updated method: ', E10.2, / )


end program tridiagonal_demo

! 13:44 dan-topas-pro-2 rditldmt $ date
! Wed Feb 24 13:44:22 CST 2016
!  13:44 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/projects/groundwater/flow/demo
!  13:44 dan-topas-pro-2 rditldmt $ make clean
! rm -rf mod_constants.o mod_precision_definitions.o mod_tridiagonal_solvers.o tridiagonal_demo.o tridiagonal_demo mod_constants.mod mod_precision_definitions.mod mod_tridiagonal_solvers.mod
! rm -f *.mod
!  13:44 dan-topas-pro-2 rditldmt $ make
! /usr/local/bin/gfortran -g -c -Wall -Wunused-parameter -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
! /usr/local/bin/gfortran -g -c -Wall -Wunused-parameter -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
! /usr/local/bin/gfortran -g -c -Wall -Wunused-parameter -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_tridiagonal_solvers.o mod_tridiagonal_solvers.f08
! /usr/local/bin/gfortran -g -c -Wall -Wunused-parameter -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o tridiagonal_demo.o tridiagonal_demo.f08
! /usr/local/bin/gfortran -g -o tridiagonal_demo mod_constants.o mod_precision_definitions.o mod_tridiagonal_solvers.o tridiagonal_demo.o
!  13:44 dan-topas-pro-2 rditldmt $ ./tridiagonal_demo
! Program to test the Thomas Algorithm for solving tridiagonal systems Ax = b.
! For simplicity this program is restricted to Toeplitz matrices and constant data vectors.
! User inputs information to build matrix A and solution vector x. The program constructs the data vector b.
! A and b are then passed to the tridiagnonal solvers to find x_computed.
!
! Requested inputs:
! 1. Dimension n (3 <= n < 2147483647)
! 2. Diagonal value      (REAL64, nonzero)
! 3. Subdiagonal value   (REAL64, nonzero)
! 4. Superdiagonal value (REAL64, nonzero)
! 5. Solution value      (REAL64, nonzero)
! The algorithm is stable for strictly diagonally dominant matrices
!
!
! Input the dimension...
! 25
! Input the diagonal value...
! 3
! Input the subdiagonal value...
! -1
! Input the superdiagonal value...
! 2
! Input the solution value...
! 2
! Allocating vectors...
! Constructing data vector...
! Computing solutions...
! Comparing solutions...
!
! Error in first three terms...
!                                   error in       error in
!          ideal                    updated        current
!   1      2.0000000000000000        0.000E+00      0.200E+01
!   2      2.0000000000000000        0.222E-15      0.562E+00
!   3      2.0000000000000000        0.222E-15      0.158E+00
!
! Error in last three terms...
!                                   error in       error in
!          ideal                    updated        current
!  25      2.0000000000000000       -0.444E-15      0.133E-12
!  24      2.0000000000000000        0.000E+00      0.400E-12
!  23      2.0000000000000000       -0.444E-15      0.147E-11
!
! 2-Norm of the residual error vectors:
! Current method:   0.21E+01
! Updated method:   0.13E-14
!
! STOP program tridiagonal_demo complete ...
