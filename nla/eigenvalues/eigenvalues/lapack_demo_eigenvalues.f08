! https://stackoverflow.com/questions/19726193/linking-lapack-in-fortran-on-mac-os-x
include '../sharedModules/mod precision definitions.f08'
!include 'mod precision definitions.f08'

program lapack_demo_eigenvalues

    use mPrecisionDefinitions, only : rp, ip, unit_modulus, one, zero

    implicit none

    ! parameters
    integer ( ip ), parameter        :: n = 3 ! dimension of square matrix
    complex ( rp ), parameter        :: e_values_mm ( 1 : n ) = [ ( -0.6783223218245161, zero ), &
                                                                  (  4.494845902904796,  zero ), &
                                                                  (  9.18347641891972,   zero )]
    character ( len = * ), parameter :: myProgram = 'lapack_demo_eigenvalues'

    ! rank 2
    complex ( rp ), allocatable      :: A ( : , : ), e_vectors ( : , : )
    ! rank 1
    complex ( rp ), allocatable      :: e_values ( : )
    integer ( ip )                   :: alloc_status = 0, c = 0, r = 0
    character ( len = 512 )          :: alloc_msg = ''

        ! target matrix
        allocate ( A ( n, n ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
          write ( *, 100 ) ""
          write ( *, 110 ) "", "matrix A"
          write ( *, 120 ) alloc_status
          write ( *, 130 ) trim ( alloc_msg )
          stop "Fatal error in " // myProgram // "."
        end if

        ! column 1
        A ( 1, 1 ) =      ( one, zero )
        A ( 2, 1 ) = -2 * unit_modulus
        A ( 3, 1 ) =  3 * ( one, zero )

        ! column 2
        A ( 1, 2 ) = 2 * unit_modulus
        A ( 2, 2 ) = 5 * ( one, zero )
        A ( 3, 2 ) =     ( one, one )

        ! column 3
        A ( 1, 3 ) = 3 * ( one, zero )
        A ( 2, 3 ) =     ( one, -1.0_rp )
        A ( 3, 3 ) = 7 * ( one, zero )

        call wrapped_zheevd ( A, e_values, e_vectors )

        write ( *, '( /, "input matrix:" )' )
        do r = 1, 3
            write ( *, 200 ) [ ( A ( r, c ), c = 1, 3 ) ]
        end do
        write ( *, * )

        write ( *, '( "Testing LAPACK routine zheevd:", / )' )
        write ( *, 210 ) "LAPACK ", e_values
        write ( *, 210 ) "Mathematica ", e_values_mm
        write ( *, 210 ) "Differenced ", e_values_mm - e_values

        deallocate ( A, stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
          write ( *, 100 ) "de"
          write ( *, 110 ) "de", "matrix A"
          write ( *, 120 ) alloc_status
          write ( *, 130 ) trim ( alloc_msg )
        end if

        stop "Successful execution for " // myProgram // "."

  100   format ( "Memory ", A, "allocation failure." )
  110   format ( "Attempting to ", A, "allocate ", g0, "." )
  120   format ( "Status code: ", g0, "." )
  130   format ( "Error message: ", A, "." )

  200   format ( 3( "( ", F6.2, " + ", F6.2, " i )", 2X ) )
  210   format ( A, "eigenvalues:", /, 3( "( ", g0, " + ", g0, " i )", / ) )

contains

    subroutine wrapped_zheevd ( a_in, ze_values, ze_vectors )

        integer                                    :: ndim = 0

        complex ( rp ), intent( in ),  allocatable :: a_in ( : , : )
        complex ( rp ), intent( out ), allocatable :: ze_vectors ( : , : ), ze_values ( : )

        complex ( rp ), allocatable                :: A ( : , : ), work ( : )
        real ( rp ),    allocatable                :: rwork ( : ), w ( : )
        integer,        allocatable                :: iwork ( : )

        integer                                    :: info = 0, lda = 0, liwork = 0, lrwork = 0, lwork = 0, n = 0
        character ( len = 1 )                      :: jobz = 'V', uplo = 'U'

            ndim = size ( a_in ( 1, : ) )

            if ( allocated ( ze_vectors ) ) deallocate ( ze_vectors )
            if ( allocated ( ze_values  ) ) deallocate ( ze_values )

            allocate ( ze_vectors ( ndim, ndim ), ze_values ( ndim ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
              write ( *, 100 ) ""
              write ( *, 110 ) "", "matrix ze_vectors"
              write ( *, 120 ) alloc_status
              write ( *, 130 ) trim ( alloc_msg )
              stop "Fatal error in " // myProgram // "."
            end if

            n   = ndim
            lda = n

            lwork  = n * ( n + 2 )
            lrwork = n * ( 2 * n + 5 ) + 1
            liwork = 3 + 5 * n

            allocate ( a ( ndim, ndim ), w ( ndim ), work ( lwork ), rwork ( lrwork ), iwork ( liwork ), &
                       stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
              write ( *, 100 ) ""
              write ( *, 110 ) "", "matrix: a, vectors: w, work, rwork, iwork"
              write ( *, 120 ) alloc_status
              write ( *, 130 ) trim ( alloc_msg )
              stop "Fatal error in " // myProgram // "."
            end if

            a = a_in

            call zheevd ( jobz, uplo, n, a, lda, w, work, lwork, rwork, lrwork, iwork, liwork, info )

            ze_values  = w
            ze_vectors = a

            deallocate ( a, w, rwork, iwork, work )
            if ( alloc_status /= 0 ) then
              write ( *, 100 ) "de"
              write ( *, 110 ) "de", "matrix: a, vectors: w, work, rwork, iwork"
              write ( *, 120 ) alloc_status
              write ( *, 130 ) trim ( alloc_msg )
            end if

        return

      100   format ( "Memory ", A, "allocation failure." )
      110   format ( "Attempting to ", A, "allocate ", g0, "." )
      120   format ( "Status code: ", g0, "." )
      130   format ( "Error message: ", A, "." )

    end subroutine

end program lapack_demo_eigenvalues

! Muntz-Szasz:eigenvalues dantopa$ date
! Sun Jan 24 20:01:13 CST 2016
! Muntz-Szasz:eigenvalues dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/nla/eigenvalues
! Muntz-Szasz:eigenvalues dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate lapack_demo_eigenvalues.f08
! lapack_demo_eigenvalues.f08:7:8:
!
!      use mPrecisionDefinitions, only : rp, ip, unit_modulus, one, zero
!         1
! Warning: Unused parameter ‘one’ which has been explicitly imported at (1) [-Wunused-parameter]
! Muntz-Szasz:eigenvalues dantopa$ ./a.out
!
! input matrix:
! (   1.00 +   0.00 i )  (   0.00 +   2.00 i )  (   3.00 +   0.00 i )
! (  -0.00 +  -2.00 i )  (   5.00 +   0.00 i )  (   1.00 +  -1.00 i )
! (   3.00 +   0.00 i )  (   1.00 +   1.00 i )  (   7.00 +   0.00 i )
!
! Testing LAPACK routine zheevd:
!
! LAPACK eigenvalues:
! ( -.67832232182451535 + .0000000000000000 i )
! ( 4.4948459029047951 + .0000000000000000 i )
! ( 9.1834764189197191 + .0000000000000000 i )
!
! Mathematica eigenvalues:
! ( -.67832231521606445 + .0000000000000000 i )
! ( 4.4948458671569824 + .0000000000000000 i )
! ( 9.1834764480590820 + .0000000000000000 i )
!
! Differenced eigenvalues:
! ( .66084508976160805E-008 + .0000000000000000 i )
! ( -.35747812709985283E-007 + .0000000000000000 i )
! ( .29139362922592227E-007 + .0000000000000000 i )
!
! STOP Successful execution for lapack_demo_eigenvalues.
