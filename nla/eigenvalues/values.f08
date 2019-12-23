! https://stackoverflow.com/questions/19726193/linking-lapack-in-fortran-on-mac-os-x
include 'mod precision definitions.f08'

program eigenvalues

    use precision_definitions, only : wp, ip, unit_modulus, one, zero

    implicit none
    integer ( ip ), parameter        :: n = 3
    character ( len = * ), parameter :: myProgram = 'program eigenvalues'

    ! rank 2
    complex ( wp ), allocatable      :: A ( : , : ), vecs ( : , : )
    ! rank 1
    complex ( wp ), allocatable      :: eigs ( : )
    complex ( wp ), parameter        :: eigs_mm ( 1 : n ) = [ ( -0.6783223218245161, zero ), &
                                                              (  4.494845902904796,  zero ), &
                                                              (  9.18347641891972,   zero )]
    integer ( ip )                   :: alloc_status, c, r
    character ( len = 512 )          :: alloc_msg

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
        A ( 2, 3 ) =     ( one, -1.0_wp )
        A ( 3, 3 ) = 7 * ( one, zero )

        call wrapped_zheevd ( A, eigs, vecs )

        do r = 1, 3
            write ( *, 200 ) [ ( A ( r, c ), c = 1, 3 ) ]
        end do

        write ( *, 210 ) "Computed ", eigs
        write ( *, 210 ) "Mathematica ", eigs_mm
        write ( *, 210 ) "Differenced ", eigs_mm - eigs

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

  200   format ( 3( g0, 2x ) ) )
  210   format ( A, "eigenvalues:", /, 3( "( ", g0, ", ", g0, " )", / ) )

contains

    subroutine wrapped_zheevd ( matin, zvals, zvecs )

        integer                                    :: ndim

        complex ( wp ), intent( in ),  allocatable :: matin ( : , : )
        complex ( wp ), intent( out ), allocatable :: zvecs ( : , : ), zvals ( : )

        complex ( wp ), allocatable                :: A ( : , : ), work ( : )
        real ( wp ),    allocatable                :: rwork ( : ), w ( : )
        integer,        allocatable                :: iwork ( : )

        integer                                    :: info, lda, liwork, lrwork, lwork, n
        character ( len = 1 )                      :: jobz = 'V', uplo = 'U'

            ndim = size ( matin ( 1, : ) )

            if ( allocated ( zvecs ) ) deallocate ( zvecs )
            if ( allocated ( zvals ) ) deallocate ( zvals )

            allocate ( zvecs ( ndim, ndim ), zvals ( ndim ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
              write ( *, 100 ) ""
              write ( *, 110 ) "", "matrix zvecs"
              write ( *, 120 ) alloc_status
              write ( *, 130 ) trim ( alloc_msg )
              stop "Fatal error in " // myProgram // "."
            end if

            n = ndim
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

            a = matin

            call zheevd ( jobz, uplo, n, a, lda, w, work, lwork, rwork, lrwork, iwork, liwork, info )

            zvals = w
            zvecs = a

            deallocate ( a ,w, rwork, iwork, work )
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

end program eigenvalues

! dan-topas-pro-2:nla rditldmt$ date
! Fri Jan 15 18:53:07 CST 2016
! dan-topas-pro-2:nla rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/nla
! dan-topas-pro-2:nla rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate values.f08
! values.f08:6:8:
!
!      use precision_definitions, only : wp, ip, unit_modulus, one, zero
!         1
! Warning: Unused parameter ‘one’ which has been explicitly imported at (1) [-Wunused-parameter]
! dan-topas-pro-2:nla rditldmt$ ./a.out
! Computed eigenvalues:
! ( -.67832232182451535, .0000000000000000 )
! ( 4.4948459029047951, .0000000000000000 )
! ( 9.1834764189197191, .0000000000000000 )
!
! Mathematica eigenvalues:
! ( -.67832231521606445, .0000000000000000 )
! ( 4.4948458671569824, .0000000000000000 )
! ( 9.1834764480590820, .0000000000000000 )
!
! Differenced eigenvalues:
! ( .66084508976160805E-008, .0000000000000000 )
! ( -.35747812709985283E-007, .0000000000000000 )
! ( .29139362922592227E-007, .0000000000000000 )
!
! stop Successful execution for program eigenvalues.
