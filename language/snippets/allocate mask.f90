!  https://gcc.gnu.org/onlinedocs/gcc-4.9.2/gfortran/PRODUCT.html#PRODUCT
program test_mask

  use iso_fortran_env
  implicit none

  integer ( int64 )              :: i = 0, alloc_status = 0, total_even = 0, total_odd = 0
!  integer ( int64 ), parameter   :: bound = 1000000000_int64
  integer ( int64 ), parameter   :: bound = 100000000_int64
  integer ( int64 ), allocatable :: iarray ( : )

!  real ( real64 ), allocatable   :: array ( : )
  real ( real64 )                :: time0, time1

  character ( kind = kind ( 'A' ), len = 255 ) :: alloc_msg = " "

    ! allocate large array
    call cpu_time ( time0 )
    write ( *, '( "Allocating int64 array of size ", g0, "..." )' ) bound
    allocate ( iarray ( 1 : bound ), stat = alloc_status, errmsg = alloc_msg )
    if ( alloc_status /= 0 ) then
      write ( *, 110 ) ""
      write ( *, 120 ) "", bound
      write ( *, 130 ) alloc_status
      write ( *, 140 ) trim ( alloc_msg )
    end if
    call cpu_time ( time1 )
    write ( *, 100 ) time1 - time0, "allocation"

    ! assignment
    call cpu_time ( time0 )
    iarray ( 1 : bound ) = [ ( i, i = 1, bound ) ]
    call cpu_time ( time1 )
    write ( *, 100 ) time1 - time0, "load with implied do"

    ! summation with do loop
    call cpu_time ( time0 )
    total_even = 0
    total_odd  = 0
    do i = 1, bound
      if ( MOD ( i, 2_int64 ) == 0 ) then
        total_even = total_even + iarray ( i )
      else
        total_odd  = total_odd  + iarray ( i )
      end if
    end do
    call cpu_time ( time1 )
    write ( *, 150 ) total_even, total_odd
    write ( *, 100 ) time1 - time0, "even, odd totals with do loop"

    ! summation with mask
    call cpu_time ( time0 )
    total_even = sum ( iarray, MASK = MOD ( iarray, 2_int64 ) == 0 )
    total_odd  = sum ( iarray, MASK = MOD ( iarray, 2_int64 ) == 1 )
    call cpu_time ( time1 )
    write ( *, 150 ) total_even, total_odd
    write ( *, 100 ) time1 - time0, "even, odd totals with mask"

    ! deallocation
    call cpu_time ( time0 )
    deallocate ( iarray, stat = alloc_status, errmsg = alloc_msg )
    if ( alloc_status /= 0 ) then
      write ( *, 110 ) ""
      write ( *, 120 ) "", bound
      write ( *, 130 ) alloc_status
      write ( *, 140 ) trim ( alloc_msg )
    end if
    call cpu_time ( time1 )
    write ( *, 100 ) time1 - time0, "deallocation"

100 format ( F20.6, 2x, A, / )
110 format ( "Memory ", A, "allocation failure" )
120 format ( "Attempting to ", A, "allocate int64 array of ", g0, " elements" )
130 format ( "Status code: ", g0 )
140 format ( "Error message: ", A )
150 format ( "Total of evens: ", g0, /, "Total of odds:  ", g0 )

end program test_mask

! dan-topas-pro-2:snippets rditldmt$ date
! Thu Oct  1 13:22:09 CDT 2015
! dan-topas-pro-2:snippets rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/snippets
! dan-topas-pro-2:snippets rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 allocate\ mask.f90 -o allocate\ mask
! dan-topas-pro-2:snippets rditldmt$ ./allocate\ mask
! Allocating int64 array of size 1000000...
!             0.000068  allocation
!
!             0.011134  load with implied do
!
! Total of evens: 250000500000
! Total of odds:  250000000000
!             0.003831  even, odd totals with do loop
!
! Total of evens: 250000500000
! Total of odds:  250000000000
!             0.004204  even, odd totals with mask
!
!             0.000006  deallocation
!
! dan-topas-pro-2:snippets rditldmt$
