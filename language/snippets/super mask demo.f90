!  https://gcc.gnu.org/onlinedocs/gcc-4.9.2/gfortran/PRODUCT.html#PRODUCT
program test_mask

  use iso_fortran_env
  implicit none

  integer ( int64 )            :: i
  integer ( int64 ), parameter :: bound = 10000000_int64

  real ( real64 )              :: array ( 1 : bound )
  real ( real64 )              :: time0, time1, total_even, total_odd

    call cpu_time ( time0 )
    array ( 1 : bound ) = [ ( i, i = 1, bound ) ]
    call cpu_time ( time1 )
    write ( *, 100 ) time1 - time0, "load with implied do"

    call cpu_time ( time0 )
    total_even = 0.0_real64
    total_odd  = 0.0_real64
    do i = 1, bound
      if ( MOD ( i, 2 ) == 0 ) then
        total_even = total_even + array ( i )
      else
        total_odd  = total_odd  + array ( i )
      end if
    end do
    call cpu_time ( time1 )
    write ( *, 100 ) time1 - time0, "even, odd totals with do loop"

100 format ( g0, 2x, A, / )

end program test_mask