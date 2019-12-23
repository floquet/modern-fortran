program conversion
    use, intrinsic :: iso_fortran_env, only : REAL64
    implicit none
    real     ( selected_real_kind ( REAL64 ) ), parameter :: one = 1.0_REAL64
    complex  ( selected_real_kind ( REAL64 ) ), parameter :: i = ( 0.0_REAL64, one )
        print *, 'double precision 1 = ', one
        print *, 'double precision i = ', i
end program conversion
