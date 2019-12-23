module mSetPrecision

    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64

    implicit none

    integer, parameter :: sp = REAL32, dp = REAL64
    integer, parameter :: rp = sp  ! set real precision to double precision

    real ( sp ), parameter :: zero = 0.0_sp

end module mSetPrecision
