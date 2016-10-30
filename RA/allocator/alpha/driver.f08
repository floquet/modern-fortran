program driver

    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64, REAL128, INT16, INT32
    use mAllocator,                    only : allocator

    implicit none

    integer, parameter :: rpa = REAL32, rpb = REAL64, rpc = REAL128
    integer, parameter :: ipa = INT16,  ipb = INT32

    real    ( rpa ), allocatable :: reala ( : )
    real    ( rpb ), allocatable :: realb ( : )
    real    ( rpc ), allocatable :: realc ( : )
    integer ( ipa ), allocatable :: inta  ( : )
    integer ( ipb ), allocatable :: intb  ( : )

        call allocator ( reala )
        call allocator ( realb )
        call allocator ( realc )
        call allocator ( inta  )
        call allocator ( intb  )

end program driver
