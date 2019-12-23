program driver

    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64!, REAL128, INT16, INT32
    use mAllocator,                    only : allocator

    implicit none

    integer, parameter :: rpa = REAL32, rpb = REAL64!, rpc = REAL128
    !integer, parameter :: ipa = INT16,  ipb = INT32
    integer, parameter :: lambda = 10

    real    ( rpa ), allocatable :: reala ( : )
    real    ( rpb ), allocatable :: realb ( : )
    ! real    ( rpc ), allocatable :: realc ( : )
    ! integer ( ipa ), allocatable :: inta  ( : )
    ! integer ( ipb ), allocatable :: intb  ( : )

        call allocator ( reala, lambda )
        write ( * , 100 ) lambda, size ( reala ), 'REAL32', storage_size ( reala )
        print *, 'reala = ', reala

        call allocator ( realb, lambda )
        write ( * , 100 ) lambda, size ( realb ), 'REAL64', storage_size ( realb )

        ! call allocator ( realc, lambda )
        ! write ( * , 100 ) lambda, size ( realc ), 'REAL128', storage_size ( realc )
        !
        ! call allocator ( inta,  lambda )
        ! write ( * , 100 ) lambda, size ( inta ), 'INT16', storage_size ( inta )
        !
        ! call allocator ( intb,  lambda )
        ! write ( * , 100 ) lambda, size ( intb ), 'INT32', storage_size ( intb )

    100 format ( 'array has ', g0, ' elements; size  = ', g0, ', storage_size for each element of ', g0, ' = ', g0, ' bytes' )

end program driver
