program driver

    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64!, REAL128, INT16, INT32
    use mAllocator,                    only : allocator

    implicit none

    integer, parameter :: rpa = REAL32, rpb = REAL64!, rpc = REAL128
    !integer, parameter :: ipa = INT16,  ipb = INT32
    integer, parameter :: lambda = 10

    real    ( rpa ), allocatable :: reala ( : )
    !real    ( rpb ), allocatable :: realb ( : )

        call allocator ( reala, lambda )
        write ( * , 100 ) lambda, size ( reala ), 'REAL32', storage_size ( reala )
        print *, 'reala = ', reala

    100 format ( 'array has ', g0, ' elements; size  = ', g0, ', storage_size for each element of ', g0, ' = ', g0, ' bytes' )

end program driver
