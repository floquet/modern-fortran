module mAllocator
    implicit none

contains
    subroutine myAllocator ( myArray, source_type, lambda )
        class ( * ), allocatable, intent ( inout ) :: myArray ( : )
        class ( * ),              intent ( in )    :: source_type
        integer,                  intent ( in )    :: lambda

        integer                 :: alloc_status  = 0
        character ( len = 512 ) :: alloc_message = ''

            allocate ( myArray ( 1 : lambda ), source = source_type, stat = alloc_status, errmsg = alloc_message )
            if ( alloc_status /= 0 ) then
                write ( *, 100 ) lambda
                write ( *, 110 ) alloc_status
                write ( *, 120 ) trim ( alloc_message )
                stop 'Fatal error in subroutine allocator'
            end if

        100 format ( /, 'Error allocating memory for ', g0, ' elements.' )
        110 format (    '  stat = ', g0, '.' )
        120 format (    '  errmsg = ', g0, '.' )

    end subroutine myAllocator

end module mAllocator


program generic_allocation

    use mAllocator, only : myAllocator

    implicit none

    integer, parameter   :: lambda = 10
    integer, parameter   :: source_int = 1
    real,    parameter   :: source_real = 1.0

    integer, allocatable :: array_int  ( : )
    real,    allocatable :: array_real ( : )

        call myAllocator ( array_int, source_int, lambda )
        write ( * , 100 ) lambda, size ( array_int ), 'default real', storage_size ( array_int ( 1 ) )

        call myAllocator ( array_real, source_real, lambda )
        write ( * , 100 ) lambda, size ( array_real ), 'default integer', storage_size ( array_real ( 1 ) )

    100 format ( 'array has ', g0, ' elements; size  = ', g0, ', storage_size for each element of ', g0, ' = ', g0, ' bytes' )

end program driver
