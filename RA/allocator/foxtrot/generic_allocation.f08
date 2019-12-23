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
                write ( *, "( ' allocation  errmsg = ', g0, '.' )" ) trim ( alloc_message )
                stop 'Fatal error in subroutine myAllocator'
            end if
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
        call myAllocator ( array_real, source_real, lambda )
end program generic_allocation
