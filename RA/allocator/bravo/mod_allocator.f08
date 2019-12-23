module mAllocator

    !use mPrecisionDefinitions,  only : sp, dp, qp
    use, intrinsic :: iso_fortran_env, only : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

    implicit none

contains

    subroutine allocator ( myArray, lambda )
        class ( * ), intent ( inout ) :: myArray ( : )
        integer,     intent ( in )    :: lambda

        integer                 :: alloc_status  = 0
        character ( len = 512 ) :: alloc_message = '', myType = ''

            select type ( myArray )
                type is ( real ( REAL32 ) )
                    myType = 'REAL32'
                    allocate ( myArray ( 1 : lambda ), stat = alloc_status, errmsg = alloc_message )
                    myArray ( : ) = 0.0_REAL32
                    print *, 'myArray = ', myArray
                    print *, 'size ( myArray ) = ', size ( myArray )
                type is ( real ( REAL64 ) )
                    myType = 'REAL64'
                    allocate ( myArray ( 1 : lambda ), stat = alloc_status, errmsg = alloc_message )
                    myArray ( : ) = 0.0_REAL64
                class default
                    print *, 'whiff'
                    stop 'Fatal error: type not recognized'
            end select

            if ( alloc_status /= 0 ) then
                write ( *, 100 ) lambda, myType
                write ( *, 110 ) alloc_status
                write ( *, 120 ) trim ( alloc_message )
                stop 'Fatal error in subroutine allocator'
            end if

        100 format ( /, 'Error allocating memory for ', g0, ' elements of type ', g0,'.' )
        110 format (    '  stat = ', g0, '.' )
        120 format (    '  errmsg = ', g0, '.' )

    end subroutine allocator

end module mAllocator
