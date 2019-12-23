module mAllocator

    !use mPrecisionDefinitions,  only : sp, dp, qp
    use, intrinsic :: iso_fortran_env, only : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

    implicit none

contains

    subroutine allocator ( myArray )
        class ( * ), intent ( in ) :: myArray ( : )

            select type ( myArray )
                type is ( real ( REAL32 ) )
                    print *, 'type single precision'
                type is ( real ( REAL64 ) )
                    print *, 'type double precision'
                type is ( real ( REAL128 ) )
                    print *, 'type quadruple precision'
                type is ( integer ( INT8 ) )
                    print *, 'integer 8'
                type is ( integer ( INT16 ) )
                    print *, 'integer 16'
                type is ( integer ( INT32 ) )
                    print *, 'integer 32'
                type is ( integer ( INT64 ) )
                    print *, 'integer 64'
                class default
                    print *, 'whiff'
            end select

    end subroutine allocator

end module mAllocator
