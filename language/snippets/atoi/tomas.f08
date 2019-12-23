program tomas

    use, intrinsic :: iso_fortran_env,  only : INT8, INT16, INT32, INT64
    use mModConverters,                 only : str_to_int_def, str_to_INT8
    implicit none

    character ( len = 512 ) :: mystring = '1'
    integer :: mynumber = 0
    integer ( INT8 ) :: number8 = 0

        if ( COMMAND_ARGUMENT_COUNT( ) > 0 ) then
            call getarg ( 1, mystring )
        else
            mystring = '1234567'
        end if

        write ( *, 110 ) mynumber, huge ( mynumber )
        mynumber = str_to_int_def ( mystring )
        write ( *, 100 ) 'str_to_int_def', trim( mystring ), mynumber

        write ( *, 110 ) number8, huge ( number8 )
        number8 = str_to_int8 ( mystring )
        write ( *, 100 ) 'str_to_int8', trim( mystring ), mynumber

        write ( *, 110 ) number8, huge ( number8 )
        number8 = str_to_int8 ( mystring )
        write ( *, 100 ) 'str_to_int8', trim( mystring ), mynumber


    100 format ( g0, ' - input: ', g0, '; output: ', g0 )
    110 format ( 'number = ', g0, '; largest value = ', g0 )

end program tomas
