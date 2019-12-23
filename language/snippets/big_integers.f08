program big_integers

    use, intrinsic :: iso_fortran_env,  only : INT16, INT32, INT64
    implicit none

    integer ( kind = INT16 ) :: int_16 = 1_int16
    integer ( kind = INT32 ) :: int_32 = 1_int32
    integer ( kind = INT64 ) :: int_64 = 1_int64

        int_64 = 1024_int64 * 1024_int64 * 1024_int64 * 1024_int64
        print *, 'int_64 = ', int_64

        write ( *, 100 ) 'INT16', INT16
        write ( *, 100 ) 'INT32', INT32
        write ( *, 100 ) 'INT64', INT64

        write ( *, 110 ) 'int16', huge ( int_16 )
        write ( *, 110 ) 'int32', huge ( int_32 )
        write ( *, 110 ) 'int64', huge ( int_64 )

        int_16 =               32767
        int_32 =          2147483647
        int_64 = 9223372036854775807

        write ( *, 120 ) 'int_16', int_16
        write ( *, 120 ) 'int_32', int_32
        write ( *, 120 ) 'int_64', int_64

        write ( *, 130 ) 15, 2**15 - 1
        write ( *, 130 ) 31, 2**31 - 1
        write ( *, 130 ) 63, 2**63 - 1

    100 format ( 'KIND ', g0, ' = ', g0 )
    110 format ( 'huge ( ', g0, ' ) = ', g0 )
    120 format ( 'assignment: ', g0, ' = ', g0 )
    130 format ( '2**', g0, ' - 1 = ', g0 )

end program big_integers
