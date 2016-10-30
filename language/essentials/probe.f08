program probe

    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64 

    implicit none

        write ( * , 100 ) REAL32
        write ( * , 110 ) REAL64

        stop 'successful run for probe...'

    100 format ( /, 'REAL32 = ', g0 )
    110 format (    'REAL64 = ', g0, / )

end program probe 
