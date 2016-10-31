program StorageSize

    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64, REAL128

    implicit none

    real ( REAL64 ), dimension ( 1 : 5 ) :: array = 0.0_REAL64

        write ( *, 100 ) 'REAL32',  storage_size ( 1.0_REAL32 )
        write ( *, 100 ) 'REAL64',  storage_size ( 1.0_REAL64 )
        write ( *, 100 ) '1.0_REAL64',  storage_size ( 1.0, REAL64 )
        write ( *, 100 ) 'REAL128', storage_size ( 1.0_REAL128 )
        write ( *, 100 ) 'array', storage_size ( array )

    100 format ( 'type ', g0, ' storage size in bits = ', g0 )

    stop 'successful run of program storage_size...'

end program StorageSize

! dantopa@Muntz-Szasz:scratch $ date
! Sun Oct 30 21:54:19 CDT 2016
! dantopa@Muntz-Szasz:scratch $ pwd
! /Users/dantopa/Documents/hpc/modern Fortran/benchmark/memory/steppers/foxtrot/scratch
! dantopa@Muntz-Szasz:scratch $ echo $gflags
! -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5
! dantopa@Muntz-Szasz:scratch $ gf StorageSize
! dantopa@Muntz-Szasz:scratch $ ./StorageSize
! type REAL32 storage size in bits = 32
! type REAL64 storage size in bits = 64
! type 1.0_REAL64 storage size in bits = 32
! type REAL128 storage size in bits = 128
! type array storage size in bits = 64
! STOP successful run of program storage_size...
