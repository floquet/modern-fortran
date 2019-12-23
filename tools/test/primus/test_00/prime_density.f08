! http://www.training.prace-ri.eu/uploads/tx_pracetmo/coarrayvideo1.pdf
program prime_density

    use mPrimeTools,    only : numPrimes
    implicit none

    integer, parameter                                :: nimages = 16
    integer, parameter                                :: n = nimages * 2**15
    integer, dimension ( nimages ), codimension [ * ] :: nprimes
    integer                                           :: start, end

    real :: density, t0, t1, tdelta

        if ( this_image ( ) == 1 ) then
            call cpu_time ( t0 )
        end if

        start = ( this_image ( ) - 1 ) * n / num_images ( ) + 1
        end = start + n / num_images ( ) - 1

        nprimes ( this_image ( ) ) [ 1 ] = numPrimes ( start, end )
        write ( *, 200 ) this_image ( ), start, end, nprimes ( this_image ( ) ) [ 1 ]

        sync all

        if ( this_image ( ) == 1 ) then
            nprimes ( 1 ) = sum ( nprimes )
            density = real ( nprimes ( 1 ) ) / real ( n )
            call cpu_time ( t1 )
            tdelta = t1 - t0

            write ( *, 100 ) num_images ( )
            write ( *, 110 ) nprimes ( 1 ), n
            write ( *, 120 ) density
            write ( *, 130 ) 1.0 / ( log ( real ( n ) ) - 1.0 )
            write ( *, 140 ) tdelta
        end if

    100 format ( /, 'Calculating prime density on ', g0, ' images' )
    110 format ( g0, ' primes in first ', g0, ' numbers' )
    120 format ( f5.2, '% - density computed' )
    130 format ( f5.2, '% - density asypmtotic' )
    140 format ( g0, ' cpu time' )

    200 format ( 'image = ', g0, ', start = ', g0, ', end = ', g0, ', number of primes = ', g0 )

end program prime_density

!  15:56 ITL-DTOPA-MP rditldmt $  make
! caf -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib -o mod_Prime_Tools.o mod_Prime_Tools.f08
! caf -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib -o prime_density.o prime_density.f08
! caf -g -o prime_density mod_Prime_Tools.o prime_density.o
!  15:56 ITL-DTOPA-MP rditldmt $ cafrun -np 8 ./prime_density
! image = 2, start = 4097, end = 8192, number of primes = 464
! image = 1, start = 1, end = 4096, number of primes = 563
! image = 8, start = 28673, end = 32768, number of primes = 388
! image = 4, start = 12289, end = 16384, number of primes = 431
! image = 6, start = 20481, end = 24576, number of primes = 413
! image = 3, start = 8193, end = 12288, number of primes = 441
! image = 7, start = 24577, end = 28672, number of primes = 399
! image = 5, start = 16385, end = 20480, number of primes = 412
!
! Calculating prime density on 8 images
! 3511 primes in first 32768 numbers
!  0.11% - density computed
!  0.11% - density asypmtotic
! 0.624999404E-03 cpu time
!  15:56 ITL-DTOPA-MP rditldmt $ date
! Fri Apr 15 15:56:45 CDT 2016
!  15:56 ITL-DTOPA-MP rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/demos/vm_primes
