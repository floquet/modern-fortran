! http://www.training.prace-ri.eu/uploads/tx_pracetmo/coarrayvideo1.pdf
program prime_density

    use mPrimeTools,    only : numPrimes
    implicit none

    integer, parameter                                :: nimages = 8
    integer, parameter                                :: n = nimages * 10**7
    integer, dimension ( nimages ), codimension [ * ] :: nprimes
    !integer, dimension ( nimages )  :: nprimes [ * ]
    integer                                           :: start, end

    real :: density

        start = ( this_image ( ) - 1 ) * n / num_images ( ) + 1
        end = start + n / num_images ( ) - 1
        nprimes ( this_image ( ) ) [ 1 ] = numPrimes ( start, end )

        sync all

        if ( this_image ( ) == 1 ) then
            nprimes ( 1 ) = sum ( nprimes )
            density = real ( nprimes ( 1 ) ) / n
            print *,"Calculating prime density on ", num_images ( )," images"
            ! print *, numPrimes ( 1 ), ' primes in first ', n, ' numbers'
            write ( *, '( " density is ", 2P, f5.2, "%" )') density
            write ( *, '( " asymptotic theory gives ", 2P, f5.2, "%" )') 1.0 / ( log ( real ( n ) ) - 1.0 )
        end if

end program prime_density
