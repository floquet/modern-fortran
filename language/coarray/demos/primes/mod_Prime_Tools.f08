module mPrimeTools

    implicit none

contains

    function isPrime ( n ) result ( primeQ )

        integer, intent ( in )  :: n
        logical                 :: primeQ

        integer                 :: m

            primeQ = .false.

            if ( n < 2 ) return

            do m = 2, floor ( sqrt ( real ( n ) ) )
                if ( mod ( n, m ) == 0 ) return
            end do

            primeQ = .true.

    end function isPrime

    function numPrimes ( a, b ) result ( counter )

        integer, intent ( in )  :: a, b  ! check the domain [a, b]
        integer                 :: counter

        integer                 :: start, k

            counter = 0

            ! only check odd numbers > 2
            start = a
            if ( mod ( a, 2 ) == 0 ) start = a + 1

            do k = start, b, 2
                if ( isPrime ( k ) ) counter = counter + 1
            end do

    end function numPrimes

end module mPrimeTools
