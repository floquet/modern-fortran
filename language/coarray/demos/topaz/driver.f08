program driver

    use mPrimeTools, only : numPrimes
    implicit none

    integer :: a, b, nPrime

        a = 3
        b = 72
        nPrime = numPrimes ( a, b )
        write ( * , 100 ) nPrime, a, b
    100 format ( 'There are ', g0, ' primes between ', g0, ' and ', g0, /, 'Expected answer = 20.' )

end program driver

! Muntz-Szasz:primes dantopa$ date
! Sat Apr  9 13:27:16 CDT 2016
! Muntz-Szasz:primes dantopa$ pwd
! /Users/dantopa/github/fortran/coarray/demos/primes
! Muntz-Szasz:primes dantopa$ cn
! Muntz-Szasz:primes dantopa$ make
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_Prime_Tools.o mod_Prime_Tools.f08
! gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -o driver.o driver.f08
! gfortran -g -o driver driver.o mod_Prime_Tools.o
! Muntz-Szasz:primes dantopa$ cafrun -np 4 ./driver
! There are 19 primes between 3 and 72
! Expected answer = 20.
! There are 19 primes between 3 and 72
! Expected answer = 20.
! There are 19 primes between 3 and 72
! Expected answer = 20.
! There are 19 primes between 3 and 72
! Expected answer = 20.
