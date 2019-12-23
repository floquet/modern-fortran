module mPrimeNumber

    implicit none

contains

    subroutine prime_number ( n, id, p, total )

<<<<<<< HEAD
    !*****************************************************************************80
    !
    !! PRIME_NUMBER returns a part of the number of primes between 1 and N.
    !
    !  Discussion:
    !
    !    In order to divide the work up evenly among P processors, processor
    !    ID starts at 2+ID and skips by P.
    !
    !    A naive algorithm is used.
    !
    !    Mathematica can return the number of primes less than or equal to N
    !    by the command PrimePi[N].
    !
    !                N  PRIME_NUMBER
    !
    !                1           0
    !               10           4
    !              100          25
    !            1,000         168
    !           10,000       1,229
    !          100,000       9,592
    !        1,000,000      78,498
    !       10,000,000     664,579
    !      100,000,000   5,761,455
    !    1,000,000,000  50,847,534
    !
=======
>>>>>>> 9bb5e4725597a709f3c8f265e56bae45176185a5
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the maximum number to check.
    !
    !    Input, integer ( kind = 4 ) ID, the ID of this process,
    !    between 0 and P-1.
    !
    !    Input, integer ( kind = 4 ) P, the number of processes.
    !
    !    Output, integer ( kind = 4 ) TOTAL, the number of prime numbers up to N,
    !    starting at 2+ID and skipping by P.
<<<<<<< HEAD
    !
    implicit none
=======
>>>>>>> 9bb5e4725597a709f3c8f265e56bae45176185a5

    integer :: i, id, j, n, p, prime, total

        total = 0

<<<<<<< HEAD
=======
        print *, 'do i = 2 + ',id, ', ', n, ', ', p
>>>>>>> 9bb5e4725597a709f3c8f265e56bae45176185a5
        do i = 2 + id, n, p
            prime = 1
            do j = 2, i - 1
                if ( mod ( i, j ) == 0 ) then
                    prime = 0
                    exit
                end if
            end do
            total = total + prime
        end do

<<<<<<< HEAD
    end subroutine prime_number

end module mPrimeNumber
=======
            print *, 'n = ', n, '; id = ', id, '; p = ', p, '; total = ', total
            print *, 'primes found = ', total

    end subroutine prime_number

end module mPrimeNumber

! Mathematica results
! number of primes <= 2 = 1
! number of primes <= 4 = 2
! number of primes <= 8 = 4
! number of primes <= 16 = 6
! number of primes <= 32 = 11
! number of primes <= 64 = 18
! number of primes <= 128 = 31
! number of primes <= 256 = 54
! number of primes <= 512 = 97
! number of primes <= 1024 = 172
! number of primes <= 2048 = 309
! number of primes <= 4096 = 564
! number of primes <= 8192 = 1028
! number of primes <= 16384 = 1900
! number of primes <= 32768 = 3512
! number of primes <= 65536 = 6542
! number of primes <= 131072 = 12251
! number of primes <= 262144 = 23000
! number of primes <= 524288 = 43390
! number of primes <= 1048576 = 82025
! number of primes <= 2097152 = 155611
>>>>>>> 9bb5e4725597a709f3c8f265e56bae45176185a5
