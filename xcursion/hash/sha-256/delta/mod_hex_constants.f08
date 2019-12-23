! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! https://en.wikipedia.org/wiki/SHA-2

module mHexConstants
    implicit none
    integer, parameter :: nBlocks = 8
    ! initial hash value
    ! fractional parts of the square roots of the rst eight primes
    ! https://web.archive.org/web/20130526224224/http://csrc.nist.gov/groups/STM/cavp/documents/shs/sha256-384-512.pdf
    integer, parameter :: H0 ( 1 : nBlocks ) = [ z'6a09e667', z'bb67ae85', z'3c6ef372', z'a54ff53a', z'510e527f', z'9b05688c', &
                                                 z'1f83d9ab', z'5be0cd19' ]
end module mHexConstants
