! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! https://en.wikipedia.org/wiki/SHA-2

module mSHA

    use, intrinsic :: iso_fortran_env,  only : INT64
    implicit none

    integer,        parameter :: ip = INT64
    integer ( ip ), parameter :: two32 = 2147483647_ip ! 2_ip ** 32_ip

contains

    function Ma ( A, B, C ) result ( myResult )

        integer ( ip ), intent ( in )  :: A, B, C
        integer ( ip )                 :: myResult

        integer ( ip )                 :: AandB, AandC, BandC

            AandB = iand ( A, B )
            AandC = iand ( A, C )
            BandC = iand ( B, C )

            myResult = mod ( AandB + AandC,    two32 )
            myResult = mod ( myResult + BandC, two32 )

    end function Ma

    function Ch ( E, F, G ) result ( myResult )

        integer ( ip ), intent ( in )  :: E, F, G
        integer ( ip )                 :: myResult

            myResult = mod ( iand ( E, F ) + iand ( not ( F ), G ), two32 )

    end function Ch

end module mSHA
