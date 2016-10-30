! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! https://en.wikipedia.org/wiki/SHA-2

module mSHA

    use, intrinsic :: iso_fortran_env,  only : INT32
    implicit none

    integer,        parameter :: ip = INT32
    integer ( ip ), parameter :: two32 = 2147483647_ip ! 2_ip ** 32_ip

contains

! http://stackoverflow.com/questions/11248012/overflow-safe-modular-addition-and-subtraction-in-c
    function mod_safe_232 ( A, B ) result ( myResult )

        integer ( ip ), intent ( in )  :: A, B
        integer ( ip )                 :: myResult
        integer ( ip )                 :: myB

            myResult = A
            if ( B == 0 ) return

            myB  = two32 - B
            if ( A >= B ) then
                myResult = A - myB
            else
                myResult = two32 - myB + A
            end if

    end function mod_safe_232

    function Ma ( A, B, C ) result ( myResult )

        integer ( ip ), intent ( in )  :: A, B,     C
        integer ( ip )                 :: myResult

            myResult = ieor ( iand ( A, B ), iand ( B, C ) )
            myResult = ieor ( iand ( B, C ), myResult )

    end function Ma

    function Ch ( E, F, G ) result ( myResult )

        integer ( ip ), intent ( in )  :: E, F, G
        integer ( ip )                 :: myResult

            myResult = ieor ( iand ( E, F ), iand ( not ( F ), G ) )

    end function Ch

    function Sigma_0 ( A ) result ( myResult )

        integer ( ip ), intent ( in )  :: A
        integer ( ip )                 :: myResult

            myResult = ieor ( rshift ( A, 2 ), rshift ( A, 13 ) )
            myResult = ieor ( myResult, rshift ( A, 22 ) )

    end function Sigma_0

    function Sigma_1 ( A ) result ( myResult )

        integer ( ip ), intent ( in )  :: A
        integer ( ip )                 :: myResult

            myResult = ieor ( rshift ( A, 6 ), rshift ( A, 11 ) )
            myResult = ieor ( myResult, rshift ( A, 25 ) )

    end function Sigma_1

end module mSHA
