! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! https://en.wikipedia.org/wiki/SHA-2

module mSHA

    use, intrinsic :: iso_fortran_env,  only : INT32
    implicit none

    integer,        parameter :: ip = INT32
    integer ( ip ), parameter :: two32m1 = 2147483647_ip ! 2_ip ** 32_ip  -  1

    integer, private :: j, k

contains

! http://stackoverflow.com/questions/11248012/overflow-safe-modular-addition-and-subtraction-in-c
    function mod_safe_add_232 ( A, B ) result ( myResult )

        integer ( ip ), intent ( in )  :: A, B
        integer ( ip )                 :: myResult
        integer ( ip )                 :: myB

            myResult = A
            if ( B == 0 ) return

            myB  = two32m1 - B
            if ( A >= B ) then
                myResult = A - myB
            else
                myResult = two32m1 - myB + A
            end if

    end function mod_safe_add_232

    function Maj ( A, B, C ) result ( myResult )

        integer ( ip ), intent ( in )  :: A, B, C
        integer ( ip )                 :: myResult

            myResult = ieor ( iand ( A, B ), iand ( A, C ) )
            myResult = ieor ( iand ( B, C ), myResult )

    end function Maj

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

    function small_sigma_0 ( A ) result ( myResult )

        integer ( ip ), intent ( in )  :: A
        integer ( ip )                 :: myResult

            myResult = ieor ( rshift ( A, 7 ), rshift ( A, 18 ) )
            myResult = ieor ( myResult, ishftc ( A, 3 ) )

    end function small_sigma_0

    function small_sigma_1 ( A ) result ( myResult )

        integer ( ip ), intent ( in )  :: A
        integer ( ip )                 :: myResult

            myResult = ieor ( rshift ( A, 17 ), rshift ( A, 19 ) )
            myResult = ieor ( myResult, ishftc ( A, 10 ) )

    end function small_sigma_1

    function compress ( a, b, c, d, e, f, g, h ) result ( register )
        integer ( ip ), intent ( in )  :: a, b, c, d, e, f, g, h
        integer ( ip ), intent ( out ) :: register
        integer ( ip )                 :: T1, T2

            do j = 0, 63
                T1 = h + Sigma_1 ( e ) + Ch ( e, f, g ) + W
                T2 = Sigma_0 ( a ) + Maj ( a, b, c )
                h  = g
                g  = f
                f  = e
                e  = d + T1
                d  = c
                c  = b
                b  = a
                a  = T1 + T2
            end do

    end function compress ( )

end module mSHA
