! https://rosettacode.org/wiki/Bitwise_operations#Fortran
program bits_rosetta
    implicit none

        call bitwise ( 14, 3 )

 contains

    subroutine bitwise ( a, b )
        implicit none
        integer, intent ( in ) :: a, b
        integer                :: c
        character ( len = * ), parameter :: fmt1 = ' ( 2 ( a, i10 ) ) '
        character ( len = * ), parameter :: fmt2 = ' ( 3 ( a, b32.32 ), i20 ) '

            write ( *, fmt1 ) 'input a = ', a, ' b = ',  b
            write ( *, fmt2 ) 'and : ',   a, ' &  ', b,' = ', iand   ( a, b ), iand ( a, b )
            write ( *, fmt2 ) 'or  : ',   a, ' |  ', b,' = ', ior    ( a, b ), ior ( a, b )
            write ( *, fmt2 ) 'xor : ',   a, ' ^  ', b,' = ', ieor   ( a, b ), ieor ( a, b )
            write ( *, fmt2 ) 'lsh : ',   a, ' << ', b,' = ', shiftl ( a, b ), shiftl ( a, b )  !since F2008, otherwise use ishft ( a, abs(b ) )
            write ( *, fmt2 ) 'rsh : ',   a, ' >> ', b,' = ', shiftr ( a, b ), shiftr ( a, b )  !since F2008, otherwise use ishft ( a, -abs(b ) )
            write ( *, fmt2 ) 'not : ',   a, ' ~  ', b,' = ', not    ( a ),    not ( a )
            write ( *, fmt2 ) 'rot : ',   a, ' r  ', b,' = ', ishftc ( a,     -abs ( b ) ), ishftc ( a, -abs( b ) )

            c = b
            call mvbits ( a, 0, 3, c, 4 )  ! copy a sequence of 4 bits from a starting at bit 2 into b starting at bit 0
            write ( *, fmt2 ) 'mv  : ',   a, ' mv ', b,' = ', c, c

    end subroutine bitwise

end program bits_rosetta

! dantopa@Muntz-Szasz.local:rosetta $ date
! Sat Jun  4 13:29:59 CDT 2016
! dantopa@Muntz-Szasz.local:rosetta $ pwd
! /Users/dantopa/Documents/hpc/fortran/xcursion/hash/sha-256/rosetta
! dantopa@Muntz-Szasz.local:rosetta $
! dantopa@Muntz-Szasz.local:rosetta $ gf bits_rosetta
! dantopa@Muntz-Szasz.local:rosetta $ ./bits_rosetta
! input a =         14 b =          3
! and : 00000000000000000000000000001110 &  00000000000000000000000000000011 = 00000000000000000000000000000010                   2
! or  : 00000000000000000000000000001110 |  00000000000000000000000000000011 = 00000000000000000000000000001111                  15
! xor : 00000000000000000000000000001110 ^  00000000000000000000000000000011 = 00000000000000000000000000001101                  13
! lsh : 00000000000000000000000000001110 << 00000000000000000000000000000011 = 00000000000000000000000001110000                 112
! rsh : 00000000000000000000000000001110 >> 00000000000000000000000000000011 = 00000000000000000000000000000001                   1
! not : 00000000000000000000000000001110 ~  00000000000000000000000000000011 = 11111111111111111111111111110001                 -15
! rot : 00000000000000000000000000001110 r  00000000000000000000000000000011 = 11000000000000000000000000000001         -1073741823
! mv  : 00000000000000000000000000001110 mv 00000000000000000000000000000011 = 00000000000000000000000000000011                   3
