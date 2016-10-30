! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 22

module mLegendre

    use mPrecisionDefinitions, only : ip, rp

    implicit none

    contains

!       ===============================================================================================                   myFunction

        pure function norm_legendre ( k ) result ( output )

            integer ( ip ), intent ( in ) :: k
            real ( rp )                   :: output

                output = 2 * ( k + 1 ) / 2

            return

        end subroutine norm_legendre

end module mLegendre
