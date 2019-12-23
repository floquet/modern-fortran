! Modern Fortran, Figure 13.3, p. 263
program f133
    implicit none

    type t
        PROCEDURE ( obp ), pointer, pass ( x ) :: p
    end type t

    abstract interface
        subroutine obp ( w, x )
            import      :: t
            integer     :: w
            class ( t ) :: x
        end subroutine obp
    end interface

    type ( t ) :: a

        a % p => my_obp_sub

        call a % p ( 32 )  ! equivalent to 'call my_obp_sub ( 32, a )

end program f133














































































