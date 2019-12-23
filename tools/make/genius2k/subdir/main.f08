program main

    use myModule
    implicit none

    real :: z

    interface
        real function y ( z )
            real, intent ( in ) :: z
        end function y
    end interface

        write ( * , '( "y = ", g0, / )' ) y ( x )

        call subroutine1 ( x, z )
        write ( * , '( "subroutine1 = ", g0, / )' ) z

    stop "Execution complete for main.f08"

end program main
