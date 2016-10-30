module tester

    use precision_definitions, only : is, wp, one, zero

    implicit none

        real ( wp ), public               :: x = one
        real ( wp ), private, allocatable :: y ( : )

    contains

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine task (  )

            x = x + 3
            write ( *, '( "x = ", g0 )' ) x

        end subroutine task

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine inflate (  )

!        use precision_definitions, only : is, one, zero

        implicit none

        integer ( is )          :: alloc_status
        character ( len = 255 ) :: alloc_msg = " "

            allocate ( y ( 1 : 3 ), stat = alloc_status, errmsg = alloc_msg )
            if ( alloc_status /= 0 ) then
                write ( *, '( /, "Error allocating memory for real ( wp ) array y" )' )
                write ( *, '(    "requested size is 3" )' )
                write ( *, '(    "stat = " )'   ) alloc_status
                write ( *, '(    "errmsg = " )' ) alloc_msg
                write ( *, '(    "Fatal error - ending run inside subroutine inflate", / )' )
                stop
            end if
            y = [ one, zero, 3 * one ]
            write ( *, '( "y = ", g0 )' ) y

        end subroutine inflate

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine handoff (  )

        implicit none

            write ( *, '( "inside handoff: y = ", g0 )' ) y

        end subroutine handoff

end module tester
