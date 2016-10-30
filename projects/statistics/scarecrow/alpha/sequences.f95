!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'modules/mod precision definitions.f90'

program sequences

    use precision_definitions, only : is, wp, zero

    implicit NONE

    integer ( is ), parameter        :: n = 3, n2 = n**2
    integer ( is )                   :: j = 0, k = 0
    integer ( is )                   :: base_1 ( 1 : n ) = 0, &
                                        base_2 ( 1 : 2, 1 : n**2 ) = 0, &
                                        base_3 ( 1 : 3, 1 : n**3 ) = 0
    integer ( is )                   :: new2   ( 1 : n**2 ) = 0
    real    ( wp )                   :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero

    character ( len = * ), parameter :: me_program = 'program sequences'  ! self-identification

    interface joiner
        function join ( k, old_List, len_list ) result ( new_element )
            use precision_definitions, only : is
            implicit NONE
            integer ( is ), intent ( in )    :: k
            integer ( is ), intent ( in )    :: old_list ( : )
            integer ( is ), intent ( in )    :: len_list
            integer ( is )                   :: new_element ( : )
        end function join
    end interface ! joiner

        call cpu_time ( cpu_0 ) ! global cpu time - start

            base_1 = [ (j, j = 1, n ) ]

            call cpu_time ( t_0 ) ! local cpu time - start

                base_2 = [ ( join ( k, base_1, 3 ), k = 1, n**2 ) ]

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, '( /, "cpu time used for play = ", g0, " seconds", / )' ) t_1 - t_0

        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, '( /, "total cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

!   100 format ( /, "Round ", g0 )

end program sequences


function join ( k, old_List, len_list ) result ( new_element )

    use precision_definitions, only : is

    implicit NONE

    integer ( is ), intent ( in )    :: k
    integer ( is ), intent ( in )    :: old_list ( : )
    integer ( is ), intent ( in )    :: len_list
    integer ( is )                   :: new_element ( : )

    integer ( is )                   :: j = 0

        new_element ( 1 ) = k
        do j = 2, len_list + 1
            do k = 1, len_list
                new_element ( j ) = old_list ( k )
            end do
        end do
        new_element ( 1 ) = k

end function join

! gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 runner.f95