!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! begging for recursion
include 'modules/mod precision definitions.f90'

program lister

    use precision_definitions, only : is, wp, zero

    implicit NONE

    integer ( is ), parameter        :: n = 3
    integer ( is )                   :: i = 0, j = 0, k = 0
    integer ( is )                   :: A ( 1 : n,    1 : 1 )
    integer ( is )                   :: B ( 1 : n**2, 1 : 2 )
    integer ( is )                   :: C ( 1 : n**3, 1 : 3 )
    integer ( is )                   :: D ( 1 : n**4, 1 : 4 )
    real    ( wp )                   :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero

    character ( len = * ), parameter :: me_program = 'program lister'  ! self-identification

        call cpu_time ( cpu_0 ) ! global cpu time - start

            call cpu_time ( t_0 ) ! local cpu time - start

                A ( : , 1 ) = [ ( k, k = 1, n ) ]
                write ( *, 100 ) 'A', A ( : , 1 )

                i = 0
                do k = 1, n
                    do j = 1, n
                        i = i + 1
                        B ( i, 1 ) = k
                        B ( i, 2 ) = A ( j, 1 )
                        write ( *, 110 ) i, B ( i, : )
                    end do
                end do

                i = 0
                do k = 1, n
                    do j = 1, n**2
                        i = i + 1
                        C ( i, 1 ) = k
                        C ( i, 2 : 3 ) = B ( j, 1 : 2 )
                        write ( *, 120 ) i, C ( i, : )
                    end do
                end do

                i = 0
                do k = 1, n
                    do j = 1, n**3
                        i = i + 1
                        D ( i, 1 ) = k
                        D ( i, 2 : 4 ) = C ( j, : )
                        write ( *, 130 ) i, D ( i, : )
                    end do
                end do

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, '( /, "cpu time used for play = ", g0, " seconds", / )' ) t_1 - t_0

        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, '( /, "total cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

    100 format ( /, g0, ':', 1000g0 )
    110 format ( 'B (', I3,' ) = ', 2I2 )
    120 format ( 'C (', I3,' ) = ', 3I2 )
    130 format ( 'D (', I3,' ) = ', 9I2 )

end program lister

! gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 runner.f95