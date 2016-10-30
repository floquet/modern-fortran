!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'modules/mod precision definitions.f90'

program runner

    use precision_definitions, only : is, wp, zero

    implicit NONE

    integer ( is )                   :: k = 0
    integer ( is )                   :: numRounds = 0
    integer ( is )                   :: p_cards = ( 1 : 21 )
    real    ( wp )                   :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero

    character ( len = * ), parameter :: me_program = 'program runner'  ! self-identification

    type ( shoes )                   :: myShoe
    type ( players )                 :: dealer, player

        call cpu_time ( cpu_0 ) ! global cpu time - start

            call myShoe % init_random_seed ( mySeed = seedC )
            call myShoe % shuffle ( )
            call myShoe % initial_spectrum ( )
            numRounds = 2

            call cpu_time ( t_0 ) ! local cpu time - start

                do k = 1, numRounds
                end do

            call cpu_time ( t_1 ) ! local cpu time - finish
            write ( *, '( /, "cpu time used for play = ", g0, " seconds", / )' ) t_1 - t_0

        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, '( /, "total cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

!   100 format ( /, "Round ", g0 )

end program runner

! gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 runner.f95