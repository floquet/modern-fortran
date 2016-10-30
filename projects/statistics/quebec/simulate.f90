!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'modules/mod precision definitions.f90'
include 'modules/mod house parameters.f90'
include 'modules/mod shared variables.f90'
include 'modules/mod pip.f90'
include 'modules/mod suit.f90'
include 'modules/mod deck.f90'
include 'modules/mod shoe.f90'
include 'modules/mod hand.f90'
include 'modules/mod player.f90'
include 'modules/mod rules.f90'
include 'modules/mod table.f90'
include 'modules/mod seeds.f90'

program simulate

    use precision_definitions, only : wp, zero!, is
    use mTable
    use seeds
    use mHand

    implicit NONE

    integer ( is )                   :: k = 0
    real    ( wp )                   :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero

    character ( len = * ), parameter :: me_program = 'program simulate'  ! self-identification

    type ( tables )                  :: myTable
    type ( hands )                   :: myHand

        call cpu_time ( cpu_0 ) ! global cpu time - start

        call cpu_time ( t_0 ) ! local cpu time - start

            call myTable % set_table ( nPlayers = 5, bank = chip * [ 100, 0, 200, 10, 20 ], rule_set = [ 1, 2, 1, 1, 2 ] )
!           call myTable % print_set_table ( )

            call myTable % myShoe % init_random_seed ( mySeed = seedC )
            call myTable % shuffle ( )

            call myHand % test_rules ( )

            k = 1
            write ( *, 100 ) k
            call myTable % play_one_round ( )
            k = k + 1
            write ( *, 100 ) k
            call myTable % play_one_round ( )

        call cpu_time ( t_1 ) ! local cpu time - finish
        write ( *, '( /, "cpu time used for play = ", g0, " seconds", / )' ) t_1 - t_0

        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, '( /, "total cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

    100 format ( /, "Round ", g0 )

end program simulate

!gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds simulate.f90 