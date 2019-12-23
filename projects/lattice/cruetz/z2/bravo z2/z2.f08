! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 10 03

! http://thy.phy.bnl.gov/~creutz/z2/
include 'myIncludes.f08'

program z2

    use mPrecisionDefinitions, only : rp, zero, one
    use mQueries
    use mRandoms
    use mSubroutines
    use mParameters
    use mLattice

    implicit none

    type ( lattice )          :: myLattice
    integer ( ip ), parameter :: numData = 100
    integer ( ip )            :: value = 0

    real    ( rp ) :: results ( 1 : numData, 1 : 2 )
    real    ( rp ) :: beta = zero, dbeta = zero, action = zero
    real    ( rp ) :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero

    integer        :: k

    character ( len = * ), parameter :: myProgram = 'program Z2'  ! self-identification

        write ( * , '( /, "System identifiers..." )' )
        call qLib_write_system ( )  ! host name, compiler version, compilation options, execution command
        write ( * , '( /, "Command line arguments..." )' )
        call harvest_command_line_arguments ( echo = .true. )  ! command line arguments

        call cpu_time ( cpu_0 )   ! global cpu time - start
            dbeta = 0.125
            call init_random_seed_sub ( mySeed = [ ( k, k = 1, 12 ) ] )
            write ( * , * )
            call myLattice % coldstart ( value )

            call cpu_time ( t_0 ) ! specific task - start
!               heat it up
                write ( * , * ) 'Heating lattice...'
                beta = one
                k = 1
                do
                    call myLattice % update ( beta, action )
                    results ( k, : ) = [ beta, action ]
                    write ( * , 200 ) k, [ beta, action ]
                    beta = beta - dbeta
                    if ( beta < zero ) exit
                    k = k + 1
                end do
            call cpu_time ( t_1 ) ! specific task - stop
            write ( *, 100 ) 'heating lattice', t_1 - t_0

            call cpu_time ( t_0 ) ! specific task - start
!               cool it down
                write ( * , * )
                write ( * , * ) 'Cooling lattice...'
                beta = zero
                k = 1
                do
                    call myLattice % update ( beta, action )
                    results ( k, : ) = [ beta, action ]
                    write ( * , 200 ) k, [ beta, action ]
                    beta = beta + dbeta
                    if ( beta > one ) exit
                    k = k + 1
                end do
            call cpu_time ( t_1 ) ! specific task - stop
            write ( *, 100 ) 'cooling lattice', t_1 - t_0

        call cpu_time ( cpu_1 )   ! global cpu time - stop
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop 'successful completion for ' // myProgram // '...'  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', g0, ' = ', E10.3, ' seconds', / )

  200   format ( 'results (', g0, ') = [ ', g0, ', ', g0, ' ]' )

end program z2
