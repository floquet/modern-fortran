! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include '../modules/mod precision_definitions.f95'
include 'myModules/mod data.f95'
include 'myModules/mod solution_normal.f95'

program normal_equations

    use iso_fortran_env
    use precision_definitions, only : is, wp, zero
    use mQueries

    implicit none

    type ( simulations )             :: mySimulation

    real    ( wp )                   :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero
 
    integer ( is )                   :: numArgsInput
    integer ( is )                   :: c_arg_list ( 1 : numArgsMax )

    character ( len = * ), parameter :: myProgram = 'program normal_equations'  ! self-identification

    call ping_system ( )
    call harvest_command_line_arguments ( c_arg_list, numArgsInput, .true. )

        call cpu_time ( cpu_0 ) ! global cpu time - start

!             call cpu_time ( t_0 ) ! specific task - start
!             call cpu_time ( t_1 ) ! specific task - stop
!             write ( *, 100 ) 'testing simulation', t_1 - t_0
 
        call cpu_time ( cpu_1 ) ! global cpu time - finish
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop "successful completion for " // myProgram // "."  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', g0, ' = ', g0, ' seconds', / )

end program normal_equations
