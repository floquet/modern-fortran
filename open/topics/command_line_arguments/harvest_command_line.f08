program harvest_command_line

    implicit none

    integer :: nCommandLineArguments = 0, LenCommand = 0
    integer :: k = 0, gca_status = 0

    character ( len = 128 ) :: launch_command = '', thisArgument = ''

        call get_command ( command = launch_command )
        nCommandLineArguments = command_argument_count ( )

        write ( *, 100 ) trim ( launch_command ), nCommandLineArguments

        ! loop over arguments
        do k = 1, nCommandLineArguments
            call get_command_argument ( number = k, value = thisArgument, length = LenCommand, status = gca_status )
            write ( *, 110 ) k, trim ( thisArgument ), LenCommand
        end do ! k

        stop 'successful completion for harvest_command_line...'

    100 format ( /, 'The launch command was ', g0, '.', /, 'There are ', g0, ' arguments.' )
    110 format ( 'command line argument ', g0, ' is "', g0, '", length = ', g0 )

end program harvest_command_line
