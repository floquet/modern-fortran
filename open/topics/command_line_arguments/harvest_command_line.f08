program harvest_command_line

    implicit none

    integer :: nCommandLineArguments = 0

    character ( len = 128 ) :: launch_command = ''

        call get_command ( launch_command )
        nCommandLineArguments = command_argument_count ( )

        write ( *, 100 ) trim ( launch_command ), nCommandLineArguments

        stop 'successful completion for harvest_command_line...'

    100 format ( /, 'The launch command was ', g0, '.', /, 'There are ', g0, ' arguments.' )

end program harvest_command_line
