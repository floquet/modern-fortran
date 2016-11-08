program harvest_command_line

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options

    implicit none

    integer :: nCommandLineArguments = 0, LenCommand = 0
    integer :: k = 0, gca_status = 0

    character ( len = 128 ) :: launch_command = '', thisArgument = ''

            ! harvest the launch command; e.g. ./harvest_command_line mydata thesemodules title_for_run
            call get_command ( command = launch_command )
            nCommandLineArguments = command_argument_count ( ) ! how many arguments in command line

            write ( *, 100 ) trim ( launch_command ), nCommandLineArguments ! echo launch command

            ! loop over arguments and print out
            do k = 1, nCommandLineArguments
                call get_command_argument ( number = k, value = thisArgument, length = LenCommand, status = gca_status )
                write ( *, 110 ) k, trim ( thisArgument ), LenCommand
            end do ! k

        write ( *, 200 )
        write ( *, 200 ) 'compiler version: ', compiler_version ()
        write ( *, 200 ) 'compiler options: ', compiler_options ()
        write ( *, 200 )

        stop 'successful completion for harvest_command_line...'

    100 format ( /, 'The launch command was ', g0, '.', /, 'There are ', g0, ' arguments.' )
    110 format ( 'command line argument ', g0, ' is "', g0, '", length = ', g0 )

    200 format ( g0, g0 )

end program harvest_command_line
