program paths

    implicit none

    integer :: status = 0, rank = 99, io_stat = 0
    character ( len = 512 ) :: host_name = '', machine_name = '', yo = ''
    character ( len = 512 ) :: str_rank = '', myName = '', myPath = '', myGit = '', io_msg = ''

        status = hostnm ( host_name )
        machine_name = 'machine_name'
        host_name = 'host_name'
        call get_environment_variable ( "myGit", myGit )
        call get_environment_variable ( "pwd", myName )

        write ( unit = str_rank, fmt = '( I5.5 )', iostat = io_stat, iomsg = io_msg ) rank
        if ( io_stat /= 0 ) then
            write ( *, '( /, "Error writing rank = ", g0," to string variable str_rank (len = 512 )" )' ) rank
            write ( *, '( "iomsg = ",  g0, "." )' ) trim ( io_msg )
            write ( *, '( "iostat = ", g0, "." )' ) io_stat
        end if
        yo = trim ( machine_name ) // "_" // trim ( host_name )
        myPath = trim ( myGit ) // 'transporter/' // trim ( yo )
        write ( * , '( "yo     = ", g0 )') trim ( yo )
        write ( * , '( "myPath = ", g0 )') trim ( myPath )
        call execute_command_line ( 'mkdir ' // trim ( myPath) )
        myName = trim ( myPath ) // 'transporter/' // trim ( yo ) // "_" // trim ( str_rank ) // "_paths.txt"
        write ( * , '( "myName = ", g0 )') trim ( myName )

        myPath = trim ( myGit ) // 'transporter' // trim ( yo )
        write ( * , '( "mkdir = ", g0 )')  trim ( myPath) // 'info'
        write ( * , '( "myName = ", g0 )') trim ( myPath ) // '/info/info_' // trim ( yo ) // '_cpu.txt'
        write ( * , '( "myName = ", g0 )') trim ( myPath ) // '/info/info_' // trim ( yo ) // '_mem.txt'


        stop !'successful completion for cpu.f08...'

end program paths
