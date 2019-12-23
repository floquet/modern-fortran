module mQueries

    use mPrecisionDefinitions,  only : ip, ascii
    use mParameters,            only : stdout
    use mBlocks,                only : announce, nBlock
    use mShared,                only : timestamp, open_file_output, alert_io, io_status, io_msg

    implicit none

    contains

        subroutine queries ( io_unit, file_out )

            integer ( ip ),        intent ( in )           :: io_unit
            character ( len = * ), intent ( in ), optional :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_queries ( io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
                call print_queries ( io_unit )
            else
                call print_queries ( io_unit )
            end if

        end subroutine queries

    subroutine print_queries ( myIO )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   print_queries

        use, intrinsic :: iso_fortran_env,  only : compiler_options, compiler_version

        integer ( ip ), intent ( in ) :: myIO
        !character ( kind = ascii, len = * ), intent ( in ) :: file_name

        integer ( ip )  :: uid = 0, gid = 0, pid = 0, host_status = 0

        character ( kind = ascii, len = * ), parameter :: c_options = compiler_options ( )
        character ( kind = ascii, len = * ), parameter :: c_version = compiler_version ( )

        character ( kind = ascii, len = 255 ) :: user_name = ' ', dir_home = ' ', dir_working = ' ', host = ' ', cmd = ' ', &
                                                 device = ' ', uuid = ' '

            call announce ( "System Queries", myIO )

            ! query routines
            call admin       ( dir_home, dir_working )                           ! directory structure
            call tags        ( user_name, uid, pid, gid )                        ! user and process identifications
            call get_command ( cmd )
            call get_uuid    ( uuid )
            host_status =  hostnm ( host )

            ! write identifiers
            write ( myIO, '( "home directory    = ", A, "." )' ) trim ( dir_home )
            write ( myIO, '( "working directory = ", A, "." )' ) trim ( dir_working )
            write ( myIO, '( "date and time     = ", A, "." )' ) trim ( timestamp( ) )
            write ( myIO, * )

            !   write to target file
            write ( myIO, '( "user name                                           = ", g0 )' ) trim ( user_name )
            write ( myIO, '( "numerical user ID of the current process            = ", g0 )' ) uid
            write ( myIO, '( "numerical group ID of the current process           = ", g0 )' ) gid
            write ( myIO, '( "numerical process identifier of the current process = ", g0 )' ) pid
            write ( myIO, * )

            write ( myIO, '( "compiler version   = ", A, "." )' ) trim ( c_version )
            write ( myIO, '( "compiler options   = ", A, "." )' ) trim ( c_options )
            write ( myIO, '( "host system        = ", A, "." )' ) trim ( host )
            write ( myIO, '( "invocation command = ", A, "." )' ) trim ( cmd )
            call TTYNAM ( myIO, device )
            write ( myIO, '( "terminal device ", g0, "  = ", A, " (output file)" )' ) stdout, trim ( device )
            call TTYNAM ( 5, device )
            write ( myIO, '( "terminal device 5  = ", A, "." )' ) trim ( device )
            call TTYNAM ( 6, device )
            write ( myIO, '( "terminal device 6  = ", A, "." )' ) trim ( device )
            write ( myIO, * )

            write ( myIO, '( "universally unique identifier (UUID) = ", A, "." )' ) trim ( uuid )
            write ( myIO, * )

            flush ( myIO )

            call execute_command_line ( 'cp /proc/cpuinfo cpuinfo.txt' )
            call execute_command_line ( 'cp /proc/meminfo meminfo.txt' )

    end subroutine print_queries

    subroutine admin ( dir_home, dir_working )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +      ADMIN

        character ( kind = ascii, len = 255 ), intent ( out ) :: dir_home, dir_working

            call get_environment_variable ( 'HOME', dir_home )  !  home directory
            call getcwd ( dir_working )                         !  current working directory

    end subroutine admin

    subroutine tags ( user_name, uid, pid, gid )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +     TAGS

        integer ( ip )                  :: getuid, getpid, getgid
        integer ( ip ), intent ( out )  ::    uid,    pid,    gid

        character ( kind = ascii, len =  64 ), intent ( out ) :: user_name

        call getlog  ( user_name )  ! moniker
        uid = getuid ( )            ! numerical user ID of the current process
        gid = getgid ( )            ! numerical group ID of the current process
        pid = getpid ( )            ! numerical process identifier of the current process

    end subroutine tags

    subroutine get_uuid ( uuid )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +     GET_UUID

        !     The uuidgen program creates a new universally unique identifier (UUID) using the libuuid(3) library.
        !     The new UUID can reasonably be considered unique among all UUIDs created on the local system, and among UUIDs
        !            created on other systems in the past and in the future.

        integer ( ip ) :: io_unit_uuid = 0

        character ( kind = ascii, len = * ),   parameter      :: file_name_uuid = "temporary_uuid_file"
        character ( kind = ascii, len = 255 ), intent ( out ) :: uuid

        ! Quick way to get a UUID via command line.
        call execute_command_line ( 'uuidgen >> ' // file_name_uuid )

        ! open file containing uuid
        open ( newunit = io_unit_uuid, file = file_name_uuid, iostat = io_status, iomsg = io_msg )

        ! read file
        read ( unit = io_unit_uuid, fmt = '( A )', iostat = io_status, iomsg = io_msg ) uuid

        ! close file
        close ( unit = io_unit_uuid, iostat = io_status, iomsg = io_msg )

        call execute_command_line ( 'rm ' // file_name_uuid ) ! clean up

    end subroutine get_uuid

end module mQueries
