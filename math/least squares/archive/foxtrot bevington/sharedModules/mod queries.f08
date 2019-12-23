! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 21

!   qLib_ping_system ( )
!   qLib_write_system ( )
!   harvest_command_line_arguments ( echo )

module mQueries

    use iso_fortran_env, only : compiler_options, compiler_version
    use mPrecisionDefinitions, only : ip!, wp, zero, one

    implicit none

    integer, private                   :: kQ
    integer ( ip ), parameter, private :: numArgsMax  = 5

    integer ( ip )                     :: numArgsInput
    integer ( ip )                     :: c_arg_lipt ( 1 : numArgsMax )

    character ( len = 512 ) :: myHost           = ''
    character ( len = 512 ) :: compilerVersion  = ''
    character ( len = 512 ) :: compilerOptions  = ''
    character ( len = 512 ) :: executionCommand = ''

    character ( len = * ), parameter :: myModule = 'module mQueries'  ! self-identification

    contains                                                                                    ! methods: subroutines and functions

!       ===============================================================================================                  ping_system

        subroutine qLib_ping_system ( )

            integer ( ip )                   :: status_host

            character ( len = * ), parameter :: c_version = compiler_version( )
            character ( len = * ), parameter :: c_options = compiler_options( )

            character ( len = 512 )          :: cmd    = " ", host = " "

!               queries
                call get_command ( cmd )
                call hostnm      ( host, status_host )

!               write identifiers
                write ( myHost,           '( "host system       = ", g0 )' ) trim ( host )
                write ( compilerVersion,  '( "compiler version  = ", g0 )' ) c_version
                write ( compilerOptions,  '( "compiler options  = ", g0 )' ) trim ( c_options )
                write ( executionCommand, '( "execution command = ", g0 )' ) trim ( cmd )

        end subroutine qLib_ping_system

!       ===============================================================================================                  ping_system

        subroutine qLib_write_system ( )  ! first call qLib_ping_system ( )

!               queries
                call qLib_ping_system ( )

!               display
                write ( * , 100 ) trim ( myHost )
                write ( * , 100 ) trim ( compilerVersion )
                write ( * , 100 ) trim ( compilerOptions )
                write ( * , 100 ) trim ( executionCommand )

  100           format ( g0, '.' )

        end subroutine qLib_write_system

!       ============================================================================================= harvest_command_line_arguments

        subroutine harvest_command_line_arguments ( echo )

            logical, optional, intent ( in ) :: echo

            integer ( ip )                   :: io_status = 0
            integer ( ip )                   :: c_arg_int = 0

            character ( len =  16 )          :: c_arg_char = ""
            character ( len = 256 )          :: io_msg = ""

                numArgsInput = 0

                do kQ = 1, numArgsMax  ! harvest command line arguments
                    call get_command_argument ( kQ, c_arg_char )
                    if ( len_trim ( c_arg_char ) == 0 ) exit
                    read ( c_arg_char, '( I16 )', iostat = io_status, iomsg = io_msg ) c_arg_int
                    if ( io_status /= 0 ) then
                        write ( *, 100 ) kQ
                        write ( *, 110 ) io_status
                        write ( *, 120 ) trim ( io_msg )
                    end if
                    numArgsInput = kQ
                    c_arg_lipt ( kQ ) = c_arg_int
                end do

                if ( present ( echo ) ) then
                    if ( echo .eqv. .true. ) then
                        write ( * , 200 ) numArgsInput
                        write ( * , 210 ) c_arg_lipt ( 1 : numArgsInput )
                    end if
                end if

                return

  100           format ( /, 'Read error for statement: read ( c_arg_int, "( I16 )" ) c_arg_int (', g0, ')' )
  110           format ( 'iostat = ', g0 )
  120           format ( 'iomsg = ', g0, '.' )

  200           format ( g0, ' command line arguments found:' )
  210           format ( 10I10 )

        end subroutine harvest_command_line_arguments

end module mQueries
