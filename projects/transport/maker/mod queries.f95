! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mQueries

    use iso_fortran_env
    use precision_definitions, only : is!, wp, zero, one
    !use random

    implicit none

    integer, private          :: kQ
    integer ( is ), parameter :: numArgsMax = 5

    contains                                                                                    ! methods: subroutines and functions

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  ping_system

        subroutine ping_system ( )

            integer ( is )                   :: status = 0

            character ( len = * ), parameter :: c_options = compiler_options( )
            character ( len = * ), parameter :: c_version = compiler_version( )
            character ( len = 255 )          :: cmd = " ", host = " "

!               queries
                call get_command ( cmd )
                call hostnm      ( host, status )

!               write identifiers
                write ( *, '( /, "host system       = ", g0    )' ) trim ( host )
                write ( *, '(    "compiler version  = ", g0    )' ) c_version
                write ( *, '(    "compiler options  = ", g0    )' ) trim ( c_options )
                write ( *, '(    "execution command = ", g0, / )' ) trim ( cmd )

        end subroutine ping_system

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ harvest_command_line_arguments

        subroutine harvest_command_line_arguments ( c_arg_list, numArgsInput, echo )

            integer ( is ), intent ( out )   :: numArgsInput
            integer ( is ), intent ( out )   :: c_arg_list ( 1 : numArgsMax )
            logical, optional, intent ( in ) :: echo

            integer ( is )                   :: io_status = 0
            integer ( is )                   :: c_arg_int = 0

            character ( len =  16 )          :: c_arg_char = ""
            character ( len = 256 )          :: io_msg = ""

                numArgsInput = 0
!                 print *, 'size ( c_arg_list ) = ', size ( c_arg_list )
!                 print *, 'numArgsInput = ', numArgsInput
                do kQ = 1, numArgsMax  ! harvest command line arguments  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                    call get_command_argument ( kQ, c_arg_char )
                    if ( len_trim ( c_arg_char ) == 0 ) exit
                    read ( c_arg_char, '( I16 )', iostat = io_status, iomsg = io_msg ) c_arg_int
                    if ( io_status /= 0 ) then
                        write ( *, 100 ) kQ
                        write ( *, 110 ) io_status
                        write ( *, 120 ) trim ( io_msg )
                    end if
                    numArgsInput = kQ
!                     print *, 'kQ = ', kQ
!                     print *, 'numArgsInput = ', numArgsInput
                    c_arg_list ( kQ ) = c_arg_int
                end do

                if ( present ( echo ) ) then
                    if ( echo .eqv. .true. ) then
                        write ( * , 200 ) numArgsInput
                        !flush 6
                        write ( * , 210 ) c_arg_list ( 1 : numArgsInput )
                    end if
                end if

                return

  100           format ( /, 'Read error for statement: read ( c_arg_int, "( I16 )" ) c_arg_int (', g0, ')' )
  110           format ( 'iostat = ', g0 )
  120           format ( 'io_msg = ', g0, '.' )

  200           format ( g0, ' command line arguments found:' )
  210           format ( 10I10 )

        end subroutine harvest_command_line_arguments

end module mQueries
