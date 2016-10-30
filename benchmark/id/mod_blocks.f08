module mBlocks                                                               ! label each block of tests

    ! USE DECLARATIONS
    use mPrecisionDefinitions,  only : ip, ascii
    use mParameters,            only : stdout
    use mShared,                only : open_file_output, alert_io, io_status, io_msg

    implicit none

    integer   ( kind = ip ),              public :: nBlock
    character ( kind = ascii, len = 4 ), private :: roman ( 1 : 10 ) = [ "I   ", "II  ", "III ", "IV  ", "V   ", "VI  ", "VII ", &
                                                                         "VIII", "IX  ", "X   " ]

    contains

        ! METHODS

        subroutine announce ( title, myIO )   !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +

            integer   ( kind = ip ),             intent ( in ) :: myIO
            character ( kind = ascii, len = * ), intent ( in ) :: title

                write  ( myIO,  *  )
                write  ( myIO, 100 ) trim ( roman ( nBlock ) ), trim ( title ) ! label the block test
                write  (  * ,  100 ) trim ( roman ( nBlock ) ), trim ( title )

                flush ( myIO )

            return

            100 format ( "Block ", A, ": ", A, "." )

        end subroutine announce

        subroutine print_banner ( myIO )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +

            integer ( ip ), intent ( in ) :: myIO

                write ( myIO, 100 )
                write ( myIO, 110 )
                write ( myIO, 120 )
                write ( myIO, 130 )
                write ( myIO, 140 )
                write ( myIO, 150 )
                write ( myIO, *   )

                flush ( myIO )

            return

            100 format ( "F O R T R A N   2 0 0 8   C O M P I L E R   T E S T   S U I T E", / )
            110 format ( "                          Daniel Topa" )
            120 format ( "                  daniel.topa@engilitycorp.com", / )
            130 format ( "                      HPCMPO PETTT program" )
            140 format ( "                          ACE on-site" )
            150 format ( "                           2016 02 10" )

        end subroutine print_banner

end module mBlocks
