!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! Mathematica nb: /Users/rditldmt/Dropbox/ nb/drc/molecular dynamics/orientation/fortran loader 02.nb
include 'precision definitions.f90'

program reader

    use precision_definitions, only : is, wp, zero

    implicit NONE

    integer ( is ) :: io_unit = 0, io_status = 0
    integer ( is ) :: k = 0

    real    ( wp ) :: x ( 1 : 1024 ) = zero, y ( 1 : 1024 ) = zero

    character ( len =  11 ), parameter :: fname  = 'xy_data.txt'               ! data from Jeff's simulation
    character ( len =   * ), parameter :: me     = 'program tester'            ! Metcalf, Reid, Cohen: p. 309
    character ( len = 511 )            :: io_msg = " "

!       OPEN FILE
        open ( file = fname, newunit = io_unit, status = 'OLD', iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then                                             ! can't open file
            write ( *, * )
            write ( *, * ) 'ERROR: unable to open file ', fname
            write ( *, * ) 'io unit = ', io_unit
            write ( *, * ) 'iostat  = ', io_status
            write ( *, * ) 'iomsg   = ', io_msg
            stop 'Fatal error; execution will terminate'
        end if

!       READ DATA
        do k = 1, 1024
            read ( io_unit, *, iostat = io_status, iomsg = io_msg ) x ( k ), y ( k )
            if ( io_status /= 0 ) then                                         ! can't read data
                write ( *, * )
                write ( *, * ) 'READ ERROR: file ', fname
                write ( *, * ) 'attempting to read line ', k
                write ( *, * ) 'io unit = ', io_unit
                write ( *, * ) 'iostat  = ', io_status
                write ( *, * ) 'iomsg   = ', io_msg
                write ( *, * ) 'Non-fatal error; execution will continue'
            end if
        end do

!       CLOSE FILE
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then                                             ! can't close file
            write ( *, * )
            write ( *, * ) 'ERROR: unable to close file ', fname
            write ( *, * ) 'io unit = ', io_unit
            write ( *, * ) 'iostat  = ', io_status
            write ( *, * ) 'iomsg   = ', io_msg
            write ( *, * ) 'Non-fatal error; execution will continue'
        end if

        stop "successful completion for " // me  ! must reduce to constant expression

end program reader