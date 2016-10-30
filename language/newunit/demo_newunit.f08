program demo_newunit

    implicit none

    integer :: io_in_pressure, io_out_temperature ! io handles
    integer :: io_stat

    character ( len = * ), parameter :: f_in_pressure     = 'pressure_list.txt', &
                                        f_out_temperature = 'temperature_list.txt'
    character ( len = 256 ) :: io_err_msg, action

        action = 'write'
        open ( NEWUNIT = io_out_temperature, FILE = f_out_temperature, ACTION = trim ( action ), STATUS = 'unknown', &
               IOSTAT = io_stat, IOMSG = io_err_msg )
        if ( io_stat /= 0 ) then
            write ( *, 100 ) f_out_temperature, io_out_temperature
            write ( *, 110 ) io_stat
            write ( *, 120 ) trim ( io_err_msg )
            stop 'Execution halting on fatal error.'
        end if

        write ( * , 200 ) f_out_temperature, trim ( action ), io_out_temperature
        close ( io_out_temperature, IOSTAT = io_stat, IOMSG = io_err_msg )

        action = 'read'
        open ( NEWUNIT = io_in_pressure, FILE = f_in_pressure, ACTION = trim ( action ), STATUS = 'old', &
               IOSTAT = io_stat, IOMSG = io_err_msg )
        if ( io_stat /= 0 ) then
            write ( *, 100 ) f_in_pressure, io_in_pressure
            write ( *, 110 ) io_stat
            write ( *, 120 ) trim ( io_err_msg )
            stop 'Execution halting on fatal error.'
        end if

        write ( *, 200 ) f_in_pressure, trim ( action ), io_in_pressure
        close ( io_in_pressure, IOSTAT = io_stat, IOMSG = io_err_msg )

        stop

    100 format ( 'Error trying to open file ', g0, ' with io unit ', g0 )
    110 format ( 'IOSTAT = ', g0 )
    120 format ( 'IOMSG = ', g0 )

    200 format ( 'File ', g0, ' is open for ', g0, ' with io unit ', g0 )

end program demo_newunit

! 13:26 topaz03 dantopa $ date
! Thu Mar 17 13:27:17 CDT 2016
! 13:27 topaz03 dantopa $ /apps/gnu_compiler/5.3.0/bin/gfortran  -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 demo_newunit.f08
!  13:27 topaz03 dantopa $ ./a.out
! File temperature_list.txt is open for write at file handle -10
! Error trying to open file pressure_list.txt with io unit -11
! IOSTAT = 2
! IOMSG = File 'pressure_list.txt' does not exist
! STOP Execution halting on fatal error.
