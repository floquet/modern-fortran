program demo
    implicit none
    real    :: array_pressure ( 1 : 10 ) = 0.0, array_temperature ( 1 : 10 ) = 0.0
    integer :: io_in_pressure, io_out_temperature ! io handles
    integer :: io_stat, k
    integer, parameter :: numPressures = 10
    character ( len = * ), parameter :: f_in_pressure = 'pressure_list.txt', &
                                        f_out_temperature = 'temperature_list.txt'
    character ( len = 256 ) :: io_err_msg

        open ( NEWUNIT = io_in_pressure, FILE = f_in_pressure, STATUS = 'old', IOSTAT = io_stat, IOMSG = io_err_msg )
        if ( io_stat /= 0 ) then
            write ( *, 100 ) f_in_pressure
            write ( *, 110 ) io_stat
            write ( *, 120 ) trim( io_err_msg )
            stop 'Execution halting on fatal error.'
        end if

        do k = 1, numPressures ! read in the pressures; exit on error
            read ( UNIT = io_in_pressure, FMT = *, IOSTAT = io_stat, IOMSG = io_err_msg ) array_pressure ( k )
            if ( io_stat /= 0 ) then
                write ( *, '( "Error trying to read element ", g0, " in file ", g0 )' ) k, f_in_pressure
                write ( *, 110 ) io_stat
                write ( *, 120 ) trim( io_err_msg )
                exit
            end if
        end do

        close ( io_in_pressure )

        array_temperature = 3.2 * array_pressure ! ideal gas law

        open ( NEWUNIT = io_out_temperature, FILE = f_out_temperature, STATUS = 'unknown', IOSTAT = io_stat, IOMSG = io_err_msg )
        if ( io_stat /= 0 ) then
            write ( *, 100 ) f_out_temperature
            write ( *, 110 ) io_stat
            write ( *, 120 ) trim( io_err_msg )
            stop 'Execution halting on fatal error.'
        end if

        do k = 1, numPressures
            write ( io_out_temperature, * ) array_temperature ( k )
        end do

        flush ( io_out_temperature )

        close ( io_out_temperature, IOSTAT = io_stat, IOMSG = io_err_msg )
        if ( io_stat /= 0 ) then
            write ( *, 130 ) f_out_temperature
            write ( *, 110 ) io_stat
            write ( *, 120 ) trim( io_err_msg )
        end if

        stop

    100 format ( "Error trying to open file ", g0 )
    110 format ( "IOSTAT = ", g0 )
    120 format ( "IOMSG = ", g0 )
    130 format ( "Error trying to close file ", g0 )

end program demo

!  18:19 dan-topas-pro-2 rditldmt $ date
! Thu Mar 10 18:19:35 CST 2016
!  18:19 dan-topas-pro-2 rditldmt $ gfortran -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 demo.f08
!  18:19 dan-topas-pro-2 rditldmt $ ./a.out
!  18:19 dan-topas-pro-2 rditldmt $ ./a.out
