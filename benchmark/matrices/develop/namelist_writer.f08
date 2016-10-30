program namelist_printer

    use mPrecisionDefinitions,  only : ip

    implicit none

    integer ( ip ) :: m, n, p, lambda
    integer ( ip ) :: j, k
    integer ( ip ) :: io_nml, io_status

    integer ( ip ), parameter :: nMultipies = 5
    integer ( ip ), parameter :: multiplier ( 1 : nMultipies ) = [ ( 2 * k, k = 1, nMultipies ) ]

    namelist / shape_info / m, n, p

    character ( len = * ), parameter    :: file_name = 'shape parameters.txt' ! results from all blocks
    character ( len = * ), parameter    :: myProgram = 'namelist_printer' ! results from all blocks
    character ( len = 512 )             :: io_msg = ''

        m = 2
        n = 2
        p = 2

        open  ( newunit = io_nml, file = file_name, delim = 'apostrophe', iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) write ( * , 100 ) 'OPEN', io_status,  trim ( io_msg ), trim ( file_name ), io_nml

        write ( unit = io_nml, nml = shape_info, iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) write ( * , 100 ) 'WRITE', io_status,  trim ( io_msg ), trim ( file_name ), io_nml

        do k = 1, nMultipies
            !lambda = multiplier ( k )
            do j = 1,
            p = p * 2
            write ( unit = io_nml, nml = shape_info, iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) write ( * , 100 ) 'WRITE', io_status,  trim ( io_msg ), trim ( file_name ), io_nml
        end do

      close ( io_nml, iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) write ( * , 100 ) 'CLOSE', io_status,  trim ( io_msg ), trim ( file_name ), io_nml

    stop "normal completion for " // myProgram // "."  ! string must reduce to constant expression

    100  format ( 'I/O during ', A, /, 'iostatus = ', g0, /, 'iomsg = ', g0, '.', /, &
                  'File name = ', g0, '.', /, 'io handle = ', g0, / )

end program namelist_printer
