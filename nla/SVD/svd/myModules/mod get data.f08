module mGetData

    use iso_fortran_env
    use mPrecisionDefinitions, only : rp, one, zero
    use mSVDparameters

    implicit none

    integer                          :: io_status = 0, io_read = 0
    character ( len = 512 )          :: io_msg = ''
    character ( len =  25 )          :: fmt_str = ''
    character ( len = * ), parameter :: myModule = 'module mGetData'

contains

    subroutine read_myfile ( myFile )

        character ( len = * ), intent ( in )  :: myFile

            write ( * , * ) 'Attempting to open ', myFile
            open ( newunit = io_read, file = myFile, action = 'READ', status = 'OLD', iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', io_status, trim ( io_msg )
                stop 'Unsuccessful open for write in ' // myModule // '.'
            end if

            write ( * , * ) 'Attempting to read ', myFile
            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) ! header
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', myFile, io_status, trim ( io_msg )
                stop 'Can''t read file header in ' // myModule // '.'
            end if

            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) M, N
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', myFile, io_status, trim ( io_msg )
                stop 'Can''t read M and N in ' // myModule // '.'
            end if
            rank = min ( m, n )

            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) ( ( A ( row, col ), col = 1, N ), row = 1, M ) ! data file is row major
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', myFile, io_status, trim ( io_msg )
                stop 'Can''t read matrix A in ' // myModule // '.'
            end if

            if ( M > MMAX .or. N > NMAX ) then
                write ( io_write, *, iostat = io_status ) 'Data check failed: either M > MMAX .or. N > NMAX:'
                write ( io_write, *, iostat = io_status ) 'M = ', M, ', MMAX = ', MMAX, ', N = ', N, 'NMAX = ', NMAX, '.'
                stop 'Unsuccessful completion in ' // myModule // '.'
            end if

            ! write ( fmt_str, 210 ) N
            ! write ( * , * ) trim( fmt_str )
            ! do row = 1, m
            !     !write ( * , fmt = '( 4( F10.4, 2X ) )    ' )  ( A ( row, col ), col = 1, N )
            !     write ( * , fmt = fmt_str )  ( A ( row, col ), col = 1, N )
            ! end do

        return

        100   format ( g0, ' error in file ', g0, ':', /, 'io_status = ', g0, /, 'iomsg = ', g0, '.' )

    end subroutine read_myfile

end module mGetData
