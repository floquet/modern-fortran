module mGetData

    use mPrecisionDefinitions, only : rp, one, zero
    use mDataSVD

    implicit none

    integer                          :: io_status = 0, io_read = 0
    character ( len = 512 )          :: io_msg = ''
    character ( len = * ), parameter :: myModule = 'module mGetData'

contains

    subroutine read_myfile ( myFile, myProgram )

        character ( len = * ), intent ( in )  :: myFile, myProgram

            chain = trim( myFile // ' in ' // myProgram // ' from ' // myModule )

            write ( * , * ) 'Attempting to open ', myFile
            open ( newunit = io_read, file = myFile, action = 'READ', status = 'OLD', iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', io_status, trim ( io_msg )
                stop 'Unsuccessful open for write for file ' // myFile // ' in ' // myProgram // ' from ' // myModule // '.'
            end if

            write ( * , * ) 'Attempting to read ', myFile
            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) ! header
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', io_status, trim ( io_msg )
                stop 'Can''t read header for file ' // myFile // ' in ' // myProgram // ' from ' // myModule // '.'
            end if
            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) M, N
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', io_status, trim ( io_msg )
                stop 'Can''t read M and N for file ' // myFile // ' in ' // myProgram // ' from ' // myModule // '.'
            end if
            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) ( ( A ( row, col ), col = 1, N ), row = 1, M ) ! data file is row major
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', io_status, trim ( io_msg )
                stop 'Can''t read matrix A for file ' // myFile // ' in ' // myProgram // ' from ' // myModule // '.'
            end if

            if ( M > MMAX .or. N > NMAX ) then
                write ( io_write, *, iostat = io_status ) 'Data check failed: either M > MMAX .or. N > NMAX:'
                write ( io_write, *, iostat = io_status ) 'M = ', M, ', MMAX = ', MMAX, ', N = ', N, 'NMAX = ', NMAX, '.'
                stop 'Unsuccessful completion for ' // myProgram // ', ' // myModule // '.'
            end if

        return

  100   format ( g0, ' error:', /, 'io_status = ', g0, /, 'iomsg = ', g0, '.' )

    end subroutine read_myfile

end module mGetData
