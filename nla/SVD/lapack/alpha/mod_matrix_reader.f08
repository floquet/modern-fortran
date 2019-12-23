module mMatrixReader

    use mFileHandling,  only : safeopen_readonly
    use mSetPrecision,  only : rp
    use mSVDparameters, only : M, N, A, rank, row, col, io_write, MMAX, NMAX

    implicit none

    integer                          :: io_status = 0, io_read = 0
    character ( len = 512 )          :: io_msg = ''
    character ( len =  25 )          :: fmt_str = ''
    character ( len = * ), parameter :: myModule = 'module mMatrixReader'

contains

    ! file format:
    ! header
    ! m n
    ! a_11 a_12 ... a_1n
    !  .    .    .   .
    ! a_m1 a_m2 ... a_mn

    subroutine read_matrix_file ( myFile )

        character ( len = * ), intent ( in )  :: myFile

            ! open
            io_read = safeopen_readonly ( myFile )

            ! read
            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) ! header
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Open', myFile, io_status, trim ( io_msg )
                stop 'Can''t read file header in ' // myModule // '.'
            end if

            read  ( io_read, *, iostat = io_status, iomsg = io_msg ) M, N ! rows and columns
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

            ! close
            close ( io_read, iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( * , 100 ) 'Close', io_status, trim ( io_msg )
                write ( * , 110 )
            end if

            ! verify
            if ( M > MMAX .or. N > NMAX ) then
                write ( io_write, *, iostat = io_status ) 'Data check failed: either M > MMAX .or. N > NMAX:'
                write ( io_write, *, iostat = io_status ) 'M = ', M, ', MMAX = ', MMAX, ', N = ', N, 'NMAX = ', NMAX, '.'
                stop 'Unsuccessful completion in ' // myModule // '.'
            end if

        return

        100   format ( g0, ' error in file ', g0, ':', /, 'io_status = ', g0, /, 'iomsg = ', g0, '.' )
        110   format ( 'Execution continues.')

    end subroutine read_matrix_file

end module mMatrixReader
