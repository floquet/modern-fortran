program test_file_inquire

    use, intrinsic :: iso_fortran_env,  only : compiler_options, compiler_version
    use mFileInquire,                   only : fileInquire, byteSizeInquire, test_stat

    implicit none

    integer, parameter     :: nPts = 100000
    integer                :: myData ( 1 : nPts ) = 0
    integer                :: io_unit, io_status, k

    character ( len = * ), parameter :: myFile = 'binary.i32'
    character ( len = 128 )          :: io_msg = ''

        write ( * , '( "compiler version: ", g0 )' ) compiler_version ( )
        write ( * , '( "compiler options: ", g0 )' ) compiler_options ( )

        call byteSizeInquire ( )

        !   write a binary file
        open ( newunit = io_unit, file = myFile, &
                action = 'WRITE', status = 'REPLACE', form = 'UNFORMATTED', &
                iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , 100 ) io_status, trim ( io_msg )
            stop 'Unsuccessful open file ' // myFile // ' for write.'
        end if

        write ( unit = io_unit, iostat = io_status, iomsg = io_msg ) [ ( 2 * k - 1, k = 1, nPts ) ]
        if ( io_status /= 0 ) then
            write ( * , 100 ) io_status, trim ( io_msg )
            stop 'Unsuccessful write to file ' // myFile // '.'
        end if

        call test_stat ( myFile )

        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , 100 ) io_status, trim ( io_msg )
            stop 'Unsuccessful close of file ' // myFile // '.'
        end if

        !   read that binary file
        open ( newunit = io_unit, file = myFile, &
                action = 'READ', status = 'OLD', form = 'UNFORMATTED', &
                iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , 100 ) io_status, trim ( io_msg )
            stop 'Unsuccessful open file ' // myFile // ' for read.'
        end if

        call fileInquire ( io_unit ) ! panoply of status indicators

        myData ( : ) = 2
        read  ( unit = io_unit, iostat = io_status, iomsg = io_msg ) myData ( : )
        if ( io_status /= 0 ) then
            write ( * , 100 ) io_status, trim ( io_msg )
            stop 'Unsuccessful read of file ' // myFile // ' in "test_file_inquire.f08"'
        end if

        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )

        !   write out first and last terms of sequence
        write ( * , 120 ) [ ( myData ( k ), k = 1, 3 ) ]
        write ( * , 130 ) [ ( myData ( k ), k = nPts - 2, nPts ) ]

        stop 'Successful run for "test_file_inquire.f08" creating ' // myFile // '.'

   100  format ( /, 'Read error:', /, 'iostat = ', g0, /, 'iomsg = ', g0, '.' )
   120  format ( /, '  First 3 elements:', 3( 2X, g0 ) )
   130  format (    '  Last  3 elements:', 3( 2X, g0 ), / )

end program test_file_inquire

! dan-topas-pro-2:wiki rditldmt$ date
! Fri Mar 18 18:46:38 CDT 2016
! dan-topas-pro-2:wiki rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/file inquiry/wiki
! dan-topas-pro-2:wiki rditldmt$ make
! /usr/local/bin/gfortran -o file_inquire.out -Og mod_file_inquire.o file_inquire.o
! dan-topas-pro-2:wiki rditldmt$ ./file_inquire.out
! compiler version: GCC version 5.1.0
! compiler options: -fPIC -mmacosx-version-min=10.11.3 -mtune=core2 -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5
!
! Byte sizes for ISO_FORTRAN_ENV types:
! real32:        4 bytes
! real64:        8 bytes
! real128:      16 bytes
! int8:          1 bytes
! int16:         2 bytes
! int32:         4 bytes
! int64:         8 bytes
! .true.:        4 bytes
! 'A':           1 bytes
! Targetfile:                                 binary.i32
! Device ID:                                    16777220
! Inode number:                                122329877
! File mode (octal):                              100644
! Number of links:                                     1
! Owner's uid:                                 595980521
! Owner's gid:                                2011572450
! Device where located:                                0
! File size:                                      400004
! Last access time:             Fri Mar 18 18:46:18 2016
! Last modification time        Fri Mar 18 18:46:49 2016
! Last status change time:      Fri Mar 18 18:46:49 2016
! Preferred block size:                             4096
! No. of blocks allocated:                           784
!
! File inquiry results on unit -11:
! file name:    binary.i32
! exist:        T                   'TRUE', 'FALSE'
! opened:       T                   'TRUE', 'FALSE'
! named:        T                   'TRUE', 'FALSE'
!
! recl:         -1                  RECORD LENGTH for direct access file
! nextrec:      0                   last record + 1 for direct access file
! pos:          0                   POSITION for next read or write
!
! access:       SEQUENTIAL          'SEQUENTIAL', 'DIRECT', 'STREAM'
! sequential:   YES                 'YES', 'NO' 'UNKNOWN'
! direct:       NO                  'YES', 'NO' 'UNKNOWN'
! stream:       NO                  'YES', 'NO' 'UNKNOWN'
! position:     ASIS                'REWIND', 'APPEND', 'ASIS', 'UNDEFINED'
! form:         UNFORMATTED         'UNFORMATTED', 'FORMATTED'
! formatted:    NO                  'YES', 'NO' 'UNKNOWN'
! unformatted:  YES                 'YES', 'NO' 'UNKNOWN'
!
! action:       READ                'READ', 'WRITE', 'READWRITE', 'UNDEFINED'
! read:         YES                 'YES', 'NO' 'UNKNOWN'
! write:        NO                  'YES', 'NO' 'UNKNOWN'
! readwrite:    NO                  'YES', 'NO' 'UNKNOWN'
!
! asynchronous: NO                  'YES', 'NO'
! encoding:     UNDEFINED           'UTF-8', 'UNDEFINED', 'UNKNOWN'
! ID:           0                   id for pending asynchronous transfer
! pending:      F                   'TRUE', 'FALSE'
!
! blank:        UNDEFINED           'ZERO', 'NULL'
! delim:        UNDEFINED           'APOSTROPHE', 'QUOTE', 'NONE', 'UNKNOWN'
! pad:          UNDEFINED           'YES', 'NO'
! round:        PROCESSOR_DEFINED   'UP', 'DOWN', 'ZERO', 'NEAREST', 'COMPATIBLE', 'PROCESSOR DEFINED'
! sign:         PROCESSOR_DEFINED   'PLUS', 'SUPPRESS', 'PROCESSOR DEFINED'
!
!   First 3 elements:  1  3  5
!   Last  3 elements:  199995  199997  199999
!
! STOP Successful run for "test_file_inquire.f08" creating binary.i32.
