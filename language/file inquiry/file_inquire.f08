program file_inquire

    use iso_fortran_env, only : compiler_options, compiler_version
    use mFileInquire,    only : fileInquire

    implicit none

    integer, parameter     :: nPts = 10
    integer                :: myData ( 1 : nPts )
    integer                :: io_unit, io_status, k

    character ( len = * ), parameter :: myFile = 'binary.i32'
    character ( len = 128 )          :: io_msg

        write ( * , '( "compiler version: ", g0 )' ) compiler_version ( )
        write ( * , '( "compiler options: ", g0 )' ) compiler_options ( )

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
            stop 'Unsuccessful read of file ' // myFile // ' in "file_inquire.f08"'
        end if

        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )

        !   write out first and last terms of sequence
        write ( * , 120 ) [ ( myData ( k ), k = 1, 3 ) ]
        write ( * , 130 ) [ ( myData ( k ), k = nPts - 2, nPts ) ]

        stop 'Successful run for "file_inquire.f08" creating ' // myFile // '.'

   100  format ( /, 'Read error:', /, 'iostat = ', g0, /, 'iomsg = ', g0, '.' )
   120  format ( /, '  First 3 elements:', 3( 2X, g0 ) )
   130  format (    '  Last  3 elements:', 3( 2X, g0 ), / )

end program file_inquire

! dantopa@Muntz-Szasz:~/Box Sync/fortran/demos/inquire$ pwd
! /Users/dantopa/Box Sync/fortran/demos/inquire
! dantopa@Muntz-Szasz:~/Box Sync/fortran/demos/inquire$ make clean
! rm -f mod\ file\ inquire.o file_inquire.o *.mod *.o
! dantopa@Muntz-Szasz:~/Box Sync/fortran/demos/inquire$ make
! /usr/local/bin/gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og mod\ file\ inquire.f08
! /usr/local/bin/gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og mod\ file\ inquire.f08
! /usr/local/bin/gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og file_inquire.f08
! /usr/local/bin/gfortran -o a.out -Og mod\ file\ inquire.o file_inquire.o
! dantopa@Muntz-Szasz:~/Box Sync/fortran/demos/inquire$ aa
! compiler version: GCC version 5.1.0
! compiler options: -fPIC -mmacosx-version-min=10.11.3 -mtune=core2 -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5
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
!   Last  3 elements:  15  17  19
!
! STOP Successful run for "file_inquire.f08" creating binary.i32.
