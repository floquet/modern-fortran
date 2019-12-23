program binaryio

    use iso_fortran_env, only : compiler_options, compiler_version, REAL32, REAL64, INT64

    implicit none

    ! precision parameters
    integer, parameter        :: dp = selected_real_kind ( REAL64 )
    integer, parameter        :: sp = selected_real_kind ( REAL32 )
    integer, parameter        :: rp = dp ! set real precision to double
    integer, parameter        :: ip = selected_int_kind  ( INT64 )

    integer ( ip ), parameter :: nPts = 123456789 ! number of elements to write
                                 ! double precision file size nPts * 8 = 987654312

    real ( rp ), dimension ( 1 : nPts ) :: myData = 0.0_rp
    real ( rp )                         :: t0, t1

    integer ( ip ) :: file_size = 0, io_unit = 0, io_status = 0, k = 0

    character ( len = 128 ) :: myFile = 'list.r64'
    character ( len = 128 ) :: io_msg = ''

        write ( * , '( /, "Fortran compiler version: ", g0 )' ) compiler_version ( )
        write ( * , '(    "Fortran compiler options: ", g0 )' ) compiler_options ( )

        ! write a binary file
        myFile = 'list.r64'
        myData ( : ) = [ ( real ( k, rp ), k = 1, nPts ) ]
        write ( *, '( /, ''Writing the integer sequence 1..'', g0, '' as double precision in the BINARY file '', g0, ''...'' )' ) &
            nPts, trim ( myFile )

        call cpu_time ( t0 )

        open ( newunit = io_unit, file = trim ( myFile ), &
               action = 'WRITE', status = 'REPLACE', form = 'UNFORMATTED', iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'opening', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if

        write ( unit = io_unit, iostat = io_status, iomsg = io_msg ) myData ( : )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'writing', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'closing', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if

        call cpu_time ( t1 )
        myData ( : ) = 0.0_rp
        write ( *, 300 ) t1 - t0, 'write', nPts, 'binary'
        call cpu_time ( t0 )

        ! read the binary file
        write ( *, '( /, ''Reading double precision BINARY file '', g0, ''...'' )' ) trim ( myFile )
        open ( newunit = io_unit, file = trim ( myFile ), action = 'READ', form = 'UNFORMATTED', iostat = io_status, iomsg = io_msg)
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'opening', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if
        read ( unit = io_unit, iostat = io_status, iomsg = io_msg ) myData ( : )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'reading', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'closing', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if

        call cpu_time ( t1 )
        write ( *, 300 ) t1 - t0, 'read', nPts, 'binary'

        ! write out first and last terms of sequence
        write ( * , 200 ) [ ( myData ( k ), k = 1, 3 ) ]
        write ( * , 210 ) [ ( myData ( k ), k = nPts - 2, nPts ) ]

        inquire ( FILE = myFile, SIZE = file_size )
        write ( *, 400 ) trim ( myFile ), file_size

        !  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *

        ! write an ASCII file
        myFile = 'list.txt'
        myData ( : ) = [ ( real ( k, rp ), k = 1, nPts ) ]
        write ( *, '( /, ''Writing the integer sequence 1..'', g0, '' as double precision in the ASCII file '', g0, ''...'' )' ) &
            nPts, trim ( myFile )

        call cpu_time ( t0 )

        open ( newunit = io_unit, file = trim ( myFile ), &
               action = 'WRITE', status = 'REPLACE', iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'opening', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if

        do k = 1, nPts
            write ( io_unit, '( g0 )', iostat = io_status, iomsg = io_msg ) myData ( k )
        end do
            if ( io_status /= 0 ) then ! only checks last write
                write ( *, 100 ) 'writing', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'closing', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if

        call cpu_time ( t1 )
        myData ( : ) = 0.0_rp
        write ( *, 300 ) t1 - t0, 'write', nPts, 'ASCII'
        call cpu_time ( t0 )

        ! read the ASCII file
        write ( *, '( /, ''Reading double precision ASCII file '', g0, ''...'' )' ) trim ( myFile )
        open ( newunit = io_unit, file = trim ( myFile ), action = 'READ', iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'opening', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if
        do k = 1, nPts
            read ( io_unit, *, iostat = io_status, iomsg = io_msg ) myData ( k )
        end do
            if ( io_status /= 0 ) then ! only checks last read
                write ( *, 100 ) 'reading', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'closing', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if

        call cpu_time ( t1 )
        write ( *, 300 ) t1 - t0, 'read', nPts, 'ASCII'

        ! write out first and last terms of sequence
        write ( * , 200 ) [ ( myData ( k ), k = 1, 3 ) ]
        write ( * , 210 ) [ ( myData ( k ), k = nPts - 2, nPts ) ]

        inquire ( FILE = myFile, SIZE = file_size )
        write ( *, 400 ) trim ( myFile ), file_size

        stop 'successful run...'

        100 format ( 'Error ', g0, ' file ', g0, '.')
        110 format ( 'iomsg  = ', g0, '.')
        120 format ( 'iostat = ', g0, '.')

        200 format ( /, '  First 3 elements:', 3( 2X, g0 ) )
        210 format (    '  Last  3 elements:', 3( 2X, g0 ), / )

        300 format ( g0, ' seconds to ', g0, ' ', g0, ' elements in ', g0, ' format')

        400 format ( 'The size of file ', g0, ' is ', g0, ' bytes')

end program binaryio

! rditldmt@ITL-DTOPA-MP:bravo $ pwd
! /Users/rditldmt/hpc/fortran/RA/binary io/bravo
! rditldmt@ITL-DTOPA-MP:bravo $
! rditldmt@ITL-DTOPA-MP:bravo $ echo $gflags
! -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only
! rditldmt@ITL-DTOPA-MP:bravo $
! rditldmt@ITL-DTOPA-MP:bravo $ date
! Mon Sep 26 14:53:16 CDT 2016
! rditldmt@ITL-DTOPA-MP:bravo $ gfortran $gflags binaryio.f08 -o binaryio
! date
! ./binaryiobinaryio.f08:9:35:
!
!      integer, parameter        :: sp = selected_real_kind ( REAL32 )
!                                    1
! Warning: Unused parameter 'sp' declared at (1) [-Wunused-parameter]
! rditldmt@ITL-DTOPA-MP:bravo $ date
! Mon Sep 26 15:36:32 CDT 2016
! rditldmt@ITL-DTOPA-MP:bravo $ ./binaryio
!
! Fortran compiler version: GCC version 7.0.0 20160918 (experimental)
! Fortran compiler options: -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -mtune=core2 -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -Wuse-without-only -ffpe-trap=denormal -fbacktrace -fcheck=bounds -fmax-errors=5
!
! Writing the integer sequence 1..123456789 as double precision in the BINARY file list.r64...
! 1.0153070000000000 seconds to write 123456789 elements in binary format
!
! Reading double precision BINARY file list.r64...
! 0.55272699999999997 seconds to read 123456789 elements in binary format
!
!   First 3 elements:  1.0000000000000000  2.0000000000000000  3.0000000000000000
!   Last  3 elements:  123456787.00000000  123456788.00000000  123456789.00000000
!
! The size of file list.r64 is 987654320 bytes
!
! Writing the integer sequence 1..123456789 as double precision in the ASCII file list.txt...
! 205.64807800000000 seconds to write 123456789 elements in ASCII format
!
! Reading double precision ASCII file list.txt...
! 144.94712499999997 seconds to read 123456789 elements in ASCII format
!
!   First 3 elements:  1.0000000000000000  2.0000000000000000  3.0000000000000000
!   Last  3 elements:  123456787.00000000  123456788.00000000  123456789.00000000
!
! The size of file list.txt is -1949288305 bytes
! STOP successful run...
