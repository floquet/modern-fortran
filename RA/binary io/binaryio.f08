program binaryio

    use iso_fortran_env, only : compiler_options, compiler_version, REAL32, REAL64, INT64

    implicit none

    ! precision parameters
    integer, parameter        :: dp = selected_real_kind ( REAL64 )
    integer, parameter        :: sp = selected_real_kind ( REAL32 )
    integer, parameter        :: rp = dp ! set real precision to double
    integer, parameter        :: ip = selected_int_kind  ( INT64 )

    integer ( ip ), parameter :: nPts = 3!123456789 ! number of elements to write
                                 ! double precision file size nPts * 8 = 987654312

    real ( rp ), dimension ( 1 : nPts ) :: myData = 0.0_rp

    integer ( ip ) :: io_unit = 0, io_status = 0, k = 0

    character ( len = 128 ) :: myFile = 'list.r64'
    character ( len = 128 ) :: io_msg = ''

        write ( * , '( "Fortran compiler version: ", g0 )' ) compiler_version ( )
        write ( * , '( "Fortran compiler options: ", g0 )' ) compiler_options ( )

        ! write a binary file
        myFile = 'list.r64'
        write ( *, '( /, ''Writing the integer sequence 1..'', g0, '' in the double precision file '', g0, ''...'' )' ) &
            nPts, trim ( myFile )

        open ( newunit = io_unit, file = trim ( myFile ), &
               action = 'WRITE', status = 'REPLACE', form = 'UNFORMATTED', iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then
                write ( *, 100 ) 'opening', trim ( myFile )
                write ( *, 110 ) io_msg
                write ( *, 120 ) io_status
            end if
        write ( unit = io_unit, iostat = io_status, iomsg = io_msg ) [ ( real ( k, rp ), k = 1, nPts ) ]
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

        ! read the binary file
        write ( *, '( /, ''Reading double precision file '', g0, ''...'' )' ) trim ( myFile )
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

        ! write out first and last terms of sequence
        write ( * , 200 ) [ ( myData ( k ), k = 1, 3 ) ]
        write ( * , 210 ) [ ( myData ( k ), k = nPts - 2, nPts ) ]

        stop 'successful run...'

        100 format ( 'Error ', g0, ' file ', g0, '.')
        110 format ( 'iomsg  = ', g0, '.')
        120 format ( 'iostat = ', g0, '.')

        200 format ( /, '  First 3 elements:', 3( 2X, g0 ) )
        210 format (    '  Last  3 elements:', 3( 2X, g0 ), / )

end program binaryio

! rditldmt@ITL-DTOPA-MP:binary io $ date
! Mon Sep 26 14:00:37 CDT 2016
! rditldmt@ITL-DTOPA-MP:binary io $ pwd
! /Users/rditldmt/hpc/fortran/RA/binary io
! rditldmt@ITL-DTOPA-MP:binary io $ echo $gflags
! -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only
! rditldmt@ITL-DTOPA-MP:binary io $
! rditldmt@ITL-DTOPA-MP:binary io $ gfortran $gflags binaryio.f08 -o binaryio
! binaryio.f08:9:35:
!
!      integer, parameter        :: sp = selected_real_kind ( REAL32 )
!                                    1
! Warning: Unused parameter 'sp' declared at (1) [-Wunused-parameter]
! rditldmt@ITL-DTOPA-MP:binary io $ ./binaryio
! Fortran compiler version: GCC version 7.0.0 20160918 (experimental)
! Fortran compiler options: -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -mtune=core2 -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -Wuse-without-only -ffpe-trap=denormal -fbacktrace -fcheck=bounds -fmax-errors=5
!
! Writing double precision file list.r64...
!
! Reading double precision file list.r64...
!
!   First 3 elements:  1.0000000000000000  2.0000000000000000  3.0000000000000000
!   Last  3 elements:  123454.00000000000  123455.00000000000  123456.00000000000
!
! STOP successful run...
