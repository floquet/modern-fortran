program basics

    use iso_fortran_env
    implicit none

    integer                          :: size_seed, myUnit
    integer                          :: host_status, io_status, alloc_status
    integer, allocatable             :: seed ( : )

    character ( len = * ), parameter :: c_options = compiler_options( )
    character ( len = * ), parameter :: c_version = compiler_version( )

    character ( len = 255 )          :: host = "", cmd = "", io_message = "", alloc_message = ""
    character ( len = 8 )            :: file_name_uuid = 'uuid.out'
    character ( len = 32 )           :: uuid

!       queries
        call hostnm      ( host, host_status )
        call get_command ( cmd )

!       write identifiers
        write ( *, '( /, "host system       = ", g0    )' ) trim ( host )
        write ( *, '(    "compiler version  = ", g0    )' ) c_version
        write ( *, '(    "compiler options  = ", g0    )' ) trim ( c_options )
        write ( *, '(    "execution command = ", g0, / )' ) trim ( cmd )

!       The uuidgen program creates a new universally unique identifier (UUID) using the libuuid(3) library.
        call execute_command_line ( 'uuidgen >> ' // file_name_uuid )

!       We should interrogate iostat and capture iomsg. Failure to capture these variables may lead to a crash on error.
        open  ( newunit = myUnit, file = file_name_uuid, iostat = io_status, iomsg = io_message )
        if ( io_status == 0 ) then
            read  ( unit    = myUnit, fmt = '( A )',         iostat = io_status, iomsg = io_message ) uuid
            close ( unit    = myUnit, iostat = io_status,    iomsg  = io_message )
            call execute_command_line ( 'rm ' // file_name_uuid )
            write ( *, '(    "uuid              = ", g0 )' ) uuid
        end if

!       Random number seed
        call random_seed ( size = size_seed )  ! measure seed size for allocation
        allocate ( seed ( size_seed ), stat = alloc_status, errmsg = alloc_message )
        open ( newunit = myUnit, file = "/dev/urandom", access = "stream", form = "unformatted", action = "read", &
                                                        status = "old",  iostat = io_status )
        if ( io_status == 0 ) then
            read  ( myUnit ) seed
            close ( myUnit )
            write ( *, '(    "rng seed          = ", 50(g0, 2x), / )' ) seed
        end if

end program basics

! dan-topas-pro-2:essentials rditldmt$ date
! Mon Nov 16 17:15:51 CST 2015
! dan-topas-pro-2:essentials rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/essentials
! dan-topas-pro-2:essentials rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fbacktrace -g  -fmax-errors=5 basics.f08
! dan-topas-pro-2:essentials rditldmt$ ./a.out
!
! host system       = dan-topas-pro-2.erdc.dren.mil
! compiler version  = GCC version 5.1.0
! compiler options  = -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.9.4 -mtune=core2 -g -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fbacktrace -fmax-errors=5
! execution command = ./a.out
!
! uuid              = 1D92F70F-3B87-40C8-A0D5-17C44E65
! rng seed          = -379794094  988815651  557648273  1086582876  497677712  -906201315  1469361720  -682490570  12292547  -1446876085  2046336803  -1095905007
! dan-topas-pro-2:essentials rditldmt$
