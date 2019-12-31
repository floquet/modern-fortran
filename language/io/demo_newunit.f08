program demo_newunit
  
    use iso_fortran_env, only : compiler_options, compiler_version

    implicit none

    integer                          :: myUnit = 0
    integer                          :: io_status = 0

    character ( len = 512 )          :: io_msg = ''
    character ( len = * ), parameter :: myFile = "dgesvd-ex.d"

        write ( * , * ) 'Attempting to open ', myFile
        open ( newunit = myUnit, file = myFile, action = 'READ', status = 'OLD', iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , 100 ) 'Open', myFile, io_status, trim ( io_msg )
            stop 'Fatal I/O error.'
        end if
        write ( * , * ) 'Success opening ', myFile

        read  ( myUnit, *, iostat = io_status, iomsg = io_msg ) ! header
        if ( io_status /= 0 ) then
            write ( * , 100 ) 'Read', myFile, myFile, io_status, trim ( io_msg )
            stop 'Fatal I/O error.'
        end if

        close ( myUnit, iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , 100 ) 'Close', myFile, io_status, trim ( io_msg )
            stop 'Fatal I/O error.'
        end if

        write ( * , * ) 'myUnit was set to ', myUnit

        stop "successful completion"

  100   format ( g0, ' error in file ', g0, ':', /, 'io_status = ', g0, /, 'iomsg = ', g0, '.' )

end program demo_newunit

! dan-topas-pro-2:io rditldmt$ date
! Fri Jan 22 11:16:05 CST 2016
! dan-topas-pro-2:io rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/io
! dan-topas-pro-2:io rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -framework Accelerate demo_newunit.f08
! dan-topas-pro-2:io rditldmt$ ./a.out
!  Attempting to open dgesvd-ex.d
!  Success opening dgesvd-ex.d
!  myUnit was set to          -10
! STOP successful completion
