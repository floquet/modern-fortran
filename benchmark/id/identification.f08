program identification

    use, intrinsic :: iso_fortran_env,  only : compiler_options, compiler_version

    implicit none

    integer                          :: host_status

    character ( len = 255 )          :: host = '', dir_home = '', dir_working = '', user_name = ''
    character ( len = * ), parameter :: c_options = compiler_options( )
    character ( len = * ), parameter :: c_version = compiler_version( )
    character ( len = * ), parameter :: myProgram = 'program identification' ! self-identification


        call hostnm  ( host, host_status )
        call get_environment_variable ( 'HOME', dir_home )
        call getcwd  ( dir_working )
        call getlog  ( user_name )

        write ( * , '( /, "host system       = ", g0    )'  ) trim ( host )
        write ( * , '(    "home directory    = ", A, "." )' ) trim ( dir_home )
        write ( * , '(    "working directory = ", A, "." )' ) trim ( dir_working )
        write ( * , '(    "user name         = ", A, "." )' ) trim ( user_name )
        write ( * , '(    "compiler version  = ", A, "." )' ) c_version
        write ( * , '(    "compiler options  = ", A, "." )' ) c_options

    stop "successful completion for " // myProgram // "."  ! string must reduce to constant expression

end program identification

! dan-topas-pro-2:id rditldmt$ date
! Thu Feb 11 12:46:42 CST 2016
! dan-topas-pro-2:id rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/benchmarks/id
! dan-topas-pro-2:id rditldmt$ gfortran -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 identification.f08
! dan-topas-pro-2:id rditldmt$ aa
!
! host system       = dan-topas-pro-2.erdc.dren.mil
! home directory    = /Users/rditldmt.
! working directory = /Users/rditldmt/Box Sync/fortran/benchmarks/id.
! user name         = rditldmt.
! compiler version  = GCC version 5.1.0.
! compiler options  = -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.9.4 -mtune=core2 -g -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5.
! STOP successful completion for program identification.
