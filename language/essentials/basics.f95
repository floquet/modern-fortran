program basics

    use iso_fortran_env
    implicit none


    integer :: k, quarter, status

    character ( len = * ), parameter :: c_options = compiler_options( )
    character ( len = * ), parameter :: c_version = compiler_version( )
    character ( len = 255 )          :: host = " ", cmd = " "

!       queries
        call hostnm      ( host, status )
        call get_command ( cmd )

!       write identifiers
        write ( *, '( /, "host system       = ", g0    )' ) trim ( host )
        write ( *, '(    "compiler version  = ", g0    )' ) c_version
        write ( *, '(    "compiler options  = ", g0    )' ) trim ( c_options )
        write ( *, '(    "execution command = ", g0, / )' ) trim ( cmd )

end program basics