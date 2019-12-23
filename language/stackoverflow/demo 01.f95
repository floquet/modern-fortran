program demo
    use iso_fortran_env
    implicit none

    real,    parameter :: pi = acos ( -1.0 ) 
    integer, parameter :: n = 8193

    real, dimension ( 1 : n ) :: x, y, z
    real                      :: a = 0.0, b = 2 * pi
    real                      :: increment

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

        increment = ( b - a ) / ( n - 1 )
        print *, 'increment = ', increment
        quarter = n / 4

!       mesh accumulates errors
        x ( 1 ) = 0.0
        do k = 2, n
            x ( k ) = x ( k - 1 ) + increment
        end do
        y = tan ( x )

        print *, 'Compare mesh points'
        !print *, x ( quarter - 1 : quarter + 1 )
        print *, x ( n - 3 : n )

!       better mesh
        x ( 1 ) = 0.0
        do k = 2, n
            x ( k ) = ( k - 1 ) * increment
        end do
        z = tan ( x )
        !print *, x ( quarter - 1 : quarter + 1 )
        print *, x ( n - 3 : n )

        print *, 'Compare function values at these mesh points'
        print *, y ( quarter - 1 : quarter + 1 )
        print *, z ( quarter - 1 : quarter + 1 )

end program demo