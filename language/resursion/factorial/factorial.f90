include 'mod factorials.f08'

program factorial

    use iso_fortran_env
    use mFactorial
    implicit none

    integer ( int64 ) :: n

        write ( *, * ) 'Enter n:'
        read  ( *, * ) n
        write ( *, * ) n, '! = ', fact ( n )

end program factorial
