! https://gcc.gnu.org/onlinedocs/gfortran/FRACTION.html
program test_fraction
    use, intrinsic :: iso_fortran_env, only : INT64
    real :: x
    integer ( INT64 ) :: myInt = z'b5c0fbcf', firstInt = z'428a2f98'

        write ( *, 100 ) myInt, myInt, myInt
        write ( *, 100 ) firstInt, firstInt, firstInt
    100 format ( 'myInt = ', Z8, ' = ', B32, ' = ', g0 )

        x = exp ( log ( 2.0 ) / 3.0 )
        write ( *, 110 ) x, x
    110 format ( 'x = ', g0, '; binary = ', B64 )

        x = 178.1387e-4
        print *, 'x = ', x
        print *, fraction(x), x * radix(x)**(-exponent(x))

        x = exp ( log ( 5.0 ) / 3.0 )
        print *, 'x = ', x
        print *, fraction(x), x * radix(x)**(-exponent(x))
        write ( *, '( ''hex = '', Z16, ''; decimal = '', g0 )' ) fraction(x), fraction(x)
        write ( *, '( ''hex = '', Z16, ''; binary = '', B32, ''; decimal = '', g0 )' ) x - 1.0, x - 1.0, x - 1.0
end program test_fraction
