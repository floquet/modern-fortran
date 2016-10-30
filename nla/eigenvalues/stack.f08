module precision_definitions

    use iso_fortran_env
    implicit NONE

    integer, parameter        :: wp = selected_real_kind ( REAL64 )

    ! real constants
    real ( wp ), parameter    :: zero = 0.0_wp
    real ( wp ), parameter    :: one  = 1.0_wp

    ! complex constants
    complex ( wp ), parameter :: unit_modulus = ( zero, one )  ! a.k.a i

end module precision_definitions

program complex_numbers

    use precision_definitions, only : wp, unit_modulus, one, zero

    implicit none

    complex ( wp ) :: x

        x = 2 * unit_modulus
        write ( *, 100 ) x

        x = -3 * ( one, zero )
        write ( *, 100 ) x

        stop

  100   format ( 'x = ( ', g0, ', ', g0, ' ).' )

end program complex_numbers
