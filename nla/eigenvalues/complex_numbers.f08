include 'mod precision definitions.f08'

program complex_numbers

    use precision_definitions, only : wp, unit_modulus, one, zero

    implicit none

    complex ( wp ) :: x
    real    ( wp ) :: y

        x = 2 * unit_modulus
        write ( *, 100 ) x

        x = -3 * ( one, zero )
        write ( *, 100 ) x

        x = ( one, -5.0_wp )
        write ( *, 100 ) x

        y = zero
        write ( *, 110 ) y

        stop

  100   format ( 'x = ( ', g0, ', ', g0, ' ).' )
  110   format ( 'y = ', g0, '.' )

end program complex_numbers
