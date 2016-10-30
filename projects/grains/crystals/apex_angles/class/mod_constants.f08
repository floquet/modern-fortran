module mConstants

    ! https://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90/8508757#8508757
    use, intrinsic :: iso_fortran_env,  only : input_unit, output_unit, error_unit
    use mPrecisionDefinitions,          only : ip, rp

    implicit none

    ! real constants
    real ( rp ), parameter    :: zero = 0.0_rp
    real ( rp ), parameter    :: one  = 1.0_rp
    real ( rp ), parameter    :: half = 0.5_rp

    real ( rp ), parameter    :: pi = acos ( -one )
    !real ( rp ), parameter    :: pi = 3.1415926535897932384626433832795028841971693993751_rp

    real ( rp ), parameter    :: deg_to_rad = pi / 180.0_rp
    real ( rp ), parameter    :: rad_to_deg = 180.0_rp / pi

    real ( rp ), parameter    :: machine_epsilon = epsilon ( one )

    ! https://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90/8508757#8508757
    integer ( ip ), parameter :: stdin  = input_unit
    integer ( ip ), parameter :: stdout = output_unit
    integer ( ip ), parameter :: stderr = error_unit

    ! complex constants
    complex ( rp ), parameter :: unit_modulus = ( zero, one )  ! a.k.a i

    ! real vectors
    real ( rp ), parameter    :: vec_up ( 1 : 2 ) = [ one, zero ]
    real ( rp ), parameter    :: vec_dn ( 1 : 2 ) = [ zero, one ]

    ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

    ! identity matrix
    real ( rp ), parameter    :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ vec_up, vec_dn ], [ 2, 2 ] )

    character ( len = * ), parameter :: fmt_tail           = '3X, E25.15, " +/-", E25.15 )'
    character ( len = * ), parameter :: fmt_fortran        = '( g0, " Fortran    ", ' // fmt_tail
    character ( len = * ), parameter :: fmt_mathematica    = '( g0, " Mathematica", ' // fmt_tail
    character ( len = * ), parameter :: fmt_difference     = '( "Difference", 19X, E10.3, 19X, E10.3 )'
    character ( len = * ), parameter :: fmt_difference_eps = '( "Difference, epsilons", 9X, F12.1, 19X, E12.1, / )'

end module mConstants
