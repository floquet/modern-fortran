module mConstants

    ! https://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90/8508757#8508757
    use, intrinsic :: iso_fortran_env,  only : output_unit
    use mPrecisionDefinitions,          only : ip, rp

    implicit none

    ! real constants
    real ( rp ), parameter    :: zero = 0.0_rp
    real ( rp ), parameter    :: one  = 1.0_rp
    real ( rp ), parameter    :: half = 0.5_rp

    real ( rp ), parameter    :: pi = 3.1415926535897932384626433832795028841971693993751_rp

    real ( rp ), parameter    :: deg_to_rad =  0.017453292519943295769236907684886127134428718885417_rp
    real ( rp ), parameter    :: rad_to_deg = 57.295779513082320876798154814105170332405472466564_rp

    ! https://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90/8508757#8508757
    integer ( ip ), parameter :: stdout = output_unit

    ! complex constants
    complex ( rp ), parameter :: unit_modulus = ( zero, one )  ! a.k.a i

    ! real vectors
    real ( rp ), parameter    :: vec_up ( 1 : 2 ) = [ one, zero ]
    real ( rp ), parameter    :: vec_dn ( 1 : 2 ) = [ zero, one ]

    ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

    ! identity matrix
    real ( rp ), parameter    :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ vec_up, vec_dn ], [ 2, 2 ] )

end module mConstants
