module mPrecisionDefinitions

    use iso_fortran_env

    implicit NONE

    ! kind parameters

    ! INTEGERS
    integer, parameter        :: aint    = selected_int_kind  ( INT8 )
    integer, parameter        :: sint    = selected_int_kind  ( INT16 )
    integer, parameter        :: lint    = selected_int_kind  ( INT32 )
    integer, parameter        :: zint    = selected_int_kind  ( INT64 )

    ! REALS
    integer, parameter        :: sp      = selected_real_kind ( REAL32 )
    integer, parameter        :: dp      = selected_real_kind ( REAL64 )
    integer, parameter        :: qp      = selected_real_kind ( REAL128 )

    ! CHARACTERS
    integer, parameter        :: def     = selected_char_kind ( 'DEFAULT' )    ! required by Fortran standard
    integer, parameter        :: kindA   =               kind ( 'A' )          ! Metcalf, Reid, Cohen: p. 309
    integer, parameter        :: ascii   = selected_char_kind ( 'ASCII' )      ! optional

    ! Define working precision: Hansen and Tompkins, p. 22
    integer, parameter        :: rp = dp
    integer, parameter        :: ip = zint

    ! real constants
    real ( rp ), parameter    :: zero = 0.0_rp
    real ( rp ), parameter    :: half = 0.5_rp
    real ( rp ), parameter    :: one  = 1.0_rp
    real ( rp ), parameter    :: two  = 2.0_rp

    real ( rp ), parameter    :: pi = 3.1415926535897932384626433832795028841971693993751_rp

    ! complex constants
    complex ( rp ), parameter :: unit_modulus = ( zero, one )  ! a.k.a i

    ! real vectors
    real ( rp ), parameter    :: vec_up ( 1 : 2 ) = [ one, zero ]
    real ( rp ), parameter    :: vec_dn ( 1 : 2 ) = [ zero, one ]

    ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

    ! identity matrix
    real ( rp ), parameter    :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ vec_up, vec_dn ], [ 2, 2 ] )

end module mPrecisionDefinitions
