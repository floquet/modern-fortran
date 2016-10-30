! Hansen and Tompkins, p. 22
! make all real variables _rp ( working precision )
! then real precision is controlled by one setting
module mPrecisionDefinitions

    use iso_fortran_env
    implicit NONE

    ! kind parameters
    ! INTEGERS
    integer, parameter          :: aint    = selected_int_kind  ( INT8 )
    integer, parameter          :: sint    = selected_int_kind  ( INT16 )
    integer, parameter          :: lint    = selected_int_kind  ( INT32 )
    integer, parameter          :: zint    = selected_int_kind  ( INT64 )

    ! REALS
    integer, parameter          :: sp      = selected_real_kind ( REAL32 )
    integer, parameter          :: dp      = selected_real_kind ( REAL64 )
    integer, parameter          :: qp      = selected_real_kind ( REAL128 )

    ! CHARACTERS
    integer, parameter          :: def     = selected_char_kind ( 'DEFAULT' )    ! required by Fortran standard
    integer, parameter          :: kindA   =               kind ( 'A' )          ! Metcalf, Reid, Cohen: p. 309
    integer, parameter          :: ascii   = selected_char_kind ( 'ASCII' )      ! optional

    ! Set working precision
    integer, parameter          :: rp      = dp
    integer, parameter          :: ip      = zint

    ! real constants
    real ( rp ), parameter      :: zero = 0.0_rp
    real ( rp ), parameter      :: half = 0.5_rp
    real ( rp ), parameter      :: one  = 1.0_rp
    real ( rp ), parameter      :: two  = 2.0_rp

    real ( dp ), parameter      :: pi = 3.1415926535897932384626433832795028841971693993751_rp

    real ( dp ), parameter      :: deg_to_rad =  0.017453292519943295769236907684886127134428718885417_rp
    real ( dp ), parameter      :: rad_to_deg = 57.295779513082320876798154814105170332405472466564_rp

    ! complex constants
    complex ( rp ), parameter   :: c_zero       = ( zero, zero )  ! origin in complex plane
    complex ( rp ), parameter   :: unit_modulus = ( zero, one )   ! a.k.a i

    ! real vectors
    real ( rp ), parameter      :: vec_up ( 1 : 2 ) = [ one, zero ]
    real ( rp ), parameter      :: vec_dn ( 1 : 2 ) = [ zero, one ]

    ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

    ! identity matrix
    real ( rp ), parameter      :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ vec_up, vec_dn ], [ 2, 2 ] )

end module mPrecisionDefinitions
