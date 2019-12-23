! Hansen and Tompkins, p. 22
! make all real variables _wp (working precision )
! then real precision is controlled by one setting
module precision_definitions

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
  integer, parameter          :: wp      = dp
  integer, parameter          :: is      = zint

  ! real constants
  real ( wp ), parameter      :: zero = 0.0_wp
  real ( wp ), parameter      :: half = 0.5_wp
  real ( wp ), parameter      :: one  = 1.0_wp
  real ( wp ), parameter      :: two  = 2.0_wp

  real ( dp ), parameter      :: pi = 3.1415926535897932384626433832795028841971693993751_wp

  ! complex constants
  complex ( wp ), parameter   :: c_zero       = ( zero, zero )  ! origin in complex plane
  complex ( wp ), parameter   :: unit_modulus = ( zero, one )  ! a.k.a i

  ! real vectors
  real ( wp ), parameter      :: vec_up ( 1 : 2 ) = [ one, zero ]
  real ( wp ), parameter      :: vec_dn ( 1 : 2 ) = [ zero, one ]

  ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

  ! identity matrix
  real ( wp ), parameter      :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ vec_up, vec_dn ], [ 2, 2 ] )

end module precision_definitions