module basic_parameters

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

  ! Define working precision
  integer, parameter          :: wp = dp
  integer, parameter          :: is = zint

  ! IO
  integer ( zint ), parameter :: io_unit_target = 2

  ! real constants
  real ( wp ), parameter      :: zero = 0.0_wp
  real ( wp ), parameter      :: half = 0.5_wp
  real ( wp ), parameter      :: one  = 1.0_wp

  real ( dp ), parameter      :: pi = 3.1415926535897932384626433832795028841971693993751_wp

  ! complex constants
  complex ( wp ), parameter   :: unit_modulus = ( zero, one )  ! a.k.a i

  ! real vectors
  real ( wp ), parameter      :: vec_up ( 1 : 2 ) = [ one, zero ]
  real ( wp ), parameter      :: vec_dn ( 1 : 2 ) = [ zero, one ]

  ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

  ! identity matrix
  real ( wp ), parameter      :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ vec_up, vec_dn ], [ 2, 2 ] )

end module basic_parameters