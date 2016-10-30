module data_types

  ! this should be two modules, but primitive attempt created errors
  implicit NONE

  ! kind parameters
  integer, parameter          :: sint = selected_int_kind   (  8 )
  integer, parameter          :: lint = selected_int_kind   ( 16 )
  integer, parameter          :: sp   = selected_real_kind  (  6,  37 )
  integer, parameter          :: dp   = selected_real_kind  ( 15, 307 )

  ! dimensional parameters
  integer ( lint ), parameter :: num_dim   =      4         !  INPUT
  integer ( lint ), parameter :: num_nodes = 160000         !  INPUT
  integer ( lint ), parameter :: seed_dim  =     14         !  INPUT

  ! io_units
  integer ( lint ), parameter :: io_unit_nml     = 1
  integer ( lint ), parameter :: io_unit_system  = 2
  integer ( lint ), parameter :: io_unit_arrays  = 3
  integer ( lint ), parameter :: io_unit_stats   = 4
  integer ( lint ), parameter :: io_unit_error   = 6
  integer ( lint ), parameter :: io_unit_default = 6
  integer ( lint ), parameter :: io_unit_goodbye = 6

  integer ( lint ), parameter :: io_unit_trace   = 11
  integer ( lint ), parameter :: io_unit_measure = 12

  ! real constants
  real ( dp ), parameter      :: pi = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803483_dp
  real ( dp ), parameter      :: machine_eps = 2.221D-16    ! 1 + eps > 1

  real ( dp ), parameter      :: zero = 0.0_dp
  real ( dp ), parameter      :: half = 0.5_dp
  real ( dp ), parameter      :: one  = 1.0_dp

  ! complex constants
  complex ( dp ), parameter   :: unit_modulus = ( zero, one )  ! a.k.a i

  ! real vectors
  real ( dp ), parameter      :: vec_up ( 1 : 2 ) = [ one, zero ]
  real ( dp ), parameter      :: vec_dn ( 1 : 2 ) = [ zero, one ]

  ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

  ! identity matrix
  real ( dp ), parameter      :: id_matrix_2 ( 1 : 2, 1 : 2 ) = reshape ( [ vec_up, vec_dn ], [ 2, 2 ] )

end module data_types