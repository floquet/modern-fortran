module kind_types

  implicit NONE

  ! kind parameters
  integer, parameter          :: sint = selected_int_kind   (  8 )
  integer, parameter          :: lint = selected_int_kind   ( 16 )
  integer, parameter          :: sp   = selected_real_kind  (  6,  37 )
  integer, parameter          :: dp   = selected_real_kind  ( 15, 307 )
  integer, parameter          :: wp   = dp  ! set working precision to double precision

end module kind_types