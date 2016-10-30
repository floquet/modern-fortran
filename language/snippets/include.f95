module data_types

      implicit NONE

!     parameters
      integer, parameter    :: sint = selected_int_kind   (  8 )
      integer, parameter    :: lint = selected_int_kind   ( 16 )
      integer, parameter    :: sp   = selected_real_kind  (  6,  37 )
      integer, parameter    :: wp   = selected_real_kind  ( 15, 307 )

!     constants
      real( wp ), parameter :: pi = 3.141592653589793238462643383279502884197_wp
      real( wp ), parameter :: machine_eps = 2.221D-16

end module data_types