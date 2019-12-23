module thermodynamics

  use kind_types
  implicit none

  !  define the inputs
  real ( dp ),      parameter                :: epsilon    = 0.01
  real ( dp ),      parameter                :: delta_pres = 0.1
  real ( dp ),      parameter                :: delta_dens = 0.1

  real ( dp ),      parameter                :: boundary_left  = zero
  real ( dp ),      parameter                :: boundary_right = one

  integer ( sint ), parameter                :: num_mesh_pts = 100

  real ( dp ), dimension ( 1 : num_mesh_pts) :: pressure, density, temperature

  real ( dp )                                :: map_slope, map_intpt               ! relate line of sight to physical space

    contains                                                                       ! bound procedures

      ! functions

      ! subroutines
      procedure, public                      :: put                   =>    put_sub

  end type                                      HELIOS                             !  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *

  private                                    :: put_sub

  contains                                                                                      ! methods: subroutines and functions

    elemental function g ( k ) result ( x )

      use kind_types
      implicit none

      real ( dp )                            :: x
      integer ( lint )                       :: k

      x = map_slope * dble ( k ) + map_intpt

    end function g



end module thermodynamics