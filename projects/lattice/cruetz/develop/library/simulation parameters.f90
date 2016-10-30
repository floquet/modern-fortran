module simulation_parameters

  use data_types

  implicit none

  ! dimensional parameters
  integer ( is ), parameter :: size      =    6_is                             !  number of plaquettes on edge
  integer ( is ), parameter :: num_dim   =    4_is                             !  space time dimensions
  integer ( is ), parameter :: num_nodes = 1296_is                             !  number of vertices

end module simulation_parameters