module constants_and_parameters

  use kind_types
  implicit NONE

  ! dimensional parameters
  integer ( lint ), parameter :: num_mesh =     4         !  number of mesh points
  integer ( lint ), parameter :: dof      =    10         !  order of fits, for now fixed
  integer ( lint ), parameter :: seed_dim =    14         !  architecture dependent

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

  ! constants
  real ( wp ), parameter      :: pi = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803483_wp
  real ( wp ), parameter      :: machine_eps = 2.221D-16    ! 1 + eps > 1

  real ( wp ), parameter      :: zero = 0.0_wp
  real ( wp ), parameter      :: half = 0.5_wp
  real ( wp ), parameter      :: one  = 1.0_wp

  ! kth term is the number of monomials in Taylor polynomial of order k
  integer ( lint )            :: num_terms ( 1 : 20 ) = [ 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, &
                                                        105, 120, 136, 153, 171, 190, 210, 231 ]
  integer ( lint ), parameter :: t = 66 ! num_terms ( dof )
!  integer ( lint ), parameter :: num_intervals = 5 ! num_terms ( dof )


  character ( 11 )            :: whoami ( 1 : 2 ) = [ 'emmissivity', 'opacity    ' ]

  ! based on data files
  integer ( lint ), parameter :: num_pts_temp    = 81
  integer ( lint ), parameter :: num_pts_density = 51

end module constants_and_parameters