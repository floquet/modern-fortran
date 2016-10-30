module input_parameters

  use constants_and_parameters
  implicit NONE

  type               :: input_deck

    integer ( lint ) :: num_intervals = 1_lint                   ! intervals in domain
    real ( dp )      :: domain_a = zero, domain_b = zero         ! domain extent
    real ( dp )      :: pressure_xi = zero,  density_xi = zero   ! depth parameter
    real ( dp )      :: pressure_eta = zero, density_eta = zero  ! thickness

  end type              input_deck

end module input_parameters