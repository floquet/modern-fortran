module polynomial_fit

  use quality_parameters
  implicit none

  type                           :: surface_fit

    ! amplitudes describing the Helios surface
    real ( wp )                  :: amplitudes ( 1 : t ) = zero                ! from Mathematica
    real ( wp )                  :: errors     ( 1 : t ) = zero                ! from Mathematica

    type ( surface_fit_quality ) :: sf_quality

  end type                          surface_fit

end module polynomial_fit