module quality_parameters

  use constants_and_parameters
  implicit none

  type                           :: surface_fit_quality

    real ( wp )                  :: error_norm_L1        = zero
    real ( wp )                  :: error_norm_L2        = zero
    real ( wp )                  :: error_norm_Linf      = zero
    real ( wp )                  :: error_maximum        = zero
    real ( wp )                  :: error_rms            = zero
    real ( wp )                  :: condition_number     = zero

  end type                          surface_fit_quality

end module quality_parameters
