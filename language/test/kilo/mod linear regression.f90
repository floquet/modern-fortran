module linear_regression_results

  use kind_types
  implicit none

  type          :: lr_results

    real ( wp ) :: intercept_value
    real ( wp ) :: slope_value

    real ( wp ) :: intercept_error
    real ( wp ) :: slope_error

    real ( wp ) :: error_rms

  end type         lr_results

end module linear_regression_results