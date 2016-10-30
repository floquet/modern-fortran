module raw_arrays

  use constants_and_parameters
  implicit none

  type                             :: array_characteristics

    real ( wp )                    :: array_max  = zero
    real ( wp )                    :: array_min  = zero
    real ( wp )                    :: array_mean = zero
    real ( wp )                    :: array_sd   = zero

  end type                            array_characteristics

end module raw_arrays

module HELIOS_files

!  use constants_and_parameters
  use linear_regression_results
  use raw_arrays
  implicit none

  type                             :: HELIOS_output

    ! HELIOS output
    real ( wp )                    :: array_scaled     ( num_pts_temp, num_pts_density ) = zero  ! HELIOS output array
    real ( wp )                    :: array_normalized ( num_pts_temp, num_pts_density ) = zero  ! HELIOS output array

    type ( array_characteristics ) :: array_scaled_characteristics                               ! min, max, ave, standard deviation
    type ( array_characteristics ) :: array_normalized_characteristics

    real ( wp )                    :: pts_temp         ( 1 : num_pts_temp )    = zero            ! Te_ID.txt
    real ( wp )                    :: pts_density      ( 1 : num_pts_density ) = zero            ! Ne_ID.txt

    type ( lr_results )            :: pts_temp_lr
    type ( lr_results )            :: pts_density_lr

  end type                           HELIOS_output

end module HELIOS_files