module HELIOS_data

!  use constants_and_parameters
  use linear_regression_results
  use polynomial_fit
  use HELIOS_files
  implicit none

  type, public                             :: HELIOS                                     !  *  *  *  *  *  *  *  *  *  *  *  *  *  *

    ! amplitudes describing the Helios surface
    type ( surface_fit )                   :: poly_fit                      ! from Mathematica
    type ( HELIOS_output )                 :: HELIOS_tables

    contains                                                                ! bound procedures

      ! functions

      ! subroutines
      procedure, public                    :: put                   =>    put_sub

  end type                                    HELIOS                            !  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *

  private                                  :: put_sub

  contains                                                                                      ! methods: subroutines and functions

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       Accessor routines

    subroutine put_sub ( self, amplitudes, errors, sf_quality, pts_temp, pts_density, pts_temp_lr, pts_density_lr, &
                         array_scaled, array_scaled_characteristics, array_normalized, array_normalized_characteristics )

      use constants_and_parameters
      use quality_parameters
      use polynomial_fit
      implicit none

      class ( HELIOS ), target                        :: self
      real ( wp ), pointer                            :: surface_amplitudes ( : )
      real ( wp ), pointer                            :: surface_errors     ( : )
      real ( wp ), pointer                            :: ptr_pts_temp       ( : )
      real ( wp ), pointer                            :: ptr_pts_density    ( : )

      real ( wp ), optional                           :: amplitudes       ( 1 : t )
      real ( wp ), optional                           :: errors           ( 1 : t )
      real ( wp ), optional                           :: array_scaled     ( 1 : num_pts_temp, 1 : num_pts_density )
      real ( wp ), optional                           :: array_normalized ( 1 : num_pts_temp, 1 : num_pts_density )
      real ( wp ), optional                           :: pts_temp         ( 1 : num_pts_temp )
      real ( wp ), optional                           :: pts_density      ( 1 : num_pts_density )

      type ( surface_fit_quality ),       optional    :: sf_quality
      type ( lr_results ),                optional    :: pts_temp_lr, pts_density_lr
      type ( array_characteristics ),     optional    :: array_scaled_characteristics, array_normalized_characteristics

      ! HELIOS output
      ! maps
      if ( present ( pts_temp ) ) then
        ptr_pts_temp => self % HELIOS_tables % pts_temp
        ptr_pts_temp =  pts_temp
        ptr_pts_temp => null ( )
      end if

      if ( present ( pts_density ) ) then
        ptr_pts_density => self % HELIOS_tables % pts_density
        ptr_pts_density =  pts_density
        ptr_pts_density => null ( )
      end if

      if ( present ( pts_temp_lr ) ) then
        self % HELIOS_tables % pts_temp_lr =  pts_temp_lr
      end if

      if ( present ( pts_density_lr ) ) then
        self % HELIOS_tables % pts_density_lr = pts_density_lr
      end if

      !  array tables
      if ( present ( array_scaled ) ) then
        self % HELIOS_tables % array_scaled = array_scaled
      end if

      if ( present ( array_scaled_characteristics ) ) then
        self % HELIOS_tables % array_scaled_characteristics = array_scaled_characteristics
      end if

      if ( present ( array_normalized ) ) then
        self % HELIOS_tables % array_normalized = array_normalized
      end if

      if ( present ( array_normalized_characteristics ) ) then
        self % HELIOS_tables % array_normalized_characteristics = array_normalized_characteristics
      end if

      ! surface fit
      if ( present ( amplitudes ) ) then
        surface_amplitudes => self % poly_fit % amplitudes
        surface_amplitudes =  amplitudes
        surface_amplitudes => null ( )
      end if

      if ( present ( errors ) ) then
        surface_errors => self % poly_fit % errors
        surface_errors =  errors
        surface_errors => null ( )
      end if

      if ( present ( sf_quality ) ) then
        self % poly_fit % sf_quality = sf_quality
      end if

    end subroutine put_sub

end module HELIOS_data