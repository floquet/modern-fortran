! Full Energy Balance Temperature Simulation Module 
!
module modHeatFlux
  use modGlobal,      only: R8, nRegion, r, dt, avg_surface_area, avg_volume, q_solar, wind_speed, pressure_atm, TairC, cloudiness,  &
                            eair_mb, use_TsedC, TwaterC, dTwaterCdt, TsedC, wind_coef_a, wind_coef_b, wind_coef_c, wind_kh_kw, wind_Richardson
  implicit none
  !
  ! Temperature group input parameters
  real(R8), allocatable, dimension(:) :: pb               ! bulk density of sediment                  (kg/m3)
  real(R8), allocatable, dimension(:) :: Cps              ! specific heat of sediment                 (J/kg/C)
  real(R8), allocatable, dimension(:) :: h2               ! effective thickness of the sediment layer (m) 
  real(R8), allocatable, dimension(:) :: alphas           ! sediment thermal diffusivity              (m2/d) 
  !
  ! kinetic pathways
  real(R8) :: q_net                                       ! net heat flux (W/m2)
  real(R8) :: q_latent                                    ! latent heat   (W/m2)
  real(R8) :: q_sensible                                  ! sensible heat (W/m2)
  real(R8) :: q_longwave_up                               ! back (upwelling) longwave radiation                     (W/m2)
  real(R8) :: q_longwave_down                             ! atmospheric (downwelling) longwave radiation            (W/m2)
  real(R8) :: q_sediment                                  ! sediment-water heat flux                                (W/m2)
  real(R8) :: TeqC                                        ! equilibrium temperature for current met conditions      (C)
  real(R8) :: Ta_Tw                                       ! difference between air and water temperature            (C)
  real(R8) :: Esat_Eair                                   ! difference between saturated and current vapor pressure (mb)
  !
  ! kinetic pathway index
  integer :: q_net_index
  integer :: q_solar0_index
  integer :: q_latent_index
  integer :: q_sensible_index
  integer :: q_longwave_up_index
  integer :: q_longwave_down_index
  integer :: q_sediment_index
  integer :: TeqC_index
  integer :: Ta_Tw_index
  integer :: Esat_Eair_index
  !
	!	coefficients for computing saturated vapor pressure
	real(R8), parameter :: a0	= 6984.505294	
	real(R8), parameter :: a1	= -188.903931	
	real(R8), parameter :: a2	= 2.133357675	
	real(R8), parameter :: a3	= -1.288580973D-2	
	real(R8), parameter :: a4	= 4.393587233D-5
	real(R8), parameter :: a5	= -8.023923082D-8
	real(R8), parameter :: a6	= 6.136820929D-11
  !
	!	physical constants and empirical values
  real(R8), parameter :: stefan_boltzmann = 5.67D-8       ! Stefan Boltzman constant (W/m2/K)
  real(R8), parameter :: Cp_air  = 1005.0                 ! Specific heat of air     (J/kg/K)
  real(R8), parameter :: gravity = 9.806                  ! gravity acceleration     (m/s2)
  real(R8), parameter :: emissivity_water = 0.97          ! emissivity of water      (unitless)
  !
  contains
  !
  !===========================================================================================================================
  ! allocate and initialize all input parameters with default values
  subroutine InitializeTemperature()
    if (use_TsedC) then
      if (allocated(pb)) deallocate(pb)
      allocate(pb(nRegion))
      pb = 1600.0
      !
      if (allocated(Cps)) deallocate(Cps)
      allocate(Cps(nRegion))
      Cps = 1673.0
      !
      if (allocated(h2)) deallocate(h2)
      allocate(h2(nRegion))
      h2 = 0.1
      !
      if (allocated(alphas)) deallocate(alphas)
      allocate(alphas(nRegion))
      alphas = 0.0432
    end if
    !
  end subroutine
  !
  !===========================================================================================================================
  ! set a real parameter
  logical function SetTemperatureRealParameter(name, paramValue)
    character*(*), intent(in) :: name
    real(R8),      intent(in) :: paramValue(nRegion)      
    SetTemperatureRealParameter = .true.
    !
    select case (name)
      case ('pb')
        pb            = paramValue
      case ('Cps')
        Cps           = paramValue
      case ('h2')
        h2            = paramValue
      case ('alphas')
        alphas        = paramValue
      case default
        !
        ! did not find the parameter, return false
        SetTemperatureRealParameter = .false. 
    end select
  end function
  !
  !===========================================================================================================================
  ! find pathway index
  logical function SetTemperaturePathwayIndex(pathwayName, index)
    character*(*), intent(in) :: pathwayName    
    integer,       intent(in) :: index    
    !
    SetTemperaturePathwayIndex = .true.
    !
    select case (pathwayName)
      case ('q_net')
        q_net_index = index
      case ('q_solar')
        q_solar0_index = index
      case ('q_latent')
        q_latent_index = index
      case ('q_sensible')
        q_sensible_index = index
      case ('q_longwave_up')
        q_longwave_up_index = index
      case ('q_longwave_down')
        q_longwave_down_index = index
      case ('q_sediment')
        q_sediment_index = index
      case ('TeqC')
        TeqC_index = index
      case ('Ta-Tw')
        Ta_Tw_index = index
      case ('Esat-Eair')
        Esat_Eair_index = index
      case default
        !
        SetTemperaturePathwayIndex = .false.
    end select
    !
  end function
  !
  !===========================================================================================================================
  ! Water temperature
  subroutine  ComputeHeatFlux()
	  !
    ! temperatue
	  real(R8)	TwaterK	                 ! water temperature           (K)
    real(R8)  TairK                    ! current air temperature     (K)
    real(R8)  TeqK, Teqnext            ! equilibrium temperature     (K)
    !
    ! pressure
    real(R8)  Pa_mb                    ! air pressure                (mb)
	  real(R8)  esat_mb	                 ! saturated vapor pressure computed from water temperature at previous time step (mb)
	  !
	  !	functions of air temperature
	  real(R8)  density_air			         ! density                     (kg/m3)
	  real(R8)	emissivity_air		       ! emissivity of air           (unitless)
	  !
	  !	functions of water temperature
	  real(R8)  Cp_water			           ! specific heat               (J/kg/K)
	  real(R8)	density_water	           ! density                     (kg/m3)
	  real(R8)  Lv					             ! latent heat of vaporization (J/kg)
	  real(R8)	density_air_sat	         ! density of air computed at water surface temperature (kg/m3)
    !
	  !	wind function, stability and flux partitioning
    real(R8)  wind_function	           ! value of wind function      (unitless)
	  real(R8)  Ri_fctn				           ! stablility function         (unitless)
	  real(R8)	mixing_ratio_air	       ! mixing ratio                (unitless)
	  !
	  !	variables for computing equilibrium temperature
	  real(R8)	d_qnet_dT, d_longwave_up_dT, d_latent_dT, d_esat_dT, d_sensible_dT, d_sediment_dT
 	  !
	  !	counters
	  integer		count
    !
    !_________________________________________________________________________________________________
		!	unit conversions
		TairK = TairC + 273.16		
    Pa_mb = pressure_atm * 1013.25
		!
		!------------------------------------------------------------------------
		! compute density of air (kg/m3)
		mixing_ratio_air	= 0.622 * eair_mb / (Pa_mb - eair_mb)
		density_air			  = 0.348 * (Pa_mb / TairK) * (1. + mixing_ratio_air)  &
							        	/(1. + 1.61 * mixing_ratio_air)		
		!____________________________________________________________________________________
		!  Computations that are not a function of water temperature.
		!	 Note: solar radiation comes in directly from the interface.
		!------------------------------------------------------------------------
		! Downwelling (atmospheric) longwave radiation
		emissivity_air  = 0.00000937 * TairK**2.0
		q_longwave_down = (1 + 0.17 * cloudiness**2) * emissivity_air * stefan_boltzmann * TairK**4.0
		!
		!------------------------------------------------------------------------
		! Wind function for latent and sensible heat
		wind_function = (wind_coef_a / 1000000. + wind_coef_b / 1000000. * wind_speed**wind_coef_c)
    !		
		!---------------------------------------------------------------------------------------------------------
		! Obtain equilibrium temperature for current met conditions
		!---------------------------------------------------------------------------------------------------------
		! Newton-Rhapson interation.  Begin with air temperature as first guess.
		Teqnext =	TairK 
		TeqK	  =	TairK - 10.
		count	  =   0
		do while (abs(TeqK - Teqnext) > 0.01  .and. count .le. 10)       ! Potential issues exist for count > 10???
      !
			TeqK = Teqnext
			TeqC = TeqK - 273.16
      !
			!---------------------------------------------------------------------------------------------------------
			!	Compute physical values that are functions of water temperature
			Lv				    = mf_latent_heat_vaporization(TeqK)
			density_water	= mf_density_water(TeqC)
			esat_mb		  	= mf_esat_mb(TeqK)
			Cp_water		  = mf_Cp_water(TeqC)
			!
			!---------------------------------------------------------------------------------------------------------
			! Upwelling Longwave Radiation and its derivative
			!---------------------------------------------------------------------------------------------------------
			q_longwave_up		  = mf_q_longwave_up(TeqK)
			d_longwave_up_dT	= 4. * emissivity_water * stefan_boltzmann * TeqK**3	
			!---------------------------------------------------------------------------------------------------------
			! Surface Fluxes
			! Compute Richardson Number for latent and sensible heat fluxes
			Ri_fctn = 1.
			if (int(wind_richardson) == 1) then
				density_air_sat	=	mf_density_air_sat(esat_mb, Pa_mb, TeqK)
				Ri_fctn					=	mf_Ri_fctn(density_air_sat, density_air)
			end if
			! Compute Sensible Heat flux and its derivative
			q_sensible		=   mf_q_sensible(Ri_fctn, density_water, wind_function, TairK, TeqK)
			d_sensible_dT	= - wind_Kh_Kw * Ri_fctn * Cp_air * density_water * wind_function 
			!
			! Compute latent heat flux and its derivative
			q_latent	  = mf_q_latent(Ri_fctn, Pa_mb, Lv, density_water, wind_function, esat_mb)
			d_esat_dT	  = mf_d_esat_dT(TeqK)
			d_latent_dT = Ri_fctn * (0.622 / Pa_mb) * Lv * density_water * wind_function * d_esat_dT
      !
      ! Compute sediment-water interface flux
      if (use_TsedC) then
        q_sediment    = pb(r)  * Cps(r) * alphas(r) / 0.5 / h2(r) * (TsedC - TwaterC) / 86400.0
        d_sediment_dT = -pb(r) * Cps(r) * alphas(r) / 0.5 / h2(r) / 86400.0
      else
        q_sediment    = 0.0
        d_sediment_dT = 0.0
      end if
      !
			d_qnet_dT			  = - d_longwave_up_dT - d_latent_dT + d_sensible_dT + d_sediment_dT	
      !
			q_net				    = q_sensible - q_latent - q_longwave_up + q_longwave_down + q_solar + q_sediment
      !
			Teqnext				  = TeqK - ( q_net / d_qnet_dT )
      !
			count = count + 1
		end do
	  TeqC = 0.5 * (TeqK + Teqnext) - 273.16
		! ____________________________________________________________________________________
		!  Energy Balance Comptuations that are functions of water temperature
		! ____________________________________________________________________________________
		!
		!	Get water temperature from previous time step plus 1/2 AD change??? dll can not do it. 
    !
		!	Physical values that are functions of water tempeature
		TwaterK			  = TwaterC + 273.16	
		Lv				    = mf_latent_heat_vaporization(TwaterK)
		density_water	= mf_density_water(TwaterC)
		esat_mb			  = mf_esat_mb(TwaterK)
		Cp_water		  = mf_Cp_water (TwaterC)
		!------------------------------------------------------------------------
		! Upwelling (back or water surface) longwave radiation
		q_longwave_up	= mf_q_longwave_up(TwaterK)
		!------------------------------------------------------------------------
		!
    ! Surface Fluxes
		! Compute Richardson Number and check stability
		Ri_fctn = 1.
		if (int(wind_richardson) == 1) then
			density_air_sat	=	mf_density_air_sat(esat_mb, Pa_mb, TwaterK) 
			Ri_fctn					=	mf_Ri_fctn (density_air_sat, density_air)
		end if
		!------------------------------------------------------------------------
		! latent heat flux
		q_latent          = Ri_fctn * (0.622 / Pa_mb) * Lv * density_water * wind_function * (esat_mb - eair_mb)
		!------------------------------------------------------------------------
	  ! sensible heat flux
		q_sensible        = mf_q_sensible(Ri_fctn, density_water, wind_function, TairK, TwaterK)
		!------------------------------------------------------------------------
    ! sediment heat flux
    if (use_TsedC) then
      q_sediment     = pb(r) * Cps(r) * alphas(r) / 0.5 / h2(r) * (TsedC - TwaterC) / 86400.
    else
      q_sediment     = 0.0
    end if
		!------------------------------------------------------------------------
    ! net heat flux	
		q_net = q_sensible - q_latent - q_longwave_up + q_longwave_down + q_solar + q_sediment
		!------------------------------------------------------------------------
    ! dTwaterCdt???
    dTwaterCdt = q_net * avg_surface_area / (avg_volume * density_water * Cp_water) * 86400.
		!------------------------------------------------------------------------------------		
    Ta_Tw      = TairC - TwaterC
    Esat_Eair  = esat_mb - eair_mb
  	!____________________________________________________________________________________
		!  Compute sediment temperature
		!____________________________________________________________________________________
    if (use_TsedC) TsedC = TsedC + dt * alphas(r) / (0.5 * h2(r) * h2(r)) * (TwaterC - TsedC)
    ! 
  end subroutine
  !
  !---------------------------------------------------------------------------------------------------------
  ! A series of functions used by HeatFlux subroutine
  !---------------------------------------------------------------------------------------------------------
  ! Longwave Radiation
  !---------------------------------------------------------------------------------------------------------
  ! Upwelling longwave radiation
  real(R8) function mf_q_longwave_up(TwaterK)
	  real(R8) TwaterK
	  !
	  mf_q_longwave_up = emissivity_water * stefan_boltzmann * TwaterK**4
  end function
  !
  !---------------------------------------------------------------------------------------------------------
  ! Surface Fluxes and Stability 
  !---------------------------------------------------------------------------------------------------------
  !..........................................................................................................
  ! Latent heat flux
  !..........................................................................................................
  real(R8) function mf_q_latent(Ri_fctn, Pa_mb, Lv, density_water, wind_function, esat_mb)
	  real(R8) Ri_fctn, Pa_mb, Lv, density_water, wind_function, esat_mb	
	  !		
	  mf_q_latent = Ri_fctn * (0.622 / Pa_mb) * Lv * density_water * wind_function * (esat_mb - eair_mb)
  end function
  !
  !..........................................................................................................
  ! Sensible heat flux
  !..........................................................................................................
  real(R8) function mf_q_sensible(Ri_fctn, density_water, wind_function, TairK, TwaterK)
	  real(R8) Ri_fctn, density_water, wind_function, TairK, TwaterK
    !
	  mf_q_sensible = wind_Kh_Kw * Ri_fctn * Cp_air * density_water * wind_function * (TairK - TwaterK)
  end function
  !
  !..........................................................................................................
  ! Richardson number (used in latent and sensible heat flux computations to correct for atm stability)
  !..........................................................................................................
  real(R8) function mf_Ri_fctn(density_air_sat, density_air)
	  real(R8)	RichardsonNumber, density_air_sat, density_air
	  !	compute Richardson Number
	  !		0.01	>= Ri_fctn	> -1		unstable
	  !		0.01	<= Ri_fctn	<  2		stable
	  !	 -0.01	<  Ri_fctn	< 0.01	neutral
    !
	  RichardsonNumber	= -gravity * (density_air - density_air_sat) * 2. &
				                / (density_air * (wind_speed**2))
	  ! set bounds
	  If (RichardsonNumber > 2.)  RichardsonNumber = 2.
	  If (RichardsonNumber < -1.) RichardsonNumber = -1.

	  If (RichardsonNumber < 0.) Then
		  If (RichardsonNumber >= - 0.01) Then
	  !		neutral
			  mf_Ri_fctn = 1.
		  Else
	  !		unstable
			  mf_Ri_fctn = (1. - 22. * RichardsonNumber)**0.80
		  End If
	  Else
		  If (RichardsonNumber <= 0.01) Then
	  !		neutral
			  mf_Ri_fctn = 1.
		  Else
	  !		stable
			  mf_Ri_fctn = (1. + 34. * RichardsonNumber)**-0.80
		  End If
	  End If
  end function
  !
  !---------------------------------------------------------------------------------------------------------
  ! Functions that compute physical values that are functions of water temperature
  !---------------------------------------------------------------------------------------------------------
  !...............................................................................................................
  ! Latent heat of vaporization (used in latent heat formulation)
  !...............................................................................................................
  real(R8) function mf_latent_heat_vaporization(TwaterK)
	  real(R8) TwaterK
    !
	  mf_latent_heat_vaporization = 2499999-2385.74 * TwaterK
  end function
  !
  !...............................................................................................................
  ! Density of water (used in latent heat formulation)
  !...............................................................................................................
  real(R8) function mf_density_water(TwaterC0)
    real(R8) TwaterC0
    !
	  mf_density_water = 999.973 * (1 - (((TwaterC0 - 3.9863) * (TwaterC0 - 3.9863) * (TwaterC0 + 288.9414))  &
								   /(508929.2 * (TwaterC0 + 68.12963))))
  end function
  !
  !...............................................................................................................
  ! Saturation vapor pressure (used in latent heat formulation)
  !...............................................................................................................
  real(R8) function mf_esat_mb(TwaterK)
	  !	fitting parameters for vapor pressure
	  !	Brutsaert (1982) Evaporation into the Atmosphere. p42
    !
	  real(R8) TwaterK
	  mf_esat_mb = a0 + TwaterK*(a1 + TwaterK*(a2 + TwaterK *(a3 + TwaterK *(a4     &
				                 +TwaterK*(a5 + TwaterK*a6)))))
  end function
  !
  !...............................................................................................................
  ! Density of saturated air (used in latent heat formulation)
  !...............................................................................................................
  real(R8) function mf_density_air_sat(esat_mb, Pa_mb, TwaterK)
	  real(R8) mixing_ratio_sat, esat_mb, Pa_mb, TwaterK
	  !	compute density of saturated air at water surface temperature (kg/m3)
	  mixing_ratio_sat	 = 0.622 * esat_mb / (Pa_mb - esat_mb)
	  mf_density_air_sat = 0.348 * (Pa_mb / TwaterK) * (1. + mixing_ratio_sat)  &
											    / (1. + 1.61 * mixing_ratio_sat)
  end function
  !
  !...............................................................................................................
  ! Specific Heat of Water (used in computing source/sink term)
  !...............................................................................................................
  real(R8) function mf_Cp_water(TwaterC0)
	  real(R8) TwaterC0
	  !					 				
	  if (TwaterC0 <= 0.0) then
		  mf_Cp_water = 4218.0 		
	  elseif (TwaterC0  <= 5.0) then
		  mf_Cp_water = 4202.0
	  elseif (TwaterC0 <= 10.0) then
		  mf_Cp_water = 4192.0
	  elseif (TwaterC0 <= 15.0) then
		  mf_Cp_water = 4186.0
	  elseif (TwaterC0 <= 20.0) then
		  mf_Cp_water = 4182.0
	  elseif (TwaterC0 <= 25.0) then
		  mf_Cp_water = 4180.0
	  else
		  mf_Cp_water = 4178.0
	  endif	
  end function	
  !
  !---------------------------------------------------------------------------------------------------------
  ! Derivatives used in equilibrium temperature computations
  !---------------------------------------------------------------------------------------------------------
  !...............................................................................................................
  ! Derivative of function computing saturation vapor pressure as function of water temperature
  !...............................................................................................................
  real(R8) function mf_d_esat_dT(TwaterK)
	  !	fitting parameters for vapor pressure	 
	  !	Brutsaert (1982) Evaporation into the Atmosphere. p42
    real(R8) TwaterK
    !
	  mf_d_esat_dT	=	a1 + 2. * a2 * TwaterK + 3. * a3 * TwaterK**2 + 4. * a4 * TwaterK**3  &
					        + 5. * a5 * TwaterK**4 + 6. * a6 * TwaterK**5
  end function
  !
  !===========================================================================================================================
  ! Output pathways
  subroutine HeatFluxPathwayOutput(na, a)
    integer  na
    real(R8) a(na)  
    !
    if (q_net_index > 0)            a(q_net_index)            = q_net
    if (q_solar0_index > 0)         a(q_solar0_index)         = q_solar
    if (q_latent_index > 0)         a(q_latent_index)         = q_latent
    if (q_sensible_index > 0)       a(q_sensible_index)       = q_sensible
    if (q_longwave_up_index > 0)    a(q_longwave_up_index)    = q_longwave_up
    if (q_longwave_down_index > 0)  a(q_longwave_down_index)  = q_longwave_down
    if (q_sediment_index > 0)       a(q_sediment_index)       = q_sediment
    if (TeqC_index > 0)             a(TeqC_index)             = TeqC
    if (Ta_Tw_index > 0)            a(Ta_Tw_index)            = Ta_Tw
    if (Esat_Eair_index > 0)        a(Esat_Eair_index)        = Esat_Eair
  end subroutine   
  !
end module