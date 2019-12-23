!===============================================================================================  
!
! Full Energy Balance Temperature Simulation Module
! 
! Sediment temperature is allowed to turn on and off.
! Pathways can be turned on and off.
!
!===============================================================================================  
module modMain  
  use       modGlobal
  use       modDLL
  use       modHeatFlux
  implicit none
  !
  contains
  !===============================================================================================  
  ! initialize DLL
  !===============================================================================================  
  logical function InitializeDLL(errorMessage)         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"InitializeDLL" :: InitializeDLL
    character*(*), intent(out) :: errorMessage
    InitializeDLL = .false.
    call InitializeDLLCounters    
    !
    ! dependant variables  (Tair, wind_speed, solar_radiation, air pressure, cloudiness, vapour pressure, volume, surface_area)      
    if (allocated(DependentVariable)) deallocate(DependentVariable)     
    allocate (DependentVariable(13))
    if (AddDependentVariable(avg_surface_area_name,   errorMessage) == .false.) return 
    if (AddDependentVariable(avg_volume_name,         errorMessage) == .false.) return 
    if (AddDependentVariable(q_solar_name,            errorMessage) == .false.) return 
    if (AddDependentVariable(wind_speed_name,         errorMessage) == .false.) return
    if (AddDependentVariable(wind_coef_a_name,        errorMessage) == .false.) return
    if (AddDependentVariable(wind_coef_b_name,        errorMessage) == .false.) return
    if (AddDependentVariable(wind_coef_c_name,        errorMessage) == .false.) return
    if (AddDependentVariable(wind_kh_kw_name,         errorMessage) == .false.) return
    if (AddDependentVariable(wind_Richardson_name,    errorMessage) == .false.) return
    if (AddDependentVariable(pressure_atm_name,       errorMessage) == .false.) return 
    if (AddDependentVariable(TairC_name,              errorMessage) == .false.) return 
    if (AddDependentVariable(cloudiness_name,         errorMessage) == .false.) return 
    if (AddDependentVariable(eair_mb_name,            errorMessage) == .false.) return 
    !
    ! state variables   (TwaterC, TsedimentC)     
    if (allocated(StateVariable)) deallocate(StateVariable)      
    allocate (StateVariable(2))
    call AddStateVariable(TwaterC_name, 'oC',   'Water Temperature',     1, 1,  .true.)
    call AddStateVariable(TsedC_name,   'oC',   'Sediment Temperature',  0, 1,  .false.)
    !
    ! return true if still here
    InitializeDLL = .true.
  end function
  !
  !===============================================================================================================  
  ! set state variables count used in computations
  !===============================================================================================================
  subroutine SetStateVariableCount(name, count)  
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"SetStateVariableCount" :: SetStateVariableCount
    character*(*), intent(in) :: name
    integer,       intent(in) :: count
    !
    ! find the state variable
    select case (name)
      case (TsedC_name)
        !
        ! set use_TsedC
        if (count > 0) then
          use_TsedC = .true.
        else
          use_TsedC = .false.
        end if
    end select
  end subroutine 
  !
  !===============================================================================================================  
  ! continue to initialize DLL, add derived variables, parameters and pathways. 
  !===============================================================================================================
  logical function ContinueInitialization()
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"ContinueInitialization" :: ContinueInitialization
    integer :: i
    character*maxChar :: name_tempt
    ContinueInitialization = .false.
    !
    ! no derived variable
    !
    ! allocate spaces for full parameters list including groups, integer and real parameters
    if (allocated(ParameterItem)) then
      deallocate(ParameterItem, GroupParameter, RealParameter)
      nParameterItem     = 0
      nGroupParameter    = 0
      nRealParameter     = 0
      nIntegerParameter  = 0
    end if
    !
    if (use_TsedC) then 
      allocate(ParameterItem(5), GroupParameter(1), RealParameter(4))
      call AddParameterGroup(TempGroup, '')
      call AddRealParameter('h2',       'm',         0.1,     0.01,     1.0,    0.001, 10.0,    .false.,  0.0, 'Sediment layer thickness')
      call AddRealParameter('pb',       'kg/m3',     1600.0,  1500.0,   2700.0,  0.0,  5000.0,  .false.,  0.0, 'Sediment bulk density')
      call AddRealParameter('Cps',      'J/kg/m3',   1674.72, 795.492,  2219.0,  0.0,  8000.0,  .false.,  0.0, 'Sediment specific heat capacity')
      call AddRealParameter('alphas',   'm2/d',      0.0432,  0.01728,  0.10368, 0.0,  10.0,    .false.,  0.0, 'Sediment thermal diffusivity')
    end if  
    !
    ! Pathways
    if (allocated(Pathway)) then
      deallocate(Pathway)
      nPathway = 0
    end if
    !
    if (use_TsedC) then
      allocate(Pathway(9 + 1 + 1))
    else
      allocate(Pathway(9 + 1))
    end if
    !
    call AddPathway(TempGroup,                    ''    ,    '',                                                        '',           '')
    call AddPathway('q_net',                      'W/m2',    'Net heat flux',                                           'Sun',        TwaterC_name) 
    call AddPathway('q_solar',                    'W/m2',    'Solar radiation flux',                                    TwaterC_name, 'Air') 
    call AddPathway('q_latent',                   'W/m2',    'Latent heat flux',                                        'Air',        TwaterC_name) 
    call AddPathway('q_sensible',                 'W/m2',    'Sensible heat flux',                                      TwaterC_name, 'Air') 
    call AddPathway('q_longwave_up',              'W/m2',    'Back (upwelling) longwave radiation flux',                TwaterC_name, 'Air') 
    call AddPathway('q_longwave_down',            'W/m2',    'Atmospheric (downwelling) longwave radiation flux',       'Air',        TwaterC_name) 
    if (use_TsedC) call AddPathway('q_sediment',  'W/m2',    'Sediment-water heat flux',                                TsedC_name,   TwaterC_name) 
    call AddPathway('TeqC',                       'oC',      'Equilibrium temperature',                                 TwaterC_name, '') 
    call AddPathway('Ta-Tw',                      'oC',      'Difference between air and water temperature',            '',           '') 
    call AddPathway('Esat-Eair',                  'mb',      'Difference between saturated and current vapor pressure', '',           '') 
    !
    ! return true if still here
    ContinueInitialization = .true.
  end function
  ! 
  !===============================================================================================  
  ! set the data for a real type parameter
  !===============================================================================================  
  logical function SetRealParameter(groupName, groupIndex, paramName, paramValue)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"SetRealParameter" :: SetRealParameter
    character*(*), intent(in) :: groupName
    integer,       intent(in) :: groupIndex
    character*(*), intent(in) :: paramName
    real(R8),      intent(in) :: paramValue(nRegion)
    !
    !
    if (groupName == TempGroup) then          
      SetRealParameter = SetTemperatureRealParameter(paramName, paramValue)
    else
      !
      ! did not find the groupname, return false
      SetRealParameter = .false.
    end if
  end function  
  !
  !===============================================================================================  
  ! Find all pathway indexes
  !=============================================================================================== 
  subroutine SetPathwayIndex(n, names, pathwayName)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"SetPathwayIndex" :: SetPathwayIndex
    integer,       intent(in) :: n
    character*(*), intent(in) :: names(n)
    character*(*), intent(in) :: pathwayName
    integer   index
    !
    ! 
    if (SetOptionalIndex(n, names, pathwayName, index) == .true.) then
      if (SetTemperaturePathwayIndex(pathwayName, index) == .true.) return
    end if
  end subroutine
  !
  !===============================================================================================  
  ! complete initialization
  !=============================================================================================== 
  subroutine CompleteInitialization(n, names, errorMessage)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"CompleteInitialization" :: CompleteInitialization
    integer,       intent(in)  :: n
    character*(*), intent(in)  :: names(n)
    character*(*), intent(out) :: errorMessage
    !
    ! locals
    integer         i
    logical         success
    !
    !
    if (nRegion == 0) then 
      errorMessage = "Number of Regions not set"
      return
    end if
    !
    ! assign indexes for the array used to transfer information into this dll
    !
    ! dependent variable indexes
    if (SetRequiredIndex(n, names, avg_surface_area_name,    avg_surface_area_index,   errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, avg_volume_name,          avg_volume_index,         errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, q_solar_name,             q_solar_index,            errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, wind_speed_name,          wind_speed_index,         errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, wind_coef_a_name,         wind_coef_a_index,        errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, wind_coef_b_name,         wind_coef_b_index,        errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, wind_coef_c_name,         wind_coef_c_index,        errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, wind_kh_kw_name,          wind_kh_kw_index,         errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, wind_Richardson_name,     wind_Richardson_index,    errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, pressure_atm_name,        pressure_atm_index,       errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, TairC_name,               TairC_index,              errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, cloudiness_name,          cloudiness_index,         errorMessage) == .false.) return
    if (SetRequiredIndex(n, names, eair_mb_name,             eair_mb_index,            errorMessage) == .false.) return
    !
    ! state variable indexes   
    if (SetRequiredIndex(n, names, TwaterC_name,            TwaterC_index,             errorMessage) == .false.) return
    dTwaterCdt_index = TwaterC_index + 1
    if (use_TsedC) then
      if (SetRequiredIndex(n, names, TsedC_name,            TsedC_index,               errorMessage) == .false.) return
    end if
    !
    ! initialize 
    call InitializeTemperature()
  end subroutine
  !
  !===============================================================================================  
  ! compute kinetics
  !=============================================================================================== 
  subroutine ComputeKinetics(region, tDay, dtDay, na, a)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"ComputeKinetics" :: ComputeKinetics
    integer,  intent(in)    :: region
    real(R8), intent(in)    :: tDay
    real(R8), intent(in)    :: dtDay
    integer,  intent(in)    :: na
    real(R8), intent(inout) :: a(na)
    integer   i
    !
    ! set region number
    r = region
    ! 
    ! set time step
    dt = dtDay
    !
    ! get the values from the array to local variables
    ! dependent variables   
    avg_surface_area = a(avg_surface_area_index)
    avg_volume       = a(avg_volume_index) 
    q_solar          = a(q_solar_index)
    wind_speed       = a(wind_speed_index)
    wind_coef_a      = a(wind_coef_a_index)
    wind_coef_b      = a(wind_coef_b_index)
    wind_coef_c      = a(wind_coef_c_index)
    wind_kh_kw       = a(wind_kh_kw_index)
    wind_Richardson  = a(wind_Richardson_index)
    pressure_atm     = a(pressure_atm_index)
    TairC            = a(TairC_index)
    cloudiness       = a(cloudiness_index)
    eair_mb          = a(eair_mb_index)
    !
    ! state variables
    TwaterC = a(TwaterC_index)
    if (use_TsedC) TsedC = a(TsedC_index)
    !
    ! compute kinetics
    call ComputeHeatFlux()
    !
    ! copy change in state variables back to the transfer array
    a(dTwaterCdt_index) = dTwaterCdt
    if (use_TsedC) a(TsedC_index) = TsedC
    !
    ! copy pathways back to array aPathway
    call HeatFluxPathwayOutput(na, a)
    !
  end subroutine  
  !
  !===============================================================================================================================  
  ! compute derived variables
  !===============================================================================================================================
  subroutine ComputeDerivedVariables(region, tDay, na, a)
  !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"ComputeDerivedVariables" :: ComputeDerivedVariables
    integer,  intent(in)    :: region
    integer,  intent(in)    :: tDay
    integer,  intent(in)    :: na
    real(R8), intent(inout) :: a(na)
    !
    ! No derived variables
    !
  end subroutine  
!
end module