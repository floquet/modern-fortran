! Full Energy Balance Temperature Simulation Module 
! Global parameters and dependent variables, state variables, derived variables 
!
module modGlobal
  implicit none
  !
  !
  integer, parameter :: R8 = 8
  integer, parameter :: R4 = 4            
  integer, parameter :: maxChar = 64
  integer, parameter :: maxDesc = 256
  integer, parameter :: maxList = 256     
  !
  ! number of region
  integer nRegion
  ! 
  ! current region
  integer r
  !
  ! time step
  real(R8) dt
  !
  ! stat variables on/off option
  logical use_TsedC
  !
  !===============================================================================================  
  ! general DLL information
  !===============================================================================================  
  character*maxDesc :: mTitle   = 'Water Temperature Simulation Module (TEMP)'
  character*maxDesc :: mAuthor  = 'IWR-HEC and ERDC-EL'
  character*maxDesc :: mVersion = 'V1.1 Beta'
  !===============================================================================================  
  ! dependant hydraulic variables
  !===============================================================================================  
  !
!  character*maxChar :: depth_name       = 'Depth'
!  real(R8)             depth
!  integer              depth_index   
  !
  real(R8)             avg_surface_area
  character*maxChar :: avg_surface_area_name   = 'Average Surface Area'
  integer              avg_surface_area_index     
  !
  real(R8)             avg_volume
  character*maxChar :: avg_volume_name         = 'Average Volume'
  integer              avg_volume_index     
  !
  !===============================================================================================  
  ! dependant meteorological variables
  !=============================================================================================== 
  real(R8)             q_solar
  integer              q_solar_index 
  character*maxChar :: q_solar_name = 'Solar Radiation'
  !     
  real(R8)             wind_speed
  integer              wind_speed_index 
  character*maxChar :: wind_speed_name = 'Wind Speed'
  !
  real(R8)             pressure_atm
  integer              pressure_atm_index
  character*maxChar :: pressure_atm_name = 'Atmospheric Pressure'  
  !
  real(R8)             TairC
  integer              TairC_index
  character*maxChar :: TairC_name = 'Air Temperature'          
  !
  real(R8)             cloudiness
  integer              cloudiness_index
  character*maxChar :: cloudiness_name ='Cloud Cover'         
  !
  real(R8)             eair_mb
  integer              eair_mb_index
  character*maxChar :: eair_mb_name ='Vapor Pressure'   
  !
  real(R8)             wind_coef_a
  integer              wind_coef_a_index
  character*maxChar :: wind_coef_a_name ='Coefficient a in wind function'   
  !
  real(R8)             wind_coef_b
  integer              wind_coef_b_index
  character*maxChar :: wind_coef_b_name ='Coefficient b in wind function'   
  !
  real(R8)             wind_coef_c
  integer              wind_coef_c_index
  character*maxChar :: wind_coef_c_name ='Coefficient c in wind function'   
  !
  real(R8)             wind_kh_kw
  integer              wind_kh_kw_index
  character*maxChar :: wind_kh_kw_name ='Diffusivity ratio'   
  !
  real(R8)             wind_Richardson                             ! 1 = Richardson Number used, 0 = Richardson Number ignored
  integer              wind_Richardson_index
  character*maxChar :: wind_Richardson_name ='Use of Richardson Number'   

  ! 
  !===============================================================================================  
  ! state variables
  !=============================================================================================== 
  real(R8)                        TwaterC               
  integer                         TwaterC_index  
  character*maxChar, parameter :: TwaterC_name     = 'Water Temperature'       
  real(R8)                     :: dTwaterCdt  
  integer                         dTwaterCdt_index
  !     
  real(R8)                        TsedC               
  integer                         TsedC_index  
  character*maxChar, parameter :: TsedC_name       = 'Sediment Temperature'       
  !
  !===============================================================================================  
  ! group name
  !=============================================================================================== 
  character*maxChar, parameter :: TempGroup        = 'Temperature'       
  !
end module