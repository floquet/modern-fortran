! Water QUality Module DLL definitions and functions
!
module modDLL
  use modGlobal
  implicit none
  !
  ! dependent varibles
  integer nDependentVariable  
  character*maxChar,allocatable:: DependentVariable(:)  
  !===============================================================================================  
  ! state variable structure and arrays
  !===============================================================================================  
  type StateVariableStruct   
    character*maxChar  name
    character*maxChar  units
    character*maxDesc  description
    integer            minCount, maxCount
    logical            isAdvected
  end type 
  integer nStateVariable
  type(StateVariableStruct), allocatable :: StateVariable(:)
  !===============================================================================================  
  ! Sediment diagenesis state variable structure -- only for NSMII
  !===============================================================================================  
  type SedFluxStateVariableStruct   
    character*maxChar  name
    character*maxChar  units
    character*maxDesc  description
  end type 
  integer nSedFluxStateVariable
  type(SedFluxStateVariableStruct), allocatable :: SedFluxStateVariable(:)
  !===============================================================================================  
  ! Kinetics selection structure/matrix -- only for CSM
  !===============================================================================================  
  integer nKineticSelection
  character*maxChar, allocatable :: KineticSelection(:)
  !===============================================================================================  
  ! derived variables structure
  !===============================================================================================  
  type DerivedVariableStruct   
    character*maxChar  name
    character*maxChar  units
    character*maxDesc  description
  end type
  integer nDerivedVariable
  type(DerivedVariableStruct), allocatable :: DerivedVariable(:)
  !===============================================================================================  
  ! parameter structures
  !===============================================================================================  
  !
  ! parameters
  type ParameterItemStruct
    integer     :: groupIndex
    integer     :: realIndex
    integer     :: integerIndex    
  end type      
  integer                                   nParameterItem
  type (ParameterItemStruct),allocatable :: ParameterItem(:)        
  !
  ! parameter groups
  type GroupParameterStruct   
    character*maxChar :: groupname
    character*maxChar :: associatedStateVariable
  end type    
  character*maxChar   :: currentGroup
  integer                                    nGroupParameter
  type (GroupParameterStruct),allocatable :: GroupParameter(:)      
  !
  ! real parameter structures
  type RealParameterStruct   
    character*maxChar :: groupname
    character*maxChar :: name
    character*maxChar :: units    
    real(R4)          :: default    
    real(R4)          :: suggestedMin
    real(R4)          :: suggestedMax
    real(R4)          :: absoluteMin
    real(R4)          :: absoluteMax
    logical           :: isTemperatureCorrected    
    real(R4)          :: theta
    character*128     :: description
  end type 
  integer                                      nRealParameter      
  type (RealParameterStruct),allocatable    :: RealParameter(:)  
  !
  ! integer parameters (selecting from list)
  type IntegerParameterStruct   
    character*maxChar :: groupname
    character*maxChar :: name        
    integer           :: default    
    character*maxList :: possiblenames
    character*maxDesc :: description
  end type 
  integer                                      nIntegerParameter
  type (IntegerParameterStruct),allocatable :: IntegerParameter(:)    
  !===============================================================================================  
  ! pathways
  !===============================================================================================  
  type PathwayStruct
    integer           :: index
    character*maxChar :: name
    character*maxChar :: units
    character*maxDesc :: description
    character*maxChar :: fromConstituent
    character*maxChar :: toConstituent
  end type 
  integer                             nPathway
  type(PathwayStruct), allocatable :: Pathway(:)
  !===============================================================================================  
  ! general structures
  !===============================================================================================  
  !
  ! Rate coefficient temperature correction    
  type TempCorrectionStruct
    real(R8) :: rc20             
    real(R8) :: theta             
  end type 
  contains
  !===============================================================================================  
  ! general DLL information
  !===============================================================================================  
  !
  ! DLL title
  character*maxDesc function Title()
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"Title" :: Title            
    Title = mTitle
  end function
  !
  ! DLL author
  character*maxDesc function Author()
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"Author" :: Author      
    Author = mAuthor
  end function
  !
  ! DLL version
  character*maxDesc function Version()
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"Version" :: Version        
    Version = mVersion
  end function
  !
  ! set the region count
  subroutine SetRegionCount(regionCount)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"SetRegionCount" :: SetRegionCount    
    integer, intent(in):: regionCount
    nRegion = regionCount
  end subroutine
  !
  ! 
  subroutine InitializeDLLCounters()
    nRegion               = 0 
    nDependentVariable    = 0
    nStateVariable        = 0
    nSedFluxStateVariable = 0
    nKineticSelection     = 0
    nDerivedVariable      = 0
    nParameterItem        = 0
    nGroupParameter       = 0
    nRealParameter        = 0
    nIntegerParameter     = 0
    nPathway              = 0
  end subroutine
  !===============================================================================================  
  ! dependent variable routines
  !===============================================================================================  
  !
  ! routine to add dependent variables
  logical function AddDependentVariable(name, errorMessage) 
    character*(*), intent(in)  :: name
    character*(*), intent(out) :: errorMessage
    nDependentVariable = nDependentVariable + 1
    if (nDependentVariable > size(DependentVariable)) then
      errorMessage ='Kinetics DependentVariable array not allocated large enough for all dependent variables'
      AddDependentVariable = .false.
    else
      DependentVariable(nDependentVariable) = name
      AddDependentVariable = .true.
    end if
  end function
  !
  ! get count of dependent variables and the data
  integer function DependentVariableCount()         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"DependentVariableCount" :: DependentVariableCount
    DependentVariableCount = nDependentVariable
  end function
  !
  subroutine DependentVariableData(index, name)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"DependentVariableData" :: DependentVariableData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    name = DependentVariable(index) 
  end subroutine
  !===============================================================================================  
  ! state variable routines
  !===============================================================================================  
  !
  ! routine to add state variables
  subroutine AddStateVariable(name, units, description, minCount, maxCount, isAdvected)    
    character*(*), intent(in) :: name
    character*(*), intent(in) :: units
    character*(*), intent(in) :: description
    integer,       intent(in) :: minCount
    integer,       intent(in) :: maxCount
    logical,       intent(in) :: isAdvected
    !
    !
    nStateVariable = nStateVariable + 1
    if (nStateVariable > size(StateVariable)) stop 'Kinetics StateVariable array not allocated large enough for all state variables'
    StateVariable(nStateVariable).name        = name
    StateVariable(nStateVariable).units       = units
    StateVariable(nStateVariable).description = description
    StateVariable(nStateVariable).minCount    = minCount
    StateVariable(nStateVariable).maxCount    = maxCount
    StateVariable(nStateVariable).isAdvected  = isAdvected
  end subroutine 
  !
  ! get count of state variables and the data
  integer function StateVariableCount()         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"StateVariableCount" :: StateVariableCount
    StateVariableCount = nStateVariable
  end function
  !
  subroutine StateVariableData(index, name, units, description, minCount, maxCount, isAdvected)         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"StateVariableData" :: StateVariableData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    character*(*), intent(out) :: units
    character*(*), intent(out) :: description
    integer,       intent(out) :: minCount
    integer,       intent(out) :: maxCount
    logical,       intent(out) :: isAdvected
    name        = StateVariable(index).name
    units       = StateVariable(index).units
    description = StateVariable(index).description
    minCount    = StateVariable(index).minCount
    maxCount    = StateVariable(index).maxCount
    isAdvected  = StateVariable(index).isAdvected
  end subroutine
  !===============================================================================================  
  ! Sediment diagenesis state variable routines -- only for NSMII
  !===============================================================================================  
  !
  ! routine to add sediment diagenesis state variables
  subroutine AddSedFluxStateVariable(name, units, description)    
    character*(*), intent(in) :: name
    character*(*), intent(in) :: units
    character*(*), intent(in) :: description
    !
    !
    nSedFluxStateVariable = nSedFluxStateVariable + 1
    if (nSedFluxStateVariable > size(SedFluxStateVariable)) stop 'Kinetics StateVariable array not allocated large enough for all state variables'
    SedFluxStateVariable(nSedFluxStateVariable).name        = name
    SedFluxStateVariable(nSedFluxStateVariable).units       = units
    SedFluxStateVariable(nSedFluxStateVariable).description = description
  end subroutine 
  !
  ! get count of sediment diagenesis state variables and the data
  integer function SedFluxStateVariableCount()         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"SedFluxStateVariableCount" :: SedFluxStateVariableCount
    SedFluxStateVariableCount = nSedFluxStateVariable
  end function
  !
  subroutine SedFluxStateVariableData(index, name, units, description)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"SedFluxStateVariableData" :: SedFluxStateVariableData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    character*(*), intent(out) :: units
    character*(*), intent(out) :: description
    name        = SedFluxStateVariable(index).name
    units       = SedFluxStateVariable(index).units
    description = SedFluxStateVariable(index).description
  end subroutine
  !===============================================================================================  
  ! Kinetic selection routines -- only for CSM
  !===============================================================================================  
  !
  ! routine to add kinetic selection
  subroutine AddKineticSelection(name)    
    character*(*), intent(in) :: name
    !
    !
    nKineticSelection = nKineticSelection + 1
    if (nKineticSelection > size(KineticSelection)) stop 'Kinetics Selection array not allocated large enough for all state variables'
    KineticSelection(nKineticSelection) = name
  end subroutine 
  !
  ! get count of kinetic selection and the data
  integer function KineticSelectionCount()         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"KineticSelectionCount" :: KineticSelectionCount
    KineticSelectionCount = nKineticSelection
  end function
  !
  subroutine KineticSelectionData(index, name)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"KineticSelectionData" :: KineticSelectionData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    name        = KineticSelection(index)
  end subroutine
  !===============================================================================================  
  ! derived routines
  !===============================================================================================  
  !
  ! routine to add derived variables
  subroutine AddDerivedVariable(name, units, description)    
    character*(*), intent(in) :: name
    character*(*), intent(in) :: units
    character*(*), intent(in) :: description
    !
    !
    nDerivedVariable = nDerivedVariable + 1
    if (nDerivedVariable > size(DerivedVariable)) stop 'Kinetics Derived Variable array not allocated large enough for all state variables'
    DerivedVariable(nDerivedVariable).name        = name
    DerivedVariable(nDerivedVariable).units       = units
    DerivedVariable(nDerivedVariable).description = description
  end subroutine 
  !
  ! get count of derived variables and the data
  integer function DerivedVariableCount()         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"DerivedVariableCount" :: DerivedVariableCount
    DerivedVariableCount = nDerivedVariable
  end function
  !
  subroutine DerivedVariableData(index, name, units, description)         
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"DerivedVariableData" :: DerivedVariableData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    character*(*), intent(out) :: units
    character*(*), intent(out) :: description
    !
    !
    name        = DerivedVariable(index).name
    units       = DerivedVariable(index).units
    description = DerivedVariable(index).description
  end subroutine  
  !===============================================================================================  
  ! parameters
  !===============================================================================================  
  !
  ! routine to add parameter (used internally by AddParameterGroup, AddRealParameter and AddIntegerParameter)
  subroutine AddParameterItem(groupIndex, realIndex, integerIndex)
    integer, intent(in)  :: groupIndex
    integer, intent(in)  :: realIndex
    integer, intent(in)  :: integerIndex
    nParameterItem = nParameterItem + 1
    if (nParameterItem > size(ParameterItem)) stop 'Kinetics ParameterItem array not allocated large enough for all parameters'    
    ParameterItem(nParameterItem).groupIndex   = groupIndex
    ParameterItem(nParameterItem).realIndex    = realIndex
    ParameterItem(nParameterItem).integerIndex = integerIndex
  end subroutine
  !
  ! add a group
  subroutine AddParameterGroup(groupname, associatedStateVariable)     
    character*(*), intent(in)  :: groupname
    character*(*), intent(in)  :: associatedStateVariable
    !
    ! store the curent group name so that it gets set in subsequent AddIntegerParameter and AddRealParameter calls
    currentGroup = groupname    
    !
    ! add to the group parameters
    nGroupParameter = nGroupParameter + 1
    if (nGroupParameter > size(GroupParameter)) stop 'Kinetics GroupParameter array not allocated large enough for all parameters'
    GroupParameter(nGroupParameter).groupname               = currentGroup
    GroupParameter(nGroupParameter).associatedStateVariable = associatedStateVariable
    !
    ! add to parameter items
    call AddParameterItem(nGroupParameter, 0, 0)
  end subroutine
  !
  ! add a real parameter
  subroutine AddRealParameter(name, units, default, sugMin, sugMax, absMin, absMax, isTC, theta, description)     
    character*(*), intent(in)  :: name
    character*(*), intent(in)  :: units
    character*(*), intent(in)  :: description
    real(kind=4),  intent(in)  :: default
    real(kind=4),  intent(in)  :: sugMin
    real(kind=4),  intent(in)  :: sugMax
    real(kind=4),  intent(in)  :: absMin
    real(kind=4),  intent(in)  :: absMax
    logical,       intent(in)  :: isTC      
    real(kind=4),  intent(in)  :: theta    
    !
    ! add to the real parameters
    nRealParameter = nRealParameter + 1
    if (nRealParameter > size(RealParameter)) stop 'Kinetics RealParameter array not allocated large enough for all parameters'
    RealParameter(nRealParameter).groupname              = currentGroup
    RealParameter(nRealParameter).name                   = name
    RealParameter(nRealParameter).units                  = units
    RealParameter(nRealParameter).description            = description
    RealParameter(nRealParameter).default                = default
    RealParameter(nRealParameter).suggestedMin           = sugMin
    RealParameter(nRealParameter).suggestedMax           = sugMax
    RealParameter(nRealParameter).absoluteMin            = absMin
    RealParameter(nRealParameter).absoluteMax            = absMax
    RealParameter(nRealParameter).isTemperatureCorrected = isTC      
    RealParameter(nRealParameter).theta                  = theta
    !
    ! add to parameter items
    call AddParameterItem(0, nRealParameter, 0)
  end subroutine
  !
  ! add an integer parameter
  subroutine AddIntegerParameter(name, default, possiblenames, description)   
    character*(*), intent(in)  :: name
    integer,       intent(in)  :: default
    character*(*), intent(in)  :: possiblenames
    character*(*), intent(in)  :: description
    !
    ! add to the integer parameters
    nIntegerParameter = nIntegerParameter + 1
    if (nIntegerParameter > size(IntegerParameter)) stop 'Kinetics IntegerParameter array not allocated large enough for all parameters'
    IntegerParameter(nIntegerParameter).groupname     = currentGroup
    IntegerParameter(nIntegerParameter).name          = name      
    IntegerParameter(nIntegerParameter).description   = description
    IntegerParameter(nIntegerParameter).default       = default
    IntegerParameter(nIntegerParameter).possiblenames = possiblenames   ! pipe|delimeted
    !
    ! add to parameter items
    call AddParameterItem(0, 0, nIntegerParameter)
  end subroutine
  !
  ! determine if a particular parameter is a 'group' type
  integer function ParameterCount() 
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"ParameterCount" :: ParameterCount
    ParameterCount = nParameterItem
  end function
  !
  ! determine if a particular parameter is a 'group' type
  logical function ParameterIsGroup(index) 
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"ParameterIsGroup" :: ParameterIsGroup    
    integer, intent(in)  :: index
    ParameterIsGroup = .false.
    if (ParameterItem(index).groupIndex > 0) ParameterIsGroup = .true.
  end function
  !
  ! determine if a particular parameter is a 'real' type
  logical function ParameterIsReal(index)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"ParameterIsReal" :: ParameterIsReal
    integer, intent(in)  :: index
    ParameterIsReal = .false.
    if (ParameterItem(index).realIndex > 0) ParameterIsReal = .true.
  end function
  !
  ! determine if a particular parameter is an 'integer' type
  logical function ParameterIsInteger(index)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"ParameterIsInteger" :: ParameterIsInteger
    integer, intent(in)  :: index
    ParameterIsInteger = .false.
    if (ParameterItem(index).integerIndex > 0) ParameterIsInteger = .true.
  end function
  !
  ! get the data for a Real type parameter
  logical function GroupParameterData(index, groupname, associatedStateVariable)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"GroupParameterData" :: GroupParameterData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: groupname
    character*(*), intent(out) :: associatedStateVariable
    !
    ! local variable
    integer  groupIndex    
    !
    ! 
    If (parameterItem(index).groupIndex > 0) then
      groupIndex = parameterItem(index).groupIndex
      groupname               = GroupParameter(groupIndex).groupname
      associatedStateVariable = GroupParameter(groupIndex).associatedStateVariable       
      GroupParameterData = .true.
    else
      GroupParameterData = .false.
    end if    
  end function
  !
  ! get the data for a Real type parameter
  logical function RealParameterData(index, name, groupname, units, default, sugMin, sugMax, absMin, absMax, isTC, theta, description)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"RealParameterData" :: RealParameterData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    character*(*), intent(out) :: groupname      
    character*(*), intent(out) :: units      
    real(R4),      intent(out) :: default
    real(R4),      intent(out) :: sugMin
    real(R4),      intent(out) :: sugMax
    real(R4),      intent(out) :: absMin
    real(R4),      intent(out) :: absMax
    logical,       intent(out) :: isTC
    real(R4),      intent(out) :: theta      
    character*(*), intent(out) :: description
    !
    ! local variable
    integer  realIndex    
    !
    ! 
    If (parameterItem(index).realIndex > 0) then
      realIndex   = parameterItem(index).realIndex      
      name        = RealParameter(realIndex).name
      groupname   = RealParameter(realIndex).groupname
      units       = RealParameter(realIndex).units
      description = RealParameter(realIndex).description 
      default     = RealParameter(realIndex).default 
      sugMin      = RealParameter(realIndex).suggestedMin
      sugMax      = RealParameter(realIndex).suggestedMax
      absMin      = RealParameter(realIndex).absoluteMin
      absMax      = RealParameter(realIndex).absoluteMax
      isTC        = RealParameter(realIndex).isTemperatureCorrected
      theta       = RealParameter(realIndex).theta        
      RealParameterData = .true.
    else
      RealParameterData = .false.
    end if
  end function
  !
  ! get the data for an Integer type parameter
  logical function IntegerParameterData(index, name, groupname, default, possiblenames, description)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"IntegerParameterData" :: IntegerParameterData   
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    character*(*), intent(out) :: groupname      
    integer,       intent(out) :: default
    character*(*), intent(out) :: possiblenames      
    character*(*), intent(out) :: description      
    !
    ! local variables
    integer       integerIndex
    !
    ! 
    If (parameterItem(index).integerIndex > 0) then
      integerIndex = parameterItem(index).integerIndex    
      name          = IntegerParameter(integerIndex).name
      groupname     = IntegerParameter(integerIndex).groupname        
      default       = IntegerParameter(integerIndex).default 
      possiblenames = IntegerParameter(integerIndex).possibleNames 
      description   = IntegerParameter(integerIndex).description 
      IntegerParameterData = .true.
    else
      IntegerParameterData = .false.
    end if
  end function  
  !===============================================================================================  
  ! pathways
  !===============================================================================================  
  integer function PathwayCount
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"PathwayCount" :: PathwayCount
    PathwayCount = nPathway
  end function
  !
  ! add pathway
  subroutine AddPathway(name, units, description, fromConstituent, toConstituent)    
    character*(*), intent(in) :: name
    character*(*), intent(in) :: units
    character*(*), intent(in) :: description
    character*(*), intent(in) :: fromConstituent
    character*(*), intent(in) :: toConstituent
    nPathway = nPathway + 1
    pathway(nPathway).name            = name
    pathway(nPathway).Units           = units
    pathway(nPathway).description     = description
    pathway(nPathway).fromConstituent = fromConstituent
    pathway(nPathway).toConstituent   = toConstituent
  end subroutine
  !
  ! get a pathways data
  subroutine PathwayData(index, name, units, description, fromConstituent, toConstituent)
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"PathwayData" :: PathwayData
    integer,       intent(in)  :: index
    character*(*), intent(out) :: name
    character*(*), intent(out) :: units
    character*(*), intent(out) :: description
    character*(*), intent(out) :: fromConstituent
    character*(*), intent(out) :: toConstituent
    !
    ! set data 
    name            = Pathway(index).name
    units           = Pathway(index).Units
    description     = Pathway(index).Description
    fromConstituent = Pathway(index).FromConstituent
    toConstituent   = Pathway(index).ToConstituent
  end subroutine
  !===============================================================================================  
  ! functions to find variables in the main array
  !===============================================================================================  
  !
  ! required variables
  logical function SetRequiredIndex(n, names, name, index, errorMessage)        
    integer,       intent(in)  :: n       
    character*(*), intent(in)  :: names(n)
    character*(*), intent(in)  :: name
    integer,       intent(out) :: index       
    character*(*), intent(out) :: errorMessage
    integer i
    !
    ! find index for variable in list    
    do i = 1, n
      if (name == names(i)) then
        index = i
        SetRequiredIndex = .true.        
        return
      end if
    end do
    !
    ! still here, did not find the index
    index = 0
    errorMessage = "Required variable '" // trim(name) // "' not found in variable list." 
    SetRequiredIndex = .false.
  end function
  !
  ! optional variables
  logical function SetOptionalIndex(n, names, name, index)   
    integer,       intent(in)  :: n       
    character*(*), intent(in)  :: names(n)
    character*(*), intent(in)  :: name
    integer,       intent(out) :: index       
    integer i
    !
    ! find index for variable in list    
    do i = 1, n
      if (name == names(i)) then
        index = i
        SetOptionalIndex = .true.        
        return
      end if
    end do
    !
    ! still here, did not find the index
    index = 0
    SetOptionalIndex = .false.
  end function
  !
  !
  character*maxChar function AddIndex(basename, index)
    integer,       intent(in) :: index
    character*(*), intent(in) :: basename
    character*8 :: buffer
    write(buffer,'(I8)') index
    AddIndex = trim(basename) // trim(adjustl(buffer))
  end function
  !===============================================================================================  
  ! temperature correction function used in all kinetics sets
  !===============================================================================================    
  function Arrhenius_TempCorrection(coeff, TwaterC)    
    real(R8)                   :: Arrhenius_TempCorrection
    type(TempCorrectionStruct) :: coeff
    real(R8)                   :: TwaterC
    !
    ! perform the correction
    Arrhenius_TempCorrection = coeff.rc20 * (coeff.theta**(TwaterC - 20.0))	  
  end function 
end module
