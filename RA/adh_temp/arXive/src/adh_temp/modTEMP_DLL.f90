! TEMP.dll interfaces
!
module modTEMP_DLL
  use kernel32
  use, intrinsic :: iso_c_binding
  implicit none
  !
  !===============================================================================================================
  ! Structure and array definitions
  integer nDependentVariable
  character*64, allocatable :: DependentVariable(:)
  !
  type StateVariableStruct
    character*64 :: name
    character*64 :: units
    integer      :: minCount
    integer      :: maxCount
    logical      :: isAdvected
    integer      :: count
  end type
  integer nStateVariable
  type(StateVariableStruct), allocatable :: StateVariable(:)
  !
  type DerivedVariableStruct   
    character*64 :: name
    character*64 :: units
    logical      :: isOn
  end type
  integer nDerivedVariable
  type(DerivedVariableStruct), allocatable :: DerivedVariable(:)
  !
  integer nParameter
  character*64, allocatable :: nameParameter(:)
  !
  type PathwayStruct
    character*64 :: name
    character*64 :: units
    logical      :: isOn
  end type 
  integer nPathway
  type(PathwayStruct), allocatable :: Pathway(:)
  !
  ! DLL basic information
  character*64 :: Title, Author, Version
  !
  !===============================================================================================================
  interface
    character*256 function DLLCharFunction_Delegate()
    end function
    !
    ! Subroutines SetRegionCount use this delegation.
    subroutine DLLSetCount_Delegate(Count)
      integer, intent(in):: Count
    end subroutine
    !
    ! Functions InitializeDLL and ContinueInitialization use this delegation. 
    logical function DLLLogicalFunction_Delegate()
    end function
    !
    ! Functions DependentVariableCount, StateVariableCount, DerivedVariableCount, ParameterCount, PathwayCount
    integer function DLLIntegerFunction_Delegate()
    end function
    !
    ! Subroutines DependentVariableData use this delegate
    subroutine DLLVariableName_Delegate(index, name)
      integer,       intent(in)  :: index
      character*(*), intent(out) :: name
    end subroutine
    !
    subroutine StateVariableData_Delegate(index, name, units, description, minCount, maxCount, isAdvected)         
      integer,       intent(in)  :: index
      character*(*), intent(out) :: name
      character*(*), intent(out) :: units
      character*(*), intent(out) :: description
      integer,       intent(out) :: minCount
      integer,       intent(out) :: maxCount
      logical,       intent(out) :: isAdvected
    end subroutine
    !
    subroutine DerivedVariableData_Delegate(index, name, units, description)         
      integer,       intent(in)  :: index
      character*(*), intent(out) :: name
      character*(*), intent(out) :: units
      character*(*), intent(out) :: description
    end subroutine
    !
    ! Functions ParameterIsGroup, ParameterIsReal and ParameterIsInteger use this delegate
    logical function ParameterIsGroupRealInteger_Delegate(index)
      integer, intent(in)  :: index
    end function
    !
    logical function GroupParameterData_Delegate(index, groupname, associatedStateVariable)
      integer,       intent(in)  :: index
      character*(*), intent(out) :: groupname
      character*(*), intent(out) :: associatedStateVariable
    end function
    !
    logical function RealParameterData_Delegate(index, name, groupname, units, default, sugMin, sugMax, absMin, absMax, isTC, theta, description)
      integer,       intent(in)  :: index
      character*(*), intent(out) :: name
      character*(*), intent(out) :: groupname      
      character*(*), intent(out) :: units      
      real(4),       intent(out) :: default
      real(4),       intent(out) :: sugMin
      real(4),       intent(out) :: sugMax
      real(4),       intent(out) :: absMin
      real(4),       intent(out) :: absMax
      logical,       intent(out) :: isTC
      real(4),       intent(out) :: theta      
      character*(*), intent(out) :: description
    end function
    !
    logical function IntegerParameterData_Delegate(index, name, groupname, default, possiblenames, description)
      integer,       intent(in)  :: index
      character*(*), intent(out) :: name
      character*(*), intent(out) :: groupname      
      integer,       intent(out) :: default
      character*(*), intent(out) :: possiblenames      
      character*(*), intent(out) :: description      
    end function
    !
    subroutine PathwayData_Delegate(index, name, units, description, fromConstituent, toConstituent)
      integer,       intent(in)  :: index
      character*(*), intent(out) :: name
      character*(*), intent(out) :: units
      character*(*), intent(out) :: description
      character*(*), intent(out) :: fromConstituent
      character*(*), intent(out) :: toConstituent
    end subroutine
    !
    subroutine SetStateVariableCount_Delegate(name, count)  
      character*(*), intent(in) :: name
      integer,       intent(in) :: count
    end subroutine
    !
    logical function SetRealParameter_Delegate(groupName, groupIndex, paramName, paramValue)
      character*(*), intent(in) :: groupName
      integer,       intent(in) :: groupIndex
      character*(*), intent(in) :: paramName
      real(8),       intent(in) :: paramValue(*)
    end function  
    !
    logical function SetIntegerParameter_Delegate(groupName, groupIndex, paramName, paramValue)
      character*(*),intent(in) :: groupName
      integer,      intent(in) :: groupIndex
      character*(*),intent(in) :: paramName
      integer,      intent(in) :: paramValue(*)
    end function
    !
    subroutine SetPathwayIndex_Delegate(n, names, pathwayName)
      integer,       intent(in) :: n
      character*(*), intent(in) :: names(n)
      character*(*), intent(in) :: pathwayName
    end subroutine
    !
    subroutine CompleteInitialization_Delegate(n, names, errorMessage)
      integer,       intent(in)  :: n
      character*(*), intent(in)  :: names(n)
      character*(*), intent(out) :: errorMessage
    end subroutine
    !
    subroutine ComputeKinetics_Delegate(region, tDay, dtDay, na, a)
      integer,  intent(in)     :: region
      real(8),  intent(in)     :: tDay, dtDay
      integer,  intent(in)     :: na
      real(8),  intent(inout)  :: a(na)
    end subroutine
    !
    subroutine ComputeDerivedVariables_Delegate(region, tDay, na, a)
      integer,  intent(in)    :: region
      real(8), intent(in)     :: tDay
      integer,  intent(in)    :: na
      real(8),  intent(inout) :: a(na)
    end subroutine
    !
  end interface
  !===============================================================================================================
  !
  ! Declare procedure pointer used to call the functions and subroutines in DLL
  procedure(DLLCharFunction_Delegate),             pointer :: DLLCharFunction
  procedure(DLLIntegerFunction_Delegate),          pointer :: DLLIntegerFunction
  procedure(DLLSetCount_Delegate),                 pointer :: SetRegionCount
  procedure(DLLLogicalFunction_Delegate),          pointer :: InitializeDLL
  procedure(DLLLogicalFunction_Delegate),          pointer :: ContinueInitialization
  procedure(SetStateVariableCount_Delegate),       pointer :: SetStateVariableCount
  procedure(ParameterIsGroupRealInteger_Delegate), pointer :: ParameterIsGroup
  procedure(ParameterIsGroupRealInteger_Delegate), pointer :: ParameterIsReal
  procedure(ParameterIsGroupRealInteger_Delegate), pointer :: ParameterIsInteger
  procedure(SetRealParameter_Delegate),            pointer :: SetRealParameter
  procedure(SetIntegerParameter_Delegate),         pointer :: SetIntegerParameter
  procedure(CompleteInitialization_Delegate),      pointer :: CompleteInitialization  
  procedure(SetPathwayIndex_Delegate),             pointer :: SetPathwayIndex
  procedure(ComputeKinetics_Delegate),             pointer :: ComputeKinetics
  procedure(ComputeDerivedVariables_Delegate),     pointer :: ComputeDerivedVariables
  !
  ! DLL handle
  integer(handle)     :: dll_handle
  !
  ! an addressed_sized integer variable that will get the return value of GetProcAddress
  integer(c_intptr_t) :: p_sub
  !
  ! Declare a Fortran function pointer used to convert the subroutine address returned by GetProcAddress to a procedure pointer
  type(c_funptr)      :: sub_c_funptr
  !===============================================================================================================
  contains
  !
  subroutine LoadDLL()
    !
    ! Load the DLL and get a 'handle' to it
    dll_handle = LoadLibrary("TEMP.dll"C)
    ! Check for errors
    if (dll_handle == NULL) then
      write(12, *) "error in loading 'TEMP.dll'"
      stop
    end if
    !
    ! Look up the routine address of Title, Author, Version
    p_sub = GetProcAddress(dll_handle, "Title"C)    
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'Title'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), DLLCharFunction)
    Title = DLLCharFunction()
    !---------------------------------------------------------------------------------
    !
    p_sub = GetProcAddress(dll_handle, "Author"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'Author'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), DLLCharFunction)
    Author = DLLCharFunction()
    !---------------------------------------------------------------------------------
    !
    p_sub = GetProcAddress(dll_handle, "Version"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'Version'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), DLLCharFunction)
    Version = DLLCharFunction()
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of SetRegionCount, SetMaxVerticalLayerNumber, SetSolidCount
    p_sub = GetProcAddress(dll_handle, "SetRegionCount"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'SetRegionCount'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), SetRegionCount)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of initializeDLL and ContinueInitialization
    p_sub = GetProcAddress(dll_handle, "InitializeDLL"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'InitializeDLL'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), InitializeDLL)
    !---------------------------------------------------------------------------------
    !
    p_sub = GetProcAddress(dll_handle, "ContinueInitialization"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'ContinueInitialization'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), ContinueInitialization)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of SetStateVariableCount
    p_sub = GetProcAddress(dll_handle, "SetStateVariableCount"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'SetStateVariableCount'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), SetStateVariableCount)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of SetRealParameter
    p_sub = GetProcAddress(dll_handle, "SetRealParameter"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'SetRealParameter'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), SetRealParameter)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of SetIntegerParameter
    ! TEMP.dll does not have SetIntegerParameter subroutine
!    p_sub = GetProcAddress(dll_handle, "SetIntegerParameter"C)
!    if (p_sub == NULL) then
!      write(12, *) "error in looking up function 'SetIntegerParameter'"
!      stop
!    end if
!    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), SetIntegerParameter)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of CompleteInitialization
    p_sub = GetProcAddress(dll_handle, "CompleteInitialization"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'CompleteInitialization'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), CompleteInitialization)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of SetPathwayIndex
    p_sub = GetProcAddress(dll_handle, "SetPathwayIndex"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'SetPathwayIndex'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), SetPathwayIndex)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of ComputeKinetics
    p_sub = GetProcAddress(dll_handle, "ComputeKinetics"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'ComputeKinetics'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), ComputeKinetics)
    !---------------------------------------------------------------------------------
    !
    ! Look up the routine address of ComputeDerivedVariables
    p_sub = GetProcAddress(dll_handle, "ComputeDerivedVariables"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'ComputeDerivedVariables'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), ComputeDerivedVariables)
    !
  end subroutine
  !
  !===============================================================================================================
  subroutine LoadStateVariables()
    procedure(DLLIntegerFunction_Delegate),        pointer :: StateVariableCount
    procedure(StateVariableData_Delegate),         pointer :: StateVariableData
    integer       :: index
    character*64  :: name
    character*64  :: units
    character*256 :: description
    integer       :: minCount
    integer       :: maxCount
    logical       :: isAdvected
    !
    ! Look up the routine address of StateVariableCount
    p_sub = GetProcAddress(dll_handle, "StateVariableCount"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'StateVariableCount'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), StateVariableCount)
    ! 
    ! Look up the routine address of StateVariableData
    p_sub = GetProcAddress(dll_handle, "StateVariableData"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'StateVariableData'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), StateVariableData)
    !
    nStateVariable = StateVariableCount()
    allocate(StateVariable(nStateVariable))
    do index = 1, nStateVariable
      call StateVariableData(index, name, units, description, minCount, maxCount, isAdvected)
      StateVariable(index).name       = name
      StateVariable(index).units      = units
      StateVariable(index).minCount   = minCount
      StateVariable(index).maxCount   = maxCount
      StateVariable(index).isAdvected = isAdvected
    end do
  end subroutine
  !
  !===============================================================================================================
  subroutine LoadDependentVariables()
    procedure(DLLIntegerFunction_Delegate),   pointer :: DependentVariableCount
    procedure(DLLVariableName_Delegate),      pointer :: DependentVariableData
    integer        :: index
    character*(64) :: name
    !
    ! Look up the routine address of DependentVariableCount
    p_sub = GetProcAddress(dll_handle, "DependentVariableCount"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'DependentVariableCount'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), DependentVariableCount)
    ! 
    ! Look up the routine address of DependentVariableData
    p_sub = GetProcAddress(dll_handle, "DependentVariableData"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'DependentVariableData'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), DependentVariableData)
    !
    nDependentVariable = DependentVariableCount()
    allocate(DependentVariable(nDependentVariable))
    do index = 1, nDependentVariable
      call DependentVariableData(index, name)
      DependentVariable(index) = name
    end do
  end subroutine
  !
  !===============================================================================================================
  !
  subroutine LoadDerivedVariables()
    procedure(DLLIntegerFunction_Delegate),        pointer :: DerivedVariableCount
    procedure(DerivedVariableData_Delegate),       pointer :: DerivedVariableData
    integer         :: index
    character*(64)  :: name
    character*(64)  :: units
    character*(256) :: description
    !
    ! Look up the routine address of DerivedVariableCount
    p_sub = GetProcAddress(dll_handle, "DerivedVariableCount"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'DerivedVariableCount'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), DerivedVariableCount)
    ! 
    ! Look up the routine address of DerivedVariableData
    p_sub = GetProcAddress(dll_handle, "DerivedVariableData"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'DerivedVariableData'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), DerivedVariableData)
    !
    nDerivedVariable = DerivedVariableCount()
    allocate(DerivedVariable(nDerivedVariable))
    do index = 1, nDerivedVariable
      call DerivedVariableData(index, name, units, description) 
      DerivedVariable(index).name  = name
      DerivedVariable(index).units = units
    end do
  end subroutine
  !
  !===============================================================================================================
  subroutine LoadParameters()  
    procedure(DLLIntegerFunction_Delegate),     pointer :: ParameterCount
    procedure(GroupParameterData_Delegate),     pointer :: GroupParameterData
    procedure(RealParameterData_Delegate),      pointer :: RealParameterData
    procedure(IntegerParameterData_Delegate),   pointer :: IntegerParameterData
    integer         :: index
    character*(64)  :: name
    character*(64)  :: groupname
    character*(64)  :: associatedStateVariable
    character*(64)  :: units      
    real(4)         :: default
    real(4)         :: sugMin
    real(4)         :: sugMax
    real(4)         :: absMin
    real(4)         :: absMax
    logical         :: isTC
    real(4)         :: theta      
    character*(256) :: description
    !
    integer         :: default_int
    character*(256) :: possiblenames
    !
    ! Look up the routine address of ParameterCount
    p_sub = GetProcAddress(dll_handle, "ParameterCount"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'ParameterCount'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), ParameterCount)
    ! 
    ! Look up the routine address of ParameterIsGroup, ParameterIsReal and ParameterIsInteger
    p_sub = GetProcAddress(dll_handle, "ParameterIsGroup"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'ParameterIsGroup'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), ParameterIsGroup)
    !
    p_sub = GetProcAddress(dll_handle, "ParameterIsReal"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'ParameterIsReal'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), ParameterIsReal)
    !
    p_sub = GetProcAddress(dll_handle, "ParameterIsInteger"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'ParameterIsInteger'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), ParameterIsInteger)
    ! 
    ! Look up the routine address of GroupParameterData, RealParameterData and IntegerParameterData
    p_sub = GetProcAddress(dll_handle, "GroupParameterData"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'GroupParameterData'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), GroupParameterData)
    !
    p_sub = GetProcAddress(dll_handle, "RealParameterData"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'RealParameterData'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), RealParameterData)
    !
    p_sub = GetProcAddress(dll_handle, "IntegerParameterData"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'IntegerParameterData'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), IntegerParameterData)
    !
    nParameter = ParameterCount()
    allocate(nameParameter(nParameter))
    do index = 1, nParameter
      if (ParameterIsGroup(index)) then
        if (GroupParameterData(index, groupname, associatedStateVariable)) nameParameter(index) = groupname
      else if (ParameterIsReal(index)) then
        if (RealParameterData(index, name, groupname, units, default, sugMin, sugMax, absMin, absMax, isTC, theta, description)) then
          nameParameter(index) = name
        end if
      else if (ParameterIsInteger(index)) then
        if (IntegerParameterData(index, name, groupname, default_int, possiblenames, description)) then
          nameParameter(index) = name
        end if
      end if
    end do
  end subroutine
  !
  !===============================================================================================================
  subroutine LoadPathways()
    procedure(DLLIntegerFunction_Delegate),   pointer :: PathwayCount
    procedure(PathwayData_Delegate),          pointer :: PathwayData
    integer         :: index
    character*(64)  :: name
    character*(64)  :: units
    character*(256) :: description
    character*(64)  :: fromConstituent
    character*(64)  :: toConstituent
    !
    ! Look up the routine address of PathwayCount
    p_sub = GetProcAddress(dll_handle, "PathwayCount"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up function 'PathwayCount'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), PathwayCount)
    ! 
    ! Look up the routine address of PathwayData
    p_sub = GetProcAddress(dll_handle, "PathwayData"C)
    if (p_sub == NULL) then
      write(12, *) "error in looking up subroutine 'PathwayData'"
      stop
    end if
    call C_F_PROCPOINTER (TRANSFER(p_sub, sub_c_funptr), PathwayData)
    !
    nPathway = PathwayCount()
    allocate(Pathway(nPathway))
    do index = 1, nPathway
      call PathwayData(index, name, units, description, fromConstituent, toConstituent)
      Pathway(index).name  = name
      Pathway(index).units = units
    end do
  end subroutine
  !
  !===============================================================================================================
  subroutine UnloadDLL()
    integer(bool)       :: free_status
    !
    free_status = FreeLibrary(dll_handle)
    if (free_status == 0) write(12, *) "fail in unloading TEMP.dll"
    !
  end subroutine
  !
end module