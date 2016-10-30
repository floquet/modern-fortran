! TEMP.dll wrapper for AdH
! Zhonglong Zhang, PhD, PE, PH
! Email: zhonglong.zhang@erdc.dren.mil
! Phone: 601-421-9661
! 4/12/2016
!
PROGRAM ADH_TEMP
  use mSetPrecision, only : rp
  implicit none
   !use modTEMP_DLL,  only : rp, LoadDLL
  integer            :: region, nRegion
  integer            :: na
  integer            :: groupIndex
  character(len=64)  :: paramName, groupName
  character(len=256) :: errorMessage
  !
  real(rp),          allocatable, dimension(:)  :: a
  character(len=64), allocatable, dimension(:)  :: names
  real(rp),          allocatable, dimension(:)  :: paramReal, paramReal_theta
  integer,           allocatable, dimension(:)  :: paramInteger
  logical,           allocatable, dimension(:)  :: transport_a
  !
  real(rp)     :: t, dt, tStart, tEnd
  integer      :: TEMP_CON, TEMP_OPT
  !
  ! Example inputs and outputs
  character(len=*), PARAMETER :: TEMP_CONFN = 'ADH_TEMP_CON.npt'
  character(len=*), PARAMETER :: TEMP_OPTFN = 'ADH_TEMP_RESULT.opt'
  integer                     :: nParameterline  = 5
  integer                     :: nDerivedVarline = 0
  integer                     :: nPathwayline    = 11
  !
  !data TEMP_CON /66/, TEMP_OPT /88/
  !
  integer               :: i, j, count, index
  integer               :: naStateVarStart, naStateVarEnd
  character(len = 64)   :: name_tmpt
  character(len = 256)  :: io_msg
  character(len = 8)    :: switch, buffer
  logical               :: isTC
  logical               :: setParameter
  !
  !===============================================================================================================
  !
  ! open ADH-TEMP control file
  open( newunit = IO_TEMP_CON, file = TEMP_CONFN, status = 'OLD', iostat = i, iomsg = io_msg )
  if (i /= 0) then
    write(*,*) 'Control file does not exist.'
    write(*,'("Error opening file ",g0,/,"iostat = ",g0,/,"iomsg = ",g0,".")') TEMP_CONFN, i, trim(io_msg)
    stop
  end if

  end
  !
  ! load DLL
  !call LoadDLL()
  !
  !===============================================================================================================
  !
  ! open computation result file
  open( newunit = IO_TEMP_OPT, file = TEMP_OPTFN, status = 'unknown' )
  write(TEMP_OPT,'(A)') Title
  write(TEMP_OPT,'(A)') Author
  write(TEMP_OPT,'(A)') Version
  !
  !===============================================================================================================
  !
!   ! add state variables in DLL
!   if (.not. InitializeDLL()) write(*,*) 'state variables are not added correctly'
!   !
!   ! load state variable from DLL
!   call LoadStateVariables()
!   !
!   ! read state variables ON/OFF
!   read(TEMP_CON, '(///)')
!   do i = 1, nStateVariable
!     if (StateVariable(i).maxCount == 1) then
!       read(TEMP_CON, *) name_tmpt, switch
!       if (trim(switch) == 'ON') then
!         StateVariable(i).count = 1
!       else
!         StateVariable(i).count = 0
!       end if
!     else if (StateVariable(i).maxCount > 1) then
!       read(TEMP_CON, *) name_tmpt, StateVariable(i).count
!     end if
!     call SetStateVariableCount(StateVariable(i).name, StateVariable(i).count)
!   end do
!   !
!   if (.not. any(StateVariable.count > 0)) then
!     write(TEMP_OPT,'(A)') 'No state variable is turned on, water quality DLL stops.'
!     stop
!   end if
!   !
!   !===============================================================================================================
!   !
!   ! add derived variables, parameters and pathways in DLL
!   if (.not. ContinueInitialization()) write(*,*) 'Derived variables, parameters and pathways are not added correctly'
!   !
!   ! load dependent variables
!   call LoadDependentVariables()
!   !
!   ! load derived variables
!   call LoadDerivedVariables()
!   !
!   ! load parameters
!   call LoadParameters()
!   !
!   ! load pathways
!   call LoadPathways()
!   !
!   !===============================================================================================================
!   !
!   ! get value of na and names
!   na = nDependentVariable
!   do i = 1, nStateVariable
!     if (StateVariable(i).count > 0) then
!       if (StateVariable(i).isAdvected) then
!         na = na + 2 * StateVariable(i).count
!       else
!         na = na + StateVariable(i).count
!       end if
!     end if
!   end do
!   naStateVarStart = nDependentVariable + 1
!   naStateVarEnd   = na
!   allocate(transport_a(naStateVarStart:naStateVarEnd))
!   transport_a = .false.
!   !
!   if (nDerivedVariable > 0) then
!     DerivedVariable.isOn = .false.
!     read(TEMP_CON, '(/)')
!     do i = 1, nDerivedVarline
!       read(TEMP_CON, '(A)') name_tmpt
!       if (scan(name_tmpt, ',') /= 0) then
!         backspace(TEMP_CON)
!         read(TEMP_CON, *) name_tmpt, switch
!         do j = 1, nDerivedVariable
!           if (trim(name_tmpt) == DerivedVariable(j).name) then
!             if (trim(switch) == 'ON') then
!               DerivedVariable(j).isOn = .true.
!               na = na + 1
!             end if
!             exit
!           end if
!         end do
!       end if
!     end do
!   end if
!   !
!   if (nPathway > 0) then
!     Pathway.isOn = .false.
!     read(TEMP_CON, '(/)')
!     do i = 1, nPathwayline
!       read(TEMP_CON, '(A)') name_tmpt
!       if (scan(name_tmpt, ',') /= 0) then
!         backspace(TEMP_CON)
!         read(TEMP_CON, *) name_tmpt, switch
!         do j = 1, nPathway
!           if (trim(name_tmpt) == Pathway(j).name) then
!             if (trim(switch) == 'ON') then
!               Pathway(j).isOn = .true.
!               na = na + 1
!             end if
!             exit
!           end if
!         end do
!       end if
!     end do
!   end if
!   !
!   allocate(names(na), a(na))
!   names(1:nDependentVariable) = DependentVariable
!   count = nDependentVariable
!   do i = 1, nStateVariable
!     if (StateVariable(i).maxCount == 1) then
!       if (StateVariable(i).count > 0) then
!         count        = count + 1
!         names(count) = StateVariable(i).name
!         if (StateVariable(i).isAdvected) then
!           transport_a(count) = .true.
!           count              = count + 1
!           names(count)       = 'Rate change in ' // StateVariable(i).name
!         end if
!       end if
!       !
!     else if (StateVariable(i).maxCount > 1) then
!       do j = 1, StateVariable(i).count
!         count = count + 1
!         write(buffer,'(I8)') j
!         names(count) = trim(StateVariable(i).name) // trim(adjustl(buffer))
!         if (StateVariable(i).isAdvected) then
!           transport_a(count) = .true.
!           count              = count + 1
!           names(count)       = 'Rate change in ' // names(count-1)
!         end if
!       end do
!     end if
!   end do
!   !
!   do i = 1, nDerivedVariable
!     if (DerivedVariable(i).isOn) then
!       count        = count + 1
!       names(count) = DerivedVariable(i).name
!     end if
!   end do
!   do i = 1, nPathway
!     if (Pathway(i).isOn) then
!       count        = count + 1
!       names(count) = Pathway(i).name
!     end if
!   end do
!   !
!   !===============================================================================================================
!   !
!   ! read regionCount and send it to DLL
!   read(TEMP_CON, '(//i4)') nRegion
!   call SetRegionCount(nRegion)
!   allocate(paramReal(nRegion), paramReal_theta(nRegion), paramInteger(nRegion))
!   !
!   !===============================================================================================================
!   !
!   ! set dependent variable, state variable and derived variable index
!   call CompleteInitialization(na, names, errorMessage)
!   !
!   ! set pathwayIndex
!   do i = 1, nPathway
!     call SetPathwayIndex(na, names, Pathway(i).name)
!   end do
!   !
!   !===============================================================================================================
!   ! H&H model
!   ! read parameters and send them to DLL
!   read(TEMP_CON, '(/)')
!   do i = 1, nParameterline
!     read(TEMP_CON, *) name_tmpt
!     do index = 1, nParameter
!       if (trim(name_tmpt) == nameParameter(index)) exit
!     end do
!     !
!     if (index >= 1 .and. index <= nParameter) then
!       if (ParameterIsGroup(index)) then
!         groupname = name_tmpt
!         !
!         ! get groupIndex from groupname
!         do j = len(trim(groupname)), 1, -1
!           if (ichar(groupname(j:j)) < 48 .or. ichar(groupname(j:j)) > 57) exit
!         end do
!         if (j < len(trim(groupname))) then
!           read(groupname(j+1:len(trim(groupname))), '(i4)') groupIndex
!           groupname = groupname(1:j)
!         end if
!       else if (ParameterIsReal(index)) then
!         backspace(TEMP_CON)
!         read(TEMP_CON, *) paramName, (paramReal(j), paramReal_theta(j), j = 1, nRegion), isTC
!         if (isTC) then
!           setParameter = SetRealParameter(groupName, groupIndex, trim(paramName) // '_rc20', paramReal)
!           setParameter = SetRealParameter(groupName, groupIndex, trim(paramName) // '_theta', paramReal_theta)
!         else
!           setParameter = SetRealParameter(groupName, groupIndex, paramName, paramReal)
!         end if
!       else if (ParameterIsInteger(index)) then
!         backspace(TEMP_CON)
!         read(TEMP_CON, *) paramName, (paramInteger(j), j = 1, nRegion)
!         setParameter = SetIntegerParameter(groupName, groupIndex, paramName, paramInteger)
!       end if
!     end if
!   end do
!   !
!   !===============================================================================================================
!   ! H&H model
!   ! set initial conditions for state variables
!   a = 0.0_rp
!   do i = naStateVarStart, naStateVarEnd
!     select case (names(i))
!     case ('Water Temperature')
!       a(i) = 25.0_rp
!     case ('Sediment Temperature')
!       a(i) = 21.0_rp
!     end select
!   end do
!   !
!   !===============================================================================================================
!   ! H&H model
!   ! prepare to compute kinetics
!   write(TEMP_OPT, '(2A)', advance = 'no') 'Time', '|'
!   do i = naStateVarStart, na - 1
!     write(TEMP_OPT, '(2A)', advance = 'no') trim(names(i)), '|'
!   end do
!   write(TEMP_OPT, '(A)') trim(names(i))
!   !
!   tStart = 0.0_rp
!   dt     = 5.0_rp * 60.0 / 86400.0         ! time step = 5 min
!   tEnd   = 100 * dt
!   t      = tStart
!   !
!   do! while (t <= tEnd) deprecated
!     !
!     ! set current parameter WQ region number for the computation cell.
!     region = 2
!     !
!     ! update values of dependent variables at each time step.
!     do i = 1, naStateVarStart - 1
!       select case (names(i))
!       case ('Average Surface Area')
!         a(i) = 100.0_rp
!       case ('Average Volume')
!         a(i) = 150.0_rp
!       case ('Solar Radiation')
!         a(i) = 500.0_rp
!       case ('Wind Speed')
!         a(i) = 3.0_rp
!       case ('Atmospheric Pressure')
!         a(i) = 1.0_rp
!       case ('Air Temperature')
!         a(i) = 18.0_rp
!       case ('Cloud Cover')
!         a(i) = 0.6_rp
!       case ('Vapor Pressure')
!         a(i) = 26.0_rp
!       case ('Coefficient a in wind function')
!         a(i) = 1.522_rp
!       case ('Coefficient b in wind function')
!         a(i) = 3.323_rp
!       case ('Coefficient c in wind function')
!         a(i) = 1.069_rp
!       case ('Diffusivity ratio')
!         a(i) = 0.958_rp
!       case ('Use of Richardson Number')
!         a(i) = 1.0_rp
!       end select
!     end do
!     !
!     if (t == tStart) then
!       ! compute derived variables
!       call ComputeDerivedVariables(region, t, na, a)
!     else
!       !
!       ! compute change rate of variables dC/dt
!       call ComputeKinetics(region, t, dt, na, a)
!       !
!       ! do transport computation here using ADH code
!       !
!       !
!       !
!       !
!       !
!       !
!       ! add dC/dt to C for the state variable that needs transport
!       do i = naStateVarStart, naStateVarEnd
!         if (transport_a(i)) a(i) = a(i) + a(i + 1) * dt
!       end do
!       !
!       ! compute derived variables
!       call ComputeDerivedVariables(region, t, na, a)
!     end if
!     !
!     ! output state, derived variables and pathways
!     write(TEMP_OPT, '(F12.5, 200F15.5)') t, (a(i), i = naStateVarStart, na)
!     !
!     t = t + dt
!     if (t > tEnd) exit
!   end do
!   close(TEMP_OPT)
!   !===============================================================================================================
!   !
!   ! unloadDLL, deallocate variables
!   call UnloadDLL()
!   deallocate(a, names)
!   deallocate(DependentVariable, StateVariable, DerivedVariable, nameParameter, Pathway)
!   if (allocated(paramReal))      deallocate(paramReal, paramReal_theta, paramInteger)
!   if (allocated(transport_a))    deallocate(transport_a)
!   !
! END
