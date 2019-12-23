! TEMP.dll wrapper for AdH
! Zhonglong Zhang, PhD, PE, PH
! Email: zhonglong.zhang@erdc.dren.mil
! Phone: 601-421-9661
! 4/12/2016
!
PROGRAM ADH_TEMP
  ! use, intrinsic :: iso_fortran_env,    only : REAL32, REAL64
  ! integer, parameter :: sp = REAL32, dp = REAL64, rp = dp ! precision controlled at one line
  use modTEMP_DLL,  only : rp, LoadDLL
  implicit none
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
  open( newunit = TEMP_CON, file = TEMP_CONFN, status = 'OLD', iostat = i, iomsg = io_msg )
  if (i /= 0) then
    !write(*,*) 'Control file does not exist.'
    write(*,'( "Error opening file ", g0, /, "iostat = ",g0, /, "iomsg = ",g0, "." )') TEMP_CONFN, i, trim(io_msg)
    stop
  end if

  call LoadDLL()

end
