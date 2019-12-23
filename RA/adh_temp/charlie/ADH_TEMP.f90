! TEMP.dll wrapper for AdH
! Zhonglong Zhang, PhD, PE, PH
! Email: zhonglong.zhang@erdc.dren.mil
! Phone: 601-421-9661
! 4/12/2016
!
PROGRAM ADH_TEMP

    use mSetPrecision, only : rp
    use mFileHandling, only : safeopen_readonly, safeopen_writereplace

    implicit none

    ! parameters
    character ( len = * ), parameter :: path = 'io_files/'
    character ( len = * ), parameter :: TEMP_CONFN = path // 'ADH_TEMP_CON.npt'
    character ( len = * ), parameter :: TEMP_OPTFN = path // 'ADH_TEMP_RESULT.opt'
    ! initial settings
    integer :: nParameterline  = 5
    integer :: nDerivedVarline = 0
    integer :: nPathwayline    = 11

    ! allocatable rank 1 arrays
    real ( rp ),             allocatable, dimension ( : ) :: a
    real ( rp ),             allocatable, dimension ( : ) :: paramReal, paramReal_theta
    integer,                 allocatable, dimension ( : ) :: paramInteger
    character ( len = 256 ), allocatable, dimension ( : ) :: names
    logical,                 allocatable, dimension ( : ) :: transport_a

    ! rank 0
    real ( rp ) :: t = zero, dt = zero, tStart = zero, tEnd = zero ! avoid denormal variables

    integer :: na = 0
    integer :: groupIndex = 0
    integer :: region = 0, nRegion = 0
    integer :: i = 0, j = 0, count = 0, index = 0
    integer :: naStateVarStart = 0, naStateVarEnd = 0
    integer :: IO_READ_TEMP_CON = 0, IO_WRITE_TEMP_OPT = 0  ! io handles

    character ( len = 256 ) :: paramName = '', groupName = ''
    character ( len = 256 ) :: errorMessage = ''
    character ( len = 256 ) :: io_msg = ''
    character ( len = 64 )  :: name_tmpt = ''
    character ( len = 8 )   :: switch = '', buffer = ''

    logical               :: isTC = .false.
    logical               :: setParameter = .false.
    !
    !===============================================================================================================
    !
    ! open ADH-TEMP control file for read
    IO_READ_TEMP_CON = safeopen_readonly ( filename = TEMP_CONFN )
    !
    ! load DLL
    !call LoadDLL()
    !
    !===============================================================================================================
    !
    ! open computation result file
    IO_WRITE_TEMP_OPT = safeopen_writereplace ( filename = TEMP_OPTFN )
    write ( IO_WRITE_TEMP_OPT, '( A )' ) Title
    write ( IO_WRITE_TEMP_OPT, '( A )' ) Author
    write ( IO_WRITE_TEMP_OPT, '( A )' ) Version

end program ADH_TEMP
