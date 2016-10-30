! $Id: mpif.h.template,v 1.2 2014/09/08 19:34:00 danielp Exp $
!
!       (C) COPYRIGHT SILICON GRAPHICS, INC.
!       UNPUBLISHED PROPRIETARY INFORMATION.
!       ALL RIGHTS RESERVED.
!
!
! Copyright Notice
!  + 1993 University of Chicago
!  + 1993 Mississippi State University

!  Modified 2016 03 29 Daniel Topa

module mpt_h

    use, intrinsic :: iso_fortran_env, only : REAL64

    implicit none

    integer, parameter :: rp = REAL64 ! real precision

    integer, parameter :: MPI_VERSION = 3, MPI_SUBVERSION = 0

    logical, parameter :: MPI_SUBARRAYS_SUPPORTED = .FALSE., MPI_ASYNC_PROTECTS_NONBLOCKING = .FALSE.

! MPI_Status

    integer, parameter :: MPI_STATUS_SIZE = 6

! Misc Fortran declarations

    integer :: MPI_BOTTOM
    common /MPI_SGI_PRIVATE/ MPI_BOTTOM

    external MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN, MPI_DUP_FN

    integer ::                       MPI_IN_PLACE( 1 )
    common /MPI_SGI_PRIVATE_INPLACE/ MPI_IN_PLACE

! MPI-2 Section 5.3

    CHARACTER ( LEN = 1 ) ::      MPI_ARGV_NULL( 1 )
    common /MPI_SGI_PRIVATE_CHAR/ MPI_ARGV_NULL

    CHARACTER ( LEN = 1 ) ::     MPI_ARGVS_NULL( 1 )
    EQUIVALENCE ( MPI_ARGV_NULL, MPI_ARGVS_NULL( 1 ) )

    integer ::                MPI_ERRCODES_IGNORE( 1 )
    EQUIVALENCE ( MPI_BOTTOM, MPI_ERRCODES_IGNORE( 1 ) )

! MPI-1 error codes and classes

    integer, parameter :: MPI_SUCCESS                   =   0
    integer, parameter :: MPI_ERR_BUFFER                =   1
    integer, parameter :: MPI_ERR_COUNT                 =   2
    integer, parameter :: MPI_ERR_TYPE                  =   3
    integer, parameter :: MPI_ERR_TAG                   =   4
    integer, parameter :: MPI_ERR_COMM                  =   5
    integer, parameter :: MPI_ERR_RANK                  =   6
    integer, parameter :: MPI_ERR_REQUEST               =   7
    integer, parameter :: MPI_ERR_ROOT                  =   8
    integer, parameter :: MPI_ERR_GROUP                 =   9
    integer, parameter :: MPI_ERR_OP                    =  10
    integer, parameter :: MPI_ERR_TOPOLOGY              =  11
    integer, parameter :: MPI_ERR_DIMS                  =  12
    integer, parameter :: MPI_ERR_ARG                   =  13
    integer, parameter :: MPI_ERR_UNKNOWN               =  14
    integer, parameter :: MPI_ERR_TRUNCATE              =  15
    integer, parameter :: MPI_ERR_OTHER                 =  16
    integer, parameter :: MPI_ERR_INTERN                =  17
    integer, parameter :: MPI_ERR_IN_STATUS             =  18
    integer, parameter :: MPI_ERR_PENDING               =  19

! MPI-2 error codes and classes

    integer, parameter :: MPI_ERR_ACCESS                =  28
    integer, parameter :: MPI_ERR_AMODE                 =  29
    integer, parameter :: MPI_ERR_ASSERT                =  30
    integer, parameter :: MPI_ERR_BAD_FILE              =  31
    integer, parameter :: MPI_ERR_BASE                  =  32
    integer, parameter :: MPI_ERR_CONVERSION            =  33
    integer, parameter :: MPI_ERR_DISP                  =  34
    integer, parameter :: MPI_ERR_DUP_DATAREP           =  35
    integer, parameter :: MPI_ERR_FILE_EXISTS           =  36
    integer, parameter :: MPI_ERR_FILE_IN_USE           =  37
    integer, parameter :: MPI_ERR_FILE                  =  38
    integer, parameter :: MPI_ERR_INFO_KEY              =  39
    integer, parameter :: MPI_ERR_INFO_NOKEY            =  40
    integer, parameter :: MPI_ERR_INFO_VALUE            =  41
    integer, parameter :: MPI_ERR_INFO                  =  42
    integer, parameter :: MPI_ERR_IO                    =  43
    integer, parameter :: MPI_ERR_KEYVAL                =  44
    integer, parameter :: MPI_ERR_LOCKTYPE              =  45
    integer, parameter :: MPI_ERR_NAME                  =  46
    integer, parameter :: MPI_ERR_NO_MEM                =  47
    integer, parameter :: MPI_ERR_NOT_SAME              =  48
    integer, parameter :: MPI_ERR_NO_SPACE              =  49
    integer, parameter :: MPI_ERR_NO_SUCH_FILE          =  50
    integer, parameter :: MPI_ERR_PORT                  =  51
    integer, parameter :: MPI_ERR_QUOTA                 =  52
    integer, parameter :: MPI_ERR_READ_ONLY             =  53
    integer, parameter :: MPI_ERR_RMA_CONFLICT          =  54
    integer, parameter :: MPI_ERR_RMA_SYNC              =  55
    integer, parameter :: MPI_ERR_SERVICE               =  56
    integer, parameter :: MPI_ERR_SIZE                  =  57
    integer, parameter :: MPI_ERR_SPAWN                 =  58
    integer, parameter :: MPI_ERR_UNSUPPORTED_DATAREP   =  59
    integer, parameter :: MPI_ERR_UNSUPPORTED_OPERATION =  60
    integer, parameter :: MPI_ERR_WIN                   =  61
    integer, parameter :: MPI_ERR_RMA_RANGE             =  62
    integer, parameter :: MPI_ERR_RMA_ATTACH            =  63
    integer, parameter :: MPI_ERR_RMA_SHARED            =  64
    integer, parameter :: MPI_ERR_RMA_FLAVOR            =  65
    integer, parameter :: MPI_T_ERR_CANNOT_INIT         =  66
    integer, parameter :: MPI_T_ERR_NOT_INITIALIZED     =  67
    integer, parameter :: MPI_T_ERR_MEMORY              =  68
    integer, parameter :: MPI_T_ERR_INVALID_INDEX       =  69
    integer, parameter :: MPI_T_ERR_INVALID_ITEM        =  70
    integer, parameter :: MPI_T_ERR_INVALID_SESSION     =  71
    integer, parameter :: MPI_T_ERR_INVALID_HANDLE      =  72
    integer, parameter :: MPI_T_ERR_OUT_OF_HANDLES      =  73
    integer, parameter :: MPI_T_ERR_OUT_OF_SESSIONS     =  74
    integer, parameter :: MPI_T_ERR_CVAR_SET_NOT_NOW    =  75
    integer, parameter :: MPI_T_ERR_CVAR_SET_NEVER      =  76
    integer, parameter :: MPI_T_ERR_PVAR_NO_WRITE       =  77
    integer, parameter :: MPI_T_ERR_PVAR_NO_STARTSTOP   =  78
    integer, parameter :: MPI_T_ERR_PVAR_NO_ATOMIC      =  79

    integer, parameter :: MPI_ERR_LASTCODE              = 100

! Permanent keyvals

    integer, parameter :: MPI_KEYVAL_INVALID            =   0
    integer, parameter :: MPI_TAG_UB                    =   5
    integer, parameter :: MPI_HOST                      =   6
    integer, parameter :: MPI_IO                        =   7
    integer, parameter :: MPI_WTIME_IS_GLOBAL           =   8
    integer, parameter :: MPI_UNIVERSE_SIZE             =  10
    integer, parameter :: MPI_APPNUM                    =  12
    integer, parameter :: MPI_LASTUSEDCODE              =  13

! Results of the compare operations

    integer, parameter :: MPI_IDENT                     =   0
    integer, parameter :: MPI_CONGRUENT                 =   1
    integer, parameter :: MPI_SIMILAR                   =   2
    integer, parameter :: MPI_UNEQUAL                   =   3

! Topology types

    integer, parameter :: MPI_GRAPH                     =   1
    integer, parameter :: MPI_CART                      =   2
    integer, parameter :: MPI_DIST_GRAPH                =   3

    integer ::                          MPI_UNWEIGHTED
    common /MPI_SGI_PRIVATE_UNWEIGHTED/ MPI_UNWEIGHTED
    integer ::                             MPI_WEIGHTS_EMPTY
    common /MPI_SGI_PRIVATE_WEIGHTS_EMPTY/ MPI_WEIGHTS_EMPTY

! Misc constants

    integer, parameter :: MPI_MAX_PROCESSOR_NAME            = 256

    integer, parameter :: MPI_MAX_ERROR_STRING              = 256

    integer, parameter :: MPI_MAX_LIBRARY_VERSION_STRING    = 256

    integer, parameter :: MPI_MAX_OBJECT_NAME               = 128

    integer, parameter :: MPI_BSEND_OVERHEAD                =  32

    integer, parameter :: MPI_ROOT                          =  -4

    integer, parameter :: MPI_UNDEFINED                     =  -3

    integer, parameter :: MPI_ANY_SOURCE                    =  -2

    integer, parameter :: MPI_PROC_NULL                     =  -1

    integer, parameter :: MPI_ANY_TAG                       =  -1

! The following 2 lines are included in the main mpif.h
!       double precision MPI_WTIME, MPI_WTICK
!       external MPI_WTIME, MPI_WTICK

! MPI-2 Section 4.10

    integer, parameter :: MPI_MAX_INFO_KEY  =  255

    integer, parameter :: MPI_MAX_INFO_VAL  = 1024

! MPI-2 Section 5.4

    integer, parameter :: MPI_MAX_PORT_NAME =  256

! Kind values for MPI-2

    integer, parameter :: MPI_INTEGER_KIND  = 4

    integer, parameter :: MPI_OFFSET_KIND   = 8

    integer, parameter :: MPI_ADDRESS_KIND  = 8

    integer, parameter :: MPI_COUNT_KIND    = 8

! Section 6.4 bindings for one-sided communication

    integer, parameter :: MPI_MODE_NOCHECK             =  1
    integer, parameter :: MPI_MODE_NOSTORE             =  2
    integer, parameter :: MPI_MODE_NOPUT               =  4
    integer, parameter :: MPI_MODE_NOPRECEDE           =  8
    integer, parameter :: MPI_MODE_NOSUCCEED           = 16

    integer, parameter :: MPI_LOCK_SHARED              =  1
    integer, parameter :: MPI_LOCK_EXCLUSIVE           =  2

! Thread-safety support levels

    integer, parameter :: MPI_THREAD_SINGLE            = 0
    integer, parameter :: MPI_THREAD_FUNNELED          = 1
    integer, parameter :: MPI_THREAD_SERIALIZED        = 2
    integer, parameter :: MPI_THREAD_MULTIPLE          = 3

! Permanent window keyvals

    integer, parameter :: MPI_WIN_BASE          =  4
    integer, parameter :: MPI_WIN_SIZE          =  5
    integer, parameter :: MPI_WIN_DISP_UNIT     =  6
    integer, parameter :: MPI_WIN_CREATE_FLAVOR =  8
    integer, parameter :: MPI_WIN_MODEL         = 10

! Typeclasses

    integer, parameter :: MPI_TYPECLASS_INTEGER = 1
    integer, parameter :: MPI_TYPECLASS_REAL    = 2
    integer, parameter :: MPI_TYPECLASS_COMPLEX = 3

! Communicator types

    integer, parameter :: MPI_COMM_TYPE_SHARED  = 1

! Window flavors

    integer, parameter :: MPI_WIN_FLAVOR_CREATE    = 1
    integer, parameter :: MPI_WIN_FLAVOR_ALLOCATE  = 2
    integer, parameter :: MPI_WIN_FLAVOR_DYNAMIC   = 3
    integer, parameter :: MPI_WIN_FLAVOR_SHARED    = 4

    integer, parameter :: MPI_WIN_SEPARATE = 1
    integer, parameter :: MPI_WIN_UNIFIED  = 2

! MPI-2 I/O definitions

!    Fortran MPI-IO programs
!    Copyright (C) 1997 University of Chicago.

    integer, parameter :: MPI_MODE_RDONLY       = 2,   MPI_MODE_RDWR = 8,               MPI_MODE_WRONLY = 4
    integer, parameter :: MPI_MODE_CREATE       = 1,   MPI_MODE_DELETE_ON_CLOSE = 16
    integer, parameter :: MPI_MODE_UNIQUE_OPEN  = 32,  MPI_MODE_EXCL = 64
    integer, parameter :: MPI_MODE_APPEND       = 128, MPI_MODE_SEQUENTIAL = 256

    integer, parameter :: MPI_FILE_NULL = 0

    integer, parameter :: MPI_MAX_DATAREP_STRING = 128

    integer, parameter :: MPI_SEEK_SET = 600, MPI_SEEK_CUR = 602, MPI_SEEK_END = 604

    integer, parameter :: MPIO_REQUEST_NULL = 0

!      INTEGER MPI_OFFSET_KIND
!      PARAMETER (MPI_OFFSET_KIND=8)

    integer, parameter :: MPI_DISPLACEMENT_CURRENT  = -54278278  ! integer*8 MPI_DISPLACEMENT_CURRENT

    integer, parameter :: MPI_ORDER_C               =     56
    integer, parameter :: MPI_ORDER_FORTRAN         =     57
    integer, parameter :: MPI_DISTRIBUTE_BLOCK      =    121
    integer, parameter :: MPI_DISTRIBUTE_CYCLIC     =    122
    integer, parameter :: MPI_DISTRIBUTE_NONE       =    123
    integer, parameter :: MPI_DISTRIBUTE_DFLT_DARG  = -49767

    EXTERNAL MPI_CONVERSION_FN_NULL

!   End Fortran MPI-IO

! MPI_Status

    integer :: MPI_STATUS_IGNORE ( MPI_STATUS_SIZE )
    integer :: MPI_STATUSES_IGNORE ( MPI_STATUS_SIZE )
    equivalence ( MPI_STATUS_IGNORE, MPI_STATUSES_IGNORE )

    common /MPI_SGI_PRIVATE_STATUS/ MPI_STATUS_IGNORE

! Permanent window keyvals

    external MPI_COMM_NULL_COPY_FN, MPI_COMM_NULL_DELETE_FN
    external MPI_COMM_DUP_FN
    external MPI_WIN_NULL_COPY_FN, MPI_WIN_NULL_DELETE_FN
    external MPI_WIN_DUP_FN

    integer, parameter :: MPI_FLOAT_INT        = 31
    integer, parameter :: MPI_DOUBLE_INT       = 32
    integer, parameter :: MPI_LONG_INT         = 33
    integer, parameter :: MPI_2INT             = 34
    integer, parameter :: MPI_SHORT_INT        = 35
    integer, parameter :: MPI_LONG_DOUBLE_INT  = 36

    integer, parameter :: MPI_BYTE             = 27
    integer, parameter :: MPI_PACKED           = 28
    integer, parameter :: MPI_UB               = 29
    integer, parameter :: MPI_LB               = 30

    integer, parameter :: MPI_2REAL            = 37
    integer, parameter :: MPI_2INTEGER         = 39

    integer, parameter :: MPI_AINT             = 55
    integer, parameter :: MPI_OFFSET           = 56

! MPI_Op

    integer, parameter :: MPI_OP_NULL  =  0
    integer, parameter :: MPI_MAX      =  1
    integer, parameter :: MPI_MIN      =  2
    integer, parameter :: MPI_SUM      =  3
    integer, parameter :: MPI_PROD     =  4
    integer, parameter :: MPI_LAND     =  5
    integer, parameter :: MPI_BAND     =  6
    integer, parameter :: MPI_LOR      =  7
    integer, parameter :: MPI_BOR      =  8
    integer, parameter :: MPI_LXOR     =  9
    integer, parameter :: MPI_BXOR     = 10
    integer, parameter :: MPI_MAXLOC   = 11
    integer, parameter :: MPI_MINLOC   = 12
    integer, parameter :: MPI_REPLACE  = 13
    integer, parameter :: MPI_NO_OP    = 14

! MPI_Datatype

    integer, parameter :: MPI_DATATYPE_NULL         =  0

    integer, parameter :: MPI_CHAR                  =  1
    integer, parameter :: MPI_SHORT                 =  2
    integer, parameter :: MPI_INT                   =  3
    integer, parameter :: MPI_LONG                  =  4
    integer, parameter :: MPI_UNSIGNED_CHAR         =  5
    integer, parameter :: MPI_UNSIGNED_SHORT        =  6
    integer, parameter :: MPI_UNSIGNED              =  7
    integer, parameter :: MPI_UNSIGNED_LONG         =  8
    integer, parameter :: MPI_FLOAT                 =  9
    integer, parameter :: MPI_DOUBLE                = 10
    integer, parameter :: MPI_LONG_DOUBLE           = 11
    integer, parameter :: MPI_LONG_LONG             = 12
    integer, parameter :: MPI_LONG_LONG_INT         = 12

    integer, parameter :: MPI_INTEGER               = 13
    integer, parameter :: MPI_REAL                  = 14
    integer, parameter :: MPI_DOUBLE_PRECISION      = 15
    integer, parameter :: MPI_COMPLEX               = 16
    integer, parameter :: MPI_DOUBLE_COMPLEX        = 17
    integer, parameter :: MPI_LOGICAL               = 18
    integer, parameter :: MPI_CHARACTER             = 19
    integer, parameter :: MPI_INTEGER1              = 20
    integer, parameter :: MPI_INTEGER2              = 21
    integer, parameter :: MPI_INTEGER4              = 22
    integer, parameter :: MPI_INTEGER8              = 23
    integer, parameter :: MPI_REAL4                 = 24
    integer, parameter :: MPI_REAL8                 = 25
    integer, parameter :: MPI_REAL16                = 26

    integer, parameter :: MPI_2DOUBLE_PRECISION     = 38

    integer, parameter :: MPI_WCHAR                 = 40
    integer, parameter :: MPI_SIGNED_CHAR           = 41
    integer, parameter :: MPI_UNSIGNED_LONG_LONG    = 42

    integer, parameter :: MPI_INTEGER16             = 43
    integer, parameter :: MPI_COMPLEX8              = 44
    integer, parameter :: MPI_COMPLEX16             = 45
    integer, parameter :: MPI_COMPLEX32             = 46

    integer, parameter :: MPI_INT8_T                = 47
    integer, parameter :: MPI_INT16_T               = 48
    integer, parameter :: MPI_INT32_T               = 49
    integer, parameter :: MPI_INT64_T               = 50
    integer, parameter :: MPI_UINT8_T               = 51
    integer, parameter :: MPI_UINT16_T              = 52
    integer, parameter :: MPI_UINT32_T              = 53
    integer, parameter :: MPI_UINT64_T              = 54

    integer, parameter :: MPI_C_BOOL                = 57
    integer, parameter :: MPI_C_FLOAT_COMPLEX       = 58
    integer, parameter :: MPI_C_COMPLEX             = 58
    integer, parameter :: MPI_C_DOUBLE_COMPLEX      = 59
    integer, parameter :: MPI_C_LONG_DOUBLE_COMPLEX = 60

    integer, parameter :: MPI_COUNT                 = 61

! MPI_Comm

    integer, parameter :: MPI_COMM_NULL             =  0
    integer, parameter :: MPI_COMM_WORLD            =  1
    integer, parameter :: MPI_COMM_SELF             =  2

! MPI_Errhandler

    integer, parameter :: MPI_ERRHANDLER_NULL       =  0
    integer, parameter :: MPI_ERRORS_ARE_FATAL      =  1
    integer, parameter :: MPI_ERRORS_RETURN         =  2

    integer, parameter :: MPI_SOURCE                =  1
    integer, parameter :: MPI_TAG                   =  2
    integer, parameter :: MPI_ERROR                 =  3

! MPI_Group

    integer, parameter :: MPI_GROUP_NULL            =  0
    integer, parameter :: MPI_GROUP_EMPTY           =  1

    integer, parameter :: MPI_INFO_NULL             =  0
    integer, parameter :: MPI_INFO_ENV              =  1

! MPI_Message

    integer, parameter :: MPI_MESSAGE_NO_PROC       = -1
    integer, parameter :: MPI_MESSAGE_NULL          =  0

! MPI_Request

    integer, parameter :: MPI_REQUEST_NULL          =  0

    integer, parameter :: MPI_WIN_NULL              =  0

    real ( rp ) :: MPI_WTIME, MPI_WTICK, PMPI_WTIME, PMPI_WTICK
    external       MPI_WTIME, MPI_WTICK, PMPI_WTIME, PMPI_WTICK

end module mpt_h
