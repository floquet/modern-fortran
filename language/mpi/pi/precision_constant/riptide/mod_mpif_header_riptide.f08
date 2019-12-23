module mpif_riptide

    use, intrinsic :: iso_fortran_env, only : REAL64, INT64

    integer, parameter :: dp = REAL64, ip = INT64

    integer, parameter :: MPI_SOURCE = 3, MPI_TAG = 4, MPI_ERROR = 5
    integer, parameter :: MPI_STATUS_SIZE = 5

    integer :: MPI_STATUS_IGNORE   ( MPI_STATUS_SIZE )
    integer :: MPI_STATUSES_IGNORE ( MPI_STATUS_SIZE, 1 )
    integer :: MPI_ERRCODES_IGNORE ( 1 )

    character ( len = 1 ) :: MPI_ARGVS_NULL ( 1, 1 )
    character ( len = 1 ) :: MPI_ARGV_NULL ( 1 )

    integer, parameter :: MPI_SUCCESS = 0
    integer, parameter :: MPI_ERR_OTHER = 15
    integer, parameter :: MPI_ERR_WIN = 45
    integer, parameter :: MPI_ERR_FILE = 27
    integer, parameter :: MPI_ERR_COUNT = 2
    integer, parameter :: MPI_ERR_SPAWN = 42
    integer, parameter :: MPI_ERR_BASE = 46
    integer, parameter :: MPI_ERR_RMA_CONFLICT = 49
    integer, parameter :: MPI_ERR_IN_STATUS = 17
    integer, parameter :: MPI_ERR_INFO_KEY = 29
    integer, parameter :: MPI_ERR_LOCKTYPE = 47
    integer, parameter :: MPI_ERR_OP = 9
    integer, parameter :: MPI_ERR_ARG = 12
    integer, parameter :: MPI_ERR_READ_ONLY = 40
    integer, parameter :: MPI_ERR_SIZE = 51
    integer, parameter :: MPI_ERR_BUFFER = 1
    integer, parameter :: MPI_ERR_DUP_DATAREP = 24
    integer, parameter :: MPI_ERR_UNSUPPORTED_DATAREP = 43
    integer, parameter :: MPI_ERR_LASTCODE = 1073741823
    integer, parameter :: MPI_ERR_TRUNCATE = 14
    integer, parameter :: MPI_ERR_DISP = 52
    integer, parameter :: MPI_ERR_PORT = 38
    integer, parameter :: MPI_ERR_INFO_NOKEY = 31
    integer, parameter :: MPI_ERR_ASSERT = 53
    integer, parameter :: MPI_ERR_FILE_EXISTS = 25
    integer, parameter :: MPI_ERR_PENDING = 18
    integer, parameter :: MPI_ERR_COMM = 5
    integer, parameter :: MPI_ERR_KEYVAL = 48
    integer, parameter :: MPI_ERR_NAME = 33
    integer, parameter :: MPI_ERR_REQUEST = 19
    integer, parameter :: MPI_ERR_GROUP = 8
    integer, parameter :: MPI_ERR_TOPOLOGY = 10
    integer, parameter :: MPI_ERR_TYPE = 3
    integer, parameter :: MPI_ERR_TAG = 4
    integer, parameter :: MPI_ERR_INFO_VALUE = 30
    integer, parameter :: MPI_ERR_NOT_SAME = 35
    integer, parameter :: MPI_ERR_RMA_SYNC = 50
    integer, parameter :: MPI_ERR_INFO = 28
    integer, parameter :: MPI_ERR_NO_MEM = 34
    integer, parameter :: MPI_ERR_BAD_FILE = 22
    integer, parameter :: MPI_ERR_FILE_IN_USE = 26
    integer, parameter :: MPI_ERR_UNKNOWN = 13
    integer, parameter :: MPI_ERR_UNSUPPORTED_OPERATION = 44
    integer, parameter :: MPI_ERR_QUOTA = 39
    integer, parameter :: MPI_ERR_AMODE = 21
    integer, parameter :: MPI_ERR_ROOT = 7
    integer, parameter :: MPI_ERR_RANK = 6
    integer, parameter :: MPI_ERR_DIMS = 11
    integer, parameter :: MPI_ERR_NO_SUCH_FILE = 37
    integer, parameter :: MPI_ERR_SERVICE = 41
    integer, parameter :: MPI_ERR_INTERN = 16
    integer, parameter :: MPI_ERR_IO = 32
    integer, parameter :: MPI_ERR_ACCESS = 20
    integer, parameter :: MPI_ERR_NO_SPACE = 36
    integer, parameter :: MPI_ERR_CONVERSION = 23
    integer, parameter :: MPI_ERRORS_ARE_FATAL = 1409286144
    integer, parameter :: MPI_ERRORS_RETURN = 1409286145
    integer, parameter :: MPI_IDENT = 0
    integer, parameter :: MPI_CONGRUENT = 1
    integer, parameter :: MPI_SIMILAR = 2
    integer, parameter :: MPI_UNEQUAL = 3
    integer, parameter :: MPI_MAX = 1476395009
    integer, parameter :: MPI_MIN = 1476395010
    integer, parameter :: MPI_SUM = 1476395011
    integer, parameter :: MPI_PROD = 1476395012
    integer, parameter :: MPI_LAND = 1476395013
    integer, parameter :: MPI_BAND = 1476395014
    integer, parameter :: MPI_LOR = 1476395015
    integer, parameter :: MPI_BOR = 1476395016
    integer, parameter :: MPI_LXOR = 1476395017
    integer, parameter :: MPI_BXOR = 1476395018
    integer, parameter :: MPI_MINLOC = 1476395019
    integer, parameter :: MPI_MAXLOC = 1476395020
    integer, parameter :: MPI_REPLACE = 1476395021
    integer, parameter :: MPI_COMM_WORLD = 1140850688
    integer, parameter :: MPI_COMM_SELF = 1140850689
    integer, parameter :: MPI_GROUP_EMPTY = 1207959552
    integer, parameter :: MPI_COMM_NULL = 67108864
    integer, parameter :: MPI_WIN_NULL = 536870912
    integer, parameter :: MPI_FILE_NULL = 0
    integer, parameter :: MPI_GROUP_NULL = 134217728
    integer, parameter :: MPI_OP_NULL = 402653184
    integer, parameter :: MPI_DATATYPE_NULL = 201326592
    integer, parameter :: MPI_REQUEST_NULL = 738197504
    integer, parameter :: MPI_ERRHANDLER_NULL = 335544320
    integer, parameter :: MPI_INFO_NULL = 469762048
    integer, parameter :: MPI_TAG_UB = 1681915906
    integer, parameter :: MPI_HOST = 1681915908
    integer, parameter :: MPI_IO = 1681915910
    integer, parameter :: MPI_WTIME_IS_GLOBAL = 1681915912
    integer, parameter :: MPI_UNIVERSE_SIZE = 1681915914
    integer, parameter :: MPI_LASTUSEDCODE = 1681915916
    integer, parameter :: MPI_APPNUM = 1681915918
    integer, parameter :: MPI_WIN_BASE = 1711276034
    integer, parameter :: MPI_WIN_SIZE = 1711276036
    integer, parameter :: MPI_WIN_DISP_UNIT = 1711276038
    integer, parameter :: MPI_MAX_ERROR_STRING = 512-1
    integer, parameter :: MPI_MAX_PORT_NAME = 255
    integer, parameter :: MPI_MAX_OBJECT_NAME = 127
    integer, parameter :: MPI_MAX_INFO_KEY = 254
    integer, parameter :: MPI_MAX_INFO_VAL = 1023
    integer, parameter :: MPI_MAX_PROCESSOR_NAME = 128-1
    integer, parameter :: MPI_MAX_DATAREP_STRING = 127
    integer, parameter :: MPI_UNDEFINED =  ( -32766 )
    integer, parameter :: MPI_UNDEFINED_RANK =  ( -32766 )
    integer, parameter :: MPI_KEYVAL_INVALID = 603979776
    integer, parameter :: MPI_BSEND_OVERHEAD = 95
    integer, parameter :: MPI_PROC_NULL = -1
    integer, parameter :: MPI_ANY_SOURCE = -2
    integer, parameter :: MPI_ANY_TAG = -1
    integer, parameter :: MPI_ROOT = -3
    integer, parameter :: MPI_GRAPH = 1
    integer, parameter :: MPI_CART = 2
    integer, parameter :: MPI_DIST_GRAPH = 3
    integer, parameter :: MPI_VERSION = 2
    integer, parameter :: MPI_SUBVERSION = 2
    integer, parameter :: MPI_LOCK_EXCLUSIVE = 234
    integer, parameter :: MPI_LOCK_SHARED = 235
    integer, parameter :: MPI_COMPLEX = 1275070494
    integer, parameter :: MPI_DOUBLE_COMPLEX = 1275072546
    integer, parameter :: MPI_LOGICAL = 1275069469
    integer, parameter :: MPI_REAL = 1275069468
    integer, parameter :: MPI_DOUBLE_PRECISION = 1275070495
    integer, parameter :: MPI_INTEGER = 1275069467
    integer, parameter :: MPI_2INTEGER = 1275070496
    integer, parameter :: MPI_2COMPLEX = 1275072548
    integer, parameter :: MPI_2DOUBLE_PRECISION = 1275072547
    integer, parameter :: MPI_2REAL = 1275070497
    integer, parameter :: MPI_2DOUBLE_COMPLEX = 1275076645
    integer, parameter :: MPI_CHARACTER = 1275068698
    integer, parameter :: MPI_BYTE = 1275068685
    integer, parameter :: MPI_UB = 1275068433
    integer, parameter :: MPI_LB = 1275068432
    integer, parameter :: MPI_PACKED = 1275068687
    integer, parameter :: MPI_INTEGER1 = 1275068717
    integer, parameter :: MPI_INTEGER2 = 1275068975
    integer, parameter :: MPI_INTEGER4 = 1275069488
    integer, parameter :: MPI_INTEGER8 = 1275070513
    integer, parameter :: MPI_INTEGER16 = MPI_DATATYPE_NULL
    integer, parameter :: MPI_REAL4 = 1275069479
    integer, parameter :: MPI_REAL8 = 1275070505
    integer, parameter :: MPI_REAL16 = 1275072555
    integer, parameter :: MPI_COMPLEX8 = 1275070504
    integer, parameter :: MPI_COMPLEX16 = 1275072554
    integer, parameter :: MPI_COMPLEX32 = 1275076652
    integer, parameter :: MPI_ADDRESS_KIND = 8
    integer, parameter :: MPI_OFFSET_KIND = 8
    integer, parameter :: MPI_CHAR = 1275068673
    integer, parameter :: MPI_SIGNED_CHAR = 1275068696
    integer, parameter :: MPI_UNSIGNED_CHAR = 1275068674
    integer, parameter :: MPI_WCHAR = 1275069454
    integer, parameter :: MPI_SHORT = 1275068931
    integer, parameter :: MPI_UNSIGNED_SHORT = 1275068932
    integer, parameter :: MPI_INT = 1275069445
    integer, parameter :: MPI_UNSIGNED = 1275069446
    integer, parameter :: MPI_LONG = 1275070471
    integer, parameter :: MPI_UNSIGNED_LONG = 1275070472
    integer, parameter :: MPI_FLOAT = 1275069450
    integer, parameter :: MPI_DOUBLE = 1275070475
    integer, parameter :: MPI_LONG_DOUBLE = 1275072524
    integer, parameter :: MPI_LONG_LONG_INT = 1275070473
    integer, parameter :: MPI_UNSIGNED_LONG_LONG = 1275070489
    integer, parameter :: MPI_LONG_LONG = 1275070473
    integer, parameter :: MPI_FLOAT_INT = -1946157056
    integer, parameter :: MPI_DOUBLE_INT = -1946157055
    integer, parameter :: MPI_LONG_INT = -1946157054
    integer, parameter :: MPI_SHORT_INT = -1946157053
    integer, parameter :: MPI_2INT = 1275070486
    integer, parameter :: MPI_LONG_DOUBLE_INT = -1946157052
    integer, parameter :: MPI_INT8_T = 1275068727
    integer, parameter :: MPI_INT16_T = 1275068984
    integer, parameter :: MPI_INT32_T = 1275069497
    integer, parameter :: MPI_INT64_T = 1275070522
    integer, parameter :: MPI_UINT8_T = 1275068731
    integer, parameter :: MPI_UINT16_T = 1275068988
    integer, parameter :: MPI_UINT32_T = 1275069501
    integer, parameter :: MPI_UINT64_T = 1275070526
    integer, parameter :: MPI_C_BOOL = 1275068735
    integer, parameter :: MPI_C_FLOAT_COMPLEX = 1275070528
    integer, parameter :: MPI_C_COMPLEX = 1275070528
    integer, parameter :: MPI_C_DOUBLE_COMPLEX = 1275072577
    integer, parameter :: MPI_C_LONG_DOUBLE_COMPLEX = 1275076674
    integer, parameter :: MPI_AINT = 1275070531
    integer, parameter :: MPI_OFFSET = 1275070532
    integer, parameter :: MPI_COMBINER_NAMED = 1
    integer, parameter :: MPI_COMBINER_DUP = 2
    integer, parameter :: MPI_COMBINER_CONTIGUOUS = 3
    integer, parameter :: MPI_COMBINER_VECTOR = 4
    integer, parameter :: MPI_COMBINER_HVECTOR_INTEGER = 5
    integer, parameter :: MPI_COMBINER_HVECTOR = 6
    integer, parameter :: MPI_COMBINER_INDEXED = 7
    integer, parameter :: MPI_COMBINER_HINDEXED_INTEGER = 8
    integer, parameter :: MPI_COMBINER_HINDEXED = 9
    integer, parameter :: MPI_COMBINER_INDEXED_BLOCK = 10
    integer, parameter :: MPI_COMBINER_STRUCT_INTEGER = 11
    integer, parameter :: MPI_COMBINER_STRUCT = 12
    integer, parameter :: MPI_COMBINER_SUBARRAY = 13
    integer, parameter :: MPI_COMBINER_DARRAY = 14
    integer, parameter :: MPI_COMBINER_F90_REAL = 15
    integer, parameter :: MPI_COMBINER_F90_COMPLEX = 16
    integer, parameter :: MPI_COMBINER_F90_INTEGER = 17
    integer, parameter :: MPI_COMBINER_RESIZED = 18
    integer, parameter :: MPI_TYPECLASS_REAL = 1
    integer, parameter :: MPI_TYPECLASS_INTEGER = 2
    integer, parameter :: MPI_TYPECLASS_COMPLEX = 3
    integer, parameter :: MPI_MODE_NOCHECK = 1024
    integer, parameter :: MPI_MODE_NOSTORE = 2048
    integer, parameter :: MPI_MODE_NOPUT = 4096
    integer, parameter :: MPI_MODE_NOPRECEDE = 8192
    integer, parameter :: MPI_MODE_NOSUCCEED = 16384
    integer, parameter :: MPI_THREAD_SINGLE = 0
    integer, parameter :: MPI_THREAD_FUNNELED = 1
    integer, parameter :: MPI_THREAD_SERIALIZED = 2
    integer, parameter :: MPI_THREAD_MULTIPLE = 3
    integer, parameter :: MPI_MODE_RDONLY = 2
    integer, parameter :: MPI_MODE_RDWR = 8
    integer, parameter :: MPI_MODE_WRONLY = 4
    integer, parameter :: MPI_MODE_DELETE_ON_CLOSE = 16
    integer, parameter :: MPI_MODE_UNIQUE_OPEN = 32
    integer, parameter :: MPI_MODE_CREATE = 1
    integer, parameter :: MPI_MODE_EXCL = 64
    integer, parameter :: MPI_MODE_APPEND = 128
    integer, parameter :: MPI_MODE_SEQUENTIAL = 256
    integer, parameter :: MPI_SEEK_SET = 600
    integer, parameter :: MPI_SEEK_CUR = 602
    integer, parameter :: MPI_SEEK_END = 604
    integer, parameter :: MPI_ORDER_C = 56
    integer, parameter :: MPI_ORDER_FORTRAN = 57
    integer, parameter :: MPI_DISTRIBUTE_BLOCK = 121
    integer, parameter :: MPI_DISTRIBUTE_CYCLIC = 122
    integer, parameter :: MPI_DISTRIBUTE_NONE = 123
    integer, parameter :: MPI_DISTRIBUTE_DFLT_DARG = -49767

    integer ( ip ), parameter :: MPI_DISPLACEMENT_CURRENT = -54278278
    integer :: MPI_BOTTOM, MPI_IN_PLACE, MPI_UNWEIGHTED

    external MPI_DUP_FN, MPI_NULL_DELETE_FN, MPI_NULL_COPY_FN
    external MPI_WTIME, MPI_WTICK
    external PMPI_WTIME, PMPI_WTICK
    external MPI_COMM_DUP_FN, MPI_COMM_NULL_DELETE_FN
    external MPI_COMM_NULL_COPY_FN
    external MPI_WIN_DUP_FN, MPI_WIN_NULL_DELETE_FN
    external MPI_WIN_NULL_COPY_FN
    external MPI_TYPE_DUP_FN, MPI_TYPE_NULL_DELETE_FN
    external MPI_TYPE_NULL_COPY_FN
    external MPI_CONVERSION_FN_NULL

    real ( dp ) :: MPI_WTIME, MPI_WTICK
    real ( dp ) :: PMPI_WTIME, PMPI_WTICK

    common /MPIFCMB5/ MPI_UNWEIGHTED

    save   /MPIFCMB5/

    common /MPIPRIV1/ MPI_BOTTOM, MPI_IN_PLACE, MPI_STATUS_IGNORE

    common /MPIPRIV2/ MPI_STATUSES_IGNORE, MPI_ERRCODES_IGNORE
    save   /MPIPRIV1/, /MPIPRIV2/

    common /MPIPRIVC/ MPI_ARGVS_NULL, MPI_ARGV_NULL
    save   /MPIPRIVC/

end module mpif_riptide
