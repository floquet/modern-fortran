MODULE PARAM
# if defined PARALLEL
       use mpi
# endif
       IMPLICIT NONE

# if defined DOUBLE_PRECISION
    integer ( ip ), parameter :: rp = sp
# if defined PARALLEL
       INTEGER, PARAMETER::MPI_rp=MPI_DOUBLE_PRECISION
# endif
# else
       INTEGER, PARAMETER::SP=SELECTED_REAL_KIND(6,30)
# if defined PARALLEL
       INTEGER, PARAMETER::MPI_rp=MPI_REAL
# endif
# endif

    ! real constants
    real ( rp ), parameter  :: zero = 0.0_rp
    real ( rp ), parameter  :: half = 0.5_rp
    real ( rp ), parameter  :: one  = 1.0_rp
    real ( rp ), parameter  :: two  = 2.0_rp

    real ( dp ), parameter  :: pi      = 3.1415926535897932384626433832795028841971693993751_rp
    real ( dp ), parameter  :: R_earth = 6371000.0_rp
    real ( dp ), parameter  :: SMALL   = 0.000001_rp
    real ( dp ), parameter  :: LARGE   = 100000.0_rp
    real ( dp ), parameter  :: grav    = 9.81_rp
    real ( dp ), parameter  :: RHO_AW  = 0.0012041_rp
    real ( dp ), parameter  :: DEG2RAD = 0.0175_rp

    ! some global variables
    integer ( ip )  :: I, J, K
    integer ( ip )  :: itmp1, itmp2, itmp3, itmp4, itmp5
       REAL(SP):: tmp1,tmp2,tmp3,tmp4,tmp5

# if defined (ITERATION)
! give a maximum iteration number of 2 like in Nwogu
       INTEGER :: ITER_NUM = 2
       REAL(SP) :: ERRORi = 0.0001_rp
# endif

END MODULE PARAM
