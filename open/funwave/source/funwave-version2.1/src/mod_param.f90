

MODULE PARAM

       use mpi

       IMPLICIT NONE







       INTEGER, PARAMETER::SP=SELECTED_REAL_KIND(6,30)

       INTEGER, PARAMETER::MPI_rp=MPI_REAL



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







END MODULE PARAM
