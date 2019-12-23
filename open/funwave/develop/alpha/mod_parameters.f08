module mParameters

    use mPrecisionDefinitions
    implicit none

# if defined PARALLEL
       use mpi
# endif

# if defined DOUBLE_PRECISION
    integer, parameter :: rp = dp
# if defined PARALLEL
    integer, parameter :: MPI_SP = MPI_DOUBLE_PRECISION
# endif
# else
    integer, parameter :: rp = sp
# if defined PARALLEL
    integer, parameter :: MPI_SP = MPI_REAL
# endif
# endif

    ! real constants
    real ( rp ), parameter  :: zero = 0.0_rp
    real ( rp ), parameter  :: one  = 1.0_rp

    real ( rp ), parameter  :: pi      = 3.1415926535897932384626433832795028841971693993751_rp
    real ( rp ), parameter  :: R_earth = 6371000.0_rp  ! 63674447 m
    real ( rp ), parameter  :: SMALL   = 0.000001_rp
    real ( rp ), parameter  :: LARGE   = 100000.0_rp
    real ( rp ), parameter  :: grav    = 9.81_rp
    real ( rp ), parameter  :: RHO_AW  = 0.0012041_rp
    real ( rp ), parameter  :: DEG2RAD = 0.0175_rp

    real ( rp ), parameter, dimension ( 3 ) :: alpha = [ zero, one * 3 / 4, one / 3 ]
    real ( rp ), parameter, dimension ( 3 ) :: beta  = [ one, one / 4, one * 2 / 3 ]

    integer ( ip ), parameter   :: Nghost = 3

# if defined ( ITERATION )
! give a maximum iteration number of 2 like in Nwogu
    integer ( ip )  :: ITER_NUM = 2
    real ( reshape(source, shape=(//), pad=(//), order=(/2,1/))p )     :: ERRORi   = 0.0001_rp
# endif

end module mParameters
