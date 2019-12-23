module mGlobal

    use mParameters

    implicit none

# if defined (PARALLEL)
! MPI variables
    integer ( ip )                  :: myid = 0, ier = 0
    integer ( ip )                  :: comm2d = 0
    integer ( ip )                  :: n_west = 0, n_east = 0, n_suth = 0, n_nrth = 0
    integer ( ip )                  :: npx = 0, npy = 0
    integer ( ip )                  :: ndims = 2
    integer ( ip )                  :: NumberProcessor = 0
    logical                         :: reorder = .true.
    logical, dimension ( 2 )        :: periods = [ .false., .false. ]
    integer ( ip ), dimension ( 2 ) :: dims = 0, coords = 0
# endif
    integer ( ip )  :: px = 0, py = 0

    ! dummy variables, global
    integer ( ip )  :: I = 0, J = 0, K = 0
    integer ( ip )  :: itmp1 = 0, itmp2 = 0, itmp3 = 0, itmp4 = 0, itmp5 = 0
    real ( rp )     :: tmp1 = zero, tmp2 = zero, tmp3 = zero, tmp4 = zero, tmp5 = zero

    ! station data
    integer ( ip ), dimension ( 2 ), allocatable :: ista, jsta, nsta
    integer ( ip )  :: NumberStations = 0
    real ( rp )     :: PLOT_INTV_STATION = 0, PLOT_COUNT_STATION = 0

    ! define parameters
    character ( ascii, len = 80 ) :: TITLE = '', DEPTH_TYPE = '', DEPTH_FILE = '', ETA_FILE = '', U_FILE = '', V_FILE = '',  &
                                     DX_FILE = '', DY_FILE = '', Coriolis_FILE = '', OBSTACLE_FILE = '', RESULT_FOLDER = '', &
                                     STATIONS_FILE = '', WaveMaker = '', DepthFormat = '', Time_Scheme = '', ReadType = '',  &
                                     CONSTR = '', HIGH_ORDER = ''
    character ( ascii, len = 14 ) :: FORMAT_LEN = ''


    real ( rp ) :: Kappa   ! parameter for order of muscl

    ! some global variables
    ! Mloc1 = Mloc + 1, Nloc1 = Nloc + 1
    integer ( ip )  :: Mglob = 0, Nglob = 0, Mloc = 0, Nloc = 0, Mloc1 = 0, Nloc1 = 0
    integer ( ip )  :: Ibeg = 0, Iend = 0, Jbeg = 0, Jend = 0, Iend1 = 0, Jend1 = 0
# if defined (CARTESIAN)
    real ( rp ) :: DX = zero, DY = zero
# else
    real ( rp ) :: Lon_West,Lat_South,Dphi,Dtheta
# endif
    real ( rp ) :: DT = zero, TIME = zero, TOTAL_TIME = zero, PLOT_INTV = zero, PLOT_COUNT = zero, &
                   SCREEN_INTV = zero, SCREEN_COUNT = zero
    real ( rp ) :: HOTSTART_INTV = zero, HOTSTART_COUNT = zero

    integer ( ip ) :: icount = 0  ! for output file number
    integer ( ip ) :: icount_hotstart = 0
    integer ( ip ) :: FileNumber_HOTSTART

    real ( rp ) :: CFL         = 0.15_rp
    real ( rp ) :: MinDepth    = 0.001
    real ( rp ) :: FroudeCap   = 10.0_rp
    real ( rp ) :: SWE_ETA_DEP = 0.7_rp
    real ( rp ) :: MinDepthFrc = 0.5

! switch for dispersion
!   gamma1 is for extra M term
    real ( rp ) :: gamma1 = zero
# if defined (CARTESIAN)
    real ( rp ) :: gamma2 = zero
# endif
! gamma3 is for linear shallow water equation
    real ( rp ) :: gamma3   = zero
    real ( rp ) :: Beta_ref = -0.531_rp
! kennedy's equations
    real ( rp ) :: Beta_1 = zero, Beta_2 = zero
! a1=beta_ref^2/2-1/6, a2=beta_ref+1/2, b1=beta_ref^2, b2=beta_ref
    real ( rp ) :: a1,a2,b1,b2

    logical     :: DISPERSION = .FALSE., DISP_TIME_LEFT = .FALSE., StretchGrid = .FALSE.

! some local variables
    real ( rp ), dimension ( : , : ), allocatable :: &
# if defined (CARTESIAN)
        U4xL, U4xR, V4yL, V4yR, &
        U4, V4, U1p, V1p,       &
        U1pp, V1pp,             &
        U2, V2, U3, V3,         &
# else
# if defined(ZALPHA)
        U4xL, U4xR, V4yL, V4yR, &
        U4, V4,                 &
# endif
        Dx, Dy, Coriolis, Lat_theta, &
        U1p, V1p, SlopeX, SlopeY,    &
# endif
        DelxU, DelxHU, DelxV, DelxEtar,     &
        DelxHV, DelyHU,                     &
        DelyU, DelyHV, DelyV, DelyEtar,     &
        UxL, UxR, VxL, VxR,                 &
        HUxL, HUxR, HUyL, HUyR, HxL, HxR,   &
        EtaRxL, EtaRxR,                     &
        UyL, UyR, VyL, VyR,                 &
        HVxL, HVxR, HVyL, HVyR, HyL, HyR,   &
        EtaRyL, EtaRyR,                     &
        PL, PR, QL, QR,                     &
        FxL, FxR, FyL, FyR,                 &
        GxL, GxR, GyL, GyR,                 &
        SxL, SxR, SyL, SyR,                 &
! cross-derivatives
        Vxy, DVxy, Uxy, DUxy,               &
! second-derivatives
        Uxx, DUxx, Vyy, DVyy,               &
! first-derivatives
        Ux, Vx, Uy, Vy, DUx, DUy, DVx, DVy, &
        ETAx, ETAy, ETAT, ETATx, ETATy,     &
! time-derivative
        U0, V0, Ut, Vt, Utx, Vty, Utxx, Utxy, Vtxy, Vtyy,   &
        DUtxx, DUtxy, DVtxy, DVtyy, DUtx, DVty,             &
! original variables
          Fx, Fy, U, V, HU, HV,                             &
          Gx, Gy, P, Q, SourceX, SourceY, Int2Flo,          &
          tmp4preview, HeightMax, HeightMin, VelocityMax,   &
          MomentumFluxMax, VorticityMax
# if defined (ITERATION)
    real ( rp ), dimension ( : , : ), allocatable :: Ui, Vi, ETAi, UbarOld, VbarOld, EtaOld
    integer ( ip )  :: Ki = 0
    logical         :: ACCURATE = .FALSE.
# endif
!
! wetting and drying
    !integer ( ip ), dimension ( 2 ), allocatable  :: MASK, MASK_STRUC, MASK9
    integer ( ip ), dimension ( : ), allocatable  :: MASK, MASK_STRUC, MASK9
! wave maker
    ! rank 2
    real ( rp ), dimension ( : , : ), allocatable :: D_gen_ir, rlamda_ir, phase_ir
    real ( rp ), dimension ( : , : ), allocatable :: Beta_gen_ir, omgn_ir
    real ( rp ), dimension ( : , : ), allocatable :: Cm, Sm
    ! rank 0
    real ( rp )     :: AMP_SOLI = zero, DEP_SOLI = zero, LAG_SOLI = zero, CPH_SOLI = zero, XWAVEMAKER = zero,               &
                       Xc = zero, Yc = zero, WID = zero, Xc_WK = zero, Tperiod = zero, AMP_WK = zero, DEP_WK = zero,        &
                       Theta_WK = zero, rlamda = zero, Time_ramp = zero, D_gen = zero, Beta_gen = zero, Width_WK = zero,    &
                       Delta_WK = zero, Ywidth_WK = zero, Yc_WK = zero
    real ( rp )     :: FreqMin = zero, FreqMax = zero, FreqPeak = zero, GammaTMA = zero, Hmo = zero, ThetaPeak = zero, &
                       Sigma_Theta = zero
    integer ( ip )  :: Nfreq = 100, Ntheta = 21
    real ( rp )     :: x1_Nwave = 5.0_rp,       &
                       x2_Nwave = 5.0_rp,       &
                       a0_Nwave = one,          &
                       gamma_Nwave = -2.0_rp,   &
                       dep_Nwave = one
! for measure time series
    ! rank 2
    real ( rp ), dimension ( : , : ), allocatable :: WAVE_COMP,                     &
                                                     Beta_genS, D_genS,             &
                                                     Beta_gen2D, D_gen2D, rlamda2D, &
                                                     Phase2D,                       &
                                                     Freq, Dire                     &
    ! rank 0
    real ( rp )                   :: PeakPeriod = zero
    integer ( ip )                :: NumWaveComp = 0, NumFreq = 0, NumDir = 0
    character ( ascii, len = 80 ) :: WaveCompFile = ''
! friction
    real ( rp ), dimension ( : , : ), allocatable :: Cd
    real ( rp ) :: Cd_fixed
    logical     :: IN_Cd = .FALSE.
    character ( ascii, len = 80 ) :: CD_FILE

! sponge
    real ( rp ), dimension ( : , : ), allocatable :: SPONGE
    real ( rp ) :: Sponge_west_width  = zero, Sponge_east_width  = zero, &
                   Sponge_south_width = zero, Sponge_north_width = zero, &
                   R_rponge = zero, A_rponge = zero

! breaking, this criteria only for bubble generation
    real ( rp ), dimension ( : , : ), allocatable :: AGE_BREAKING
    real ( rp ) :: Cbrk1 = 0.65, Cbrk2 = 0.35, T_brk = zero
! use T_brk to judge breakers
    logical :: INI_UVZ = .FALSE.

! smagorinsky and wave height
# if defined (MIXING)
    real ( rp ), dimension ( : , : ), allocatable    :: Umean, Vmean, ETAmean, Usum, Vsum, ETAsum, nu_smg, &
                                                        WaveHeightRMS, WaveHeightAve, Emax, Emin, HrmsSum, HavgSum
    integer ( ip ), dimension ( : , : ), allocatable :: Num_Zero_Up
    real ( rp ) :: T_INTV_mean = 20.0_rp, T_sum = zero, C_smg = 0.25_rp
# endif

! depth H=Eta+Depth,
    real ( rp ), dimension ( : , : ), allocatable :: Depth, H, DepthNode, Depthx, Depthy
    real ( rp ) :: Depth_Flat = zero, SLP = zero, Xslp = zero

! updating variables
    real ( rp ), dimension ( : , : ), allocatable :: Ubar0 = zero, Vbar0 = zero, Eta0 = zero, &
                                                     Ubar = zero, Vbar = zero, Eta = zero

# if defined(COUPLING)
! coupling
    real ( rp ), dimension ( : , : ), allocatable :: U_COUPLING_EAST  = zero, V_COUPLING_EAST  = zero, Z_COUPLING_EAST  = zero, &
                                                     U_COUPLING_WEST  = zero, V_COUPLING_WEST  = zero, Z_COUPLING_WEST  = zero, &
                                                     U_COUPLING_SOUTH = zero, V_COUPLING_SOUTH = zero, Z_COUPLING_SOUTH = zero, &
                                                     U_COUPLING_NORTH = zero, V_COUPLING_NORTH = zero, Z_COUPLING_NORTH = zero
    real ( rp ) :: TIME_COUPLING_1 = zero, TIME_COUPLING_2 = zero
    integer ( ip )  :: icount_coupling = 1
    integer ( ip )  :: N_COUPLING_EAST = 0, N_COUPLING_WEST = 0, N_COUPLING_SOUTH = 0, N_COUPLING_NORTH = 0, N_COUPLING_DATA = 0, &
                       J_START_EAST = 0, J_START_WEST = 0, I_START_SOUTH = 0, I_START_NORTH = 0
    integer ( ip )  :: Kstart_EAST = 0,  Kend_EAST  = 0, Kshift_EAST  = 0, Kstart_WEST  = 0, Kend_WEST  = 0, Kshift_WEST  = 0, &
                       Kstart_SOUTH = 0, Kend_SOUTH = 0, Kshift_SOUTH = 0, Kstart_NORTH = 0, Kend_NORTH = 0, Kshift_NORTH = 0
    character ( ascii, len = 80 ) :: COUPLING_FILE = ''
    logical :: IN_DOMAIN_EAST = .false., IN_DOMAIN_WEST = .false., IN_DOMAIN_SOUTH = .false., IN_DOMAIN_NORTH = .false.
# endif

! output logical parameters
    integer ( ip )  :: OUTPUT_RES = 1
    logical :: OUT_U = .FALSE., OUT_V = .FALSE.,OUT_ETA = .FALSE., OUT_MASK = .FALSE.,OUT_SXL = .FALSE.,OUT_SXR = .FALSE.,  &
               OUT_SYL = .FALSE., OUT_SYR = .FALSE., OUT_SourceX = .FALSE., OUT_SourceY = .FALSE.,                          &
               OUT_P = .FALSE., OUT_Q = .FALSE., OUT_Fx = .FALSE., OUT_Fy = .FALSE., OUT_Gx = .FALSE., OUT_Gy = .FALSE.,    &
               OUT_MASK9 = .FALSE., OUT_DEPTH = .FALSE., OUT_TMP = .FALSE., OUT_AGE = .FALSE.,                              &
               OUT_Hmax = .FALSE., OUT_Hmin = .FALSE., OUT_Umax = .FALSE., OUT_MFmax = .FALSE., OUT_VORmax = .FALSE.
# if defined (MIXING)
    logical :: OUT_Umean = .FALSE., OUT_Vmean = .FALSE., OUT_ETAmean = .FALSE., OUT_WaveHeight = .FALSE.
# endif
!# if defined (CARTESIAN)
! periodic boundary conditions
    logical :: PERIODIC = .FALSE.
!# endif
! dispersion control
    logical :: OBSTACLE = .FALSE., HOT_START = .FALSE.
! sponge
    logical :: SPONGE_ON = .FALSE.
! breaking
    logical :: SHOW_BREAKING = .TRUE.
! slope control, use it in caution, should set false unless for a spherical
! ocean basin domain and slope > 1:5
    real ( rp ) :: MAX_SLOPE = zero
    logical :: SLOPE_CTR = .FALSE.
# if defined ( WIND )
! wind
    real ( rp ), dimension ( : , : ), allocatable :: WindU2D,WindV2D
    real ( rp ), dimension ( : ),     allocatable :: TimeWind
    real ( rp ), dimension ( : ),     allocatable :: WU,WV
    real ( rp ) :: Cdw = zero, WindCrestPercent = zero
    integer ( ip ), dimension ( : , : ), allocatable :: MASK_WIND
    integer ( ip )  :: NumTimeWindData = 0
    integer ( ip )  :: icount_winddata = 1
    logical :: WindForce = .TRUE.
    character ( ascii, len = 80 ) :: WIND_FILE = ''
# endif

end module mGlobal
