MODULE GLOBAL
       USE PARAM
       IMPLICIT NONE

       SAVE


! MPI variables
       INTEGER :: myid,ier
       INTEGER :: comm2d
       INTEGER :: n_west, n_east, n_suth, n_nrth
       INTEGER :: npx,npy
       INTEGER :: ndims=2
       INTEGER :: NumberProcessor
       INTEGER, DIMENSION(2) :: dims, coords
       LOGICAL, DIMENSION(2) :: periods
       LOGICAL :: reorder = .true.

       INTEGER :: px,py

! station data
       INTEGER :: NumberStations
       INTEGER,DIMENSION(:),ALLOCATABLE :: ista,jsta,nsta
       REAL(SP):: PLOT_INTV_STATION,PLOT_COUNT_STATION


! define parameters
       CHARACTER(LEN=80) TITLE
       CHARACTER(LEN=80) ReadType
       CHARACTER(LEN=80) DEPTH_TYPE
       CHARACTER(LEN=80) DEPTH_FILE
       CHARACTER(LEN=80) ETA_FILE
       CHARACTER(LEN=80) U_FILE
       CHARACTER(LEN=80) V_FILE
       CHARACTER(LEN=80) DX_FILE
       CHARACTER(LEN=80) DY_FILE
       CHARACTER(LEN=80) Coriolis_FILE
       CHARACTER(LEN=80) OBSTACLE_FILE
       CHARACTER(LEN=80) RESULT_FOLDER
       CHARACTER(LEN=80) STATIONS_FILE
       CHARACTER(LEN=80) WaveMaker
       CHARACTER(LEN=80) DepthFormat
       CHARACTER(LEN=80) Time_Scheme
       CHARACTER(LEN=80) CONSTR
       CHARACTER(LEN=80) HIGH_ORDER

        CHARACTER(LEN=14)::FORMAT_LEN=''


       REAL(SP),PARAMETER,DIMENSION(3)::alpha=(/0.0_SP,3.0_SP/4.0_SP,1.0_SP/3.0_SP/)
        REAL(SP),PARAMETER,DIMENSION(3)::beta=(/1.0_SP,1.0_SP/4.0_SP,2.0_SP/3.0_SP/)
       REAL(SP)::Kappa   ! parameter for order of muscl

! some global variables
! Mloc1=Mloc+1, Nloc1=Nloc+1
       INTEGER :: Mglob,Nglob,Mloc,Nloc,Mloc1,Nloc1
       INTEGER, PARAMETER :: Nghost = 3
       INTEGER :: Ibeg,Iend,Jbeg,Jend,Iend1,Jend1

       REAL(SP):: DX,DY



       REAL(SP)::  DT,TIME,TOTAL_TIME,PLOT_INTV,PLOT_COUNT,&
                  SCREEN_INTV,SCREEN_COUNT
       REAL(SP) :: HOTSTART_INTV,HOTSTART_COUNT
       INTEGER :: icount=0  ! for output file number
       INTEGER :: icount_hotstart=0
       INTEGER :: FileNumber_HOTSTART

       REAL(SP) :: MinDepth=0.001
       REAL(SP) :: MinDepthFrc=0.5
       REAL(SP) :: CFL=0.15_SP
       REAL(SP) :: FroudeCap=10.0_SP
       REAL(SP) :: SWE_ETA_DEP=0.7_SP

! switch for dispersion
!   gamma1 is for extra M term
       REAL(SP) :: gamma1

       REAL(SP) :: gamma2

! gamma3 is for linear shallow water equation
       REAL(SP) :: gamma3
       REAL(SP) :: Beta_ref=-0.531_SP
! kennedy's equations
       REAL(SP) :: Beta_1,Beta_2
! a1=beta_ref^2/2-1/6, a2=beta_ref+1/2, b1=beta_ref^2, b2=beta_ref
       REAL(SP) :: a1,a2,b1,b2
       LOGICAL :: DISPERSION=.FALSE.
       LOGICAL :: DISP_TIME_LEFT=.FALSE.
       LOGICAL :: StretchGrid = .FALSE.
                 ! put time derivative dispersion term on left

! some local variables
       REAL(SP),DIMENSION(:,:),ALLOCATABLE :: &

          U4xL,U4xR,V4yL,V4yR, &
          U4,V4,U1p,V1p, &
          U1pp,V1pp, &
          U2,V2,U3,V3, &








          DelxU,DelxHU,DelxV,DelxEtar,&
          DelxHV, DelyHU, &
          DelyU,DelyHV,DelyV,DelyEtar,&
          UxL,UxR,VxL,VxR,&
          HUxL,HUxR,HUyL,HUyR,HxL,HxR, &
          EtaRxL,EtaRxR,&
          UyL,UyR,VyL,VyR,&
          HVxL,HVxR,HVyL,HVyR,HyL,HyR, &
          EtaRyL,EtaRyR, &
          PL,PR,QL,QR, &
          FxL,FxR,FyL,FyR, &
          GxL,GxR,GyL,GyR, &
          SxL,SxR,SyL,SyR, &
! cross-derivatives
          Vxy,DVxy,Uxy,DUxy, &
! second-derivatives
          Uxx,DUxx,Vyy,DVyy, &
! first-derivatives
          Ux,Vx,Uy,Vy,DUx,DUy,DVx,DVy, &
          ETAx,ETAy, ETAT, ETATx,ETATy, &
! time-derivative
          U0,V0,Ut,Vt,Utx,Vty,Utxx,Utxy,Vtxy,Vtyy,&
          DUtxx,DUtxy,DVtxy,DVtyy,DUtx,DVty,&
! original variables
          Fx,Fy,U,V,HU,HV,&
          Gx,Gy,P,Q,SourceX,SourceY,Int2Flo, &
          tmp4preview,HeightMax,HeightMin,VelocityMax,&
          MomentumFluxMax,VorticityMax






!
! wetting and drying
        INTEGER,DIMENSION(:,:),ALLOCATABLE :: MASK,MASK_STRUC,MASK9
! wave maker
        REAL(SP)::AMP_SOLI,DEP_SOLI,LAG_SOLI, CPH_SOLI,XWAVEMAKER, &
                  Xc,Yc, WID,Xc_WK,Tperiod,AMP_WK,DEP_WK,Theta_WK, &
                  rlamda,Time_ramp,D_gen,Beta_gen,Width_WK,Delta_WK,&
                  Ywidth_WK,Yc_WK
        REAL(SP),DIMENSION(:,:),ALLOCATABLE :: D_gen_ir,rlamda_ir,phase_ir
        REAL(SP),DIMENSION(:),ALLOCATABLE :: Beta_gen_ir,omgn_ir
        REAL(SP) :: FreqMin,FreqMax,FreqPeak,GammaTMA,Hmo,ThetaPeak,&
                    Sigma_Theta
        REAL(SP),DIMENSION(:,:,:),ALLOCATABLE ::Cm,Sm
        INTEGER :: Nfreq=100,Ntheta=21
        REAL(SP)::x1_Nwave = 5.0, &
                  x2_Nwave = 5.0, &
                  a0_Nwave = 1.0, &
                  gamma_Nwave = -3.0, &
                  dep_Nwave = 1.0
!      for measure time series
       REAL(SP),DIMENSION(:,:),ALLOCATABLE :: WAVE_COMP
       REAL(SP),DIMENSION(:),ALLOCATABLE :: Beta_genS,D_genS
       REAL(SP),DIMENSION(:,:),ALLOCATABLE :: Beta_gen2D,D_gen2D,rlamda2D, &
                                              Phase2D
       REAL(SP),DIMENSION(:),ALLOCATABLE :: Freq,Dire
       REAL(SP) :: PeakPeriod
       INTEGER :: NumWaveComp,NumFreq,NumDir
       CHARACTER(LEN=80) WaveCompFile
! friction
        LOGICAL :: IN_Cd=.FALSE.
        REAL(SP):: Cd_fixed
        CHARACTER(LEN=80) CD_FILE
        REAL(SP),DIMENSION(:,:),ALLOCATABLE :: Cd



! sponge
        REAL(SP),DIMENSION(:,:),ALLOCATABLE :: SPONGE
        REAL(SP)::Sponge_west_width,Sponge_east_width, &
                  Sponge_south_width,Sponge_north_width, &
                  R_sponge,A_sponge

! breaking, this criteria only for bubble generation
        REAL(SP),DIMENSION(:,:),ALLOCATABLE :: AGE_BREAKING
        REAL(SP) :: Cbrk1=0.65,Cbrk2=0.35,T_brk
        ! use T_brk to judge breakers
        LOGICAL :: INI_UVZ=.FALSE.

! smagorinsky and wave height

      REAL(SP),DIMENSION(:,:),ALLOCATABLE :: Umean,Vmean,&
                  ETAmean,Usum,Vsum,ETAsum, nu_smg
      REAL(SP)::T_INTV_mean = 20.0,T_sum=0.0,C_smg=0.25
      REAL(SP),DIMENSION(:,:),ALLOCATABLE :: &
                WaveHeightRMS,WaveHeightAve,Emax,Emin,&
                HrmsSum,HavgSum
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: Num_Zero_Up


! depth H=Eta+Depth,
       REAL(SP),DIMENSION(:,:),ALLOCATABLE :: Depth,H,&
            DepthNode,Depthx,Depthy
       REAL(SP)::Depth_Flat, SLP,Xslp

! updating variables
       REAL(SP),DIMENSION(:,:),ALLOCATABLE::Ubar0,Vbar0,Eta0,&
                               Ubar,Vbar,Eta



! output logical parameters
       INTEGER :: OUTPUT_RES = 1
       LOGICAL :: OUT_U=.FALSE., OUT_V=.FALSE.,OUT_ETA=.FALSE., &
                  OUT_MASK=.FALSE.,OUT_SXL=.FALSE.,OUT_SXR=.FALSE.,&
                  OUT_SYL=.FALSE., OUT_SYR=.FALSE.,&
                  OUT_SourceX=.FALSE., OUT_SourceY=.FALSE., &
                  OUT_P=.FALSE., OUT_Q=.FALSE., &
                  OUT_Fx=.FALSE., OUT_Fy=.FALSE.,&
                  OUT_Gx=.FALSE., OUT_Gy=.FALSE.,&
                  OUT_MASK9=.FALSE., OUT_DEPTH=.FALSE., &
                  OUT_TMP=.FALSE., OUT_AGE=.FALSE., &
                  OUT_Hmax=.FALSE., &
                  OUT_Hmin=.FALSE., &
                  OUT_Umax=.FALSE.
       LOGICAL :: OUT_MFmax=.FALSE., &
                  OUT_VORmax=.FALSE.

       LOGICAL :: OUT_Umean=.FALSE.,OUT_Vmean=.FALSE.,&
                  OUT_ETAmean=.FALSE.,&
                  OUT_WaveHeight = .FALSE.

!# if defined (1)
! periodic boundary conditions
       LOGICAL :: PERIODIC=.FALSE.
!# endif
! dispersion control
       LOGICAL :: OBSTACLE=.FALSE., HOT_START=.FALSE.
! sponge
       LOGICAL :: SPONGE_ON=.FALSE.
! breaking
       LOGICAL :: SHOW_BREAKING=.TRUE.
! slope control, use it in caution, should set false unless for a spherical
! ocean basin domain and slope > 1:5
       LOGICAL :: SLOPE_CTR = .FALSE.
       REAL(SP) :: MAX_SLOPE


END MODULE GLOBAL
