! ---------------------------------------------------
!    This is subroutine to allocate variables
!  called by 
!        MAIN
!    Last Update: 09/26/2013 Babak Tehranirad, University of Delaware
! --------------------------------------------------

SUBROUTINE ALLOCATE_VARIABLES
     USE GLOBAL

! allocate variables
     ALLOCATE(DelxU(Mloc,Nloc),DelxHU(Mloc,Nloc),DelxV(Mloc,Nloc),DelxEtar(Mloc,Nloc),&
              DelyU(Mloc,Nloc),DelyHV(Mloc,Nloc),DelyV(Mloc,Nloc),DelyEtar(Mloc,Nloc),&
              DelxHV(Mloc,Nloc),DelyHU(Mloc,Nloc), &
! U V HU H in x-direction
              UxL(Mloc1,Nloc),UxR(Mloc1,Nloc),VxL(Mloc1,Nloc),VxR(Mloc1,Nloc),&
              HUxL(Mloc1,Nloc),HUxR(Mloc1,Nloc),HVxL(Mloc1,Nloc),HVxR(Mloc1,Nloc), &
              HxL(Mloc1,Nloc),HxR(Mloc1,Nloc), &
! U V HV H in y-direction
              UyL(Mloc,Nloc1),UyR(Mloc,Nloc1),VyL(Mloc,Nloc1),VyR(Mloc,Nloc1),&
              HVyL(Mloc,Nloc1),HVyR(Mloc,Nloc1),HUyL(Mloc,Nloc1),HUyR(Mloc,Nloc1), &
              HyL(Mloc,Nloc1),HyR(Mloc,Nloc1), &
! cross-derivatives
              Uxy(Mloc,Nloc),Vxy(Mloc,Nloc),DUxy(Mloc,Nloc),DVxy(Mloc,Nloc), &
! second-derivatives
              Uxx(Mloc,Nloc),Vyy(Mloc,Nloc),DUxx(Mloc,Nloc),DVyy(Mloc,Nloc), &
! 1st-derivatives
              Ux(Mloc,Nloc),Uy(Mloc,Nloc),Vx(Mloc,Nloc),Vy(Mloc,Nloc), &
              DUx(Mloc,Nloc),DUy(Mloc,Nloc),DVx(Mloc,Nloc),DVy(Mloc,Nloc), &
              ETAT(Mloc,Nloc),ETAx(Mloc,Nloc),ETAy(Mloc,Nloc), &
              ETATx(Mloc,Nloc),ETATy(Mloc,Nloc), &
! time-derivatives
              U0(Mloc,Nloc),V0(Mloc,Nloc),Ut(Mloc,Nloc),Vt(Mloc,Nloc),&
              Utx(Mloc,Nloc),Vty(Mloc,Nloc),Utxx(Mloc,Nloc),Utxy(Mloc,Nloc),&
              Vtxy(Mloc,Nloc),Vtyy(Mloc,Nloc),&
              DUtxx(Mloc,Nloc),DUtxy(Mloc,Nloc),&
              DVtxy(Mloc,Nloc),DVtyy(Mloc,Nloc),DUtx(Mloc,Nloc),DVty(Mloc,Nloc),&
! P Q Eta, Fx, Fy
              PL(Mloc1,Nloc),PR(Mloc1,Nloc),QL(Mloc,Nloc1),QR(Mloc,Nloc1), &
              FxL(Mloc1,Nloc),FxR(Mloc1,Nloc),FyL(Mloc,Nloc1),FyR(Mloc,Nloc1), &
              GxL(Mloc1,Nloc),GxR(Mloc1,Nloc),GyL(Mloc,Nloc1),GyR(Mloc,Nloc1), &
              EtaRxL(Mloc1,Nloc),EtaRxR(Mloc1,Nloc), &
              EtaRyL(Mloc,Nloc1),EtaRyR(Mloc,Nloc1), &
! sponge
              SPONGE(Mloc,Nloc), &
! original variables at notes
              Fx(Mloc1,Nloc),Fy(Mloc,Nloc1),&
              U(Mloc,Nloc),V(Mloc,Nloc), HU(Mloc,Nloc),HV(Mloc,Nloc),&
              Gx(Mloc1,Nloc),Gy(Mloc,Nloc1), &
              P(Mloc1,Nloc),Q(Mloc,Nloc1), &
              SxL(Mloc1,Nloc),SxR(Mloc1,Nloc), &
              SyL(Mloc,Nloc1),SyR(Mloc,Nloc1),SourceX(Mloc,Nloc), &
              SourceY(Mloc,Nloc), &
! others

              Umean(Mloc,Nloc),Vmean(Mloc,Nloc),ETAmean(Mloc,Nloc),&
              Usum(Mloc,Nloc),Vsum(Mloc,Nloc),ETAsum(Mloc,Nloc), &
              nu_smg(Mloc,Nloc), &
              Num_Zero_Up(Mloc,Nloc), &
              WaveHeightRMS(Mloc,Nloc),  &
              WaveHeightAve(Mloc,Nloc),  &
              Emax(Mloc,Nloc),  &
              Emin(Mloc,Nloc), &
              HrmsSum(Mloc,Nloc), &
              HavgSum(Mloc,Nloc), &


              U4xL(Mloc1,Nloc),U4xR(Mloc1,Nloc),&
              V4yL(Mloc,Nloc1),V4yR(Mloc,Nloc1) &

              )
      ALLOCATE(Depth(Mloc,Nloc),H(Mloc,Nloc),&
               Depthx(Mloc1,Nloc),Depthy(Mloc,Nloc1), &
               MASK(Mloc,Nloc),DepthNode(Mloc1,Nloc1), &
               MASK_STRUC(Mloc,Nloc),MASK9(Mloc,Nloc), &
               tmp4preview(Mloc,Nloc),Int2Flo(Mloc,Nloc),&
               Cd(Mloc,Nloc) &
              )
! updating variables
      ALLOCATE(Eta(Mloc,Nloc),Eta0(Mloc,Nloc), &
               Ubar0(Mloc,Nloc),Vbar0(Mloc,Nloc),&
               Ubar(Mloc,Nloc),Vbar(Mloc,Nloc))
! dispersion updating variables

      ALLOCATE(U4(Mloc,Nloc),V4(Mloc,Nloc),U1p(Mloc,Nloc), & 
               V1p(Mloc,Nloc),U1pp(Mloc,Nloc),V1pp(Mloc,Nloc),&
               U2(Mloc,Nloc),V2(Mloc,Nloc),U3(Mloc,Nloc),V3(Mloc,Nloc))

      IF(WAVEMAKER(1:6)=='WK_IRR')THEN
       ALLOCATE(D_gen_ir(Nfreq,Ntheta),rlamda_ir(Nfreq,Ntheta),phase_ir(Nfreq,Ntheta),&
                Beta_gen_ir(Nfreq),omgn_ir(Nfreq), &
                Cm(Mloc,Nloc,Nfreq),Sm(Mloc,Nloc,Nfreq))
      ENDIF

      IF(WAVEMAKER(1:7)=='WK_TIME')THEN
        ALLOCATE(WAVE_COMP(NumWaveComp,3),Beta_genS(NumWaveComp),D_genS(NumWaveComp) )
      ENDIF

      IF(SHOW_BREAKING)THEN
       ALLOCATE(AGE_BREAKING(Mloc,Nloc))
      ENDIF

      IF(OUT_Hmax)THEN
        ALLOCATE(HeightMax(Mloc,Nloc))
        HeightMax=ZERO
      ENDIF
      IF(OUT_Hmin)THEN
        ALLOCATE(HeightMin(Mloc,Nloc))
        HeightMin=ZERO
      ENDIF
      IF(OUT_Umax)THEN
        ALLOCATE(VelocityMax(Mloc,Nloc))
        VelocityMax=ZERO
      ENDIF
      IF(OUT_VORmax)THEN
        ALLOCATE(VorticityMax(Mloc,Nloc))
        VorticityMax=ZERO
      ENDIF
      IF(OUT_MFmax)THEN
        ALLOCATE(MomentumFluxMax(Mloc,Nloc))
        MomentumFluxMax=ZERO
      ENDIF
      

END SUBROUTINE ALLOCATE_VARIABLES

! ----------------------------------------------------
!    This is subroutine initialization
!  called by 
!        MAIN
!    Last Update: 26/09/2013 Babak Tehranirad, University of Delaware
! --------------------------------------------------
SUBROUTINE INITIALIZATION
     USE GLOBAL
     USE Input_Util



     IMPLICIT NONE
     CHARACTER(LEN=80) :: WHAT

     REAL(SP),DIMENSION(:,:),ALLOCATABLE :: VarGlob




! parameter kappa for order of MUSCL
     IF(HIGH_ORDER(1:3)=='SEC')THEN
      Kappa = -1.0_SP
     ELSE
      Kappa = 1.0_SP/3.0_SP
     ENDIF

! set zeros

     T_sum = ZERO
     Umean = ZERO
     Vmean = ZERO
     ETAmean = ZERO
     nu_smg = ZERO
     Num_Zero_Up = 0
     WaveHeightRMS = ZERO
     WaveHeightAve =ZERO 
     Emax = ZERO
     Emin = ZERO
     HrmsSum = ZERO
     HavgSum = ZERO

     DelxU=0.0_SP
     DelxHU=0.0_SP
     DelxV=0.0_SP
     DelxEtar=0.0_SP
     DelyU=0.0_SP
     DelyHV=0.0_SP
     DelyV=0.0_SP
     DelyEtar=0.0_SP
     DelxHV=0.0_SP
     DelyHU=0.0_SP
     UxL=0.0_SP
     UxR=0.0_SP
     VxL=0.0_SP
     VxR=0.0_SP
     HUxL=0.0_SP
     HUxR=0.0_SP
     HVxL=0.0_SP
     HVxR=0.0_SP
     HxL=0.0_SP
     HxR=0.0_SP
     UyL=0.0_SP
     UyR=0.0_SP
     VyL=0.0_SP
     VyR=0.0_SP
     HVyL=0.0_SP
     HVyR=0.0_SP
     HUyL=0.0_SP
     HUyR=0.0_SP
     HyL=0.0_SP
     HyR=0.0_SP

     U4xL=ZERO
     U4xR=ZERO
     V4yL=ZERO
     V4yR=ZERO

     Uxy=ZERO
     Vxy=ZERO
     DUxy=ZERO
     DVxy=ZERO
     Uxx=ZERO
     Vyy=ZERO
     DUxx=ZERO
     DVyy=ZERO 
     U0=ZERO
     V0=ZERO
     Ut=ZERO
     Vt=ZERO
     Utx=ZERO
     Vty=ZERO
     Utxx=ZERO
     Utxy=ZERO
     Vtxy=ZERO
     Vtyy=ZERO
     DUtxx=ZERO
     DUtxy=ZERO
     DVtxy=ZERO
     DVtyy=ZERO
     DUtx=ZERO
     DVty=ZERO    
     PL=0.0_SP
     PR=0.0_SP
     QL=0.0_SP
     QR=0.0_SP
     FxL=0.0_SP
     FxR=0.0_SP
     FyL=0.0_SP
     FyR=0.0_SP
     GxL=0.0_SP
     GxR=0.0_SP
     GyL=0.0_SP
     GyR=0.0_SP
     SxL=0.0_SP
     SxR=0.0_SP
     SyL=0.0_SP
     SyR=0.0_SP
! original variables
     Ubar=0.0_SP
     Vbar=0.0_SP
     Ubar0=0.0_SP
     Vbar0=0.0_SP
     U=0.0_SP
     V=0.0_SP
     HU=0.0_SP
     HV=0.0_SP
     Fx=0.0_SP
     Fy=0.0_SP
     Gx=0.0_SP
     Gy=0.0_SP
     P=0.0_SP
     Q=0.0_SP
     U1p=ZERO
     V1p=ZERO

     U4=ZERO
     V4=ZERO
     U1pp=ZERO
     V1pp=ZERO
     U2=ZERO
     V2=ZERO
     U3=ZERO
     V3=ZERO

     Depth=10.0_SP
     DepthNode=10.0_SP
     H=0.0_SP
     Eta=0.0_SP
     SourceX=0.0_SP
     SourceY=0.0_SP
     PLOT_COUNT=0.0_SP
     PLOT_COUNT_STATION=0.0_SP
     HOTSTART_COUNT=ZERO
     MASK=1
     MASK_STRUC=1
     SCREEN_COUNT=ZERO
     SPONGE=1.0_SP
      

     
     IF(SHOW_BREAKING)THEN
     AGE_BREAKING = ZERO
     ENDIF

     PLOT_COUNT=PLOT_INTV+SMALL
     PLOT_COUNT_STATION=PLOT_INTV_STATION+SMALL

     SCREEN_COUNT=SCREEN_INTV+SMALL
   
     DT=ZERO

     IF(Gamma3 > ZERO) THEN
     ELSE
!      gamma3 = 0 means linear shallow water equation
      Gamma1 =ZERO

      Gamma2 = ZERO

     ENDIF

     IF(DISPERSION)THEN
        ! make sure gamma1 and gamma2 are right
     ELSE
       Gamma1=ZERO

       Gamma2=ZERO

     ENDIF


! physics - below are fully nonlinear Boussinesq for reference
      a1=Beta_ref*Beta_ref/2.0_SP - 1.0_SP/6.0_SP
      a2=Beta_ref + 1.0_SP/2.0_SP
      b1=Beta_ref*Beta_ref
      b2=Beta_ref
! kennedy's equation
      Beta_1=Beta_ref+1.0_SP
      Beta_2=(1.0_SP/5.0_SP)**2/1.0_SP
!      Beta_2=ZERO     



! bathymetry

  IF(DEPTH_TYPE(1:3)=='DAT')THEN

     call GetFile (DEPTH_FILE,Depth)

  ENDIF

  IF(DEPTH_TYPE(1:3)=='FLA') THEN
    DO J=1,Nloc
     DO I=1,Mloc
      Depth(I,J) = Depth_FLat
     ENDDO
    ENDDO

    DepthFormat(1:3)='ELE'
  ENDIF



  IF(DEPTH_TYPE(1:3)=='SLO') THEN
    IF(.NOT.ALLOCATED(VarGlob)) ALLOCATE (VarGlob(Mglob,Nglob))
    DO J=1,Nglob
     DO I=1,Mglob
      VarGlob(I,J) = Depth_FLat
     ENDDO

     DO I=INT(Xslp/DX)+1,Mglob
      VarGlob(I,J) = Depth_Flat-SLP*(I-(INT(Xslp/DX)+1))*DX
     ENDDO





    ENDDO ! end of J

    CALL DISTRIBUTE_VarGlob (VarGlob,Depth)

    DEALLOCATE (VarGlob)
    DepthFormat(1:3)='ELE'
  ENDIF



! depth at ghost cells and re-construct depth at x,y-interfaces


    IF(DepthFormat(1:3)=='ELE')THEN

! done at the last step


! re-construct Depth

     DO J=1,Nloc
     DO I=2,Mloc
      DepthX(I,J)=0.5_SP*(Depth(I-1,J)+Depth(I,J))
     ENDDO
     ENDDO
     DO J=1,Nloc
      DepthX(1,J)=0.5_SP*(3.0_SP*Depth(1,J)-Depth(2,J))
      DepthX(Mloc1,J)=0.5_SP*(3.0_SP*Depth(Mloc,J)-Depth(Mloc-1,J))
     ENDDO

     DO J=2,Nloc
     DO I=1,Mloc
      DepthY(I,J)=0.5_SP*(Depth(I,J-1)+Depth(I,J))
     ENDDO
     ENDDO
     DO I=1,Mloc
      DepthY(I,1)=0.5_SP*(3.0_SP*Depth(I,1)-Depth(I,2))
      DepthY(I,Nloc1)=0.5_SP*(3.0_SP*Depth(I,Nloc)-Depth(I,Nloc-1))
     ENDDO

    ELSEIF(DepthFormat(1:3)=='NOD')THEN

! ghost cells
     DO I=1,Mloc1
       DO J=1,Nghost
        DepthNode(I,J)=DepthNode(I,Jbeg)
       ENDDO
       DO J=Jend1+1,Nloc1
        DepthNode(I,J)=DepthNode(I,Jend1)
       ENDDO
     ENDDO

     DO J=1,Nloc1
       DO I=1,Nghost
        DepthNode(I,J)=DepthNode(Ibeg,J)
       ENDDO
       DO I=Iend1+1,Mloc1
        DepthNode(I,J)=DepthNode(Iend1,J)
       ENDDO
     ENDDO    

! reconstruct depth at x,y-interfaces
     DO J=1,Nloc
     DO I=1,Mloc1
      DepthX(I,J)=0.5_SP*(DepthNode(I,J)+DepthNode(I,J+1))
     ENDDO
     ENDDO
     DO J=1,Nloc1
     DO I=1,Mloc
      DepthY(I,J)=0.5_SP*(DepthNode(I,J)+DepthNode(I+1,J))
     ENDDO
     ENDDO
      
    ENDIF

!   don't need to re-construct depth in terms of using well-balanced scheme
!   01/21/2012
!   add this on to keep consistent with the old version 09/06/2012
     DO J=1,Nloc
     DO I=1,Mloc
       Depth(I,J)=0.25_SP*(Depthx(I,J)+Depthx(I+1,J)+Depthy(I,J)+Depthy(I,J+1))
     ENDDO
     ENDDO
 
!friction
     IF(IN_Cd) THEN

      call GetFile(CD_FILE,Cd)







      ELSE
          DO J=1,Nloc
            DO I=1,Mloc
              Cd(I,J) = Cd_fixed
          ENDDO
          ENDDO
      ENDIF



! initial eta u and v for deforming
     IF(INI_UVZ) THEN
        CALL INITIAL_UVZ(Mloc,Nloc,Nghost,U_FILE,V_FILE,ETA_FILE,U,V,Eta)
     ENDIF 

! initial solitary wave
     IF(WaveMaker(1:7)=='INI_SOL') THEN
       CALL INITIAL_SOLITARY_WAVE(Mloc,Nloc, DX,Xwavemaker,& 
          AMP_SOLI,Dep_Soli,Beta_ref,U,V,Eta)
     ENDIF



! initial N wave
     IF(WaveMaker(1:6)=='N_WAVE') THEN
       CALL INITIAL_N_WAVE(Mloc,Nloc, DX,x1_Nwave,& 
          x2_Nwave,a0_Nwave,gamma_Nwave,dep_Nwave,U,V,Eta)
     ENDIF



! initial rectangular hump
     IF(WaveMaker(1:7)=='INI_REC') THEN

       CALL INITIAL_RECTANGULAR(Mloc,Nloc,Nghost,DX,DY,Xc,Yc,WID,AMP_SOLI, &
                      Eta)




     ENDIF



! initial gausian hump
     IF(WaveMaker(1:7)=='INI_GAU') THEN

       CALL INITIAL_GAUSIAN(Mloc,Nloc,Nghost,DX,DY,Xc,Yc,AMP_SOLI, WID,&
                      Eta)




     ENDIF
! initial dipole from x-derivative of gausian hump
     IF(WaveMaker(1:7)=='INI_DIP') THEN

       CALL INITIAL_DIPOLE(Mloc,Nloc,Nghost,DX,DY,Xc,Yc,AMP_SOLI, WID,&
                      Eta)




     ENDIF



! initial wave surface
     IF(WaveMaker(1:7)=='INI_OTH') THEN
       CALL INITIAL_WAVE
     ENDIF

   
!!!  now include spherical # if defined (AlsoSphericalCARTESIAN)
! internal wavemaker of wei and kirby

     IF(WaveMaker(1:6)=='WK_REG') THEN


     Yc_WK=(INT((Nloc-1)/2)+Nghost)*DY

    
     IF(PERIODIC)THEN

    ! calculate wave number
!--- here's from the curvilinear code, Shi etal 2001
!        alpha=-0.39
!        alpha1=alpha+1./3.
!        omgn=2.*pi/Tperiod
!
!        tb=omgn*omgn*h_gen/grav
!        tc=1.+tb*alpha
!        IF(h_gen==ZERO.OR.Tperiod==ZERO)THEN
!         WRITE(*,*)'re-set depth, Tperiod for wavemaker, STOP!'
!         STOP
!        ELSE
!          wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))  &
!                /(2.0_SP*alpha1))/h_gen
!---

       tmp1 = -0.39_SP + 1.0_SP / 3.0_SP  ! alpha1
       tmp2 = 2.*pi/Tperiod               ! omgn
       tmp2 = tmp2*tmp2*DEP_WK/grav       ! tb
       tmp3 = 1.0_SP + tmp2*(-0.39_SP)    ! tc

      IF(DEP_WK==ZERO.OR.Tperiod==ZERO)THEN
         WRITE(*,*)'re-set depth, Tperiod for wavemaker, STOP!'
         STOP
      ELSE       
       tmp1 = SQRT((tmp3-SQRT(tmp3*tmp3-4.0_SP*tmp1*tmp2))  &
                /(2.0_SP*tmp1))/DEP_WK     ! wkn 
      ENDIF 
     IF(Theta_WK.NE.ZERO)THEN 
      IF(Theta_WK.GT.ZERO)THEN   
       tmp3=ZERO
       I=0
       Do WHILE (tmp3<Theta_WK)
         I=I+1
!         tmp2=I*2.0_SP*pi/DY/(Jend-Jbeg)     ! rlamda

          tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)



         IF(tmp2.GE.tmp1)THEN
          tmp3=90.0
          WRITE(*,*)'should enlarge domain for periodic boundary with this wave angle, STOP'
          STOP
         ELSE
           tmp3=ASIN(tmp2/tmp1)*180.0_SP/pi    ! theta, based on rlamda=wkn*sin(theta)
         ENDIF
         IF(I>1000)THEN
           WRITE(*,*) 'could not find a wave angle for periodic boundary condition, STOP'
         ENDIF
       ENDDO
      ELSEIF(Theta_WK.LT.ZERO)THEN
       tmp3=ZERO
       I=0
       Do WHILE (tmp3>Theta_WK)
         I=I+1
!         tmp2=I*2.0_SP*pi/DY/(Jend-Jbeg)     ! rlamda

         tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)     ! rlamda



         IF(tmp2.GE.tmp1)THEN
          tmp3=-90.0
          WRITE(*,*)'should enlarge domain for periodic boundary with this wave angle, STOP'
          STOP
         ELSE
           tmp3=-ASIN(tmp2/tmp1)*180.0_SP/pi    ! theta, based on rlamda=wkn*sin(theta)
         ENDIF
         IF(I>1000)THEN
           WRITE(*,*) 'could not find a wave angle for periodic boundary condition, STOP'
         ENDIF
       ENDDO
      ENDIF

       WRITE(*,*) 'wave angle you set:', Theta_WK
       WRITE(*,*) 'wave angle in calculation to make periodic boundary:', tmp3
   

   ! do nothing







       Theta_WK = tmp3
     ENDIF
    ENDIF ! end theta .ne.zero

       CALL WK_WAVEMAKER_REGULAR_WAVE & 
               (Tperiod,AMP_WK,Theta_WK,DEP_WK,Delta_WK,D_gen,rlamda,beta_gen,Width_WK)

      IF(SHOW_BREAKING)THEN
       T_brk=Tperiod*0.5_SP
      ENDIF
     ENDIF
     
! ****
     IF(WaveMaker(1:9)=='WK_DATA2D')THEN
      OPEN(1,FILE=TRIM(WaveCompFile))
       READ(1,*)NumFreq,NumDir
       ALLOCATE (WAVE_COMP(NumFreq,NumDir),Beta_gen2D(NumFreq,NumDir),D_gen2D(NumFreq,NumDir),  &
          Phase2D(NumFreq,NumDir), &
          Freq(NumFreq),Dire(NumDir),rlamda2D(NumFreq,NumDir))
       READ(1,*)PeakPeriod
       DO J=1,NumFreq
          READ(1,*)Freq(J)
       ENDDO
       DO I=1,NumDir
          READ(1,*)Dire(I)
       ENDDO
       DO I=1,NumDir
         READ(1,*)(WAVE_COMP(J,I),J=1,NumFreq)
       ENDDO

       CALL WK_WAVEMAKER_2D_SPECTRAL_DATA & 
               (NumFreq,NumDir,Freq,Dire,WAVE_COMP,PeakPeriod,DEP_WK,Delta_WK,D_gen2D,beta_gen2D,&
               rlamda2D,Width_WK)
               
! random phase
       DO J=1,NumFreq
       DO I=1,NumDir



          Phase2D(J,I)=rand(0)*2.0_SP*pi

       ENDDO
       ENDDO

      IF(SHOW_BREAKING)THEN
       T_brk=WAVE_COMP(NumWaveComp,1) 
      ENDIF

     ENDIF ! end wk_data2d
! ****
       
     IF(WaveMaker(1:7)=='WK_TIME')THEN
      OPEN(1,FILE=TRIM(WaveCompFile))
       DO J=1,NumWaveComp
         READ(1,*)(WAVE_COMP(J,I),I=1,3)
       ENDDO


       CALL WK_WAVEMAKER_TIME_SERIES &
               (NumWaveComp,WAVE_COMP,PeakPeriod,DEP_WK,Delta_WK,D_genS,beta_genS,Width_WK)

      IF(SHOW_BREAKING)THEN
       T_brk=WAVE_COMP(NumWaveComp,1) 
      ENDIF

     ENDIF !

! wei and kirby, 1999, irregular wavemaker
      
     IF(WaveMaker(1:6)=='WK_IRR') THEN

      CALL WK_WAVEMAKER_IRREGULAR_WAVE & 
       (Nfreq,Ntheta,delta_WK,DEP_WK,FreqPeak,FreqMax,FreqMin,GammaTMA,Hmo,ThetaPeak, &
         sigma_theta,rlamda_ir,beta_gen_ir,D_gen_ir,Phase_ir,Width_WK,omgn_ir,&
         Periodic,DY,Nglob)
      CALL CALCULATE_Cm_Sm(Mloc,Nloc,DX,DY,Xc_WK,Ibeg,Jbeg,Nfreq,Ntheta,&
               D_gen_ir,Phase_ir,Width_WK,rlamda_ir,beta_gen_ir,Cm,Sm)











      IF(SHOW_BREAKING)THEN
       T_brk=1.0_SP/FreqMax
      ENDIF

     ENDIF
!   now include spherical # endif
      
     IF(SPONGE_ON)THEN
       CALL CALCULATE_SPONGE(Mloc,Nloc,Nghost,DX,DY,&
                            Sponge_west_width,Sponge_east_width,&
                            Sponge_south_width,Sponge_north_width, &
                            R_sponge,A_sponge,SPONGE)
     ENDIF

! get Eta and H
     H=MAX(Eta*Gamma3+Depth,MinDepthFrc)
     HU=H*U
     HV=H*V
    
     IF(DISPERSION)THEN
       CALL CAL_DISPERSION
     ENDIF
    
     Ubar=HU+gamma1*U1p*H
     Vbar=HV+gamma1*V1p*H

     DO J=1,Nloc
     DO I=1,Mloc
      IF(Eta(I,J)<-DEPTH(I,J))THEN
       MASK(I,J)=0
       Eta(I,J)=MinDepth-Depth(I,J)
      ELSE
       MASK(I,J)=1
      ENDIF
     ENDDO
     ENDDO
      
! read obstacle structures ! $$$
     IF(OBSTACLE)THEN
     IF(.not.check_exist(TRIM(OBSTACLE_FILE)))THEN
      !WRITE(*,*)TRIM(OBSTACLE_FILE), ' specified in input.txt but does not exist in folder.'
      STOP
     ENDIF
     

     IF(.NOT.ALLOCATED(VarGlob)) ALLOCATE (VarGlob(Mloc,Nloc)) ! use local here 
     call GetFile (TRIM(OBSTACLE_FILE),VarGlob)
     MASK_STRUC = INT(VarGlob)
     DEALLOCATE(VarGlob)







     ENDIF
       


     DO J=1,Nloc
     DO I=1,Mloc
      IF(MASK_STRUC(I,J)==0)THEN
        Depth(I,J)=-LARGE
      ENDIF
     ENDDO
     ENDDO

     MASK=MASK*MASK_STRUC

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      MASK9(I,J)=MASK(I,J)*MASK(I-1,J)*MASK(I+1,J)  &
                *MASK(I+1,J+1)*MASK(I,J+1)*MASK(I-1,J+1) &
                *MASK(I+1,J-1)*MASK(I,J-1)*MASK(I-1,J-1) 
     ENDDO
     ENDDO





! deal with masks, this is great for an extremely large bed slope
! at edges of mask point, depth at cell interface can cause unreasonable large 
! depth gradient, even if the slope cap is on. Making depth locally flat can avoid
! this happening. This scheme is also used in update_mask subroutine
! 01/21/2012  

      DO J=2,Nloc-1
      DO I=2,Mloc-1
        IF(MASK(I,J)<1)THEN
         DepthX(I,J)=Depth(I-1,J)
         DepthX(I+1,J)=Depth(I+1,J)
         DepthY(I,J)=Depth(I,J-1)
         DepthY(I,J+1)=Depth(I,J+1)
        ENDIF
      ENDDO
      ENDDO

END SUBROUTINE INITIALIZATION

! ----------------------------------------------------
!    This is subroutine to index for MPI
!  called by 
!        MAIN
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------

SUBROUTINE INDEX
    USE GLOBAL
    IMPLICIT NONE

! TEMP


    NumberProcessor = px*py
    dims(1) = px
    dims(2) = py
    periods(1) = .false.
    periods(2) = .false.
    IF(PERIODIC) periods(2) = .true.
    coords(1) = 0
    coords(2) = 0

    call MPI_CART_CREATE( MPI_COMM_WORLD, ndims, dims, &
         periods, reorder, comm2d, ier )
    call MPI_CART_COORDS( comm2d, myid, 2, coords, ier)

    npx = coords(1)
    npy = coords(2)

    call MPI_Cart_shift( comm2d, 0, 1, n_west, n_east, ier )
    call MPI_Cart_shift( comm2d, 1, 1, n_suth, n_nrth, ier )

! check
! print*,myid, n_west,n_east,n_suth,n_nrth
!      call MPI_FINALIZE ( ier )






! now for serial code
    Mloc=Mglob/px+2*Nghost
    Nloc=Nglob/py+2*Nghost
    Mloc1=Mloc+1
    Nloc1=Nloc+1

    Ibeg=Nghost+1
    Iend=Mloc-Nghost
    Iend1=Mloc1-Nghost
    Jbeg=Nghost+1
    Jend=Nloc-Nghost
    Jend1=Nloc1-Nghost

END SUBROUTINE INDEX

! ---------------------------------------------------
!    This is subroutine of sponge layer to get SPONGE
!  called by
!      INITIALIZATION
!    Last Update: 10/27/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE CALCULATE_SPONGE(M,N,Nghost,DX,DY,&
                            Sponge_west_width,Sponge_east_width,&
                            Sponge_south_width,Sponge_north_width, &
                            R_sponge,A_sponge,SPONGE)
     USE PARAM

     USE GLOBAL, ONLY : n_west, n_east, n_suth, n_nrth

     IMPLICIT NONE
     INTEGER, INTENT(IN)::M,N,Nghost

     REAL(SP),INTENT(IN)::DX,DY



     REAL(SP),INTENT(IN) :: &
                          Sponge_west_width,Sponge_east_width,&
                          Sponge_south_width,Sponge_north_width, &
                          R_sponge,A_sponge
     REAL(SP),DIMENSION(M,N),INTENT(INOUT)::SPONGE
     REAL(SP)::ri,lim
     INTEGER::Iwidth

! west

     if ( n_west .eq. MPI_PROC_NULL) then

     IF(Sponge_west_width>ZERO)THEN

     Iwidth=INT(Sponge_west_width/DX)+Nghost



     DO J=1,N
     DO I=1,Iwidth
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
       ri=R_sponge**(50*(I-1)/(Iwidth-1))
       SPONGE(I,J)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO
     ENDIF

     endif

! east

     if ( n_east .eq. MPI_PROC_NULL) then

     IF(Sponge_east_width>ZERO)THEN

     Iwidth=INT(Sponge_east_width/DX)+Nghost



     DO J=1,N
     DO I=M-Iwidth+1,M
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
       ri=R_sponge**(50*(M-I)/(Iwidth-1))
       SPONGE(I,J)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO
     ENDIF

     endif


! south

     if ( n_suth .eq. MPI_PROC_NULL) then

     IF(Sponge_south_width>ZERO)THEN

     Iwidth=INT(Sponge_south_width/DY)+Nghost



     DO I=1,M
     DO J=1,Iwidth
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
       ri=R_sponge**(50*(J-1)/(Iwidth-1))
       SPONGE(I,J)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO
     ENDIF

     endif


! north

     if ( n_nrth .eq. MPI_PROC_NULL) then

     IF(Sponge_north_width>ZERO)THEN

     Iwidth=INT(Sponge_north_width/DY)+Nghost



     DO I=1,M
     DO J=N-Iwidth+1,N
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
       ri=R_sponge**(50*(N-J)/(Iwidth-1))
       SPONGE(I,J)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO
     ENDIF

     endif


END SUBROUTINE CALCULATE_SPONGE


! ---------------------------------------------------
!    This is subroutine of given initial u v and eta 
!  called by
!      INITIALIZATION
!    Last Update: 02/03/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE INITIAL_UVZ(M,N,Nghost,U_FILE,V_FILE,ETA_FILE,U,V,ETA)
      USE PARAM
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: M,N,Nghost
      REAL(SP),DIMENSION(M,N),INTENT(OUT) :: U,V,ETA
      CHARACTER(LEN=80),INTENT(IN) :: ETA_FILE
      CHARACTER(LEN=80),INTENT(IN) :: U_FILE
      CHARACTER(LEN=80),INTENT(IN) :: V_FILE


      call GetFile(U_FILE,U)
      call GetFile(V_FILE,V)
      call GetFile(ETA_FILE,ETA)


END SUBROUTINE INITIAL_UVZ


! --------------------------------------------------
!    This is subroutine to provide initial wave condition
!    it can be specified in input.txt giving 'INI'
!    called by
!       MAIN
!    Last Update: 10/01/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE INITIAL_WAVE
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),Dimension(Mloc,Nloc)::XX,YY
     REAL(SP):: sigma,x_c,y_c,a,r
     INTEGER :: ii1,ii2

! the initial wave domain includes ghost cells

     do j=1,Nloc
     do i=1,Mloc

       xx(i,j)=(i-1.)*dx
       yy(i,j)=(j-1.)*dy




     enddo
     enddo

     sigma=0.5
     x_c=10.0
     y_c=10.0	
     a=0.1

     do j=1,Nloc
     do i=1,Mloc
     goto 200
! box
     tmp1=21+Nghost
     tmp2=31+Nghost
     if(i>tmp1.and.i<tmp2.and.j>tmp1.and.j<tmp2)then
      Eta(i,j)=1.0_SP
     else
      Eta(i,j)=zero
     endif

200  continue

     goto 100
! dam break
        if(i<100+Nghost)then
         Eta(i,j)=5.0_SP
        else
         Eta(i,j)=0.0_SP
        endif
100   continue
! gausian
!         r=sqrt((xx(i,j)-x_c)**2+(yy(i,j)-y_c)**2)
!         Eta(i,j)=a*exp(-r**2/2./sigma**2)
     enddo
     enddo

! alongshore crest
     goto 213
     ii1=21+Nghost
     ii2=25+Nghost
     DO J=1,Nloc
       DO I=ii1,ii2
         Eta(I,J)=1.0_SP
       ENDDO
     ENDDO
213  continue

END SUBROUTINE INITIAL_WAVE



! --------------------------------------------------
!    This is subroutine to provide initial rectangular hump 
!    it can be specified in input.txt giving 'INI_REC'
!    called by
!       - INITIALIZATION
!    Last Update: 10/11/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------

SUBROUTINE INITIAL_RECTANGULAR(M,N,Nghost,DX,DY,Xc,Yc,WID,AMP,Eta)




     USE PARAM

     USE GLOBAL, ONLY : npx,npy,px,py,Mglob,Nglob,myid

     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Nghost

     REAL(SP),INTENT(IN) :: DX,DY,Xc,Yc,WID,AMP



     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Eta
     INTEGER :: Il,Jl,Ir,Jr

     Eta = ZERO



     Il=Nghost+Xc/DX-WID/DX +1 - npx*Mglob/px
     Ir=Nghost+Xc/DX+WID/DX +1 - npx*Mglob/px
     Jl=Nghost+Yc/DY-WID/DY +1 - npy*Nglob/py
     Jr=Nghost+Yc/DY+WID/DY +1 - npy*Nglob/py







     IF(Il>M.or.Ir<1.or.Jl>N.or.Jr<1)THEN
     ELSE
      IF(Il<1)Il=1
      IF(Ir>M)Ir=M
      IF(Jl<1)Jl=1
      IF(Jr>N)Jr=N
      Eta(Il:Ir,Jl:Jr) = AMP
     ENDIF

 
END SUBROUTINE INITIAL_RECTANGULAR




! --------------------------------------------------
!    This is subroutine to provide initial gausian hump 
!    it can be specified in input.txt giving 'INI_REC'
!    called by
!       - INITIALIZATION
!    Last Update: 10/11/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------

SUBROUTINE INITIAL_GAUSIAN(M,N,Nghost,DX,DY,Xc,Yc,AMP,WID,Eta)




     USE PARAM

     USE GLOBAL, ONLY : npx,npy,px,py,Mglob,Nglob,myid

     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Nghost

     REAL(SP),INTENT(IN) :: DX,DY,Xc,Yc,WID,AMP



     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Eta
     INTEGER :: Il,Jl,Ir,Jr
     REAL(SP) :: r,xr,yr
     
     Eta = ZERO



     DO J=1,N
     DO I=1,M
       xr=(I-1)*DX-Nghost*DX + (npx*Mglob/px)*DX
       yr=(J-1)*DY-Nghost*DY + (npy*Mglob/py)*DY
       r=SQRT((xr-Xc)**2+(yr-Yc)**2)
       Eta(I,J) = AMP*EXP(-r**2/1.0_SP/WID**2)
     ENDDO
     ENDDO



 
END SUBROUTINE INITIAL_GAUSIAN



! &&&

! --------------------------------------------------
!    This is subroutine to provide initial dipole 
!    it can be specified in input.txt giving 'INI_DIP'
!    called by
!       - INITIALIZATION
!    Last Update: 08/24/2012 Fengyan Shi, University of Delaware
! --------------------------------------------------

SUBROUTINE INITIAL_DIPOLE(M,N,Nghost,DX,DY,Xc,Yc,AMP,WID,Eta)




     USE PARAM

     USE GLOBAL, ONLY : npx,npy,px,py,Mglob,Nglob,myid

     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Nghost

     REAL(SP),INTENT(IN) :: DX,DY,Xc,Yc,WID,AMP



     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Eta
     INTEGER :: Il,Jl,Ir,Jr
     REAL(SP) :: r,xr,yr,uni
     
     Eta = ZERO



     DO J=1,N
     DO I=1,M
       xr=(I-1)*DX-Nghost*DX + (npx*Mglob/px)*DX
       yr=(J-1)*DY-Nghost*DY + (npy*Nglob/py)*DY
       r=SQRT((xr-Xc)**2+(yr-Yc)**2)
       uni=-1.414_SP/WID*(xr-Xc)
       Eta(I,J) = AMP*uni*EXP(-r**2/1.0_SP/WID**2+0.5_SP)
     ENDDO
     ENDDO



 
END SUBROUTINE INITIAL_DIPOLE





! --------------------------------------------------
!    This is subroutine to calculate Cm Sm for Wei and Kirby's 
!     internal wave maker, irregular wave (TMA)
!    called by
!       - INITIALIZATION
!    Last Update: 11/9/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE CALCULATE_Cm_Sm(M,N,DX,DY,Xc,Ibeg,Jbeg,mfreq,mtheta,D_gen,phi1, &
               width,rlamda,beta_gen,Cm,Sm)
     USE PARAM

     USE GLOBAL, ONLY : myid,npx,npy,px,py,Mglob,Nglob

     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,mfreq,mtheta,Ibeg,Jbeg    
     REAL(SP),INTENT(IN) :: DX,DY,width,Xc
     REAL(SP),DIMENSION(mfreq,mtheta),INTENT(IN) :: D_gen,phi1,rlamda 
     REAL(SP),DIMENSION(mfreq),INTENT(IN) :: beta_gen
     REAL(SP),DIMENSION(M,N,mfreq),INTENT(OUT) :: Cm,Sm
     INTEGER::kf,ktheta


        Cm=ZERO
        Sm=ZERO
        DO J=1,N
        DO I=1,M
          do kf=1,mfreq
           do ktheta=1,mtheta


            Cm(i,j,kf)=Cm(i,j,kf) &
             +D_gen(kf,ktheta)*exp(-beta_gen(kf)*((I-Ibeg +npx*Mglob/px)*DX-Xc)**2) &
          *cos(rlamda(kf,ktheta) &
          *((J-Jbeg  +npy*Nglob/py)*DY-ZERO)+phi1(kf,ktheta))

            Sm(i,j,kf)=Sm(i,j,kf) &
             +D_gen(kf,ktheta)*exp(-beta_gen(kf)*((I-Ibeg+ npx*Mglob/px)*DX-Xc)**2) &
          *sin(rlamda(kf,ktheta) &
          *((J-Jbeg +npy*Nglob/py)*DY-ZERO)+phi1(kf,ktheta))


           enddo
           enddo

        enddo
        enddo

END SUBROUTINE CALCULATE_Cm_Sm

! --------------------------------------------------
!    This is subroutine to calculate source function for Wei and Kirby's 
!     internal wave maker, irregular wave (TMA)
!    called by
!       - INITIALIZATION
!    Last Update: 11/8/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE WK_WAVEMAKER_IRREGULAR_WAVE & 
               (mfreq,mtheta,delta,h_gen,fm,fmax,fmin,gamma_spec,Hmo,theta_input,&
                sigma_theta_input,rlamda,beta_gen,D_gen,phi1,width,omgn, &
                Periodic,DY,Nglob)
     USE PARAM

     USE GLOBAL, only : myid, ier




     IMPLICIT NONE
     INTEGER,INTENT(IN) :: mfreq,mtheta,Nglob
     REAL(SP),INTENT(IN) :: delta,h_gen,fm,fmax,fmin,gamma_spec,Hmo,theta_input,&
                            sigma_theta_input,DY
     LOGICAL,INTENT(IN) :: Periodic
     REAL(SP),DIMENSION(mfreq,mtheta),INTENT(OUT) :: D_gen,phi1,rlamda 
     REAL(SP),DIMENSION(mfreq),INTENT(OUT) :: beta_gen,omgn
     REAL(SP), INTENT(OUT) :: width
     REAL(SP),DIMENSION(mfreq):: Freq
     REAL(SP), DIMENSION(mtheta) :: Hmo_each,AG
     REAL(SP), DIMENSION(10000) :: Ef10000
     REAL(SP) :: Ef,fre,omiga_spec,phi,sigma_spec,Etma,Ef100,Ef_add,sigma_theta,&
                 theta_p,theta_m,theta_10,theta_11,theta_21,alpha_spec,ap,&
                 theta,alpha,alpha1,tb,tc,wkn,C_phase,wave_length,rl_gen,rI,theta_1,&
                 omgn_tmp
     INTEGER :: kf,kff,kb,N_spec,ktotal,k_n,ktheta,mcenter

       EF=0.0_SP
! ---  get freq(100) and Hmo_each

        do kf=1,10000

        fre=fmin+(fmax-fmin)/10000.0_SP*(kf-1.0_SP)
        omiga_spec=2.0_SP*pi*fre*SQRT(h_gen/grav)
        phi=1.0_SP-0.5_SP*(2.0_SP-omiga_spec)**2
        if(omiga_spec.le.1.0_SP) phi=0.5_SP*omiga_spec**2
        if(omiga_spec.ge.2.0_SP) phi=1.0_SP

        sigma_spec=0.07_SP
        if(fre.gt.fm)sigma_spec=0.09_SP

        Etma=grav**2*fre**(-5)*(2.0_SP*pi)**(-4)*phi &
         *exp(-5.0_SP/4.0_SP*(fre/fm)**(-4)) &
         *gamma_spec**(exp(-(fre/fm-1.0_SP)**2/(2.0_SP*sigma_spec**2)))

        Ef=Ef+Etma*(fmax-fmin)/10000.0_SP
        Ef10000(kf)=Etma

        enddo

!---   get 100 frequecies
! -- it seems there's an inaccuracy caused by 100 freq if mfreq<100
!    should change to mfreq 29/10/2012
 
!        Ef100=Ef/101.0_SP
         Ef100=Ef/REAL(mfreq+1)
!        print*, Ef100, Ef
        kb=0
!        do kff=1,100
        do kff=1,mfreq

        Ef_add=0.0_SP
        do k=kb+1,10000

          Ef_add=Ef_add+Ef10000(k)*(fmax-fmin)/10000.0_SP
          if(Ef_add.ge.Ef100.or.k.eq.10000) then

            Freq(kff)=fmin+(fmax-fmin)/10000.0_SP*(k-(k-kb)/2)

             kb=k
            goto 100

          endif

        enddo
100     continue

! sometimes Freq=0 happens, 02/08/2012
        IF(Freq(kff).eq.0.0)THEN
           Freq(kff)=Freq(kff-1)
        ENDIF
        enddo

! --- directional wave
        sigma_theta=sigma_theta_input*pi/180.0_SP
        N_spec=20.0_SP/sigma_theta

        ktotal=mtheta

	  theta_1=-pi/3.0_SP
          AG(1)=(theta_1+pi)/(2.0_SP*pi)
          do k_n=1,N_spec
            AG(1)=AG(1)+1.0_SP/pi/k_n  &
             *exp(-(k_n*sigma_theta)**2/2.0_SP) &
             *(sin(k_n*theta_1))
          enddo
	
        do ktheta=2,(mtheta-1)/2
          theta_p=theta_1+2.0_SP/3.0_SP*pi/(ktotal-1.0_SP)*(ktheta-1.0_SP)
          theta_m=theta_1+2.0_SP/3.0_SP*pi/(ktotal-1.0_SP)*(ktheta-2.0_SP)
          AG(ktheta)=(theta_p-theta_m)/(2.0_SP*pi)
          do k_n=1,N_spec
            AG(ktheta)=AG(ktheta)+1./pi/k_n  &
            *exp(-(k_n*sigma_theta)**2/2.)   &
            *(sin(k_n*theta_p)-sin(k_n*theta_m))
          enddo
        enddo

	  theta_10=-2.0_SP/3.0_SP*pi/(ktotal-1.0_SP)
	  theta_11=2.0_SP/3.0_SP*pi/(ktotal-1.0_SP)
          mcenter=(mtheta-1)/2+1
          AG(mcenter)=(theta_11-theta_10)/(2.0_SP*pi)
          do k_n=1,N_spec
            AG(mcenter)=AG(mcenter)+1./pi/k_n  &
            *exp(-(k_n*sigma_theta)**2/2.)     &
            *(sin(k_n*theta_11)-sin(k_n*theta_10))
          enddo	

        do ktheta=mcenter+1,mtheta-1
          theta_p=theta_1+2.0_SP/3.0_SP*pi/(ktotal-1.0_SP)*(ktheta-0.0_SP)
          theta_m=theta_1+2.0_SP/3.0_SP*pi/(ktotal-1)*(ktheta-1.0_SP)
          AG(ktheta)=(theta_p-theta_m)/(2.0_SP*pi)
          do k_n=1,N_spec
            AG(ktheta)=AG(ktheta)+1.0/pi/k_n  &
            *exp(-(k_n*sigma_theta)**2/2.0_SP) &
            *(sin(k_n*theta_p)-sin(k_n*theta_m))
          enddo
        enddo

	  theta_21=pi/3.0_SP
          AG(mtheta)=(pi-theta_21)/(2.0_SP*pi)
          do k_n=1,N_spec
            AG(mtheta)=AG(mtheta)+1.0_SP/pi/k_n  &
            *exp(-(k_n*sigma_theta)**2/2.0_SP)  &
            *(sin(-k_n*theta_21))
          enddo
	

        alpha_spec=Hmo**2/16.0_SP/Ef

        do ktheta=1,mtheta
        Hmo_each(ktheta)=4.0_SP*SQRT((alpha_spec*Ef100*AG(ktheta)))

        enddo
! ---  wave generation parameter
        do kf=1,mfreq

        do ktheta=1,mtheta

! here should convert Hmo to Hrms
!  Hrms=1./sqrt(2)*Hmo
! 05/12/2011 Joe has reported that the conversion should be removed
!        ap=Hmo_each(ktheta)/SQRT(2.0_SP)/2.0_SP
        ap=Hmo_each(ktheta)/2.0_SP
        theta=-pi/3.0_SP+theta_input*pi/180.0_SP+2.0_SP/3.0_SP*pi/ktotal*ktheta
        alpha=-0.39_SP
        alpha1=alpha+1.0_SP/3.0_SP
        omgn(kf)=2.0_SP*pi*Freq(kf)

        tb=omgn(kf)*omgn(kf)*h_gen/grav
        tc=1.0_SP+tb*alpha
        wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen

! here I use fixed C_phase and wave_length to determine beta_gen 
! as suggested by Wei and Kirby 1999

!        C_phase=1.0_SP/wkn*Freq(kf)*2.0_SP*pi
!        wave_length=C_phase/Freq(kf)

! in case wkn=0 02/08/2012  ***************
        IF(wkn.eq.0.0)THEN
           wkn=SMALL
           C_phase=sqrt(grav*h_gen)
           wave_length=C_phase/fm
        ELSE
          C_phase=1.0_SP/wkn*fm*2.0_SP*pi
          wave_length=C_phase/fm
        ENDIF
!                          ***************

! for periodic boundary conditions  
! ________________
 
     IF(PERIODIC)THEN
       tmp1=wkn
       IF(Theta.GT.ZERO)THEN
         tmp3=ZERO
         I=0
         Do WHILE (tmp3<Theta)
           I=I+1
           tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP) 
           IF(tmp2.GE.tmp1)THEN
            tmp3=pi/2.0_SP
           ELSE
            tmp3=ASIN(tmp2/tmp1)      ! theta, based on rlamda=wkn*sin(theta)
           ENDIF
           IF(I>1000)THEN
             WRITE(*,*) 'could not find a wave angle for periodic boundary condition, STOP'
           ENDIF
         ENDDO
          IF(tmp2.LT.tmp1) tmp3=ASIN((I-1)*2.0_SP*pi/DY/(Nglob-1.0_SP)/tmp1)
       ELSEIF(Theta.LT.ZERO)THEN
         tmp3=ZERO
         I=0
         Do WHILE (tmp3>Theta)
           I=I+1
           tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)     ! rlamda
           IF(tmp2.GE.tmp1)THEN
            tmp3=-pi/2.0_SP
           ELSE           
             tmp3=-ASIN(tmp2/tmp1)      ! theta, based on rlamda=wkn*sin(theta)
           ENDIF
           IF(I>1000)THEN
             WRITE(*,*) 'could not find a wave angle for periodic boundary condition, STOP'
           ENDIF
         ENDDO
          IF(tmp2.LT.tmp1) tmp3=-ASIN((I-1)*2.0_SP*pi/DY/(Nglob-1.0_SP)/tmp1)
       ENDIF
       Theta = tmp3
     ENDIF

! ________________

        rlamda(kf,ktheta)=wkn*sin(theta)
        beta_gen(kf)=80.0_SP/delta**2/wave_length**2

        rl_gen=wkn*cos(theta)
        rI=SQRT(pi/beta_gen(kf))*exp(-rl_gen**2/4.0_SP/beta_gen(kf))

        D_gen(kf,ktheta)=2.0_SP*ap*cos(theta)  &
        *(omgn(kf)**2-alpha1*grav*wkn**4*h_gen**3)  &
             /(omgn(kf)*wkn*rI*(1.0_SP-alpha*(wkn*h_gen)**2))

        enddo
        enddo


! calculate wavemaker width
        omgn_tmp=2.0_SP*pi*fm
        tb=omgn_tmp*omgn_tmp*h_gen/grav
        tc=1.0_SP+tb*alpha
        wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen
!        C_phase=1.0_SP/wkn*fm*2.0_SP*pi
!        wave_length=C_phase/fm
        width=delta*wave_length/2.0_SP
! ---   create phi1

        do ktheta=1,mtheta
        do kf=1,mfreq



          phi1(kf,ktheta)=rand(0)*2.0_SP*pi

        enddo
        enddo

END SUBROUTINE WK_WAVEMAKER_IRREGULAR_WAVE

! --------------------------------------------------
!    This is subroutine to calculate source function for Wei and Kirby's 
!     internal wave maker
!    called by
!       - INITIALIZATION
!    Last Update: 10/22/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE WK_WAVEMAKER_REGULAR_WAVE & 
               (Tperiod,AMP_WK,Theta_WK,H_gen,delta,D_gen,rlamda,beta_gen,width)
     USE PARAM
     IMPLICIT NONE
     REAL(SP) :: alpha,alpha1,omgn,tb,tc,wkn,C_phase,wave_length,&
                 rl_gen,rI,theta
     REAL(SP),INTENT(OUT) :: Beta_gen,rlamda,D_gen,width
     REAL(SP),INTENT(IN) :: Tperiod,AMP_WK,Theta_WK,H_gen,delta

        theta=Theta_WK*pi/180.
        alpha=-0.39
        alpha1=alpha+1./3.
        omgn=2.*pi/Tperiod

        tb=omgn*omgn*h_gen/grav
        tc=1.+tb*alpha
        IF(h_gen==ZERO.OR.Tperiod==ZERO)THEN
         WRITE(*,*)'re-set depth, Tperiod for wavemaker, STOP!'
         STOP
        ELSE
          wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))  &
                /(2.0_SP*alpha1))/h_gen
          C_phase=1./wkn/Tperiod*2.*pi
        ENDIF
        wave_length=C_phase*Tperiod

        rlamda=wkn*sin(theta)
        width=delta*wave_length/2.0_SP
        beta_gen=80.0_SP/delta**2/wave_length**2
        rl_gen=wkn*cos(theta)
        rI=SQRT(3.14159/beta_gen)*exp(-rl_gen**2/4./beta_gen)

        D_gen=2.0_SP*AMP_WK  &
            *cos(theta)*(omgn**2-alpha1*grav*wkn**4*h_gen**3) &
            /(omgn*wkn*rI*(1.0_SP-alpha*(wkn*h_gen)**2))

END SUBROUTINE WK_WAVEMAKER_REGULAR_WAVE

! --------------------------------------------------
!    This is subroutine to generate time series using 
!    source function for Wei and Kirby's 
!     internal wave maker
!    called by
!       - INITIALIZATION
!    Last Update: 04/13/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE WK_WAVEMAKER_TIME_SERIES & 
               (NumWaveComp,WAVE_COMP,PeakPeriod,H_gen,delta,D_gen,beta_gen,width)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: NumWaveComp
     REAL(SP) :: alpha,alpha1,omgn,tb,tc,wkn,C_phase,wave_length,&
                 rl_gen,rI,theta,Tperiod,AMP_WK,omgn_tmp,rlamda
     REAL(SP),DIMENSION(NumWaveComp), INTENT(OUT) :: Beta_gen,D_gen
     REAL(SP),INTENT(OUT) :: width
     REAL(SP),DIMENSION(NumWaveComp,3),INTENT(IN) :: WAVE_COMP
     REAL(SP),INTENT(IN) :: H_gen,delta,PeakPeriod

!        theta=Theta_WK*pi/180.
        theta = ZERO  ! assume zero because no or few cases include directions
        alpha=-0.39
        alpha1=alpha+1./3.

       DO I=1,NumWaveComp
        omgn=2.*pi/Wave_COMP(I,1)
        Tperiod = Wave_COMP(I,1)
        AMP_WK = WAVE_COMP(I,2)

        tb=omgn*omgn*h_gen/grav
        tc=1.+tb*alpha
        IF(h_gen==ZERO.OR.Tperiod==ZERO)THEN
         WRITE(*,*)'re-set depth, Tperiod for wavemaker, STOP!'
         STOP
        ELSE
          wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))  &
                /(2.0_SP*alpha1))/h_gen
          C_phase=1./wkn/Tperiod*2.*pi
        ENDIF
        wave_length=C_phase*Tperiod

        rlamda=wkn*sin(theta)
!        width=delta*wave_length/2.0_SP
        beta_gen(I)=80.0_SP/delta**2/wave_length**2
        rl_gen=wkn*cos(theta)
        rI=SQRT(3.14159/beta_gen(I))*exp(-rl_gen**2/4./beta_gen(I))

        D_gen(I)=2.0_SP*AMP_WK  &
            *cos(theta)*(omgn**2-alpha1*grav*wkn**4*h_gen**3) &
            /(omgn*wkn*rI*(1.0_SP-alpha*(wkn*h_gen)**2))

       ENDDO

! calculate width
        omgn_tmp=2.0_SP*pi/PeakPeriod
        tb=omgn_tmp*omgn_tmp*h_gen/grav
        tc=1.0_SP+tb*alpha
        wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen
        C_phase=1.0_SP/wkn/PeakPeriod*2.0_SP*pi
        wave_length=C_phase*PeakPeriod
        width=delta*wave_length/2.0_SP

END SUBROUTINE WK_WAVEMAKER_TIME_SERIES




! now include spherical # if defined (1)
! --------------------------------------------------
!    This is subroutine to generate directional spectrum for given 2D directional spectrum
!     for given 2D directional spectrum using
!    source function for Wei and Kirby's 
!     internal wave maker
!    called by
!       - INITIALIZATION
!    Last Update: 10/17/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE WK_WAVEMAKER_2D_SPECTRAL_DATA & 
               (NumFreq,NumDir,Freq,DireD,WAVE_COMP,PeakPeriod,H_gen,delta,D_gen,beta_gen,rlamda,width)
     USE PARAM
     USE GLOBAL, ONLY : PERIODIC,DY,Nglob
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: NumFreq,NumDir
     REAL(SP) :: alpha,alpha1,omgn,tb,tc,wkn,C_phase,wave_length,&
                 rl_gen,rI,theta,Tperiod,AMP_WK,omgn_tmp
     REAL(SP),DIMENSION(NumFreq,NumDir), INTENT(OUT) :: D_gen,Beta_gen,rlamda
     REAL(SP),DIMENSION(NumFreq,NumDir) :: Dir2D
     REAL(SP),INTENT(OUT) :: width
     REAL(SP),DIMENSION(NumFreq),INTENT(IN) :: Freq
     REAL(SP),DIMENSION(NumDir),INTENT(IN) :: DireD
     REAL(SP),DIMENSION(NumDir) :: Dire
     REAL(SP),DIMENSION(NumFreq,NumDir),INTENT(IN) :: WAVE_COMP
     REAL(SP),INTENT(IN) :: H_gen,delta,PeakPeriod
     INTEGER :: nfre,ndir
     REAL(SP) :: angle1,angle2

     Dire=DireD*DEG2RAD

! reorganize direction

! ************
     IF(PERIODIC)THEN

      DO nfre=1,NumFreq
      DO ndir=1,NumDir

       tmp1 = -0.39_SP + 1.0_SP / 3.0_SP  ! alpha1
       tmp2 = 2.*pi*Freq(nfre)               ! omgn
       tmp2 = tmp2*tmp2*H_gen/grav       ! tb
       tmp3 = 1.0_SP + tmp2*(-0.39_SP)    ! tc
    
       tmp1 = SQRT((tmp3-SQRT(tmp3*tmp3-4.0_SP*tmp1*tmp2))  &
                /(2.0_SP*tmp1))/MAX(SMALL,H_gen)     ! wkn 
   
     IF(Dire(ndir).NE.ZERO)THEN 
      IF(Dire(ndir).GT.ZERO)THEN   
       tmp3=ZERO
       I=0
       Do WHILE (tmp3<Dire(ndir))
         I=I+1

         tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)     ! rlamda



         IF(tmp2.GE.tmp1)THEN
          tmp3=pi*0.5_SP-SMALL
         ELSE
           tmp3=ASIN(tmp2/tmp1)   ! theta, based on rlamda=wkn*sin(theta)
         ENDIF
         IF(I>1000)THEN
           tmp3=pi*0.5_SP-SMALL
         ENDIF
       ENDDO

! judge between I-1 and I which is closer

         angle1=ASIN((I-1)*2.0_SP*pi/DY/(Nglob-1.0_SP)/tmp1)



         if (abs(angle1-Dire(ndir))<abs(Dire(ndir)-tmp3))then
            angle2=angle1
         else
            angle2=tmp3
         endif

      ELSEIF(Dire(ndir).LT.ZERO)THEN
       tmp3=ZERO
       I=0
       Do WHILE (tmp3>Dire(ndir))
         I=I+1

         tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)     ! rlamda



         IF(tmp2.GE.tmp1)THEN
          tmp3=-pi*0.5_SP+SMALL
         ELSE
           tmp3=-ASIN(tmp2/tmp1)   ! theta, based on rlamda=wkn*sin(theta)
         ENDIF
         IF(I>1000)THEN
           tmp3=-pi*0.5_SP+SMALL
         ENDIF
       ENDDO

! judge between I-1 and I which is closer

         angle1=-ASIN((I-1)*2.0_SP*pi/DY/(Nglob-1.0_SP)/tmp1)




         if (abs(angle1-Dire(ndir))<abs(Dire(ndir)-tmp3))then
            angle2=angle1
         else
            angle2=tmp3
         endif
      ENDIF 



       Dir2D(nfre,ndir) = angle2

    ENDIF ! end theta .ne.zero

    ENDDO
    ENDDO

    ELSE ! no periodic

       DO ndir=1,NumDir
       DO nfre=1,NumFreq
         Dir2D(nfre,ndir)=Dire(ndir)
       ENDDO
       ENDDO

    ENDIF ! end periodic

! *************
        alpha=-0.39_SP
        alpha1=alpha+1.0_SP/3.0_SP

      DO ndir=1,NumDir
       DO nfre=1,NumFreq

        theta = Dir2D(nfre,ndir) 

        omgn=2.*pi*Freq(nfre)
        Tperiod = 1.0_SP/Freq(nfre)
        AMP_WK = WAVE_COMP(nfre,ndir)

        tb=omgn*omgn*h_gen/grav
        tc=1.+tb*alpha
        IF(h_gen==ZERO.OR.Tperiod==ZERO)THEN
         WRITE(*,*)'re-set depth, Tperiod for wavemaker, STOP!'
         STOP
        ELSE
          wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))  &
                /(2.0_SP*alpha1))/h_gen
          C_phase=1./wkn/Tperiod*2.*pi
        ENDIF
        wave_length=C_phase*Tperiod

        rlamda(nfre,ndir)=wkn*sin(theta)
        beta_gen(nfre,ndir)=80.0_SP/delta**2/wave_length**2
        rl_gen=wkn*cos(theta)
        rI=SQRT(3.14159/beta_gen(nfre,ndir))*exp(-rl_gen**2/4./beta_gen(nfre,ndir))

        D_gen(nfre,ndir)=2.0_SP*AMP_WK  &
            *cos(theta)*(omgn**2-alpha1*grav*wkn**4*h_gen**3) &
            /(omgn*wkn*rI*(1.0_SP-alpha*(wkn*h_gen)**2))

         ENDDO
       ENDDO

! calculate width
        omgn_tmp=2.0_SP*pi/PeakPeriod
        tb=omgn_tmp*omgn_tmp*h_gen/grav
        tc=1.0_SP+tb*alpha
        wkn=SQRT((tc-SQRT(tc*tc-4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen
        C_phase=1.0_SP/wkn/PeakPeriod*2.0_SP*pi
        wave_length=C_phase*PeakPeriod
        width=delta*wave_length/2.0_SP

END SUBROUTINE WK_WAVEMAKER_2D_SPECTRAL_DATA
! now include spherical # endif



! --------------------------------------------------
!    This is subroutine to provide initial N wave solution 
!    it can be specified in input.txt giving 'N_WAVE'
!    called by
!       - INITIAL_N_WAVE
!    Last Update: 11/30/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
!CALL INITIAL_N_WAVE(Mloc,Nloc, DX,x1_Nwave,& 
!          x2_Nwave,a0_Nwave,gamma_Nwave,dep_Nwave,U,V,Eta)
SUBROUTINE INITIAL_N_WAVE(M,N,DX,x1,x2,Ha,gamma_s,dep,U,V,Eta)

     USE GLOBAL, only: npx,px,Mglob

     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N
     REAL(SP),INTENT(IN) :: x1,x2,Ha,gamma_s,dep
     REAL(SP) :: a0,gamma

     REAL(SP),INTENT(IN) :: DX



     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: U,V,Eta
     IF(dep==ZERO)THEN
      !WRITE(*,*)'depth for Nwave should not be 0, stop!'
      STOP
     ENDIF
! recalculate a0 and gamma based on Synolakis etal (A15)
! note non-dimensional formula in Tadepalli and Synolakis we use dimensional one
! like in xi zhao etal icce 2010
     a0=3.0_SP*SQRT(3.0_SP)*Ha/2.0_SP
     gamma=3.0_SP*0.5_SP/dep*SQRT(SQRT(3.0_SP/4.0_SP)*Ha/dep)
!print*,gamma

     DO J=1,N
     DO I=1,M


! start from ghost cell
       tmp1=(npx*Mglob/px+I-1)*DX

! general n-wave
!       Eta(I,J)=a0*(tmp1-x2)/COSH(gamma*(tmp1-x1))
! isosceles n-wave
       Eta(I,J)=a0*COSH(gamma*(tmp1-x1))**(-2)*tanh(gamma*(tmp1-x1))
       U(I,J) = SQRT(GRAV/dep)*Eta(I,J)
       V(I,J) = ZERO
     ENDDO
     ENDDO


END SUBROUTINE INITIAL_N_WAVE



! --------------------------------------------------
!    This is subroutine to provide initial solitary wave solution 
!    it can be specified in input.txt giving 'SOL'
!    called by
!       - INITIAL_WAVE
!    call
!       - SOLITARY_SOLUTION
!    Last Update: 10/01/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE INITIAL_SOLITARY_WAVE(M,N,DX,Xwavemaker,&
           AMP_Soli,Dep_Soli,Beta,U,V,Eta)
     USE PARAM

     USE GLOBAL, only: npx,px,Mglob

     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N

     REAL(SP),INTENT(IN) :: DX 



     REAL(SP),INTENT(IN) :: Dep_Soli,Xwavemaker,Beta
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: U,V,Eta
     REAL(SP),INTENT(IN) :: AMP_Soli
     REAL(SP) :: A,B,A1,A2,alpha,SC,Cph,C_ph

!     alpha=0.5_SP*Beta*Beta+Beta
   ! something wrong here. if beta=-0.531, then alpha should be -0.39
   ! the solitary wave shape does not match the equation. but alpha = -0.10 seems OK
     alpha=-0.39

      CALL SUB_SLTRY(AMP_Soli,Dep_Soli,alpha,Cph,B,A1,A2,A,C_ph)


!     A=(C_ph*C_ph-GRAV*Dep_Soli)/C_ph

!     B=4.0_SP*(alpha+1.0_SP/3.0_SP)*GRAV*Dep_Soli*Dep_Soli*Dep_Soli &
!        -alpha*Dep_Soli*Dep_Soli*C_ph*C_ph

!     B=SQRT((C_ph*C_ph-GRAV*Dep_Soli)/B)
!     A1=3.0_SP*((alpha+1.0_SP/3.0_SP)*GRAV*Dep_Soli-alpha*C_ph*C_ph)

!     A1=(C_ph*C_ph-GRAV*Dep_Soli)/A1*Dep_Soli
!     A2=2.0_SP*GRAV*Dep_SOli*C_ph*C_ph  &
!        *((alpha+1.0_SP/3.0_SP)*GRAV*Dep_Soli-alpha*C_ph*C_ph)

!     A2=-(C_ph*C_ph-GRAV*Dep_Soli)*(C_ph*C_ph-GRAV*Dep_Soli) &
!         *((alpha+1.0_SP/3.0_SP)*GRAV*Dep_Soli+2.0_SP*alpha*C_ph*C_ph)*Dep_Soli/A2

     DO J=1,N
     DO I=1,M


! start from ghost cell
       SC=1.0_SP/COSH(B*(npx*Mglob/px+I-Xwavemaker/DX-1)*DX)

       Eta(I,J)=A1*SC*SC+A2*SC*SC*SC*SC
       U(I,J)=A*SC*SC
       V(I,J)=ZERO
     ENDDO
     ENDDO


END SUBROUTINE INITIAL_SOLITARY_WAVE



! --------------------------------------------------
!    This is subroutine to provide solitary wave solution of Nogu equations
!    called by
!       - INITIAL_SOLITARY_WAVE
!    Last Update: 10/05/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE SUB_SLTRY(a0, h1, alp, r1, bue, ae1, ae2, au, C_ph)
    USE PARAM
    IMPLICIT NONE
    REAL(SP), INTENT(IN) :: a0,h1,alp
    REAL(SP), INTENT(OUT) :: r1,bue,ae1,ae2,au,C_ph
    REAL(SP) :: alp2,p,q,r,x,fx, fpx,rx,cph,eps,zr,g
    INTEGER :: ite

!
!--------coefficients for third order polynomial equations
!               " x**3+p*x**2+q*x+r=0  "

         alp2 = alp + 1.0_SP/3.0_SP
         eps  = a0/h1
         g=GRAV

         p = -(alp2+2.0_SP*alp*(1.0_SP+eps))/(2.0_SP*alp)
         q = eps*alp2/alp
         r = alp2/(2.0_SP*alp)

!--------Newton-Rapson's method to solve x ( >1 )for the above equation
         ite = 0
         x=1.2_SP
1        fx  = r+x*(q+x*(p+x))
         fpx = q+x*(2.0_SP*p+3.0_SP*x)
         x = x-fx/fpx

         ite = ite+1
         if (ite.gt.10) then
            !write(*,*) 'no solitary wave solution (check eps = a0/h1)'
            stop
         endif
         if (abs(fx).ge.1e-5) goto 1
         rx = sqrt(x)
         cph = sqrt(g*h1)
         r1 = rx*cph
         C_ph=rx*cph

         !write (*,*) rx, r1


!---------coefficients for solitary solutions :
!         "   u =  au/(cosh(bue*xi))**2    "
!         "  et = ae1/(cosh(bue*xi)**2+ae2/(cosh(bue*xi)**4   "

          au =  (x-1.0_SP)/(eps*rx)*cph*eps
         bue =  sqrt((x-1.0_SP)/(4.0_SP*(alp2-alp*x)))/h1
         ae1 =  (x-1.0_SP)/(eps*3.0_SP*(alp2-alp*x))*a0
         ae2 = -(x-1.0_SP)/(2.0_SP*eps)*(x-1.0_SP)*(2.0_SP*alp*x+alp2) &
                                      /(x*(alp2-alp*x))*a0
         zr  = bue*rx*cph

END SUBROUTINE SUB_SLTRY



! --------------------------------------------------
!    This is subroutine to distribute global variable into local variable 
!    called by
!       - INITIALIZATION
!    Last Update: 09/24/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DISTRIBUTE_VarGlob (VarGlob,PHI)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(MGlob,NGlob),INTENT(IN) :: VarGlob
     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT) :: PHI

! TEMP

     if (myid.eq.0) then
        DO J=Nghost+1,NGlob+NGhost
         DO I=Nghost+1,MGlob+Nghost
           PHIGLOB(I,J) = VarGlob(I-Nghost,J-Nghost)
         ENDDO
        ENDDO
! ghost cells
        DO I=Nghost+1,MGlob+Nghost
           DO J=1,Nghost
              PHIGLOB(I,J)=PHIGLOB(I,Nghost+1)
           ENDDO
           DO J=NGlob+Nghost+1,NGlob+2*Nghost
              PHIGLOB(I,J)=PHIGLOB(I,NGlob+Nghost)
           ENDDO
        ENDDO
        DO J=1,NGlob+2*Nghost
           DO I=1,Nghost
              PHIGLOB(I,J)=PHIGLOB(Nghost+1,J)
           ENDDO
           DO I=MGlob+Nghost+1,MGlob+2*Nghost
              PHIGLOB(I,J)=PHIGLOB(MGlob+Nghost,J)
           ENDDO
        ENDDO
     endif

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

     do i=1,Mloc
     do j=1,Nloc
        if (myid.eq.0) then
           do l=1,px*py
              xx(l) = PHIGLOB(i+npxs(l)*(Iend-Ibeg+1),&
                   j+npys(l)*(Jend-Jbeg+1))
           enddo
        endif
        call MPI_Scatter(xx,1,MPI_SP,&
             PHI(i,j),1,MPI_SP,0,MPI_COMM_WORLD,ier)
     enddo
     enddo


END SUBROUTINE DISTRIBUTE_VarGlob


