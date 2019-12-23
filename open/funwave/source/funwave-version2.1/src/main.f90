Program FUNWAVE_TVD
!-------------------------------------------------------
!    VERSION 2.1
!    Last Update: 09/26/2013 Babak Tehranirad, University of Delaware
!
!    MAIN - READ_INPUT
!         - INDEX
!         - ALLOCATE_VARIABLES
!         - INITIALIZATION 
!         DO LOOP
!             - VARIABLE_UPDATING
!             - EXCHANGE
!             - ESTIMATE_DT
!                PRE/COR                 LUNGE-KUTTA
!                                      -DISPERSION
!             - FLUXES                 -FLUXES
!             - SourceTerms            -SourceTerms
!             - PREDICTOR              -ESTIMATE_HUV
!             - EXCHANGE               -EXCHANGE
!             - FLUXES                 
!             - SourceTerms again
!             - CORRECTOR
!
!             - STATISTICS
!             - PREVIEW
!          ENDDO LOOP
!-----------------------------------------------------------------
! ** OPEN FILES **
!  (1): read input, (2): output, (3): log, (4): !write/read hotstart
!-----------------------------------------------------------------
! ** HOT START DATA **
!   NOTE: read input.txt first, if HOT_START, then read  
        ! -- dimension
! Mloc,Nloc,Mloc1,Nloc1
! Nghost
! Ibeg,Iend,Jbeg,Jend,Iend1,Jend1
!   NOTE: need to confirm if the saved data is consistent with input.txt
        ! -- time
! TIME
! TOTAL_TIME
! PLOT_INTV
! PLOT_COUNT
! SCREEN_INTV
! SCREEN_COUNT
! HOTSTART_INTV
! ICOUNT
        ! spacing
! DX,DY
        ! -- physics
! DISPERSION
! Gamma1
! a1,a2,b1,b2
! SWE_ETA_DEP
        ! -- numerics
! Time_Scheme
! HIGH_ORDER
! CONSTR
! CFL
! FroudeCap
! DISP_TIME_LEFT
        ! -- wet-dry
! MinDepth,MinDepthfrc

        ! -- depth
! DEPTH
! DEPTHx
! DEPTHy
        ! variables
! U
! V
! if (.NOT.DISP_TIME_LEFT)THEN
! U0
! V0
! endif
! Ubar
! Vbar
! ETA 
! H
! MASK
! MASK9
! MAST_STRUC
!
       ! -- wavemaker
! if (WAVEMAKER is WK_IRR)
! turns out the data for Cm Sm too large, calculate it when hotstart
!
! if (WAVEMAKER is WK_REG)
! D_gen
! Beta_gen
! rlamda
! 
!
! ----------------------------------------------------------------
     USE GLOBAL
     IMPLICIT NONE

     INTEGER::ISTAGE
     REAL(SP) :: tbegin,tend
     REAL(SP) :: maxv


     call MPI_INIT ( ier )


     CALL READ_INPUT

     CALL INDEX

! allocate variables
     CALL ALLOCATE_VARIABLES

     CALL INITIALIZATION
      





! time integration

     ! record wall time
     call WALL_TIME_SECS(tbegin)

   DO WHILE (TIME<TOTAL_TIME)


!      if (myid.eq.0) write (*,*) 'Current time is...', TIME
!      call flush() ! not good on darkstar



     IF(WaveMaker(1:7)=='LEF_SOL')THEN
       CALL SOLITARY_WAVE_LEFT_BOUNDARY
     ENDIF  


! update three variables
     Eta0=Eta
     Ubar0=Ubar
     Vbar0=Vbar  
   
     CALL UPDATE_MASK

     CALL EXCHANGE

! as Jeff pointed out there's a drop off when using onewaycoupling, it is caused by
! updates in exchange  




 

   ! broadcast dt

     CALL ESTIMATE_DT(Mloc,Nloc,DX,DY,U,V,H,MinDepthFrc,DT,CFL,TIME)

       IF(Time_Scheme(1:3)=='Pre')THEN
! 2nd-order predictor/corrector
 
       ! Source Terms






    
       CALL SourceTerms

      constr(1:3)='NON'
       CALL FLUXES
! predictor
     CALL PREDICTOR

     CALL EXCHANGE







     CALL SourceTerms
     
     constr(1:3)='HLL'
     CALL FLUXES

     CALL CORRECTOR

! end predictor/corrector
      ENDIF
  
!  Runge-Kutta Scheme 
    IF (TIME_SCHEME(1:3)=='Run')THEN
     ! 3-ORDER RUNGE-KUTTA TIME STEPPING
     DO ISTAGE=1,3



       IF(DISPERSION)THEN
         CALL Cal_Dispersion
       ENDIF





       CALL FLUXES







       CALL SourceTerms   ! put sourceterms after fluxes in order to get eta_t




       CALL ESTIMATE_HUV(ISTAGE)      
       
       CALL EXCHANGE
       



         






       IF(SPONGE_ON)THEN
         CALL SPONGE_DAMPING
       ENDIF

     ENDDO
    ENDIF
!   end Runge-Kutta Scheme
      

      DO J=1,Nloc
      DO I=1,Mloc
       IF(OUT_Hmax)THEN
        IF(MASK(I,J).GT.0)THEN
        IF(Eta(I,J).GT.HeightMax(I,J)) HeightMax(I,J)=Eta(I,J)
        ENDIF
       ENDIF

       IF(OUT_Hmin)THEN
        IF(MASK(I,J).GT.0)THEN
        IF(Eta(I,J).LT.HeightMin(I,J)) HeightMin(I,J)=Eta(I,J)
        ENDIF
       ENDIF

       IF(OUT_Umax)THEN
        IF(MASK(I,J).GT.0)THEN
          maxv=SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J))
          IF(maxV.GT.VelocityMax(I,J)) VelocityMax(I,J)=maxV
        ENDIF
       ENDIF

       IF(OUT_MFmax)THEN
        IF(MASK(I,J).GT.0)THEN
          maxv=(U(I,J)*U(I,J)+V(I,J)*V(I,J))*(H(I,J))
          IF(maxv.GT.MomentumFluxMax(I,J)) MomentumFluxMax(I,J)=maxv
        ENDIF
       ENDIF


      ENDDO
      ENDDO
   

      IF(OUT_VORmax) THEN
       CALL phi_exch(VorticityMax)
      ENDIF

   

     SCREEN_COUNT=SCREEN_COUNT+DT

     IF(SCREEN_COUNT>=SCREEN_INTV)THEN
      SCREEN_COUNT=SCREEN_COUNT-SCREEN_INTV
      CALL STATISTICS
     ENDIF


! calculate mean for smagorinsky's mixing and wave height
      CALL CALCULATE_MEAN(T_INTV_mean,T_sum,DT,Mloc,Nloc,U,V,ETA,ETA0,&
           Umean,Vmean,ETAmean,Usum,Vsum,ETAsum,WaveHeightRMS, &
           WaveHeightAve,Emax,Emin,Num_Zero_Up,Ibeg,Iend,Jbeg,Jend, &
           HrmsSum,HavgSum)


! show breaking

      IF(SHOW_BREAKING)THEN
        CALL BREAKING(Mloc,Nloc,ETAx,ETAy,ETAT,Cbrk1,Cbrk2,H,MinDepthFrc,DT,&
               DX,DY,T_brk,AGE_BREAKING)
      ENDIF

! stations
      IF(NumberStations>0)THEN
      PLOT_COUNT_STATION=PLOT_COUNT_STATION+DT
      IF(PLOT_COUNT_STATION>=PLOT_INTV_STATION)THEN
       PLOT_COUNT_STATION=PLOT_COUNT_STATION-PLOT_INTV_STATION
       CALL STATIONS
      ENDIF
      ENDIF
! preview
      PLOT_COUNT=PLOT_COUNT+DT
      IF(PLOT_COUNT>=PLOT_INTV)THEN
       PLOT_COUNT=PLOT_COUNT-PLOT_INTV
       CALL PREVIEW
      ENDIF
  
   END DO





     ! record wall time at the end
     call WALL_TIME_SECS(tend)


     if(myid.eq.0) write(*,*) 'Simulation takes',tend-tbegin,'seconds'
     if(myid.eq.0) write(3,*) 'Simulation takes',tend-tbegin,'seconds'
     if (myid.eq.0) WRITE(*,*)'Normal Termination!'
     if (myid.eq.0) WRITE(3,*)'Normal Termination!'








     call MPI_FINALIZE ( ier )


END PROGRAM FUNWAVE_TVD


! --------------------------------------------------
!    This is subroutine to calculate mean u v required by 
!    smagorinsky mixing and wave height
!    mean eta is also calculated.
!    called by
!       MAIN
!    
!    Last Update: 05/02/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
      SUBROUTINE CALCULATE_MEAN(T_INTV_mean,T_sum,DT,M,N,U,V,ETA,ETA0,&
                Umean,Vmean,ETAmean,Usum,Vsum,ETAsum,&
                WaveHeightRMS, &
                WaveHeightAve,Emax,Emin,Num_Zero_Up,Ibeg,Iend,Jbeg,Jend, &
                HrmsSum,HavgSum)
! calculate mean for smagorinsky's mixing and wave height

      USE PARAM
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
      REAL(SP),DIMENSION(M,N),INTENT(IN)::U,V,ETA,ETA0
      REAL(SP),INTENT(IN) :: T_INTV_mean,DT
      REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Umean,Vmean
      REAL(SP),DIMENSION(M,N),INTENT(OUT) :: WaveHeightRMS,WaveHeightAve
      REAL(SP),DIMENSION(M,N),INTENT(INOUT) ::ETAmean
      REAL(SP),DIMENSION(M,N),INTENT(INOUT) :: Usum,Vsum,ETAsum
      REAL(SP),DIMENSION(M,N),INTENT(INOUT) :: HrmsSum,HavgSum
      REAL(SP),INTENT(OUT) :: T_sum
      REAL(SP)::Tmpe,Tmp_0
      REAL(SP),DIMENSION(M,N),INTENT(INOUT) :: Emax,Emin
      INTEGER,DIMENSION(M,N),INTENT(INOUT) :: Num_Zero_Up
      
      T_sum=T_sum+DT
      IF(T_sum.GE.T_INTV_mean)THEN
        Usum=U*DT+Usum
        Vsum=V*DT+Vsum
        ETAsum=ETA*DT+ETAsum
        Umean=Usum/T_sum
        Vmean=Vsum/T_sum
        ETAmean=ETAsum/T_sum
        T_sum=T_sum-T_INTV_mean
        Usum=ZERO
        Vsum=ZERO
        ETAsum=ZERO

! wave height
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
        IF(Num_Zero_Up(I,J)>=2)THEN
          WaveHeightAve(I,J)=HavgSum(I,J)/Num_Zero_Up(I,J)
          WaveHeightRMS(I,J)=SQRT(HrmsSum(I,J)/Num_Zero_Up(I,J))
        ENDIF
!        Num_Zero_Up(I,J)=0
!        HavgSum(I,J)=ZERO
!        HrmsSum(I,J)=ZERO
       ENDDO
       ENDDO

      ELSE

        Usum=U*DT+Usum
        Vsum=V*DT+Vsum
        ETAsum=ETA*DT+ETAsum

! wave height
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
         if(Eta(i,j)>Emax(i,j)) Emax(i,j) = Eta(i,j)
         if(Eta(i,j)<Emin(i,j)) Emin(i,j) = Eta(i,j)
         Tmpe = Eta(i,j)-ETAmean(i,j)
         Tmp_0 = Eta0(i,j)-ETAmean(i,j)
         if(Tmpe>Tmp_0.and.Tmpe*Tmp_0<=Zero) then
           Num_Zero_Up(i,j) = Num_Zero_Up(i,j)+1
           if(Num_Zero_Up(i,j)>=2) then
               HavgSum(i,j) = HavgSum(i,j)+Emax(i,j)-Emin(i,j)
               HrmsSum(i,j) = HrmsSum(i,j)+(Emax(i,j)-Emin(i,j))**2
           endif
           ! reset Emax and Emin to find next wave
           Emax(i,j) = -1000.
           Emin(i,j) = 1000.
         endif  
       ENDDO
       ENDDO

      ENDIF  ! end average time

      END SUBROUTINE CALCULATE_MEAN




! --------------------------------------------------
!    This is subroutine to show wave breaking 
!    breaking is actually automatically calculated using
!    shock wave capturing scheme, this subroutine is only for
!    demonstration or calculating bubbles or foam 
!    called by
!       MAIN
!    
!    Last Update: 11/22/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------

SUBROUTINE BREAKING(M,N,ETAx,ETAy,ETAT,Cbrk1,Cbrk2,H,MinDepthFrc,&
               DT,DX,DY,T_brk,AGE)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN)::M,N
     REAL(SP),INTENT(IN)::Cbrk1,Cbrk2,MinDepthFrc,DT,T_brk

     REAL(SP),INTENT(IN)::DX,DY



     REAL(SP),DIMENSION(M,N),INTENT(IN)::ETAx,ETAy,ETAt,H
     REAL(SP),DIMENSION(M,N),INTENT(OUT)::AGE
     REAL(SP)::C,Angle,AGE1,AGE2,AGE3,propx,propy,propxy

     DO J=1,N
     DO I=1,M

     tmp3=SQRT(GRAV*MAX(MinDepthFrc,H(I,J)))
     tmp1=Cbrk1*tmp3
     IF(ETAt(I,J).GE.tmp1.AND.(  &
       AGE(I,J).EQ.ZERO.OR.AGE(I,J).GT.T_brk))THEN
      AGE(I,J)=DT
     ELSE
      IF(AGE(I,J).GT.ZERO)THEN
        AGE(I,J)=AGE(I,J)+DT
      ELSE
        tmp1=MAX(SQRT(ETAx(I,J)*ETAx(I,J)+ETAy(I,J)*ETAy(I,J)),SMALL)
        C=MIN(ABS(ETAt(I,J))/tmp1,SQRT(GRAV*ABS(H(I,J))))
! propagation time between a dx, dy and ds

        propxy=SQRT(DX*DX+DY*DY)/MAX(C,SMALL)
        propx=SQRT(DX*DX)/MAX(C,SMALL)
        propy=SQRT(DY*DY)/MAX(C,SMALL)





        ANGLE=ATAN2(ETAy(I,J),ETAx(I,J))
        tmp2=Cbrk2*tmp3

        IF(ETAt(I,J).GE.tmp2)THEN
! 4 quadrants 
! quadrant 1
         IF(ANGLE.GE.ZERO.AND.ANGLE.LT.90.0_SP)THEN
           AGE1=AGE(I-1,J)
           AGE2=AGE(I-1,J-1)
           AGE3=AGE(I,J-1)
           IF((AGE1>=DT.AND.AGE1>propx).OR.&
              (AGE2>=DT.AND.AGE2>propxy).OR.&
              (AGE3>=DT.AND.AGE3>propy))THEN
            AGE(I,J)=DT
           ENDIF         
         ENDIF
! quadrant 2
         IF(ANGLE.GE.90.0_SP.AND.ANGLE.LT.180.0_SP)THEN
           AGE1=AGE(I+1,J)
           AGE2=AGE(I+1,J-1)
           AGE3=AGE(I,J-1)
           IF((AGE1>=DT.AND.AGE1>propx).OR.&
              (AGE2>=DT.AND.AGE2>propxy).OR.&
              (AGE3>=DT.AND.AGE3>propy))THEN
            AGE(I,J)=DT
           ENDIF         
         ENDIF
! quadrant 3
         IF(ANGLE.GE.-180.0_SP.AND.ANGLE.LT.-90.0_SP)THEN
           AGE1=AGE(I+1,J)
           AGE2=AGE(I+1,J+1)
           AGE3=AGE(I,J+1)
           IF((AGE1>=DT.AND.AGE1>propx).OR.&
              (AGE2>=DT.AND.AGE2>propxy).OR.&
              (AGE3>=DT.AND.AGE3>propy))THEN
            AGE(I,J)=DT
           ENDIF         
         ENDIF
! quadrant 4
         IF(ANGLE.GE.-90.0_SP.AND.ANGLE.LT.0.0_SP)THEN
           AGE1=AGE(I,J+1)
           AGE2=AGE(I-1,J+1)
           AGE3=AGE(I-1,J)
           IF((AGE1>=DT.AND.AGE1>propy).OR.&
              (AGE2>=DT.AND.AGE2>propxy).OR.&
              (AGE3>=DT.AND.AGE3>propx))THEN
            AGE(I,J)=DT
           ENDIF         
         ENDIF

       ENDIF

      ENDIF
     ENDIF 
     ENDDO
     ENDDO

END SUBROUTINE BREAKING
! --------------------------------------------------
!    This is subroutine to damp waves using DHI type sponge layer 
!    variables
!    called by
!       MAIN
!    
!    Last Update: 10/27/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE SPONGE_DAMPING
     USE GLOBAL
     IMPLICIT NONE

     DO J=1,Nloc
     DO I=1,Mloc
      IF(MASK(I,J)>ZERO)THEN
       ETA(I,J)=ETA(I,J)/SPONGE(I,J)
      ENDIF
       U(I,J)=U(I,J)/SPONGE(I,J)
       V(I,J)=V(I,J)/SPONGE(I,J)
     ENDDO
     ENDDO

END SUBROUTINE SPONGE_DAMPING

! --------------------------------------------------
!    This is subroutine to calculation dispersion terms
!    so far V^4 and V^1
!    called by
!       MAIN
!    call DERIVATIVE_XX
!         DERIVATIVE_XY
!    
!    Last Update: 09/26/2013 Babak Tehranirad, University of Delaware
!    Fengyan Shi change derivative_xx_high to second order
!    according to Harris' suggestion
! --------------------------------------------------
SUBROUTINE CAL_DISPERSION
     USE GLOBAL
     IMPLICIT NONE

     REAL(SP),Dimension(Mloc,Nloc) :: DU,DV,DUt,DVt
     REAL(SP) :: UxxVxy,UxyVyy,HUxxHVxy,HUxyHVyy, &
                 UxxVxy_x,UxxVxy_y,UxyVyy_x,UxyVyy_y, &
                 HUxxHVxy_x,HUxxHVxy_y,HUxyHVyy_x,HUxyHVyy_y, &
                 rh,rhx,rhy,reta,ken1,ken2,ken3,ken4,ken5

     REAL(SP) :: omega_0,omega_1
     REAL(SP),Dimension(Mloc,Nloc) :: omega            

    
! uxx
    CALL DERIVATIVE_XX(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,U,Uxx)
! uxy
    CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,U,Uxy)
! vxy
    CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,V,Vxy)
! vyy
    CALL DERIVATIVE_YY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,V,Vyy)


! gamma2.ne.0
    IF(Gamma2>ZERO)THEN
     CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,U,Ux)
     CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,V,Vx)
     CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,U,Uy)
     CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,V,Vy)
     CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,Eta,ETAx)
     CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,Eta,ETAy)
    ELSEIF(SHOW_BREAKING)THEN



     CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,Eta,ETAx)
     CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,Eta,ETAy)
    ENDIF

! DU DV
     DO J=1,Nloc
     DO I=1,Mloc
       DU(I,J)=Max(Depth(I,J),MinDepthFrc)*U(I,J)
       DV(I,J)=Max(Depth(I,J),MinDepthFrc)*V(I,J)
     ENDDO
     ENDDO 
! ETAT

    IF(Gamma2>ZERO)THEN
     IF(DISP_TIME_LEFT)THEN
       DO J=1,Nloc
       DO I=1,Mloc
         ETAT(I,J)=-(P(I+1,J)-P(I,J))/DX-(Q(I,J+1)-Q(I,J))/DY
       ENDDO
       ENDDO
      ELSE
       IF(SHOW_BREAKING)THEN
         DO J=1,Nloc
         DO I=1,Mloc
           ETAT(I,J)=-(P(I+1,J)-P(I,J))/DX-(Q(I,J+1)-Q(I,J))/DY
         ENDDO
         ENDDO         
       ENDIF
! as pointed by Steve Brandt, Ut and Vt should be evaluated outside of RK loop.
! Because it is really small term, I temporarily keep this form but need more tests
! to see if or not affect results. It is also important for hot start
       DO J=1,Nloc
       DO I=1,Mloc
        Ut(I,J)=(U(I,J)-U0(I,J))/DT/3.0_SP
        Vt(I,J)=(V(I,J)-V0(I,J))/DT/3.0_SP
        DUt(I,J)=Max(Depth(I,J),MinDepthFrc)*Ut(I,J)
        DVt(I,J)=Max(Depth(I,J),MinDepthFrc)*Vt(I,J)
       ENDDO
       ENDDO
     ENDIF
    ELSEIF(SHOW_BREAKING)THEN



       DO J=1,Nloc
       DO I=1,Mloc

         ETAT(I,J)=-(P(I+1,J)-P(I,J))/DX-(Q(I,J+1)-Q(I,J))/DY



       ENDDO
       ENDDO
    ENDIF

! DUxx
    CALL DERIVATIVE_XX(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DU,DUxx)
! DUxy
    CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,DU,DUxy)
! DVxy
    CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,DV,DVxy)
! DVyy
    CALL DERIVATIVE_YY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,DV,DVyy)
      

    IF(Gamma2>ZERO)THEN
     CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DU,DUx)
     CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DV,DVx)
     CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,DU,DUy)
     CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,DV,DVy)
     IF(DISP_TIME_LEFT)THEN
       CALL DERIVATIVE_X_High(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,ETAT,ETATx)
       CALL DERIVATIVE_Y_High(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,ETAT,ETATy)
     ELSE
       CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,Ut,Utx)
       CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,Vt,Vty)

       CALL DERIVATIVE_XX(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,Ut,Utxx)
       CALL DERIVATIVE_YY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,Vt,Vtyy)
       CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,Ut,Utxy)
       CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,Vt,Vtxy)

       CALL DERIVATIVE_X(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DUt,DUtx)
       CALL DERIVATIVE_Y(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,DVt,DVty)

       CALL DERIVATIVE_XX(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DUt,DUtxx)
       CALL DERIVATIVE_YY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DY,DVt,DVtyy)
       CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,DUt,DUtxy)
       CALL DERIVATIVE_XY(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,MASK9,DX,DY,DVt,DVtxy)
     ENDIF
    ENDIF

      
! this may affect parallel version
! I added coupling boundary 10/14/2012

!  boundary conditions

    if(n_west.eq.MPI_PROC_NULL) then




     DO J=1,Nloc
       Uxy(Ibeg,J)=ZERO
       DUxy(Ibeg,J)=ZERO
       Vxy(Ibeg,J)=ZERO
       DVxy(Ibeg,J)=ZERO
      IF(DISP_TIME_LEFT)THEN
      ELSE
       Utxy(Ibeg,J)=ZERO
       DUtxy(Ibeg,J)=ZERO
       Vtxy(Ibeg,J)=ZERO
       DVtxy(Ibeg,J)=ZERO
      ENDIF
     ENDDO



  

    endif  



    if(n_east.eq.MPI_PROC_NULL) then




     DO J=1,Nloc
       Uxy(Iend,J)=ZERO
       DUxy(Iend,J)=ZERO
       Vxy(Iend,J)=ZERO
       DVxy(Iend,J)=ZERO
      IF(DISP_TIME_LEFT)THEN
      ELSE
       Utxy(Iend,J)=ZERO
       DUtxy(Iend,J)=ZERO
       Vtxy(Iend,J)=ZERO
       DVtxy(Iend,J)=ZERO
      ENDIF
     ENDDO 




    endif  

  

    if(n_suth.eq.MPI_PROC_NULL) then




     DO I=1,Mloc
       Uxy(I,Jbeg)=ZERO
       DUxy(I,Jbeg)=ZERO
       Vxy(I,Jbeg)=ZERO
       DVxy(I,Jbeg)=ZERO

      IF(DISP_TIME_LEFT)THEN
      ELSE
       Utxy(I,Jbeg)=ZERO
       DUtxy(I,Jbeg)=ZERO
       Vtxy(I,Jbeg)=ZERO
       DVtxy(I,Jbeg)=ZERO
      ENDIF
     ENDDO   




    endif  



    if(n_nrth.eq.MPI_PROC_NULL) then




     DO I=1,Mloc
       Uxy(I,Jend)=ZERO
       DUxy(I,Jend)=ZERO
       Vxy(I,Jend)=ZERO
       DVxy(I,Jend)=ZERO
      IF(DISP_TIME_LEFT)THEN
      ELSE
       Utxy(I,Jend)=ZERO
       DUtxy(I,Jend)=ZERO
       Vtxy(I,Jend)=ZERO
       DVtxy(I,Jend)=ZERO
      ENDIF
     ENDDO 




    endif  

     
    CALL EXCHANGE_DISPERSION
     
! calculate V1p  without nonlinear dispersion
     DO J=1,Nloc
     DO I=1,Mloc







!  Kennedy et al. 2001
       U4(I,J)=(1.0_SP/3.0_SP-Beta_1+0.5_SP*Beta_1*Beta_1)*DEPTH(I,J)*DEPTH(I,J)*(Uxx(I,J)+Vxy(I,J)) &
                +(Beta_1-1.0_SP/2.0_SP)*DEPTH(I,J)*(DUxx(I,J)+DVxy(I,J))
       V4(I,J)=(1.0_SP/3.0_SP-Beta_1+0.5_SP*Beta_1*Beta_1)*DEPTH(I,J)*DEPTH(I,J)*(Uxy(I,J)+Vyy(I,J)) &
                +(Beta_1-1.0_SP/2.0_SP)*DEPTH(I,J)*(DUxy(I,J)+DVyy(I,J))      

















               

       U1p(I,J)=0.5_SP*(1.0_SP-Beta_1)  &
                *DEPTH(I,J)*DEPTH(I,J)  &
                *(Uxx(I,J)+Vxy(I,J)) &
               +(Beta_1-1.0_SP)*DEPTH(I,J)*(DUxx(I,J)+DVxy(I,J))
       V1p(I,J)=0.5_SP*(1.0_SP-Beta_1)  &
                *DEPTH(I,J)*DEPTH(I,J)  &
                *(Uxy(I,J)+Vyy(I,J)) &
               +(Beta_1-1.0_SP)*DEPTH(I,J)*(DUxy(I,J)+DVyy(I,J))



     ENDDO
     ENDDO


     IF(gamma2>ZERO)THEN
       DO J=Jbeg,Jend
       DO I=Ibeg,Iend
! 
        UxxVxy=Uxx(I,J)+Vxy(I,J)
        UxyVyy=Uxy(I,J)+Vyy(I,J)
        UxxVxy_x=(Uxx(I+1,J)+Vxy(I+1,J)-Uxx(I-1,J)-Vxy(I-1,J))/2.0_SP/DX
        UxxVxy_y=(Uxx(I,J+1)+Vxy(I,J+1)-Uxx(I,J-1)-Vxy(I,J-1))/2.0_SP/DY
        UxyVyy_x=(Uxy(I+1,J)+Vyy(I+1,J)-Uxy(I-1,J)-Vyy(I-1,J))/2.0_SP/DX
        UxyVyy_y=(Uxy(I,J+1)+Vyy(I,J+1)-Uxy(I,J-1)-Vyy(I,J-1))/2.0_SP/DY

        HUxxHVxy=DUxx(I,J)+DVxy(I,J)
        HUxyHVyy=DUxy(I,J)+DVyy(I,J)
        HUxxHVxy_x=(DUxx(I+1,J)+DVxy(I+1,J)-DUxx(I-1,J)-DVxy(I-1,J))/2.0_SP/DX
        HUxxHVxy_y=(DUxx(I,J+1)+DVxy(I,J+1)-DUxx(I,J-1)-DVxy(I,J-1))/2.0_SP/DY
        HUxyHVyy_x=(DUxy(I+1,J)+DVyy(I+1,J)-DUxy(I-1,J)-DVyy(I-1,J))/2.0_SP/DX
        HUxyHVyy_y=(DUxy(I,J+1)+DVyy(I,J+1)-DUxy(I,J-1)-DVyy(I,J-1))/2.0_SP/DY

        rh=Depth(I,J)
        rhx=(Depth(I+1,J)-Depth(I-1,J))/2.0_SP/DX
        rhy=(Depth(I,J+1)-Depth(I,J-1))/2.0_SP/DY
        reta=Eta(I,J)






! Kennedy et al. 2001
         ken1=(1.0_SP/6.0_SP-Beta_1+Beta_1*Beta_1)*rh*reta*Beta_2  &
                +(1.0_SP/2.0_SP*Beta_1*Beta_1-1.0_SP/6.0_SP)*reta*reta*Beta_2*Beta_2
         ken2=(Beta_1-1.0_SP/2.0_SP)*reta*Beta_2

        U4(I,J)=U4(I,J)+gamma2*MASK9(I,J)*(ken1*UxxVxy &
                +ken2*HUxxHVxy)
        V4(I,J)=V4(I,J)+gamma2*MASK9(I,J)*(ken1*UxyVyy &
                +ken2*HUxyHVyy)

       IF(DISP_TIME_LEFT)THEN
        U1p(I,J)=U1p(I,J)-gamma2*MASK9(I,J)*(   &
                 reta*Beta_2*ETAx(I,J)*Beta_2*(Ux(I,J)+Vy(I,J)) &
                 + reta*reta*Beta_2*Beta_2/2.0_SP &
                    *UxxVxy &
                 +ETAx(I,J)*Beta_2*(DUx(I,J)+DVy(I,J)) &
                 +reta*Beta_2*HUxxHVxy &
                  )
        V1p(I,J)=V1p(I,J)-gamma2*MASK9(I,J)*(   &
                 reta*Beta_2*ETAy(I,J)*Beta_2*(Ux(I,J)+Vy(I,J)) &
                 + reta*reta*Beta_2*Beta_2/2.0_SP &
                    *UxyVyy &
                 +ETAy(I,J)*Beta_2*(DUx(I,J)+DVy(I,J)) &
                 +reta*Beta_2*HUxyHVyy &
                  ) 
 
        U1pp(I,J)=(ETAx(I,J)*ETAT(I,J)*Beta_2*Beta_2+reta*Beta_2*ETATx(I,J)*Beta_2) &
                    *(Ux(I,J)+Vy(I,J)) &
                 +reta*Beta_2*ETAT(I,J)*Beta_2*UxxVxy &
                 +ETAx(I,J)*Beta_2*(DUx(I,J)+DVy(I,J)) &
                 +reta*Beta_2*HUxxHVxy

        V1pp(I,J)=(ETAy(I,J)*ETAT(I,J)*Beta_2*Beta_2+reta*Beta_2*ETATy(I,J)*Beta_2) &
                    *(Ux(I,J)+Vy(I,J)) &
                 +reta*Beta_2*ETAT(I,J)*Beta_2*UxyVyy &
                 +ETAy(I,J)*Beta_2*(DUx(I,J)+DVy(I,J)) &
                 +reta*Beta_2*HUxyHVyy

      ELSE







        U1pp(I,J)=-reta*Beta_2*ETAx(I,J)*Beta_2*(Utx(I,J)+Vty(I,J)) - 0.5_SP*reta*reta*Beta_2*Beta_2*(Utxx(I,J)+Vtxy(I,J))&
                  -ETAx(I,J)*Beta_2*(DUtx(I,J)+DVty(I,J)) -reta*Beta_2*(DUtxx(I,J)+DVtxy(I,J))

        V1pp(I,J)=-reta*Beta_2*ETAy(I,J)*Beta_2*(Utx(I,J)+Vty(I,J)) - 0.5_SP*reta*reta*Beta_2*Beta_2*(Utxy(I,J)+Vtyy(I,J))&
                  -ETAy(I,J)*Beta_2*(DUtx(I,J)+DVty(I,J)) -reta*Beta_2*(DUtxy(I,J)+DVtyy(I,J))

      ENDIF


!  Kennedy et al 2001
         ken1=(Beta_1-1.0_SP)*(rhx+ETAx(I,J))*Beta_2
         ken2=(Beta_1-1.0_SP)*(rh+reta)*Beta_2
         ken3=( (1.0_SP-Beta_1)*(1.0_SP-Beta_1)*rh*rhx*Beta_2*Beta_2-Beta_1*(1.0_SP-Beta_1)*(rhx*reta*Beta_2+rh*ETAx(I,J)*Beta_2) &
                    +(Beta_1*Beta_1-1.0_SP)*reta*ETAx(I,J)*Beta_2*Beta_2 )
         ken4=( 0.5_SP*(1.0_SP-Beta_1)*(1.0_SP-Beta_1)*rh*rh*Beta_2*Beta_2-Beta_1*(1.0_SP-Beta_1)*rh*reta*Beta_2 &
                      +0.5_SP*(Beta_1*Beta_1-1.0_SP)*reta*reta*Beta_2*Beta_2 )
         ken5=( (1.0_SP-Beta_1)*(1.0_SP-Beta_1)*rh*rhy*Beta_2*Beta_2-Beta_1*(1.0_SP-Beta_1)*(rhy*reta*Beta_2+rh*ETAy(I,J)*Beta_2) &
                    +(Beta_1*Beta_1-1.0_SP)*reta*ETAy(I,J)*Beta_2*Beta_2 )


        U2(I,J)=ken1*(U(I,J)*HUxxHVxy+V(I,J)*HUxyHVyy) &
                +ken2*(Ux(I,J)*HUxxHVxy+U(I,J)*HUxxHVxy_x &
                    +Vx(I,J)*HUxyHVyy+V(I,J)*HUxyHVyy_x) &
                +ken3 & 
                   *(U(I,J)*UxxVxy+V(I,J)*UxyVyy) &
                +ken4  &
                   *(Ux(I,J)*UxxVxy+U(I,J)*UxxVxy_x+Vx(I,J)*UxyVyy+V(I,J)*UxyVyy_x) &
                +Beta_2*Beta_2*(DUx(I,J)+DVy(I,J)+reta*Beta_2*(Ux(I,J)+Vy(I,J)))  &
                   *(HUxxHVxy+ETAx(I,J)*Beta_2*(Ux(I,J)+Vy(I,J))+reta*Beta_2*UxxVxy)

        V2(I,J)=ken1*(U(I,J)*HUxxHVxy+V(I,J)*HUxyHVyy) &
                +ken2*(Uy(I,J)*HUxxHVxy+U(I,J)*HUxxHVxy_y &
                    +Vy(I,J)*HUxyHVyy+V(I,J)*HUxyHVyy_y) &
                +ken5 & 
                   *(U(I,J)*UxxVxy+V(I,J)*UxyVyy) &
                +ken4  &
                   *(Uy(I,J)*UxxVxy+U(I,J)*UxxVxy_y+Vy(I,J)*UxyVyy+V(I,J)*UxyVyy_y) &
                +Beta_2*Beta_2*(DUx(I,J)+DVy(I,J)+reta*Beta_2*(Ux(I,J)+Vy(I,J)))  &
                   *(HUxyHVyy+ETAy(I,J)*Beta_2*(Ux(I,J)+Vy(I,J))+reta*Beta_2*UxyVyy)
        omega_0=Vx(I,J)-Uy(I,J)
        omega_1=b2*rhx*Beta_2*(HUxyHVyy+b2*rh*Beta_2*UxyVyy)  &
                - b2*rhy*Beta_2*(HUxxHVxy+b2*rh*Beta_2*UxxVxy)
	omega(I,J)=omega_0+omega_1
        IF(OUT_VORmax) THEN
        IF(abs(omega(I,J)).GT.VorticityMax(I,J)) THEN
        VorticityMax(I,J)=omega(I,J)
        ENDIF
        ENDIF







       ken1=((Beta_1-1.0_SP/2.0_SP)*(reta+rh)*Beta_2)
       ken2=(1.0_SP/3.0_SP-Beta_1+0.5_SP*Beta_1*Beta_1)*rh*rh*Beta_2*Beta_2  &
               + (1.0_SP/6.0_SP-Beta_1+Beta_1*Beta_1)*rh*reta*Beta_2 &
               +(1.0_SP/2.0_SP*Beta_1*Beta_1-1.0_SP/6.0_SP)*reta*reta*Beta_2*Beta_2


       U3(I,J)=-V(I,J)*omega_1 - omega_0 &
                 *(ken1*HUxyHVyy &
                   +ken2*UxyVyy)
       V3(I,J)=-V(I,J)*omega_1 - omega_0 &
                 *(ken1*HUxxHVxy &
                   +ken2*UxxVxy)
       ENDDO
       ENDDO            

     ENDIF  


END SUBROUTINE CAL_DISPERSION

! --------------------------------------------------
!    This is subroutine to calculation first-derivative y with higher-order
!    called by
!       CAL_DISPERSION
!    Last Update: 10/11/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_Y_High(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DY,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DY



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J)= (Uin(I,J+2)+2.0_SP*Uin(I,J+1)    &
                 -Uin(I,J-2)-2.0_SP*Uin(I,J-1))/DY/8.0_SP*MASK(I,J)




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_Y_High

! --------------------------------------------------
!    This is subroutine to calculation first-derivative x with higher-order
!    called by
!       CAL_DISPERSION
!    Last Update: 10/11/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_X_High(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DX,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DX



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J)= (Uin(I+2,J)+2.0_SP*Uin(I+1,J)    &
                 -Uin(I-2,J)-2.0_SP*Uin(I-1,J))/DX/8.0_SP*MASK(I,J)




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_X_High

! --------------------------------------------------
!    This is subroutine to calculation first-derivative y
!    called by
!       CAL_DISPERSION
!    Last Update: 10/11/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_Y(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DY,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DY



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J)= (Uin(I,J+1)   &
                 -Uin(I,J-1))/DY/2.0_SP*MASK(I,J)




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_Y

! --------------------------------------------------
!    This is subroutine to calculation first-derivative x
!    called by
!       CAL_DISPERSION
!    Last Update: 10/11/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_X(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DX,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DX



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J)= (Uin(I+1,J)    &
                 -Uin(I-1,J))/DX/2.0_SP*MASK(I,J)




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_X

! --------------------------------------------------
!    This is subroutine to calculation 2nd-derivative yy
!    called by
!       CAL_DISPERSION
!    Last Update: 09/21/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_YY(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DY,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DY



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

! I assume no 2nd derivative 
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J)= (Uin(I,J+1)-2.0_SP*Uin(I,J) & 
                 +Uin(I,J-1))/DY/DY*MASK(I,J)




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_YY

! --------------------------------------------------
!    This is subroutine to calculation 2nd-derivative xy
!    called by
!       CAL_DISPERSION
!    Last Update: 09/21/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_XY(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DX,DY,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DX,DY



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       tmp1=(Uin(I+1,J+1)-Uin(I+1,J-1))/2.0_SP/DY
       tmp2=(Uin(I-1,J+1)-Uin(I-1,J-1))/2.0_SP/DY
       Uout(I,J)= (tmp1-tmp2)/2.0_SP/DX*MASK(I,J)





     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_XY

! --------------------------------------------------
!    This is subroutine to calculation 2nd-derivative xx
!    called by
!       CAL_DISPERSION
!    Last Update: 09/21/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_XX(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DX,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DX



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

! I assume no 2nd derivative 
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J)= (Uin(I+1,J)-2.0_SP*Uin(I,J) & 
                 +Uin(I-1,J))/DX/DX*MASK(I,J)




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_XX

! $$$
! --------------------------------------------------
!    This is subroutine to calculation 4th-derivative xy
!    called by
!       CAL_DISPERSION
!    Last Update: 09/21/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_XY_HIGH(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DX,DY,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DX,DY



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       tmp1=1.0_SP/12.0_SP/DY*(Uin(I-2,J-2)-8.0_SP*Uin(I-2,J-1) &
                             +8.0_SP*Uin(I-2,J+1)-Uin(I-2,J+2))
       tmp2=1.0_SP/12.0_SP/DY*(Uin(I-1,J-2)-8.0_SP*Uin(I-1,J-1) &
                             +8.0_SP*Uin(I-1,J+1)-Uin(I-1,J+2))
       tmp3=1.0_SP/12.0_SP/DY*(Uin(I+1,J-2)-8.0_SP*Uin(I+1,J-1) &
                             +8.0_SP*Uin(I+1,J+1)-Uin(I+1,J+2))
       tmp4=1.0_SP/12.0_SP/DY*(Uin(I+2,J-2)-8.0_SP*Uin(I+2,J-1) &
                             +8.0_SP*Uin(I+2,J+1)-Uin(I+2,J+2))
       Uout(I,J)=MASK(I,J)/12.0_SP/DX*(tmp1-8.0_SP*tmp2 &
                             +8.0_SP*tmp3-tmp4)

     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_XY_HIGH
! --------------------------------------------------
!    This is subroutine to calculation 4th-derivative xx
!    called by
!       CAL_DISPERSION
!    Last Update: 05/30/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_XX_HIGH(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DX,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DX



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

! I assume no 2nd derivative 
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J) = MASK(I,J)*1.0_SP/12.0_SP/DX/DX*(-Uin(I-2,J)+16.0_SP*Uin(I-1,J)   &
                -30.0_SP*Uin(I,J)+16.0_SP*Uin(I+1,J)-Uin(I+2,J))




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_XX_HIGH
! --------------------------------------------------
!    This is subroutine to calculation 4th-derivative yy
!    called by
!       CAL_DISPERSION
!    Last Update: 05/30/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DERIVATIVE_YY_HIGH(M,N,Ibeg,Iend,Jbeg,Jend,MASK,DY,Uin,Uout)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N,Ibeg,Iend,Jbeg,Jend
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: Uin

     REAL(SP),INTENT(IN) :: DY



     INTEGER,DIMENSION(M,N),INTENT(IN) :: MASK
     REAL(SP),DIMENSION(M,N),INTENT(OUT) :: Uout

! I assume no 2nd derivative 
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Uout(I,J) = MASK(I,J)*1.0_SP/12.0_SP/DY/DY*(-Uin(I,J-2)+16.0_SP*Uin(I,J-1)   &
                -30.0_SP*Uin(I,J)+16.0_SP*Uin(I,J+1)-Uin(I,J+2))




     ENDDO
     ENDDO

END SUBROUTINE DERIVATIVE_YY_HIGH

! $$$

! --------------------------------------------------
!    This is subroutine to update mask
!    note that mask also be updated in fluxes routine
!    called by
!         MAIN
!    Last Update: 05/28/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE UPDATE_MASK
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP)::left,right,top,bottom

! for the serial code, MASK at ghost cells keep no change


     call phi_int_exch(MASK)
     call phi_int_exch(MASK9)


! Jeff did the following loop, also work for serial
!     DO J=Jbeg,Jend
!     DO I=Ibeg,Iend

     DO J=Jbeg-2,Jend+2
     DO I=Ibeg-2,Iend+2
! flood
     IF(MASK_STRUC(I,J)==1)THEN
       IF(MASK(I,J)<1)THEN
         ! left
        IF(I/=1)THEN
         IF(MASK(I-1,J)==1.AND.Eta(I-1,J)>Eta(I,j))THEN
           MASK(I,J)=1
         ENDIF
        ENDIF
         ! right
        IF(I/=Mloc)THEN
         IF(MASK(I+1,J)==1.AND.Eta(I+1,J)>Eta(I,j))THEN
           MASK(I,J)=1
         ENDIF
        ENDIF
         ! bottom
        IF(J/=1)THEN
         IF(MASK(I,J-1)==1.AND.Eta(I,J-1)>Eta(I,j))THEN
           MASK(I,J)=1
         ENDIF
        ENDIF
         ! top
        IF(J/=Nloc)THEN
         IF(MASK(I,J+1)==1.AND.Eta(I,J+1)>Eta(I,j))THEN
           MASK(I,J)=1
         ENDIF
        ENDIF
! drying
       ELSE
         IF(Eta(I,J)<-Depth(I,J))THEN
          MASK(I,J)=0
          Eta(I,J)=MinDepth-Depth(I,J)
         ENDIF    
       ENDIF
      ENDIF

! to avoid extreme depth gradient caused by depthx and depthy which were not
! treated in initialization, I reset depthx and depthy when drying 
! 01/21/2012

        IF(MASK(I,J)<1)THEN
         DepthX(I,J)=Depth(I-1,J)
         DepthX(I+1,J)=Depth(I+1,J)
         DepthY(I,J)=Depth(I,J-1)
         DepthY(I,J+1)=Depth(I,J+1)
        ENDIF    

     ENDDO
     ENDDO


! Jeff also did this loop
!     DO J=Jbeg,Jend
!     DO I=Ibeg,Iend
     DO J=Jbeg-1,Jend+1
     DO I=Ibeg-1,Iend+1
      MASK9(I,J)=MASK(I,J)*MASK(I-1,J)*MASK(I+1,J)  &
                *MASK(I+1,J+1)*MASK(I,J+1)*MASK(I-1,J+1) &
                *MASK(I+1,J-1)*MASK(I,J-1)*MASK(I-1,J-1) 
      IF(ABS(Eta(I,J))/MAX(DEPTH(I,J),MinDepthFrc)>SWE_ETA_DEP)THEN
       MASK9(I,J)=ZERO
      ENDIF

     ENDDO
     ENDDO
  

     call phi_int_exch(MASK)
     call phi_int_exch(MASK9)


END SUBROUTINE UPDATE_MASK

! --------------------------------------------------
!    This is subroutine for all source terms including slope term dispersion É
!    called by
!       MAIN
!    Last Update: 09/26/2013 Babak Tehranirad, University of Delaware
! --------------------------------------------------
SUBROUTINE SourceTerms
     USE GLOBAL
     IMPLICIT NONE

! depth gradient term
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend



! second order splitting method
!       SourceX(I,J)=-GRAV*Eta(I,J)/DX*(Zx(I+1,J)-Zx(I,J)) &
!                    +0.5_SP*GRAV/DX*(Zx(I+1,J)**2-Zx(I,J)**2)
!       SourceY(I,J)=-GRAV*Eta(I,J)/DY*(Zy(I,J+1)-Zy(I,J)) &
!                    +0.5_SP*GRAV/DY*(Zy(I,J+1)**2-Zy(I,J)**2)

! second order, move the second term to left-hand side
       SourceX(I,J)=GRAV*(Eta(I,J))/DX*(Depthx(I+1,J)-Depthx(I,J))*MASK(I,J) &
 ! friction



                   -Cd(I,J)*U(I,J)*SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J)) &


                       ! dispersion
                       ! (h+eta)(u*\nabla V4 + V4 * \nabla u - v1pp-v2-v3)
                   + Gamma1*MASK9(I,J)*Max(H(I,J),MinDepthFrc)*(         & 
                     U(I,J)*0.5_SP*(U4(I+1,J)-U4(I-1,J))/DX+V(I,J)*0.5_SP*(U4(I,J+1)-U4(I,J-1))/DY &
                     +U4(I,J)*0.5_SP*(U(I+1,J)-U(I-1,J))/DX+V4(I,J)*0.5_SP*(U(I,J+1)-U(I,J-1))/DY  &
                     -Gamma2*MASK9(I,J)*(U1pp(I,J)+U2(I,J)+U3(I,J)) &
                     )    &
                        ! Ht(-V4+V1p) = div(M)*(U4-U1p)
                    +Gamma1*MASK9(I,J)*((P(I+1,J)-P(I,J))/DX+(Q(I,J+1)-Q(I,J))/DY) &
                      *(U4(I,J)-U1p(I,J))
          
       SourceY(I,J)=GRAV*(Eta(I,J))/DY*(Depthy(I,J+1)-Depthy(I,J))*MASK(I,J) &
                          ! friction



                   -Cd(I,J)*V(I,J)*SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J)) &

		          ! dispersion
                          ! (h+eta)(u*\nabla V4 + V4 * \nabla u -v1pp-v2-v3)
                   + Gamma1*MASK9(I,J)*Max(H(I,J),MinDepthFrc)*(         & 
                     U(I,J)*0.5_SP*(V4(I+1,J)-V4(I-1,J))/DX+V(I,J)*0.5_SP*(V4(I,J+1)-V4(I,J-1))/DY &
                     +U4(I,J)*0.5_SP*(V(I+1,J)-V(I-1,J))/DX+V4(I,J)*0.5_SP*(V(I,J+1)-V(I,J-1))/DY  &
                     -Gamma2*MASK9(I,J)*(V1pp(I,J)+V2(I,J)+V3(I,J)) &
                     )    &
                          ! Ht(-V4+V1p) = div(Q)*(V4-V1p)
                    +Gamma1*MASK9(I,J)*((P(I+1,J)-P(I,J))/DX+(Q(I,J+1)-Q(I,J))/DY) &
                      *(V4(I,J)-V1p(I,J))




       SourceX(I,J) = SourceX(I,J) + 1.0_SP/DX*( &
                   0.5_SP*(nu_smg(I,J)+nu_smg(I+1,J))*1.0_SP/DX*(P(I+1,J)-P(I,J))  &
                 - 0.5_SP*(nu_smg(I,J)+nu_smg(I-1,J))*1.0_SP/DX*(P(I,J)-P(I-1,J)) ) &
               + 0.5_SP/DY*( &
                   0.5_SP*(nu_smg(I,J+1)+nu_smg(I,J))*1.0_SP/DY*(P(I,J+1)-P(I,J)) &
                  -0.5_SP*(nu_smg(I,J-1)+nu_smg(I,J))*1.0_SP/DY*(P(I,J)-P(I,J-1))  ) &   
               + 1.0_SP/DY*( &
                   nu_smg(I,J+1)*0.5_SP/DX*(Q(I+1,J+1)-Q(I-1,J+1)) &
                  -nu_smg(I,J-1)*0.5_SP/DX*(Q(I+1,J-1)-Q(I-1,J-1))  )

       SourceY(I,J) = SourceY(I,J) + 1.0_SP/DY*( &
                   0.5_SP*(nu_smg(I,J)+nu_smg(I,J+1))*1.0_SP/DY*(Q(I,J+1)-Q(I,J))  &
                 - 0.5_SP*(nu_smg(I,J)+nu_smg(I,J-1))*1.0_SP/DY*(Q(I,J)-Q(I,J-1)) ) &
               + 0.5_SP/DX*( &
                   0.5_SP*(nu_smg(I+1,J)+nu_smg(I,J))*1.0_SP/DX*(Q(I+1,J)-Q(I,J)) &
                  -0.5_SP*(nu_smg(I-1,J)+nu_smg(I,J))*1.0_SP/DX*(Q(I,J)-Q(I-1,J))  ) &   
               + 1.0_SP/DX*( &
                   nu_smg(I+1,J)*0.5_SP/DY*(P(I+1,J+1)-P(I+1,J-1)) &
                  -nu_smg(I-1,J)*0.5_SP/DY*(P(I-1,J+1)-P(I-1,J-1))  )






     ENDDO
     ENDDO

END SUBROUTINE SourceTerms

! --------------------------------------------------
!    This is subroutine to show statistics
!    called by
!        MAIN
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE STATISTICS
     USE GLOBAL
     IMPLICIT NONE

     REAL(SP)::MassVolume=ZERO,Energy=ZERO,MaxEta=ZERO,MinEta=ZERO, &
              MaxU=ZERO,MaxV=ZERO,Fr=ZERO,UTotal=ZERO,UTotalMax=ZERO, &
	      MaxM=ZERO

     REAL(SP)::myvar

!
     MassVolume=ZERO
     Energy=ZERO
     UTotalMax=ZERO

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

! Vol=SUM(Eta*dx*dy), reference is at z=0
! Energy=SUM(1/2*g*H^2*dx*dy+0.5*u^2*H*dx*dy)


       MassVolume=MassVolume+Eta(I,J)*DX*DY
       Energy=Energy+0.5_SP*H(I,J)*H(I,J)*GRAV*DX*DY &
             +0.5_SP*U(I,J)*U(I,J)*H(I,J)*DX*DY &
             +0.5_SP*V(I,J)*V(I,J)*H(I,J)*DX*DY






!       print*,I,J,Energy,H(I,J),U(I,J),V(I,J)       
     ENDDO
!      pause
     ENDDO
!     stop

     MaxEta=MAXVAL(Eta(Ibeg:Iend,Jbeg:Jend))
     MinEta=MINVAL(Eta(Ibeg:Iend,Jbeg:Jend))
     MaxU=MAXVAL(ABS(U(Ibeg:Iend,Jbeg:Jend)))
     MaxV=MAXVAL(ABS(V(Ibeg:Iend,Jbeg:Jend)))

! found Froude vs. max speed
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      IF(MASK(I,J)>ZERO)THEN
       Utotal=SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J))
       IF(Utotal.gt.UtotalMax)THEN
         UtotalMax=Utotal
         Fr=SQRT(GRAV*Max(H(I,J),MinDepthfrc))
       ENDIF
      ENDIF
     ENDDO
     ENDDO
     IF(Fr==ZERO)Fr=SQRT(GRAV*MinDepthfrc)


     call MPI_ALLREDUCE(MassVolume,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MassVolume = myvar
     call MPI_ALLREDUCE(Energy,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     Energy = myvar
     call MPI_ALLREDUCE(MaxEta,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxEta = myvar
     call MPI_ALLREDUCE(MinEta,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MinEta = myvar
     call MPI_ALLREDUCE(MaxU,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxU = myvar
     call MPI_ALLREDUCE(MaxV,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     MaxV = myvar
     call MPI_ALLREDUCE(UTotalMax,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     UTotalMax = myvar
     call MPI_ALLREDUCE(Fr,myvar,1,MPI_SP,MPI_MAX,MPI_COMM_WORLD,ier)
     Fr = myvar



     if (myid.eq.0) then


! print screen
     WRITE(*,*) '----------------- STATISTICS ----------------'
     WRITE(*,*) ' TIME        DT'
     WRITE(*,101) Time, DT
     WRITE(*,*) ' MassVolume  Energy      MaxEta      MinEta      Max U       Max V '
     WRITE(*,101)  MassVolume,Energy,MaxEta,MinEta,MaxU,MaxV
     WRITE(*,*) ' MaxTotalU   PhaseS      Froude '
     WRITE(*,101) UTotalMax, Fr, UTotalMax/Fr
! print log file
     WRITE(3,*) '----------------- STATISTICS ----------------'
     WRITE(3,*) ' TIME        DT'
     WRITE(3,101) Time, DT
     WRITE(3,*) ' MassVolume  Energy      MaxEta      MinEta      Max U       Max V '
     WRITE(3,101)  MassVolume,Energy,MaxEta,MinEta,MaxU,MaxV
     WRITE(3,*) ' MaxTotalU   PhaseS      Froude '
     WRITE(3,101)  UTotalMax, Fr, UTotalMax/Fr

     endif


101  FORMAT(6E12.4)

END SUBROUTINE STATISTICS

! ---------------------------------------------------
!    This is subroutine ESTIMATE_HUV 
!  for 3rd-order LK scheme 
!  called by
!      MAIN
!  call
!      GET_Eta_U_V_HU_HV
!    Last Update: 05/12/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE ESTIMATE_HUV(ISTEP)
     USE GLOBAL
     IMPLICIT NONE
     INTEGER,INTENT(IN)::ISTEP
     REAL(SP),PARAMETER::n_left=-1.0_SP,n_right=1.0_SP,n_bottom=-1.0_SP,n_top=1.0_SP
     REAL(SP)::F_left,F_right,F_bottom,F_top,WK_Source
     REAL(SP),DIMENSION(Ibeg:Iend,Jbeg:Jend)::R1,R2,R3
! now work for spherical # if defined (1)
     REAL(SP)::xmk,ymk
! now work for spherical # endif
     REAL(SP)::DXg,DYg

     INTEGER::kf,kd

! MUSCL-Hancock, Zhou et al., p. 7


     DXg=DX
     DYg=DY






! solve eta
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      F_left=P(I,J)
      F_right=P(I+1,J)
      F_bottom=Q(I,J)
      F_top=Q(I,J+1)
! now work for spherical # if defined (1)
      IF(WAVEMAKER(1:6)=='WK_IRR')THEN

            xmk=(I-Ibeg)*DXg+npx*(Mloc-2*Nghost)*DXg
            ymk=(J-Jbeg)*DYg+npy*(Nloc-2*Nghost)*DYg




         IF(ABS(xmk-Xc_WK)<Width_WK.AND. &
            ABS(ymk-Yc_WK)<Ywidth_WK/2.0_SP)THEN
          WK_Source=ZERO
          DO kf=1,Nfreq
           WK_Source=WK_Source+TANH(PI/(Time_ramp/FreqPeak)*TIME)*(Cm(I,J,kf) &
                       *COS(OMGN_IR(KF)*TIME) &
                       +Sm(I,J,kf)*SIN(OMGN_IR(KF)*TIME))
          ENDDO

          R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom) &
        ! wavemaker
                +WK_Source      
         ELSE
         R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                   -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom)
         ENDIF
       ELSEIF(WAVEMAKER(1:6)=='WK_REG')THEN

            xmk=(I-Ibeg)*DXg+npx*(Mloc-2*Nghost)*DXg
            ymk=(J-Jbeg)*DYg+npy*(Nloc-2*Nghost)*DYg




         IF(ABS(xmk-Xc_WK)<Width_WK.AND. &
            ABS(ymk-Yc_WK)<Ywidth_WK/2.0_SP)THEN
          
          R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom) &
        ! wavemaker
                +TANH(PI/(Time_ramp*Tperiod)*TIME)*D_gen &
                 *EXP(-Beta_gen*(xmk-Xc_WK)**2)&
                 *SIN(rlamda*(ymk-ZERO)-2.0_SP*PI/Tperiod*TIME)       
         ELSE
         R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                   -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom)
         ENDIF
       ELSEIF(WAVEMAKER(1:7)=='WK_TIME')THEN

            xmk=(I-Ibeg)*DXg+npx*(Mloc-2*Nghost)*DXg
            ymk=(J-Jbeg)*DYg+npy*(Nloc-2*Nghost)*DYg




         IF(ABS(xmk-Xc_WK)<Width_WK.AND. &
            ABS(ymk-Yc_WK)<Ywidth_WK/2.0_SP)THEN

           WK_Source=ZERO
           DO kf=1,NumWaveComp
             WK_Source=WK_Source &
               +TANH(PI/(Time_ramp*PeakPeriod)*TIME)*D_genS(kf) &
                 *EXP(-Beta_genS(kf)*(xmk-Xc_WK)**2)&
                 *COS(2.0_SP*PI/WAVE_COMP(kf,1)*TIME-WAVE_COMP(kf,3)) 
           ENDDO
          
          R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom) &
                +WK_Source      
         ELSE
         R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                   -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom)
         ENDIF   
! *****
       ELSEIF(WAVEMAKER(1:9)=='WK_DATA2D')THEN

            xmk=(I-Ibeg)*DXg+npx*(Mloc-2*Nghost)*DXg
            ymk=(J-Jbeg)*DYg+npy*(Nloc-2*Nghost)*DYg




         IF(ABS(xmk-Xc_WK)<Width_WK.AND. &
            ABS(ymk-Yc_WK)<Ywidth_WK/2.0_SP)THEN

           WK_Source=ZERO
           DO kf=1,NumFreq
            DO kd=1,NumDir
             WK_Source=WK_Source &
               +TANH(PI/(Time_ramp*PeakPeriod)*TIME)*D_gen2D(kf,kd) &
                 *EXP(-Beta_gen2D(kf,kd)*(xmk-Xc_WK)**2)&
                 *SIN(rlamda2D(kf,kd)*(ymk-ZERO) &
                      -2.0_SP*PI*Freq(kf)*TIME &
                      -Phase2D(kf,kd)) 
            ENDDO
           ENDDO
          
          R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom) &
                +WK_Source      
         ELSE
         R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                   -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom)
         ENDIF 
! *****    
      ELSE ! no wk_wavemaker, there's bug in version 1.1 Dxg,Dyg should be 
           ! replaced by Dxg() and Dy()

        R1(I,J)=-1.0_SP/DXg*(F_right*n_right+F_left*n_left) &
                   -1.0_SP/DYg*(F_top*n_top+F_bottom*n_bottom)




      ENDIF

! do nothing





      Eta(I,J)=ALPHA(ISTEP)*Eta0(I,J)+BETA(ISTEP)*(Eta(I,J)+DT*R1(I,J))

     ENDDO
     ENDDO

! solve ubar
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      F_left=Fx(I,J)
      F_right=Fx(I+1,J)
      F_bottom=Fy(I,J)
      F_top=Fy(I,J+1)

      R2(I,J)=-1.0_SP/DX*(F_right*n_right+F_left*n_left) &
                       -1.0_SP/DY*(F_top*n_top+F_bottom*n_bottom) &
                        +SourceX(I,J)








      Ubar(I,J)=ALPHA(ISTEP)*Ubar0(I,J)+BETA(ISTEP)*(Ubar(I,J)+DT*R2(I,J))

     ENDDO
     ENDDO

! solve vbar
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      F_left=Gx(I,J)
      F_right=Gx(I+1,J)
      F_bottom=Gy(I,J)
      F_top=Gy(I,J+1)

      R3(I,J)=-1.0_SP/DX*(F_right*n_right+F_left*n_left) &
                       -1.0_SP/DY*(F_top*n_top+F_bottom*n_bottom) &
                       +SourceY(I,J)








      Vbar(I,J)=ALPHA(ISTEP)*Vbar0(I,J)+BETA(ISTEP)*(Vbar(I,J)+DT*R3(I,J))

     ENDDO
     ENDDO

     CALL GET_Eta_U_V_HU_HV

END SUBROUTINE ESTIMATE_HUV

! ---------------------------------------------------
!    This is subroutine to obtain Eta, u,v,hu,hv
!  called by
!      PREDICTOR
!      CORRECTOR
!      ESTIMATE_HUV (Lunge-Kutta)
!  use FroudeCap to Limit Froude<FroudeCap
!    Last Update: 09/17/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE GET_Eta_U_V_HU_HV
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP)::Fr,Utotal,Utheta,dep,depl,depr,reta,retal,retar

     REAL(SP),DIMENSION(Mloc,Nloc) :: myA,myC,myD,myF
     REAL(SP),DIMENSION(Nglob) :: AperG,BperG,CperG,DperG,VperG
     REAL(SP),DIMENSION(Mglob,Nglob) ::glbA,glbC,glbD
     REAL(SP),DIMENSION(Mglob+2*Nghost,Nglob+2*Nghost)::VG

      INTEGER :: IM

! calculate etar, u and vetar, HU, HV
     H=Eta*Gamma3+Depth

!     DO J=Jbeg,Jend
!     DO I=Ibeg,Iend   
! if drying, don't mask it until updating in updat_mask
!       IF(H(I,J)<ZERO)THEN
!        H(I,J)=MinDepth-SMALL
!        Eta(I,J)=H(I,J)+Z(I,J)    
!         MASK(I,J)=0   
!       ENDIF     
!     ENDDO
!     ENDDO

!   tridiagonal coefficient
! x direction

! shift U and V
     U0=U
     V0=V

   IF(DISPERSION)THEN

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
       dep=Max(Depth(I,J),MinDepthFrc)
       depl=Max(Depth(I-1,J),MinDepthFrc)
       depr=Max(Depth(I+1,J),MinDepthFrc)


     IF(DISP_TIME_LEFT)THEN
       reta=Eta(I,J)
       retal=Eta(I-1,J)
       retar=Eta(I+1,J)

       tmp1=Gamma1*MASK9(I,J)*(b1/2.0_SP/DX/DX*dep*dep + b2/DX/DX*depl*dep) &
             -Gamma2*MASK9(I,J)*((reta+retal)*depl/2.0_SP/DX/DX+(retal+reta)*(retal+reta)/8.0_SP/DX/DX)
       tmp2=1.0_SP+Gamma1*MASK9(I,J)*(-b1/DX/DX*dep*dep-2.0_SP*b2/DX/DX*dep*dep) &
             +Gamma2*MASK9(I,J)*((retar+retal+2.0_SP*reta)/2.0_SP/DX/DX &
                        +(retar*retar+2.0_SP*reta*reta &
                          +2.0_SP*reta*retar+2.0_SP*retal*reta+retal*retal )/8.0_SP/DX/DX)
       tmp3=Gamma1*MASK9(I,J)*(b1/2.0_SP/DX/DX*dep*dep + b2/DX/DX*dep*depr) &
             -Gamma2*MASK9(I,J)*((reta+retar)*depr/2.0_SP/DX/DX+(retar+reta)*(retar+reta)/8.0_SP/DX/DX)
       tmp4=Ubar(I,J)*MASK(I,J)/Max(H(I,J),MinDepthFrc)  &
            + Gamma1*MASK9(I,J)*( -b1/2.0_SP*dep*dep*Vxy(I,J)-b2*dep*DVxy(I,J)) &
            + Gamma2*MASK9(I,J)*(reta*reta/2.0_SP*Vxy(I,J)+reta*DVxy(I,J) &
               + reta*ETAx(I,J)*Vy(I,J) + ETAx(I,J)*DVy(I,J) )
      ELSE
       tmp1=Gamma1*MASK9(I,J)*(b1/2.0_SP/DX/DX*dep*dep + b2/DX/DX*depl*dep)
       tmp2=1.0_SP+Gamma1*MASK9(I,J)*(-b1/DX/DX*dep*dep-2.0_SP*b2/DX/DX*dep*dep)
       tmp3=Gamma1*MASK9(I,J)*(b1/2.0_SP/DX/DX*dep*dep + b2/DX/DX*dep*depr)
       tmp4=Ubar(I,J)*MASK(I,J)/Max(H(I,J),MinDepthFrc)  &
            + Gamma1*MASK9(I,J)*( -b1/2.0_SP*dep*dep*Vxy(I,J)-b2*dep*DVxy(I,J))
      ENDIF









! I added coupling condition 10/14/2012




       IF(tmp2.NE.0.0_SP.OR.MASK(I,J).GT.0)THEN

          myA(I,J)=tmp1/tmp2
          myC(I,J)=tmp3/tmp2
          myD(I,J)=tmp4/tmp2





       ELSE

          myA(I,J)=ZERO
          myC(I,J)=ZERO
          myD(I,J)=ZERO





       ENDIF
     ENDDO






     ENDDO


     call TRIDx(myA,myC,myD,myF,Ibeg,Iend,Jbeg,Jend)
     U(Ibeg:Iend,Jbeg:Jend) = myF(Ibeg:Iend,Jbeg:Jend)


! y direction

     myA=ZERO
     myC=ZERO
     myD=ZERO

     DO I=Ibeg,Iend
     DO J=Jbeg,Jend
       dep=Max(Depth(I,J),MinDepthFrc)
       depl=Max(Depth(I,J-1),MinDepthFrc)
       depr=Max(Depth(I,J+1),MinDepthFrc)


     IF(DISP_TIME_LEFT)THEN
       reta=Eta(I,J)
       retal=Eta(I,J-1)
       retar=Eta(I,J+1)
       tmp1=Gamma1*MASK9(I,J)*(b1/2.0_SP/DY/DY*dep*dep + b2/DY/DY*depl*dep) &
             -Gamma2*MASK9(I,J)*((reta+retal)*depl/2.0_SP/DY/DY+(retal+reta)*(retal+reta)/8.0_SP/DY/DY)
       tmp2=1.0_SP+Gamma1*MASK9(I,J)*(-b1/DY/DY*dep*dep-2.0_SP*b2/DY/DY*dep*dep) &
             +Gamma2*MASK9(I,J)*((retar+retal+2.0_SP*reta)/2.0_SP/DY/DY &
                        +(retar*retar+2.0_SP*reta*reta &
                          +2.0_SP*reta*retar+2.0_SP*retal*reta+retal*retal)/8.0_SP/DY/DY)
       tmp3=Gamma1*MASK9(I,J)*(b1/2.0_SP/DY/DY*dep*dep + b2/DY/DY*dep*depr) &
             -Gamma2*MASK9(I,J)*((reta+retar)*depr/2.0_SP/DY/DY+(retar+reta)*(retar+reta)/8.0_SP/DY/DY)
       tmp4=Vbar(I,J)*MASK(I,J)/Max(H(I,J),MinDepthFrc)  &
             + Gamma1*MASK9(I,J)*(-b1/2.0_SP*dep*dep*Uxy(I,J)-b2*dep*DUxy(I,J)) &
            + Gamma2*MASK9(I,J)*(reta*reta/2.0_SP*Uxy(I,J)+reta*DUxy(I,J) &
                             + reta*ETAy(I,J)*Ux(I,J) + ETAy(I,J)*DUx(I,J) )
     ELSE
       tmp1=Gamma1*MASK9(I,J)*(b1/2.0_SP/DY/DY*dep*dep + b2/DY/DY*depl*dep) 
       tmp2=1.0_SP+Gamma1*MASK9(I,J)*(-b1/DY/DY*dep*dep-2.0_SP*b2/DY/DY*dep*dep) 
       tmp3=Gamma1*MASK9(I,J)*(b1/2.0_SP/DY/DY*dep*dep + b2/DY/DY*dep*depr)
       tmp4=Vbar(I,J)*MASK(I,J)/Max(H(I,J),MinDepthFrc)  &
             + Gamma1*MASK9(I,J)*(-b1/2.0_SP*dep*dep*Uxy(I,J)-b2*dep*DUxy(I,J))
     ENDIF  










  
       IF(tmp2.NE.0.0_SP.OR.MASK(I,J).GT.0)THEN

         myA(I,J)=tmp1/tmp2
         myC(I,J)=tmp3/tmp2
         myD(I,J)=tmp4/tmp2





       ELSE

         myA(I,J)=ZERO
         myC(I,J)=ZERO
         myD(I,J)=ZERO





       ENDIF
     ENDDO


! see the following ifdef parallel

     ENDDO ! end I


     IF(PERIODIC) THEN
! data gathering from processors
!    myA myC myD to glbA glbC and glbD
! NOTE glbA doesn't include ghost cells
           

       CALL GatherVariable(myA,glbA)
       CALL GatherVariable(myC,glbC)
       CALL GatherVariable(myD,glbD)

       IF(myid==0)THEN
         DO I=1,Mglob
          BperG=1.0_SP
           DO J=1,Nglob
             AperG(J) = glbA(I,J)
             CperG(J) = glbC(I,J)
             DperG(J) = glbD(I,J)
           ENDDO
           CALL TRIG_PERIODIC(AperG,BperG,CperG,DperG,VperG,Nglob)
           VG(I+Nghost,Nghost+1:Nglob+Nghost) = VperG(:)
           VG(I+Nghost,1:Nghost) = VG(I+Nghost,Nglob+1:Nglob+Nghost)
           VG(I+Nghost,Nglob+Nghost+1:Nglob+2*Nghost)=VG(I+Nghost,Nghost+1:Nghost+Nghost)
         ENDDO  ! end I
       ENDIF  ! end myid=0
! scattering VG
       CALL ScatterVariable (VG,V)

     ELSE ! no periodic
       call TRIDy(myA,myC,myD,myF,Ibeg,Iend,Jbeg,Jend)
       V(Ibeg:Iend,Jbeg:Jend) = myF(Ibeg:Iend,Jbeg:Jend)
     ENDIF


   ELSE  ! if no dispersion
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend  
        U(I,J)=Ubar(I,J)/Max(H(I,J),MinDepthFrc)
        V(I,J)=Vbar(I,J)/Max(H(I,J),MinDepthFrc)
     ENDDO
     ENDDO   

   ENDIF  ! end dispersion

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend   
       IF(MASK(I,J)<1)THEN
        Ubar(I,J)=ZERO
        Vbar(I,J)=ZERO
        U(I,J)=ZERO
        V(I,J)=ZERO
        HU(I,J)=ZERO
        HV(I,J)=ZERO
       ELSE
        HU(I,J)=Max(H(I,J),MinDepthFrc)*U(I,J)
        HV(I,J)=Max(H(I,J),MinDepthFrc)*V(I,J)
! apply Froude cap
        Utotal=SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J))
        Fr=SQRT(GRAV*Max(H(I,J),MinDepthFrc))
        IF(Utotal/Fr.gt.FroudeCap)THEN
          Utheta=ATAN2(V(I,J),U(I,J))
          U(I,J)=FroudeCap*Fr*COS(Utheta)
          V(I,J)=FroudeCap*Fr*SIN(Utheta)
          HU(I,J)=U(I,J)*Max(H(I,J),MinDepthFrc)
          HV(I,J)=V(I,J)*Max(H(I,J),MinDepthFrc)
        ENDIF
! end Froude cap
       ENDIF
     ENDDO
     ENDDO

END SUBROUTINE GET_Eta_U_V_HU_HV

! ---------------------------------------------------
!    This is subroutine evaluate dt
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE ESTIMATE_DT(M,N,DX,DY,U,V,H,MinDepthFrc,DT,CFL,TIME)
     USE PARAM

     USE GLOBAL, ONLY : ier

     IMPLICIT NONE
     INTEGER,INTENT(IN)::M,N

     REAL(SP) :: myvar



     REAL(SP),INTENT(IN)::DX,DY



     REAL(SP),INTENT(IN),DIMENSION(M,N)::U,V,H
     REAL(SP),INTENT(IN)::CFL,MinDepthFrc
     REAL(SP),INTENT(OUT)::DT
     REAL(SP),INTENT(INOUT)::TIME

     TMP3=LARGE
     DO J=1,N
     DO I=1,M
! x direction
      TMP1=ABS(U(I,J))+SQRT(GRAV*MAX(H(I,J),MinDepthFrc))
      IF(TMP1<SMALL)THEN

       TMP2=DX/SMALL



      ELSE

       TMP2=DX/TMP1



      ENDIF
      IF(TMP2<TMP3)TMP3=TMP2
! y direction
      TMP1=ABS(V(I,J))+SQRT(GRAV*MAX(H(I,J),MinDepthFrc))
      IF(TMP1<SMALL)THEN

       TMP2=DY/SMALL



      ELSE

       TMP2=DY/TMP1



      ENDIF
      IF(TMP2<TMP3)TMP3=TMP2      
     ENDDO
     ENDDO

     call MPI_ALLREDUCE (TMP3,myvar,1,MPI_SP,MPI_MIN,&
          MPI_COMM_WORLD,ier)
     TMP3 = myvar

     DT=CFL*TMP3
! TEMP
     TIME=TIME+DT

END SUBROUTINE ESTIMATE_DT


SUBROUTINE phi_exch (PHI)
    USE PARAM
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc)

    INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
    INTEGER,DIMENSION(4) :: req
    INTEGER :: nreq,len
    REAL(SP),DIMENSION(Mloc,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
    REAL(SP),DIMENSION(Nloc,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

    len = Nloc * Nghost

    nreq = 0
    if ( n_west .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rWmsg, len, MPI_SP, &
            n_west, 0, comm2d, req(nreq), ier )
       do j = 1, Nloc
       do i = 1, Nghost
          sWmsg(j,i) = PHI(Ibeg+i-1,j)
       enddo
       enddo
       nreq = nreq +1
       call MPI_ISEND( sWmsg, len, MPI_SP, &
            n_west, 1, comm2d, req(nreq), ier )
    endif

    if ( n_east .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rEmsg, len, MPI_SP, &
            n_east, 1, comm2d, req(nreq), ier )
       do j = 1, Nloc
       do i = 1, Nghost
          sEmsg(j,i) = PHI(Iend-i+1,j)
       enddo
       enddo
       nreq = nreq +1
       call MPI_ISEND( sEmsg, len, MPI_SP, &
            n_east, 0, comm2d, req(nreq), ier )
    endif

    call MPI_WAITALL( nreq, req, status, ier )

    if ( n_west .ne. MPI_PROC_NULL ) then
       do j = 1, Nloc
       do i = 1, Nghost
          PHI(Ibeg-i,j) = rWmsg(j,i)
       enddo
       enddo
    endif

    if ( n_east .ne. MPI_PROC_NULL ) then
       do j = 1, Nloc
       do i = 1, Nghost
          PHI(Iend+i,j) = rEmsg(j,i)
       enddo
       enddo
    endif

! for nrth-suth

    len = Mloc * Nghost

    nreq = 0
    if ( n_suth .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rSmsg, len, MPI_SP, &
            n_suth, 0, comm2d, req(nreq), ier )
       do i = 1, Mloc
       do j = 1, Nghost
          sSmsg(i,j) = PHI(i,Jbeg+j-1)
       enddo
       enddo
       nreq = nreq +1
       call MPI_ISEND( sSmsg, len, MPI_SP, &
            n_suth, 1, comm2d, req(nreq), ier )
    endif

    if ( n_nrth .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rNmsg, len, MPI_SP, &
            n_nrth, 1, comm2d, req(nreq), ier )
       do i = 1, Mloc
       do j = 1, Nghost
          sNmsg(i,j) = PHI(i,Jend-j+1)
       enddo
       enddo
       nreq = nreq + 1
       call MPI_ISEND( sNmsg, len, MPI_SP, &
            n_nrth, 0, comm2d, req(nreq), ier )
    endif

    call MPI_WAITALL( nreq, req, status, ier )

    if ( n_suth .ne. MPI_PROC_NULL ) then
       do i = 1, Mloc
       do j = 1, Nghost
          PHI(i,Jbeg-j) = rSmsg(i,j)
       enddo
       enddo
    endif

    if ( n_nrth .ne. MPI_PROC_NULL ) then
       do i = 1, Mloc
       do j = 1, Nghost
          PHI(i,Jend+j) = rNmsg(i,j)
       enddo
       enddo
    endif

END SUBROUTINE phi_exch




! Jeff added this subroutine to pass mask 02/14/2011
SUBROUTINE phi_int_exch (PHI)
    USE PARAM
    USE GLOBAL
    IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: PHI(Mloc,Nloc)

    INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
    INTEGER,DIMENSION(4) :: req
    INTEGER :: nreq,len
    INTEGER,DIMENSION(Mloc,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
    INTEGER,DIMENSION(Nloc,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

    len = Nloc * Nghost

    nreq = 0
    if ( n_west .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rWmsg, len, MPI_INTEGER, &
            n_west, 0, comm2d, req(nreq), ier )
       do j = 1, Nloc
       do i = 1, Nghost
          sWmsg(j,i) = PHI(Ibeg+i-1,j)
       enddo
       enddo
       nreq = nreq +1
       call MPI_ISEND( sWmsg, len, MPI_INTEGER, &
            n_west, 1, comm2d, req(nreq), ier )
    endif

    if ( n_east .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rEmsg, len, MPI_INTEGER, &
            n_east, 1, comm2d, req(nreq), ier )
       do j = 1, Nloc
       do i = 1, Nghost
          sEmsg(j,i) = PHI(Iend-i+1,j)
       enddo
       enddo
       nreq = nreq +1
       call MPI_ISEND( sEmsg, len, MPI_INTEGER, &
            n_east, 0, comm2d, req(nreq), ier )
    endif

    call MPI_WAITALL( nreq, req, status, ier )

    if ( n_west .ne. MPI_PROC_NULL ) then
       do j = 1, Nloc
       do i = 1, Nghost
          PHI(Ibeg-i,j) = rWmsg(j,i)
       enddo
       enddo
    endif

    if ( n_east .ne. MPI_PROC_NULL ) then
       do j = 1, Nloc
       do i = 1, Nghost
          PHI(Iend+i,j) = rEmsg(j,i)
       enddo
       enddo
    endif

! for nrth-suth

    len = Mloc * Nghost

    nreq = 0
    if ( n_suth .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rSmsg, len, MPI_INTEGER, &
            n_suth, 0, comm2d, req(nreq), ier )
       do i = 1, Mloc
       do j = 1, Nghost
          sSmsg(i,j) = PHI(i,Jbeg+j-1)
       enddo
       enddo
       nreq = nreq +1
       call MPI_ISEND( sSmsg, len, MPI_INTEGER, &
            n_suth, 1, comm2d, req(nreq), ier )
    endif

    if ( n_nrth .ne. MPI_PROC_NULL ) then
       nreq = nreq + 1
       call MPI_IRECV( rNmsg, len, MPI_INTEGER, &
            n_nrth, 1, comm2d, req(nreq), ier )
       do i = 1, Mloc
       do j = 1, Nghost
          sNmsg(i,j) = PHI(i,Jend-j+1)
       enddo
       enddo
       nreq = nreq + 1
       call MPI_ISEND( sNmsg, len, MPI_INTEGER, &
            n_nrth, 0, comm2d, req(nreq), ier )
    endif

    call MPI_WAITALL( nreq, req, status, ier )

    if ( n_suth .ne. MPI_PROC_NULL ) then
       do i = 1, Mloc
       do j = 1, Nghost
          PHI(i,Jbeg-j) = rSmsg(i,j)
       enddo
       enddo
    endif

    if ( n_nrth .ne. MPI_PROC_NULL ) then
       do i = 1, Mloc
       do j = 1, Nghost
          PHI(i,Jend+j) = rNmsg(i,j)
       enddo
       enddo
    endif
END SUBROUTINE phi_int_exch



! ---------------------------------------------------
!    This is subroutine predictor
!  call
!      - GET_Eta_U_V_HU_HV
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE PREDICTOR
     USE GLOBAL
     IMPLICIT NONE

! MUSCL-Hancock Zhou et al, p.6
! solve eta
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Eta(I,J)=Eta0(i,j)-DT/2.0_SP/DX*(P(i+1,j)-P(i,j))  &
                        -DT/2.0_SP/DY*(Q(i,j+1)-Q(i,j))  




     ENDDO
     ENDDO
! solve u
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Ubar(I,J)=Ubar0(i,j)-DT/2.0_SP/DX*(Fx(i+1,j)-Fx(i,j))  &
                        -DT/2.0_SP/DY*(Fy(i,j+1)-Fy(i,j)) &
                        +DT/2.0_SP*SourceX(I,J)





     ENDDO
     ENDDO
! solve v
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend

       Vbar(I,J)=Vbar0(i,j)-DT/2.0_SP/DX*(Gx(i+1,j)-Gx(i,j))  &
                        -DT/2.0_SP/DY*(Gy(i,j+1)-Gy(i,j)) &
                        +DT/2.0_SP*SourceY(I,J)





     ENDDO
     ENDDO

     CALL GET_Eta_U_V_HU_HV

END SUBROUTINE PREDICTOR

! ---------------------------------------------------
!    This is subroutine corrector
!  called by
!      MAIN
!  call
!      GET_Eta_U_V_HU_HV
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE CORRECTOR
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),PARAMETER::n_left=-1.0_SP,n_right=1.0_SP,n_bottom=-1.0_SP,n_top=1.0_SP
     REAL(SP)::F_left,F_right,F_bottom,F_top

! MUSCL-Hancock, Zhou et al., p. 7

! solve eta
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      F_left=P(I,J)
      F_right=P(I+1,J)
      F_bottom=Q(I,J)
      F_top=Q(I,J+1)

      Eta(I,J)=Eta0(I,J)-DT/DX*(F_right*n_right+F_left*n_left) &
                             -DT/DY*(F_top*n_top+F_bottom*n_bottom)




     ENDDO
     ENDDO

! solve ubar
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      F_left=Fx(I,J)
      F_right=Fx(I+1,J)
      F_bottom=Fy(I,J)
      F_top=Fy(I,J+1)

      Ubar(I,J)=Ubar0(I,J)-DT/DX*(F_right*n_right+F_left*n_left) &
                             -DT/DY*(F_top*n_top+F_bottom*n_bottom) &
                             +DT*SourceX(I,J)





     ENDDO
     ENDDO

! solve vbar
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
      F_left=Gx(I,J)
      F_right=Gx(I+1,J)
      F_bottom=Gy(I,J)
      F_top=Gy(I,J+1)

      Vbar(I,J)=Vbar0(I,J)-DT/DX*(F_right*n_right+F_left*n_left) &
                             -DT/DY*(F_top*n_top+F_bottom*n_bottom) &
                             +DT*SourceY(I,J)





     ENDDO
     ENDDO

     CALL GET_Eta_U_V_HU_HV


END SUBROUTINE CORRECTOR





! ------------------------------------------------
! This part is not subroutines
!  DEFINITIONS OF VARIABLES
! 
!    Last Update: 09/07/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
!
! Depth(): still water depth at element point
! DepthNode(): still water depth at node
! DepthX(): still water depth at x-interface
! DepthY(): still water depth at y-interface
! Eta():   surface elevation
! Eta0(): Eta at previous time level
!  for dry point, Eta() = MinDepth+Z()
! MASK(): 1 - wet
!         0 - dry
! MASK_STRUC(): 0 - permanent dry point
! MASK9: mask for itself and 8 elements around
! 
! U():  depth-averaged u or u at the reference level (u_alpha) at element
! V():  depth-averaged v or v at the reference level (v_alpha) at element
! HU(): (dep+eta)*u at element
! HV(): (dep+eta)*v at element
! P(): HU + dispersion at x-interface
! Q(): HV + dispersion at y-interface
! Fx(): F at x-interface
! Fy(): F at y-interface
! Gx(): G at x-interface
! Gy(): G at y-interface
! Ubar(:,:,:): Ubar
! Vbar(:,:,:): Vbar

! dispersion
! U1p(:,:): x-component of V1p
! V1p(:,:): y-component of V1p

! 
! EtaRxL(): Eta Left value at x-interface
! EtaRxR(): Eta Right value at x-interface
! EtaRyL(): Eta Left value at y-interface
! EtaRyR(): Eta Right value at y-interface
! HxL():   total depth  Left value at x-interface
! HxR():   total depth  Right value at x-interface
! HyL():   total depth  Left value at y-interface
! HyR():   total depth  Right value at y-interface

! HUxL(): HU Left value at x-interface
! HUxR(): HU Right value at x-interface
! HUyL(): HV Left value at y-interface
! HUyR(): HV Right value at y-interface

! PL(): HU + dispersion, Left value at x-interface
! PR(): HU + dispersion, Right value at x-interface
! QL(): HV + dispersion, Left value at y-interface
! QR(): HV + dispersion, Right value at y-interface

! FxL = HUxL*UxL + 1/2*g*(EtaRxL^2 + 2*EtaRxL*Depthx)
! FxR = HUxR*UxR + 1/2*g*(EtaRxR^2 + 2*EtaRxR*Depthx)
! FyL = HyL*UyL*VyL
! FyR = HyR*UyR*VyR

! GxL = HxL*UxL*VxL
! GxR = HxR*UxR*VxR
! GyL = HVyL*VyL + 1/2*g*(EtaRyL^2 + 2*EtaRyL*Depthy)
! GyR = HVyR*VyR + 1/2*g*(EtaRyR^2 + 2*EtaRyR*Depthy) 






