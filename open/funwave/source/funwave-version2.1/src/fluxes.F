! ---------------------------------------------------
!    This is subroutine to calculate fluxes at four sides
!  called by
!      MAIN
!  call
!      - DelxyFun
!      - CONSTRUCTION 
!      - or FLUX_AT_INTERFACE_HLLC
!      - or FLUX_AT_INTERFACE
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE FLUXES
     USE GLOBAL
     IMPLICIT NONE

     IF(HIGH_ORDER(1:3)=='FOU') THEN
       CALL CONSTRUCTION_HO
       CALL WAVE_SPEED(Mloc,Nloc,Mloc1,Nloc1,UxL,UxR,VyL,VyR,HxL,HxR,HyL,HyR, &
            SxL,SxR,SyL,SyR)
     ELSE
       CALL DelxyFun
       CALL CONSTRUCTION 
       CALL WAVE_SPEED(Mloc,Nloc,Mloc1,Nloc1,UxL,UxR,VyL,VyR,HxL,HxR,HyL,HyR, &
            SxL,SxR,SyL,SyR)
     ENDIF

     IF(CONSTR(1:3)=='HLL')THEN
       CALL FLUX_AT_INTERFACE_HLLC
     ELSE
       CALL FLUX_AT_INTERFACE
     ENDIF

     CALL BOUNDARY_CONDITION

END SUBROUTINE FLUXES

! ---------------------------------------------------
!    This is subroutine to calculate fluxes at four sides
!  called by
!      FLUXES (predictor)
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE FLUX_AT_INTERFACE
     USE GLOBAL

!     GOTO 100
! for averaging approach for predictor
     P=0.5_SP*(PR+PL)
     Fx=0.5_SP*(FxR+FxL)
     Gx=0.5_SP*(GxR+GxL)
     Q=0.5_SP*(QR+QL)
     Fy=0.5_SP*(FyR+FyL)
     Gy=0.5_SP*(GyR+GyL)

100  CONTINUE


END SUBROUTINE FLUX_AT_INTERFACE


!-----------------------------------------------------
!    This is subroutine to calculate fluxes at four sides
!  called by
!      FLUXES (corrector)
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE FLUX_AT_INTERFACE_HLLC
     USE GLOBAL
     REAL(SP)::SR,SL,FL,FR,UL,UR

     CALL HLLC(Mloc1,Nloc,SxL,SxR,PL,PR,EtaRxL,EtaRxR,P)
     CALL HLLC(Mloc,Nloc1,SyL,SyR,QL,QR,EtaRyL,EtaRyR,Q)
     CALL HLLC(Mloc1,Nloc,SxL,SxR,FxL,FxR,HUxL,HUxR,Fx)
     CALL HLLC(Mloc,Nloc1,SyL,SyR,FyL,FyR,HUyL,HUyR,Fy)
     CALL HLLC(Mloc1,Nloc,SxL,SxR,GxL,GxR,HVxL,HVxR,Gx)
     CALL HLLC(Mloc,Nloc1,SyL,SyR,GyL,GyR,HVyL,HVyR,Gy)

END SUBROUTINE FLUX_AT_INTERFACE_HLLC

!-----------------------------------------------------
!    This is subroutine HLLC scheme
!  called by
!      FLUX_AT_INTERFACE_HLLC
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE HLLC(M,N,SL,SR,FL,FR,UL,UR,FOUT)
     USE PARAM
     INTEGER,INTENT(IN)::M,N
     REAL(SP),INTENT(IN),DIMENSION(M,N)::SL,SR,FL,FR,UL,UR
     REAL(SP),INTENT(OUT),DIMENSION(M,N)::FOUT

      DO J=1,N
      DO I=1,M     
      IF(SL(I,J)>=ZERO) THEN
        FOUT(I,J)=FL(I,J)
      ELSEIF(SR(I,J)<=ZERO) THEN
        FOUT(I,J)=FR(I,J)
      ELSE
        FOUT(I,J)=SR(I,J)*FL(I,J)-SL(I,J)*FR(I,J)+SL(I,J)*SR(I,J)*(UR(I,J)-UL(I,J))
        IF((ABS(SR(I,J)-SL(I,J)))<SMALL)THEN
         FOUT(I,J)=FOUT(I,J)/SMALL
!         !WRITE(*,*) 'WARNING: (SR-SL) AT LEFT POINT IS TOO SMALL!'
!         !WRITE(*,*)'SR-SL=',(SR(I,J)-SL(I,J)),'(I,J)=', I,J
        ELSE
         FOUT(I,J)=FOUT(I,J)/(SR(I,J)-SL(I,J))
        ENDIF
      ENDIF
      ENDDO
      ENDDO

END SUBROUTINE HLLC

! ----------------------------------------------------
!    This is subroutine to calculate derivative of x and y
!  called by 
!        FLUXES
!    call
!      - DelxFun
!      - DelyFun
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DelxyFun
     USE GLOBAL

! compute DelxFun

      CALL DelxFun(DX,Mloc,Nloc,Eta,DelxEtar)
      CALL DelxFun(DX,Mloc,Nloc,U,DelxU)
      CALL DelxFun(DX,Mloc,Nloc,V,DelxV)
      CALL DelxFun(DX,Mloc,Nloc,HU,DelxHU)
      CALL DelxFun(DX,Mloc,Nloc,HV,DelxHV)

! compute DelyFun
      CALL DelyFun(DY,Mloc,Nloc,Eta,DelyEtar)
      CALL DelyFun(DY,Mloc,Nloc,U,DelyU)
      CALL DelyFun(DY,Mloc,Nloc,V,DelyV)
      CALL DelyFun(DY,Mloc,Nloc,HV,DelyHV)
      CALL DelyFun(DY,Mloc,Nloc,HU,DelyHU)

END SUBROUTINE DelxyFun

! ----------------------------------------------------
!    This is subroutine to calculate derivative of y
!  called by 
!        DelxyFun
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DelyFun(DY,M,N,DIN,DOUT)
     USE PARAM
     IMPLICIT NONE
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DY
# endif
     INTEGER,INTENT(IN)::M,N
     REAL(SP),INTENT(IN),DIMENSION(M,N)::DIN
     REAL(SP),INTENT(OUT),DIMENSION(M,N)::DOUT
!     REAL(SP) :: VANLEER_LIMITER

! van Leer Limiter     
     DO I=1,M
     DO J=2,N-1
# if defined (CARTESIAN)
      TMP1=(DIN(I,J+1)-DIN(I,J))/DY
      TMP2=(DIN(I,J)-DIN(I,J-1))/DY
# else
      TMP1=(DIN(I,J+1)-DIN(I,J))/DY(I,J)
      TMP2=(DIN(I,J)-DIN(I,J-1))/DY(I,J-1)
# endif
      IF((ABS(TMP1)+ABS(TMP2))<SMALL)THEN
!       !WRITE(*,*)'WARNING É (a+b) in limiter x is too small','(I,J)',I,J
        DOUT(I,J)=ZERO
      ELSE
!        DOUT(I,J)=(TMP1*ABS(TMP2)+ABS(TMP1)*TMP2)/(ABS(TMP1)+ABS(TMP2))
!        DOUT(I,J)=VANLEER_LIMITER(TMP1,TMP2)
         DOUT(I,J)=(TMP1*ABS(TMP2)+ABS(TMP1)*TMP2)/(ABS(TMP1)+ABS(TMP2))
      ENDIF
     ENDDO
     ENDDO

     DO I=1,M
# if defined (CARTESIAN)
      DOUT(I,1)=(DIN(I,2)-DIN(I,1))/DY
      DOUT(I,N)=(DIN(I,N)-DIN(I,N-1))/DY
# else
      DOUT(I,1)=(DIN(I,2)-DIN(I,1))/DY(I,1)
      DOUT(I,N)=(DIN(I,N)-DIN(I,N-1))/DY(I,N)
# endif
     ENDDO

END SUBROUTINE DelyFun

! ----------------------------------------------------
!    This is subroutine to calculate derivative of x
!  called by 
!        DelxyFun
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE DelxFun(DX,M,N,DIN,DOUT)
     USE PARAM
     IMPLICIT NONE
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX
# endif
     INTEGER,INTENT(IN)::M,N
     REAL(SP),INTENT(IN),DIMENSION(M,N)::DIN
     REAL(SP),INTENT(OUT),DIMENSION(M,N)::DOUT
!     REAL(SP) :: VANLEER_LIMITER

! van Leer Limiter     
     DO I=2,M-1
     DO J=1,N
# if defined (CARTESIAN)
      TMP1=(DIN(I+1,J)-DIN(I,J))/DX
      TMP2=(DIN(I,J)-DIN(I-1,J))/DX
# else
      TMP1=(DIN(I+1,J)-DIN(I,J))/DX(I,J)
      TMP2=(DIN(I,J)-DIN(I-1,J))/DX(I-1,J)
# endif
      IF((ABS(TMP1)+ABS(TMP2))<SMALL)THEN
!       !WRITE(*,*)'WARNING É (a+b) in limiter y is too small','(I,J)',I,J
        DOUT(I,J)=ZERO
      ELSE
!        DOUT(I,J)=VANLEER_LIMITER(TMP1,TMP2)
         DOUT(I,J)=(TMP1*ABS(TMP2)+ABS(TMP1)*TMP2)/(ABS(TMP1)+ABS(TMP2))
      ENDIF
     ENDDO
     ENDDO

     DO J=1,N
# if defined (CARTESIAN)
      DOUT(1,J)=(DIN(2,J)-DIN(1,J))/DX
      DOUT(M,J)=(DIN(M,J)-DIN(M-1,J))/DX
# else
      DOUT(1,J)=(DIN(2,J)-DIN(1,J))/DX(1,J)
      DOUT(M,J)=(DIN(M,J)-DIN(M-1,J))/DX(M,J)
# endif
     ENDDO

END SUBROUTINE DelxFun

! ---------------------------------------------------
!    This is subroutine to calculate wave speed
!    no shear wave yet
!    Last Update: 01/21/2012 Fengyan Shi, University of Delaware
! Steve Brandt mentioned HxL and HxR not calculated inside ghost cells
! corrected this part 
! there's a bug found in Dmitry's case. In x direction, should use M1
! and N1 for y direction. 10/129/2012
! --------------------------------------------------
SUBROUTINE WAVE_SPEED(M,N,M1,N1,UL,UR,VL,VR,HxL,HxR,HyL,HyR,&
      SxL,SxR,SyL,SyR)
     USE PARAM
     USE GLOBAL, ONLY : Nghost
     IMPLICIT NONE
     INTEGER,INTENT(IN)::M,N,M1,N1
     REAL(SP),INTENT(IN),DIMENSION(M1,N)::UL,UR,HxL,HxR
     REAL(SP),INTENT(IN),DIMENSION(M,N1)::VL,VR,HyL,HyR
     REAL(SP),INTENT(OUT),DIMENSION(M1,N)::SxL,SxR
     REAL(SP),INTENT(OUT),DIMENSION(M,N1)::SyL,SyR         
     REAL(SP)::SQR_PHI_L,SQR_PHI_R,SQR_PHI_S,U_S


! Zhou et al., 2001 approach
! x interface
!     DO J=1,N
!     DO I=1,M
     DO J=1+Nghost,N-Nghost
     DO I=1+Nghost,M1-Nghost
       SQR_PHI_L=SQRT(GRAV*ABS(HxL(I,J)))
       SQR_PHI_R=SQRT(GRAV*ABS(HxR(I,J)))
       SQR_PHI_S=0.5*(SQR_PHI_L+SQR_PHI_R)+0.25*(UL(I,J)-UR(I,J))  
       U_S=0.5*(UL(I,J)+UR(I,J))+SQR_PHI_L-SQR_PHI_R
       SxL(I,J)=MIN(UL(I,J)-SQR_PHI_L,U_S-SQR_PHI_S)
       SxR(I,J)=MAX(UR(I,J)+SQR_PHI_R,U_S+SQR_PHI_S)
     ENDDO
     ENDDO

! ghost cells, this does not really matter
     DO J=1+Nghost,N-Nghost
      DO I=1,Nghost
       SxL(I,J)=SxL(Nghost+1,J)
       SxR(I,J)=SxR(Nghost+1,J)
      ENDDO
      DO I=M1-Nghost+1,M1
       SxL(I,J)=SxL(M1-Nghost,J)
       SxR(I,J)=SxR(M1-Nghost,J)       
      ENDDO
     ENDDO

     DO I=1,M1
       DO J=1,Nghost
         SxL(I,J)=SxL(I,Nghost+1)
         SxR(I,J)=SxR(I,Nghost+1)
       ENDDO
       DO J=N-Nghost+1,N
         SxL(I,J)=SxL(I,N-Nghost)
         SxR(I,J)=SxR(I,N-Nghost)
       ENDDO
     ENDDO

! y interface
!     DO J=1,N
!     DO I=1,M
     DO J=1+Nghost,N1-Nghost
     DO I=1+Nghost,M-Nghost
       SQR_PHI_L=SQRT(GRAV*ABS(HyL(I,J)))
       SQR_PHI_R=SQRT(GRAV*ABS(HyR(I,J)))
       SQR_PHI_S=0.5*(SQR_PHI_L+SQR_PHI_R)+0.25*(VL(I,J)-VR(I,J))  
       U_S=0.5*(VL(I,J)+VR(I,J))+SQR_PHI_L-SQR_PHI_R
       SyL(I,J)=MIN(VL(I,J)-SQR_PHI_L,U_S-SQR_PHI_S)
       SyR(I,J)=MAX(VR(I,J)+SQR_PHI_R,U_S+SQR_PHI_S)
     ENDDO
     ENDDO


! ghost cells
     DO I=1+Nghost,M-Nghost
      DO J=1,Nghost
       SyL(I,J)=SyL(I,Nghost+1)
       SyR(I,J)=SyR(I,Nghost+1)
      ENDDO
      DO J=N1-Nghost+1,N1
       SyL(I,J)=SyL(I,N1-Nghost)
       SyR(I,J)=SyR(I,N1-Nghost)       
      ENDDO
     ENDDO

     DO J=1,N1
       DO I=1,Nghost
         SyL(I,J)=SyL(Nghost+1,J)
         SyR(I,J)=SyR(Nghost+1,J)
       ENDDO
       DO I=M-Nghost+1,M
         SyL(I,J)=SyL(M-Nghost,J)
         SyR(I,J)=SyR(M-Nghost,J)
       ENDDO
     ENDDO


!     DO I=1,M
!       SyL(I,N1)=SyL(I,N)
!       SyR(I,N1)=SyR(I,N)
!     ENDDO


END SUBROUTINE WAVE_SPEED

! ---------------------------------------------------
!    This is subroutine interface construction
!  called by 
!        FLUXES
!  call
!      - CONSTRUCT_X
!      - CONSTRUCT_Y
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE CONSTRUCTION
     USE GLOBAL
     IMPLICIT NONE
# if defined (CARTESIAN)
     REAL(SP),DIMENSION(Mloc1,Nloc) :: DelxU4
     REAL(SP),DIMENSION(Mloc,Nloc1) :: DelyV4
# endif

! construct in x-direction
     CALL CONSTRUCT_X(Mloc,Mloc1,Nloc,DX,U,DelxU,UxL,UxR,Kappa)
     CALL CONSTRUCT_X(Mloc,Mloc1,Nloc,DX,V,DelxV,VxL,VxR,Kappa)
     CALL CONSTRUCT_X(Mloc,Mloc1,Nloc,DX,HU,DelxHU,HUxL,HUxR,Kappa)
     CALL CONSTRUCT_X(Mloc,Mloc1,Nloc,DX,HV,DelxHV,HVxL,HVxR,Kappa)
     CALL CONSTRUCT_X(Mloc,Mloc1,Nloc,DX,Eta,DelxEtar,EtaRxL,EtaRxR,Kappa)
     HxL=EtaRxL+Depthx
     HxR=EtaRxR+Depthx
# if defined (CARTESIAN)
! dispersion
     IF(DISPERSION)THEN
      CALL DelxFun(DX,Mloc,Nloc,U4,DelxU4)
      CALL CONSTRUCT_X(Mloc,Mloc1,Nloc,DX,U4,DelxU4,U4xL,U4xR,Kappa)
     ENDIF
# endif

# if defined(CARTESIAN)

     PL(1:Mloc,1:Nloc)=HUxL(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxL(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)

     PL(Mloc1,1:Nloc)=HUxL(Mloc1,1:Nloc) &
     + Gamma1*HxL(Mloc1,1:Nloc)*U4xL(Mloc1,1:Nloc)


     PR(1:Mloc,1:Nloc)=HUxR(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxR(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)

     PR(Mloc1,1:Nloc)=HUxR(Mloc1,1:Nloc) &
     + Gamma1*HxR(Mloc1,1:Nloc)*U4xR(Mloc1,1:Nloc)
     
# else

# if defined(ZALPHA)
     PL(1:Mloc,1:Nloc)=HUxL(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxL(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)

     PL(Mloc1,1:Nloc)=HUxL(Mloc1,1:Nloc) &
     + Gamma1*HxL(Mloc1,1:Nloc)*U4xL(Mloc1,1:Nloc)


     PR(1:Mloc,1:Nloc)=HUxR(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxR(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)

     PR(Mloc1,1:Nloc)=HUxR(Mloc1,1:Nloc) &
     + Gamma1*HxR(Mloc1,1:Nloc)*U4xR(Mloc1,1:Nloc)
# else
     PL=HUxL 
     PR=HUxR
# endif

# endif


# if defined (CARTESIAN)
      FxL(1:Mloc,1:Nloc)=Gamma3*PL(1:Mloc,1:Nloc)*(UxL(1:Mloc,1:Nloc)  &
         +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(1:Mloc,1:Nloc))*(EtaRxL(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxL(Mloc1,1:Nloc)=Gamma3*PL(Mloc1,1:Nloc)*(UxL(Mloc1,1:Nloc)  &
         +Gamma1*U4xL(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(Mloc1,1:Nloc))*(EtaRxL(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))


      FxR(1:Mloc,1:Nloc)=Gamma3*PR(1:Mloc,1:Nloc)*(UxR(1:Mloc,1:Nloc)  &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(1:Mloc,1:Nloc))*(EtaRxR(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxR(Mloc1,1:Nloc)=Gamma3*PR(Mloc1,1:Nloc)*(UxR(Mloc1,1:Nloc)  &
        +Gamma1*U4xR(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(Mloc1,1:Nloc))*(EtaRxR(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))

# else
# if defined (ZALPHA)
      FxL(1:Mloc,1:Nloc)=Gamma3*PL(1:Mloc,1:Nloc)*(UxL(1:Mloc,1:Nloc)  &
         +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(1:Mloc,1:Nloc))*(EtaRxL(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxL(Mloc1,1:Nloc)=Gamma3*PL(Mloc1,1:Nloc)*(UxL(Mloc1,1:Nloc)  &
         +Gamma1*U4xL(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(Mloc1,1:Nloc))*(EtaRxL(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))


      FxR(1:Mloc,1:Nloc)=Gamma3*PR(1:Mloc,1:Nloc)*(UxR(1:Mloc,1:Nloc)  &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(1:Mloc,1:Nloc))*(EtaRxR(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxR(Mloc1,1:Nloc)=Gamma3*PR(Mloc1,1:Nloc)*(UxR(Mloc1,1:Nloc)  &
        +Gamma1*U4xR(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(Mloc1,1:Nloc))*(EtaRxR(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))
# else
      FxL=Gamma3*PL*(UxL) &
        +0.5*GRAV*((EtaRxL)*(EtaRxL)*Gamma3+2.0_SP*(EtaRxL)*(Depthx))
      FxR=Gamma3*PR*(UxR) &
        +0.5*GRAV*((EtaRxR)*(EtaRxR)*Gamma3+2.0_SP*(EtaRxR)*(Depthx))
# endif
# endif

     GxL=HxL*UxL*VxL*Gamma3
     GxR=HxR*UxR*VxR*Gamma3
     
! construct in y-direction
     CALL CONSTRUCT_Y(Mloc,Nloc,Nloc1,DY,U,DelyU,UyL,UyR,Kappa)
     CALL CONSTRUCT_Y(Mloc,Nloc,Nloc1,DY,V,DelyV,VyL,VyR,Kappa)
     CALL CONSTRUCT_Y(Mloc,Nloc,Nloc1,DY,HV,DelyHV,HVyL,HVyR,Kappa)
     CALL CONSTRUCT_Y(Mloc,Nloc,Nloc1,DY,HU,DelyHU,HUyL,HUyR,Kappa)
     CALL CONSTRUCT_Y(Mloc,Nloc,Nloc1,DY,Eta,DelyEtar,EtaRyL,EtaRyR,Kappa)
     HyL=EtaRyL+Depthy
     HyR=EtaRyR+Depthy

# if defined (CARTESIAN)
     IF(DISPERSION)THEN
      CALL DelyFun(DY,Mloc,Nloc,V4,DelyV4)
      CALL CONSTRUCT_Y(Mloc,Nloc,Nloc1,DY,V4,DelyV4,V4yL,V4yR,Kappa)
     ENDIF
# endif

# if defined (CARTESIAN)
     QL(1:Mloc,1:Nloc)=HVyL(1:Mloc,1:Nloc)   &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyL(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)

     QL(1:Mloc,Nloc1)=HVyL(1:Mloc,Nloc1)   &
       + Gamma1*HyL(1:Mloc,Nloc1)*V4yL(1:Mloc,Nloc1)


     QR(1:Mloc,1:Nloc)=HVyR(1:Mloc,1:Nloc)    &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyR(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)

     QR(1:Mloc,Nloc1)=HVyR(1:Mloc,Nloc1)    &
       + Gamma1*HyR(1:Mloc,Nloc1)*V4yR(1:Mloc,Nloc1)

# else

# if defined (ZALPHA)
     QL(1:Mloc,1:Nloc)=HVyL(1:Mloc,1:Nloc)   &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyL(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)

     QL(1:Mloc,Nloc1)=HVyL(1:Mloc,Nloc1)   &
       + Gamma1*HyL(1:Mloc,Nloc1)*V4yL(1:Mloc,Nloc1)


     QR(1:Mloc,1:Nloc)=HVyR(1:Mloc,1:Nloc)    &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyR(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)

     QR(1:Mloc,Nloc1)=HVyR(1:Mloc,Nloc1)    &
       + Gamma1*HyR(1:Mloc,Nloc1)*V4yR(1:Mloc,Nloc1)
# else
     QL=HVyL 
     QR=HVyR
# endif

# endif

     FyL=Gamma3*HyL*UyL*VyL*Gamma3
     FyR=Gamma3*HyR*UyR*VyR*Gamma3

# if defined (CARTESIAN)
     GyL(1:Mloc,1:Nloc)=Gamma3*QL(1:Mloc,1:Nloc)*(VyL(1:Mloc,1:Nloc)   &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,1:Nloc))*(EtaRyL(1:Mloc,1:Nloc))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyL(1:Mloc,Nloc1)=Gamma3*QL(1:Mloc,Nloc1)*(VyL(1:Mloc,Nloc1)   &
        +Gamma1*V4yL(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,Nloc1))*(EtaRyL(1:Mloc,Nloc1))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))


     GyR(1:Mloc,1:Nloc)=Gamma3*QR(1:Mloc,1:Nloc)*(VyR(1:Mloc,1:Nloc)    &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,1:Nloc))*(EtaRyR(1:Mloc,1:Nloc))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyR(1:Mloc,Nloc1)=Gamma3*QR(1:Mloc,Nloc1)*(VyR(1:Mloc,Nloc1)    &
        +Gamma1*V4yR(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,Nloc1))*(EtaRyR(1:Mloc,Nloc1))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))

# else
# if defined(ZALPHA)
     GyL(1:Mloc,1:Nloc)=Gamma3*QL(1:Mloc,1:Nloc)*(VyL(1:Mloc,1:Nloc)   &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,1:Nloc))*(EtaRyL(1:Mloc,1:Nloc))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyL(1:Mloc,Nloc1)=Gamma3*QL(1:Mloc,Nloc1)*(VyL(1:Mloc,Nloc1)   &
        +Gamma1*V4yL(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,Nloc1))*(EtaRyL(1:Mloc,Nloc1))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))


     GyR(1:Mloc,1:Nloc)=Gamma3*QR(1:Mloc,1:Nloc)*(VyR(1:Mloc,1:Nloc)    &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,1:Nloc))*(EtaRyR(1:Mloc,1:Nloc))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyR(1:Mloc,Nloc1)=Gamma3*QR(1:Mloc,Nloc1)*(VyR(1:Mloc,Nloc1)    &
        +Gamma1*V4yR(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,Nloc1))*(EtaRyR(1:Mloc,Nloc1))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))
# else
     GyL=Gamma3*QL*(VyL) &
        +0.5*GRAV*((EtaRyL)*(EtaRyL)*Gamma3+2.0_SP*(EtaRyL)*(Depthy))
     GyR=Gamma3*QR*(VyR) &
        +0.5*GRAV*((EtaRyR)*(EtaRyR)*Gamma3+2.0_SP*(EtaRyR)*(Depthy))
# endif
# endif
     
END SUBROUTINE CONSTRUCTION

! ---------------------------------------------------
!    This is subroutine construct variable in Y direction
!  called by 
!        CONSTRUCTION
! 
!    Last Update: 06/01/2011 Fengyan Shi, University of Delaware
! Harris replaced limiter function, saving 1/3 time
! --------------------------------------------------
SUBROUTINE CONSTRUCT_Y(M,N,N1,DY,Vin,Din,OutL,OutR,Kappa)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN)::M,N,N1
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DY
# endif
     REAL(SP),INTENT(IN),DIMENSION(M,N)::Vin,Din
     REAL(SP),INTENT(IN) :: Kappa
     REAL(SP),INTENT(OUT),DIMENSION(M,N1)::OutL,OutR

     DO J=2,N
     DO I=1,M
# if defined (CARTESIAN)
       OutL(I,J)=Vin(I,J-1)+0.25*DY      &
               *((1.0_SP-kappa)*Din(I,J-1)+(1.0_SP+kappa)*Din(I,J-1))
       OutR(I,J)=Vin(I,J)-0.25*DY     &
               *((1.0_SP+kappa)*Din(I,J)+(1.0_SP-kappa)*Din(I,J))
# else
       OutL(I,J)=Vin(I,J-1)+0.25*DY(I,J-1)      &
               *((1.0_SP-kappa)*Din(I,J-1)+(1.0_SP+kappa)*Din(I,J-1))
       OutR(I,J)=Vin(I,J)-0.25*DY(I,J)     &
               *((1.0_SP+kappa)*Din(I,J)+(1.0_SP-kappa)*Din(I,J))
# endif
     ENDDO
     ENDDO

     DO I=1,M
# if defined (CARTESIAN)
       OutL(I,N1)=Vin(I,N)+0.25*DY      &
               *((1.0_SP-kappa)*Din(I,N)+(1.0_SP+kappa)*Din(I,N))
       OutR(I,1)=Vin(I,1)-0.25*DY     &
               *((1.0_SP+kappa)*Din(I,1)+(1.0_SP-kappa)*Din(I,1))
# else
       OutL(I,N1)=Vin(I,N)+0.25*DY(I,N)      &
               *((1.0_SP-kappa)*Din(I,N)+(1.0_SP+kappa)*Din(I,N))
       OutR(I,1)=Vin(I,1)-0.25*DY(I,1)     &
               *((1.0_SP+kappa)*Din(I,1)+(1.0_SP-kappa)*Din(I,1))
# endif
       OutL(I,1)=OutR(I,1)
       OutR(I,N1)=OutL(I,N1)
     ENDDO

END SUBROUTINE CONSTRUCT_Y

! ---------------------------------------------------
!    This is subroutine construct variable in X direction
!  called by 
!        CONSTRUCTION
!    Last Update: 06/01/2011 Fengyan Shi, University of Delaware
! Harris replaced limiter function, saving 1/3 time
! add third order in the code. 
! --------------------------------------------------
SUBROUTINE CONSTRUCT_X(M,M1,N,DX,Vin,Din,OutL,OutR,Kappa)
     USE PARAM
     IMPLICIT NONE
     INTEGER,INTENT(IN)::M,M1,N
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX
# endif
     REAL(SP),INTENT(IN),DIMENSION(M,N)::Vin,Din
     REAL(SP),INTENT(IN)::Kappa
     REAL(SP),INTENT(OUT),DIMENSION(M1,N)::OutL,OutR

     DO J=1,N
     DO I=2,M
# if defined (CARTESIAN)
       OutL(I,J)=Vin(I-1,J)+0.25*DX      &
               *((1.0_SP-kappa)*Din(I-1,J)+(1.0_SP+kappa)*Din(I-1,J))
       OutR(I,J)=Vin(I,J)-0.25*DX     &
               *((1.0_SP+kappa)*Din(I,J)+(1.0_SP-kappa)*Din(I,J))
# else
       OutL(I,J)=Vin(I-1,J)+0.25*DX(I-1,J)      &
               *((1.0_SP-kappa)*Din(I-1,J)+(1.0_SP+kappa)*Din(I-1,J))
       OutR(I,J)=Vin(I,J)-0.25*DX(I,J)     &
               *((1.0_SP+kappa)*Din(I,J)+(1.0_SP-kappa)*Din(I,J))
# endif
     ENDDO
     ENDDO

     DO J=1,N
# if defined (CARTESIAN)
       OutL(M1,J)=Vin(M,J)+0.25*DX      &
               *((1.0_SP-kappa)*Din(M,J)+(1.0_SP+kappa)*Din(M,J))
       OutR(1,J)=Vin(1,J)-0.25*DX     &
               *((1.0_SP+kappa)*Din(1,J)+(1.0_SP-kappa)*Din(1,J))
# else
       OutL(M1,J)=Vin(M,J)+0.25*DX(M,J)      &
               *((1.0_SP-kappa)*Din(M,J)+(1.0_SP+kappa)*Din(M,J))
       OutR(1,J)=Vin(1,J)-0.25*DX(1,J)     &
               *((1.0_SP+kappa)*Din(1,J)+(1.0_SP-kappa)*Din(1,J))
# endif
       OutL(1,J)=OutR(1,J)
       OutR(M1,J)=OutL(M1,J)
     ENDDO

END SUBROUTINE CONSTRUCT_X

! ---------------------------------------------------------------
!    high-order interface construction (Yamamoto et al.,1998)
!    Called by 
!        FLUXES
!    Call
!        CONSTRUCT_HO_X
!        CONSTRUCT_HO_Y
!    Update: 09/07/2010 Fengyan Shi
!    1) use dummy variables
!    Update: 05/27/2010 Gangfeng Ma, University of Delaware
! --------------------------------------------------
SUBROUTINE CONSTRUCTION_HO
     USE GLOBAL
     IMPLICIT NONE
# if defined (CARTESIAN)
     REAL(SP),DIMENSION(Mloc1,Nloc) :: DelxU4
     REAL(SP),DIMENSION(Mloc,Nloc1) :: DelyV4
# endif

! construct in x-direction
     CALL CONSTRUCT_HO_X(Mloc,Nloc,Mloc1,Ibeg,Iend,Jbeg,Jend,DX,MASK,U,UxL,UxR)
     CALL CONSTRUCT_HO_X(Mloc,Nloc,Mloc1,Ibeg,Iend,Jbeg,Jend,DX,MASK,V,VxL,VxR)
     CALL CONSTRUCT_HO_X(Mloc,Nloc,Mloc1,Ibeg,Iend,Jbeg,Jend,DX,MASK,HU,HUxL,HUxR)
     CALL CONSTRUCT_HO_X(Mloc,Nloc,Mloc1,Ibeg,Iend,Jbeg,Jend,DX,MASK,HV,HVxL,HVxR)
     CALL CONSTRUCT_HO_X(Mloc,Nloc,Mloc1,Ibeg,Iend,Jbeg,Jend,DX,MASK,Eta,EtaRxL,EtaRxR)

! dispersion
     HxL=EtaRxL+Depthx
     HxR=EtaRxR+Depthx
# if defined (CARTESIAN)
     IF(DISPERSION)THEN
       CALL CONSTRUCT_HO_X(Mloc,Nloc,Mloc1,Ibeg,Iend,Jbeg,Jend,DX,MASK,U4,U4xL,U4xR)
     ENDIF

     PL(1:Mloc,1:Nloc)=HUxL(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxL(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)

     PL(Mloc1,1:Nloc)=HUxL(Mloc1,1:Nloc) &
     + Gamma1*HxL(Mloc1,1:Nloc)*U4xL(Mloc1,1:Nloc)


     PR(1:Mloc,1:Nloc)=HUxR(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxR(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)

     PR(Mloc1,1:Nloc)=HUxR(Mloc1,1:Nloc) &
     + Gamma1*HxR(Mloc1,1:Nloc)*U4xR(Mloc1,1:Nloc)

      FxL(1:Mloc,1:Nloc)=Gamma3*PL(1:Mloc,1:Nloc)*(UxL(1:Mloc,1:Nloc)  &
         +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(1:Mloc,1:Nloc))*(EtaRxL(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxL(Mloc1,1:Nloc)=Gamma3*PL(Mloc1,1:Nloc)*(UxL(Mloc1,1:Nloc)  &
         +Gamma1*U4xL(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(Mloc1,1:Nloc))*(EtaRxL(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))


      FxR(1:Mloc,1:Nloc)=Gamma3*PR(1:Mloc,1:Nloc)*(UxR(1:Mloc,1:Nloc)  &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(1:Mloc,1:Nloc))*(EtaRxR(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxR(Mloc1,1:Nloc)=Gamma3*PR(Mloc1,1:Nloc)*(UxR(Mloc1,1:Nloc)  &
        +Gamma1*U4xR(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(Mloc1,1:Nloc))*(EtaRxR(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))

# else
# if defined(ZALPHA)
     IF(DISPERSION)THEN
       CALL CONSTRUCT_HO_X(Mloc,Nloc,Mloc1,Ibeg,Iend,Jbeg,Jend,DX,MASK,U4,U4xL,U4xR)
     ENDIF
     PL(1:Mloc,1:Nloc)=HUxL(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxL(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)

     PL(Mloc1,1:Nloc)=HUxL(Mloc1,1:Nloc) &
     + Gamma1*HxL(Mloc1,1:Nloc)*U4xL(Mloc1,1:Nloc)


     PR(1:Mloc,1:Nloc)=HUxR(1:Mloc,1:Nloc) &
     + Gamma1*MASK9(1:Mloc,1:Nloc)*HxR(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)

     PR(Mloc1,1:Nloc)=HUxR(Mloc1,1:Nloc) &
     + Gamma1*HxR(Mloc1,1:Nloc)*U4xR(Mloc1,1:Nloc)

      FxL(1:Mloc,1:Nloc)=Gamma3*PL(1:Mloc,1:Nloc)*(UxL(1:Mloc,1:Nloc)  &
         +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(1:Mloc,1:Nloc))*(EtaRxL(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxL(Mloc1,1:Nloc)=Gamma3*PL(Mloc1,1:Nloc)*(UxL(Mloc1,1:Nloc)  &
         +Gamma1*U4xL(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxL(Mloc1,1:Nloc))*(EtaRxL(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxL(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))


      FxR(1:Mloc,1:Nloc)=Gamma3*PR(1:Mloc,1:Nloc)*(UxR(1:Mloc,1:Nloc)  &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*U4xR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(1:Mloc,1:Nloc))*(EtaRxR(1:Mloc,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(1:Mloc,1:Nloc))*(Depthx(1:Mloc,1:Nloc)))

      FxR(Mloc1,1:Nloc)=Gamma3*PR(Mloc1,1:Nloc)*(UxR(Mloc1,1:Nloc)  &
        +Gamma1*U4xR(Mloc1,1:Nloc)) &
        +0.5*GRAV*((EtaRxR(Mloc1,1:Nloc))*(EtaRxR(Mloc1,1:Nloc))*Gamma3  &
        +2.0_SP*(EtaRxR(Mloc1,1:Nloc))*(Depthx(Mloc1,1:Nloc)))
# else
     PL=HUxL
     PR=HUxR
     FxL=Gamma3*PL*UxL &
        +0.5*GRAV*((EtaRxL)*(EtaRxL)*Gamma3+2.0_SP*(EtaRxL)*(Depthx))
     FxR=Gamma3*PR*UxR &
        +0.5*GRAV*((EtaRxR)*(EtaRxR)*Gamma3+2.0_SP*(EtaRxR)*(Depthx))
# endif
# endif

     GxL=Gamma3*HxL*UxL*VxL
     GxR=Gamma3*HxR*UxR*VxR
     
! construct in y-direction
     CALL CONSTRUCT_HO_Y(Mloc,Nloc,Nloc1,Ibeg,Iend,Jbeg,Jend,DY,MASK,U,UyL,UyR)
     CALL CONSTRUCT_HO_Y(Mloc,Nloc,Nloc1,Ibeg,Iend,Jbeg,Jend,DY,MASK,V,VyL,VyR)
     CALL CONSTRUCT_HO_Y(Mloc,Nloc,Nloc1,Ibeg,Iend,Jbeg,Jend,DY,MASK,HV,HVyL,HVyR)
     CALL CONSTRUCT_HO_Y(Mloc,Nloc,Nloc1,Ibeg,Iend,Jbeg,Jend,DY,MASK,HU,HUyL,HUyR)
     CALL CONSTRUCT_HO_Y(Mloc,Nloc,Nloc1,Ibeg,Iend,Jbeg,Jend,DY,MASK,Eta,EtaRyL,EtaRyR)

! dispersion
     HyL=EtaRyL+Depthy
     HyR=EtaRyR+Depthy
# if defined (CARTESIAN)
     IF(DISPERSION)THEN
       CALL CONSTRUCT_HO_Y(Mloc,Nloc,Nloc1,Ibeg,Iend,Jbeg,Jend,DY,MASK,V4,V4yL,V4yR)
     ENDIF

     QL(1:Mloc,1:Nloc)=HVyL(1:Mloc,1:Nloc)   &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyL(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)

     QL(1:Mloc,Nloc1)=HVyL(1:Mloc,Nloc1)   &
       + Gamma1*HyL(1:Mloc,Nloc1)*V4yL(1:Mloc,Nloc1)


     QR(1:Mloc,1:Nloc)=HVyR(1:Mloc,1:Nloc)    &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyR(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)

     QR(1:Mloc,Nloc1)=HVyR(1:Mloc,Nloc1)    &
       + Gamma1*HyR(1:Mloc,Nloc1)*V4yR(1:Mloc,Nloc1)

     GyL(1:Mloc,1:Nloc)=Gamma3*QL(1:Mloc,1:Nloc)*(VyL(1:Mloc,1:Nloc)   &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,1:Nloc))*(EtaRyL(1:Mloc,1:Nloc))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyL(1:Mloc,Nloc1)=Gamma3*QL(1:Mloc,Nloc1)*(VyL(1:Mloc,Nloc1)   &
        +Gamma1*V4yL(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,Nloc1))*(EtaRyL(1:Mloc,Nloc1))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))


     GyR(1:Mloc,1:Nloc)=Gamma3*QR(1:Mloc,1:Nloc)*(VyR(1:Mloc,1:Nloc)    &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,1:Nloc))*(EtaRyR(1:Mloc,1:Nloc))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyR(1:Mloc,Nloc1)=Gamma3*QR(1:Mloc,Nloc1)*(VyR(1:Mloc,Nloc1)    &
        +Gamma1*V4yR(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,Nloc1))*(EtaRyR(1:Mloc,Nloc1))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))

# else

# if defined (ZALPHA)
     IF(DISPERSION)THEN
       CALL CONSTRUCT_HO_Y(Mloc,Nloc,Nloc1,Ibeg,Iend,Jbeg,Jend,DY,MASK,V4,V4yL,V4yR)
     ENDIF

     QL(1:Mloc,1:Nloc)=HVyL(1:Mloc,1:Nloc)   &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyL(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)

     QL(1:Mloc,Nloc1)=HVyL(1:Mloc,Nloc1)   &
       + Gamma1*HyL(1:Mloc,Nloc1)*V4yL(1:Mloc,Nloc1)


     QR(1:Mloc,1:Nloc)=HVyR(1:Mloc,1:Nloc)    &
       + Gamma1*MASK9(1:Mloc,1:Nloc)*HyR(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)

     QR(1:Mloc,Nloc1)=HVyR(1:Mloc,Nloc1)    &
       + Gamma1*HyR(1:Mloc,Nloc1)*V4yR(1:Mloc,Nloc1)

     GyL(1:Mloc,1:Nloc)=Gamma3*QL(1:Mloc,1:Nloc)*(VyL(1:Mloc,1:Nloc)   &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yL(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,1:Nloc))*(EtaRyL(1:Mloc,1:Nloc))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyL(1:Mloc,Nloc1)=Gamma3*QL(1:Mloc,Nloc1)*(VyL(1:Mloc,Nloc1)   &
        +Gamma1*V4yL(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyL(1:Mloc,Nloc1))*(EtaRyL(1:Mloc,Nloc1))*Gamma3   &
        +2.0_SP*(EtaRyL(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))


     GyR(1:Mloc,1:Nloc)=Gamma3*QR(1:Mloc,1:Nloc)*(VyR(1:Mloc,1:Nloc)    &
        +Gamma1*MASK9(1:Mloc,1:Nloc)*V4yR(1:Mloc,1:Nloc)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,1:Nloc))*(EtaRyR(1:Mloc,1:Nloc))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,1:Nloc))*(Depthy(1:Mloc,1:Nloc)))

     GyR(1:Mloc,Nloc1)=Gamma3*QR(1:Mloc,Nloc1)*(VyR(1:Mloc,Nloc1)    &
        +Gamma1*V4yR(1:Mloc,Nloc1)) &
        +0.5*GRAV*((EtaRyR(1:Mloc,Nloc1))*(EtaRyR(1:Mloc,Nloc1))*Gamma3    &
        +2.0_SP*(EtaRyR(1:Mloc,Nloc1))*(Depthy(1:Mloc,Nloc1)))
# else
     QL=HVyL
     QR=HVyR
     GyL=Gamma3*QL*VyL &
        +0.5*GRAV*((EtaRyL)*(EtaRyL)*Gamma3+2.0_SP*(EtaRyL)*(Depthy))
     GyR=Gamma3*QR*VyR &
        +0.5*GRAV*((EtaRyR)*(EtaRyR)*Gamma3+2.0_SP*(EtaRyR)*(Depthy))
# endif
# endif
     FyL=Gamma3*HyL*UyL*VyL
     FyR=Gamma3*HyR*UyR*VyR
     
END SUBROUTINE CONSTRUCTION_HO

! ------------------------------------------------------------------
!    This is subroutine high-order construct variable in X direction
!    Called by 
!        CONSTRUCTION_HO
!    Last Update: 06/01/2011 Fengyan Shi, University of Delaware
! Harris replaced limiter function, saving 1/3 time
! -------------------------------------------------------------------
SUBROUTINE CONSTRUCT_HO_X(M,N,M1,Ibeg,Iend,Jbeg,Jend,DX,MASK,Vin,OutL,OutR)
     USE PARAM
     IMPLICIT NONE
     INTEGER, INTENT(IN)::M,N,M1,Ibeg,Iend,Jbeg,Jend
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX
# endif
     REAL(SP),INTENT(IN),DIMENSION(M,N)::Vin
     INTEGER, INTENT(IN),DIMENSION(M,N):: MASK
     REAL(SP),INTENT(OUT),DIMENSION(M1,N)::OutL,OutR

     REAL(SP),DIMENSION(M,N) :: Din
     REAL(SP) :: TXP1,TXP2,TXP3,TXP4,DVP1,DVP2,DVP3

     ! estimate Din first
     Din=0.0_SP
     DO J=Jbeg,Jend
     DO I=Ibeg-1,Iend+2
       TXP1=Vin(I-1,J)-Vin(I-2,J)
       TXP2=Vin(I,J)-Vin(I-1,J)
       TXP3=Vin(I+1,J)-Vin(I,J)

       if (TXP1.ge.0.0_SP) then
          DVP1=MAX(0.0_SP,MIN(TXP1,2.0*TXP2,2.0*TXP3))          
       else
          DVP1=MIN(0.0_SP,MAX(TXP1,2.0*TXP2,2.0*TXP3))          
       endif
       if (TXP2.ge.0.0_SP) then
          DVP2=MAX(0.0_SP,MIN(TXP2,2.0*TXP3,2.0*TXP1))          
       else
          DVP2=MIN(0.0_SP,MAX(TXP2,2.0*TXP3,2.0*TXP1))          
       endif
       if (TXP3.ge.0.0_SP) then
          DVP3=MAX(0.0_SP,MIN(TXP3,2.0*TXP1,2.0*TXP2))          
       else
          DVP3=MIN(0.0_SP,MAX(TXP3,2.0*TXP1,2.0*TXP2))          
       endif

! dry D-2 I-1 I D+1, lower-order
       IF(MASK(I-2,J)==0.OR.MASK(I+1,J)==0)THEN
       TXP2=Vin(I,J)-Vin(I-1,J)
       TXP1=TXP2
       TXP3=TXP2
       if (TXP1.ge.0.0_SP) then
          DVP1=MAX(0.0_SP,MIN(TXP1,2.0*TXP2,2.0*TXP3))          
       else
          DVP1=MIN(0.0_SP,MAX(TXP1,2.0*TXP2,2.0*TXP3))          
       endif
       if (TXP2.ge.0.0_SP) then
          DVP2=MAX(0.0_SP,MIN(TXP2,2.0*TXP3,2.0*TXP1))          
       else
          DVP2=MIN(0.0_SP,MAX(TXP2,2.0*TXP3,2.0*TXP1))          
       endif
       if (TXP3.ge.0.0_SP) then
          DVP3=MAX(0.0_SP,MIN(TXP3,2.0*TXP1,2.0*TXP2))          
       else
          DVP3=MIN(0.0_SP,MAX(TXP3,2.0*TXP1,2.0*TXP2))          
       endif
! here actually DVP1=DVP2=DVP3=TXP2
       ENDIF    
! dry I-2 D-1 D I+1, zero gradient
       IF(MASK(I-1,J)==0.OR.MASK(I,J)==0)THEN
       DVP1=ZERO
       DVP2=ZERO
       DVP3=ZERO
       ENDIF
       
       Din(I,J)=TXP2-1.0_SP/6.0_SP*(DVP3-2.0_SP*DVP2+DVP1)
     ENDDO
     ENDDO 

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend+1
! Jeff modified the following statements 02/14/2011
       if (Din(I-1,J).ge.0.0_SP) then
          TXP1=MAX(0.0_SP,MIN(Din(I-1,J),4.0_SP*Din(I,J)))
       else
          TXP1=MIN(0.0_SP,MAX(Din(I-1,J),4.0_SP*Din(I,J)))
       endif
       if (Din(I,J).ge.0.0_SP) then
          TXP2=MAX(0.0_SP,MIN(Din(I,J),4.0_SP*Din(I-1,J)))
       else
          TXP2=MIN(0.0_SP,MAX(Din(I,J),4.0_SP*Din(I-1,J)))
       endif
! there was a HUGE bug here, 12 should versus 43, fixed. fyshi
       if (Din(I,J).ge.0.0_SP) then
          TXP4=MAX(0.0_SP,MIN(Din(I,J),4.0_SP*Din(I+1,J)))
       else
          TXP4=MIN(0.0_SP,MAX(Din(I,J),4.0_SP*Din(I+1,J)))
       endif
       if (Din(I+1,J).ge.0.0_SP) then
          TXP3=MAX(0.0_SP,MIN(Din(I+1,J),4.0_SP*Din(I,J)))
       else
          TXP3=MIN(0.0_SP,MAX(Din(I+1,J),4.0_SP*Din(I,J)))
       endif
       
       OutL(I,J)=Vin(I-1,J)+1.0_SP/6.0_SP*(TXP1+2.0_SP*TXP2)
       OutR(I,J)=Vin(I,J)-1.0_SP/6.0_SP*(TXP3+2.0_SP*TXP4)
     ENDDO
     ENDDO

END SUBROUTINE CONSTRUCT_HO_X


! ------------------------------------------------------------------
!    This is subroutine high-order construct variable in Y direction
!    Called by 
!        CONSTRUCTION
!    Last Update: 06/01/2011 Fengyan Shi, University of Delaware
! Harris replaced limiter function, saving 1/3 time
! -----------------------------------------------------------------
SUBROUTINE CONSTRUCT_HO_Y(M,N,N1,Ibeg,Iend,Jbeg,Jend,DY,MASK,Vin,OutL,OutR)
     USE PARAM
     IMPLICIT NONE
     INTEGER, INTENT(IN)::M,N,N1,Ibeg,Iend,Jbeg,Jend
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DY
# endif
     REAL(SP),INTENT(IN),DIMENSION(M,N)::Vin
     INTEGER, INTENT(IN),DIMENSION(M,N):: MASK
     REAL(SP),INTENT(OUT),DIMENSION(M,N1)::OutL,OutR

     REAL(SP),DIMENSION(M,N) :: Din
     REAL(SP) :: TYP1,TYP2,TYP3,TYP4,DVP1,DVP2,DVP3

     ! estimate Din first
     Din=0.0_SP
     DO J=Jbeg-1,Jend+2
     DO I=Ibeg,Iend
       TYP1=Vin(I,J-1)-Vin(I,J-2)
       TYP2=Vin(I,J)-Vin(I,J-1)
       TYP3=Vin(I,J+1)-Vin(I,J)

       if (TYP1.ge.0.0_SP) then
          DVP1=MAX(0.0_SP,MIN(TYP1,2.0*TYP2,2.0*TYP3))          
       else
          DVP1=MIN(0.0_SP,MAX(TYP1,2.0*TYP2,2.0*TYP3))          
       endif
       if (TYP2.ge.0.0_SP) then
          DVP2=MAX(0.0_SP,MIN(TYP2,2.0*TYP3,2.0*TYP1))          
       else
          DVP2=MIN(0.0_SP,MAX(TYP2,2.0*TYP3,2.0*TYP1))          
       endif
       if (TYP3.ge.0.0_SP) then
          DVP3=MAX(0.0_SP,MIN(TYP3,2.0*TYP1,2.0*TYP2))          
       else
          DVP3=MIN(0.0_SP,MAX(TYP3,2.0*TYP1,2.0*TYP2))          
       endif

! dry D-2 J-1 J D+1, lower-order
       IF(MASK(I,J-2)==0.OR.MASK(I,J+1)==0)THEN
       TYP2=Vin(I,J)-Vin(I,J-1)
       TYP1=TYP2
       TYP3=TYP2
       if (TYP1.ge.0.0_SP) then
          DVP1=MAX(0.0_SP,MIN(TYP1,2.0*TYP2,2.0*TYP3))          
       else
          DVP1=MIN(0.0_SP,MAX(TYP1,2.0*TYP2,2.0*TYP3))          
       endif
       if (TYP2.ge.0.0_SP) then
          DVP2=MAX(0.0_SP,MIN(TYP2,2.0*TYP3,2.0*TYP1))          
       else
          DVP2=MIN(0.0_SP,MAX(TYP2,2.0*TYP3,2.0*TYP1))          
       endif
       if (TYP3.ge.0.0_SP) then
          DVP3=MAX(0.0_SP,MIN(TYP3,2.0*TYP1,2.0*TYP2))          
       else
          DVP3=MIN(0.0_SP,MAX(TYP3,2.0*TYP1,2.0*TYP2))          
       endif
! here actually DVP1=DVP2=DVP3=TYP2
       ENDIF    
! dry J-2 D-1 D J+1, zero gradient
       IF(MASK(I,J-1)==0.OR.MASK(I,J)==0)THEN
       DVP1=ZERO
       DVP2=ZERO
       DVP3=ZERO
       ENDIF

       Din(I,J)=TYP2-1.0_SP/6.0_SP*(DVP3-2.0_SP*DVP2+DVP1)
     ENDDO
     ENDDO     


     DO J=Jbeg,Jend+1
     DO I=Ibeg,Iend
! Jeff modified the following statements 02/14/2011
       if (Din(I,J-1).ge.0.0_SP) then
          TYP1=MAX(0.0_SP,MIN(Din(I,J-1),4.0_SP*Din(I,J)))
       else
          TYP1=MIN(0.0_SP,MAX(Din(I,J-1),4.0_SP*Din(I,J)))
       endif
       if (Din(I,J).ge.0.0_SP) then
          TYP2=MAX(0.0_SP,MIN(Din(I,J),4.0_SP*Din(I,J-1)))
       else
          TYP2=MIN(0.0_SP,MAX(Din(I,J),4.0_SP*Din(I,J-1)))
       endif
! there was a HUGE bug here, 12 should versus 43, fixed. fyshi
       if (Din(I,J).ge.0.0_SP) then
          TYP4=MAX(0.0_SP,MIN(Din(I,J),4.0_SP*Din(I,J+1)))
       else
          TYP4=MIN(0.0_SP,MAX(Din(I,J),4.0_SP*Din(I,J+1)))
       endif
       if (Din(I,J+1).ge.0.0_SP) then
          TYP3=MAX(0.0_SP,MIN(Din(I,J+1),4.0_SP*Din(I,J)))
       else
          TYP3=MIN(0.0_SP,MAX(Din(I,J+1),4.0_SP*Din(I,J)))
       endif

       OutL(I,J)=Vin(I,J-1)+1.0_SP/6.0_SP*(TYP1+2.0_SP*TYP2)
       OutR(I,J)=Vin(I,J)-1.0_SP/6.0_SP*(TYP3+2.0_SP*TYP4)
     ENDDO
     ENDDO     

END SUBROUTINE CONSTRUCT_HO_Y


! ------------------------------------------------------------------------------
!    The following two functions are Van Leer and Minmod limiters.
!    Called by
!  
!    NOTE: Babak mentioned this function can not go through
!          his windows intel compiler - Fengyan Shi 09/24/2010
!    
!    Last Update: 05/27/2010 Gangfeng Ma, University of Delaware
!    some compiler complains about function explicit interface
!    put real in front of function  02/13/2012
! ------------------------------------------------------------------------------

REAL(SP) FUNCTION VANLEER_LIMITER(A,B)
    USE PARAM
    IMPLICIT NONE
    REAL(SP),INTENT(IN) :: A
    REAL(SP),OPTIONAL,INTENT(IN) :: B
!    REAL(SP) :: VANLEER_LIMITER

    IF(PRESENT(B)) THEN
      VANLEER_LIMITER=(A*ABS(B)+ABS(A)*B)/(ABS(A)+ABS(B))
    ELSE  
      VANLEER_LIMITER=(A+ABS(A))/(1.0+A)
    ENDIF

    RETURN
END FUNCTION VANLEER_LIMITER

FUNCTION MINMOD_LIMITER(A,B,C)
    USE PARAM
    IMPLICIT NONE
    REAL(SP),INTENT(IN) :: A,B
    REAL(SP),OPTIONAL,INTENT(IN) :: C
    REAL(SP) :: MINMOD_LIMITER

    IF(PRESENT(C)) THEN
      MINMOD_LIMITER=SIGN(1.0_SP,A)*MAX(0.0_SP,MIN(ABS(A),SIGN(1.0_SP,A)*B,SIGN(1.0_SP,A)*C))
    ELSE
      MINMOD_LIMITER=SIGN(1.0_SP,A)*MAX(0.0_SP,MIN(ABS(A),SIGN(1.0_SP,A)*B))
    ENDIF

    RETURN
END FUNCTION MINMOD_LIMITER
