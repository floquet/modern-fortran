!------------------------------------------------------------------------------------
!
!      FILE sponge.F
!
!      This file is part of the FUNWAVE-TVD program under the Simplified BSD license
!
!-------------------------------------------------------------------------------------
! 
!    Copyright (c) 2016, FUNWAVE Development Team
!
!    (See http://www.udel.edu/kirby/programs/funwave/funwave.html
!     for Development Team membership)
!
!    All rights reserved.
!
!    FUNWAVE_TVD is free software: you can redistribute it and/or modify
!    it under the terms of the Simplified BSD License as released by
!    the Berkeley Software Distribution (BSD).
!
!    Redistribution and use in source and binary forms, with or without
!    modification, are permitted provided that the following conditions are met:
!
!    1. Redistributions of source code must retain the above copyright notice, this
!       list of conditions and the following disclaimer.
!    2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!  
!    The views and conclusions contained in the software and documentation are those
!    of the authors and should not be interpreted as representing official policies,
!    either expressed or implied, of the FreeBSD Project.
!  
!-------------------------------------------------------------------------------------
!
!    SPONGE_DAMPING is subroutine for dampping waves using Larsen-Dancy(1983)
!    type sponge layer 
!    
!    HISTORY: 10/27/2010 Fengyan Shi
!
!-------------------------------------------------------------------------------------
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

!-------------------------------------------------------------------------------------
!
!    ABSORBING_GENERATING_BC is subroutine for wave generation using Larsen-Dancy(1983)
!      type wave-maker
!    
!    HISTORY: 
!      05/01/2014 Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE ABSORBING_GENERATING_BC
     USE GLOBAL, ONLY : ETA,U,V,SPONGEMAKER,Mloc,Nloc,MASK,I,J,DX,DY,ZERO
     USE GLOBAL, ONLY : SP,PI,Grav,TIME,NumFreq,Phase_Ser,&
                       Ibeg,Iend,Jbeg,Jend,&
                       Segma_Ser,Stokes_Drift_Ser,Cm_eta,Sm_eta,&
                       Cm_u,Sm_u,Cm_v,Sm_v,SPONGE
     IMPLICIT NONE
     INTEGER :: KK,K
     real(SP),DIMENSION(Mloc,Nloc) :: Ein2D,Din2D
     real(SP),DIMENSION(Mloc,Nloc) :: Uin2D,Vin2D


     Ein2D = ZERO
     Uin2D = ZERO
     Vin2D = ZERO


     do j = 1,Nloc
     do i = 1,Mloc
      DO KK = 1, NumFreq

       Ein2D(I,J) =Ein2D(I,J)+Cm_eta(I,J,KK)*COS(pi/2.0_SP+Segma_Ser(KK)*TIME+Phase_Ser(KK)) &
                             +Sm_eta(I,J,KK)*SIN(pi/2.0_SP+Segma_Ser(KK)*TIME+Phase_Ser(KK))
 
       Uin2D(I,J) =Uin2D(I,J)+Cm_u(I,J,KK)*COS(pi/2.0_SP+Segma_Ser(KK)*TIME+Phase_Ser(KK)) &
                             +Sm_u(I,J,KK)*SIN(pi/2.0_SP+Segma_Ser(KK)*TIME+Phase_Ser(KK))
       Vin2D(I,J) =Vin2D(I,J)+Cm_v(I,J,KK)*COS(pi/2.0_SP+Segma_Ser(KK)*TIME+Phase_Ser(KK)) &
                             +Sm_v(I,J,KK)*SIN(pi/2.0_SP+Segma_Ser(KK)*TIME+Phase_Ser(KK))
       ENDDO 

     enddo 
     enddo   

       DO J=1,Nloc
       DO I=1,Mloc
         Ein2D(I,J)=Ein2D(I,J)/SPONGE(I,J)**4
         Uin2D(I,J)=Uin2D(I,J)/SPONGE(I,J)**4
         Vin2D(I,J)=Vin2D(I,J)/SPONGE(I,J)**4
       ENDDO
       ENDDO


       do j = 1,Nloc
       do i = 1,Mloc
          Eta(i,j) = Ein2D(I,J)+ (Eta(i,j)-Ein2D(I,J))/SpongeMaker(i,j)
          U(i,j) = Uin2D(I,J)+(U(i,j)-Uin2D(I,J))/SpongeMaker(i,j)
          V(i,j) = Vin2D(I,J)+(V(i,j)-Vin2D(I,J))/SpongeMaker(i,j)
       enddo
       enddo

END SUBROUTINE ABSORBING_GENERATING_BC

!-------------------------------------------------------------------------------------
!    CALCULATE_SPONGE_MAKER is subroutine for calculation of 
!      spong layer coefficient
!    
!    HISTORY: 
!      05/01/2014 Fengyan Shi
!-------------------------------------------------------------------------------------
SUBROUTINE CALCULATE_SPONGE_MAKER(M,N,Nghost,DX,DY,&
                            Sponge_west_width, &
                            R_sponge,A_sponge,SPONGE)
     USE PARAM
# if defined (PARALLEL)
     USE GLOBAL, ONLY : n_west, n_east, n_suth, n_nrth,&
                 px,py,npx,npy,Mglob,Nglob
# endif
     IMPLICIT NONE
     INTEGER, INTENT(IN)::M,N,Nghost
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX,DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX,DY
# endif
     REAL(SP),INTENT(IN) :: &
                          Sponge_west_width, &
                          R_sponge,A_sponge
     REAL(SP),DIMENSION(M,N),INTENT(INOUT)::SPONGE
     REAL(SP)::ri,lim
     INTEGER::Iwidth


# if defined (CARTESIAN)
     Iwidth=INT(Sponge_west_width/DX)+Nghost
# else
     Iwidth=INT(Sponge_west_width/DX(1,1))+Nghost
# endif
     DO J=1,N
     DO I=1,Iwidth
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
# if defined(PARALLEL)
       ri = R_Sponge**(50*(i+npx*Mglob/px-1)/(Iwidth-1))
# else
       ri=R_sponge**(50*(I-1)/(Iwidth-1))
# endif
       Sponge(i,j)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO

END SUBROUTINE CALCULATE_SPONGE_MAKER

!-------------------------------------------------------------------------------------
!
!    CALCULATE_FRICTION_SPONGE is subroutine for coefficient of 
!      friction type sponge layer
!    
!    HISTORY: 05/01/2014 Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE CALCULATE_FRICTION_SPONGE(M,N,Nghost,DX,DY,&
                            Sponge_west_width,Sponge_east_width,&
                            Sponge_south_width,Sponge_north_width, &
                            R_sponge,A_sponge,SPONGE)
     USE PARAM
     USE GLOBAL, ONLY : depth,MinDepthFrc,CDsponge
                        
# if defined (PARALLEL)
     USE GLOBAL, ONLY : n_west, n_east, n_suth, n_nrth,px,py,npx,npy,&
                        Mglob,Nglob
# endif
     IMPLICIT NONE
     INTEGER, INTENT(IN)::M,N,Nghost
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX,DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX,DY
# endif
     REAL(SP),INTENT(IN) :: &
                          Sponge_west_width,Sponge_east_width,&
                          Sponge_south_width,Sponge_north_width, &
                          R_sponge,A_sponge
     REAL(SP),DIMENSION(M,N),INTENT(OUT)::SPONGE
     REAL(SP)::ri,lim
     INTEGER::Iwidth
     REAL(SP) :: DXg,DYg,xx
     REAL(SP),DIMENSION(M,N) :: tmp_2d_1,tmp_2d_2


! note that I used SPONGE represent CD from input, do not mess up 
! with the sponge defined in the direction sponge 

# if defined (CARTESIAN)
     DXg=DX
     DYg=DY
# else
     DXg=DX(1,1)
     DYg=DY(1,1)
# endif

     SPONGE = ZERO
     tmp_2d_2 = ZERO
     tmp_2d_1 = ZERO

! west

     IF(Sponge_west_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_west_width/DX)+Nghost
# else
     Iwidth=INT(Sponge_west_width/DX(1,1))+Nghost
# endif
     DO J=1,N
     DO I=1,M
         lim=0.0_SP
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-i-npx*Mglob/px))
# else
       ri=MAX(0.0,REAL(Iwidth-I))
# endif
       tmp_2d_1(i,j)=MAX(CDsponge*TANH(ri/10.0),lim)

     ENDDO
     ENDDO
     ENDIF

! east

     IF(Sponge_east_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_east_width/DX)+Nghost
# else
     Iwidth=INT(Sponge_east_width/DX(1,1))+Nghost
# endif
     DO J=1,N
     DO I=1,M
         lim=0.0_SP
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-M+I-(px-npx-1)*Mglob/px))
# else
       ri=MAX(0.0,REAL(Iwidth-M+I))
# endif
       tmp_2d_2(i,j)=MAX(CDsponge*TANH(ri/10.0),lim)
     ENDDO
     ENDDO
     ENDIF

      DO J=1,N
      DO I=1,M
        IF(tmp_2d_1(I,J)>tmp_2d_2(I,J)) THEN
          Sponge(I,J)=tmp_2d_1(I,J)
        ELSE
          Sponge(I,J)=tmp_2d_2(I,J)
        ENDIF
      ENDDO
      ENDDO

! south

     IF(Sponge_south_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_south_width/DY)+Nghost
# else
     Iwidth=INT(Sponge_south_width/DY(1,1))+Nghost
# endif
     DO I=1,M
     DO J=1,N
       IF(SPONGE(I,J)>0.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=0.0_SP
       ENDIF
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-J-npy*Nglob/py))
# else
       ri=MAX(0.0,REAL(Iwidth-J))
# endif
       tmp_2d_1(i,j)=MAX(CDsponge*TANH(ri/10.0),lim)
     ENDDO
     ENDDO
     ENDIF


! north

     IF(Sponge_north_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_north_width/DY)+Nghost
# else
     Iwidth=INT(Sponge_north_width/DY(1,1))+Nghost
# endif
     DO I=1,M
     DO J=1,N
       IF(SPONGE(I,J)>0.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=0.0_SP
       ENDIF
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-N+J-(py-npy-1)*Nglob/py))
# else
       ri=MAX(0.0,REAL(Iwidth-N+J))
# endif
       tmp_2d_2(i,j)=MAX(CDsponge*TANH(ri/10.0),lim)
     ENDDO
     ENDDO
     ENDIF

      DO J=1,N
      DO I=1,M
        IF(tmp_2d_1(I,J)>tmp_2d_2(I,J)) THEN
          Sponge(I,J)=tmp_2d_1(I,J)
        ELSE
          Sponge(I,J)=tmp_2d_2(I,J)
        ENDIF
      ENDDO
      ENDDO


END SUBROUTINE CALCULATE_FRICTION_SPONGE

!-------------------------------------------------------------------------------------
!
!    CALCULATE_DIFFUSION_SPONGE is subroutine for coefficient of 
!      diffusion type sponge layer
!    
!    HISTORY: 05/01/2014 Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE CALCULATE_DIFFUSION_SPONGE(M,N,Nghost,DX,DY,&
                            Sponge_west_width,Sponge_east_width,&
                            Sponge_south_width,Sponge_north_width, &
                            R_sponge,A_sponge,SPONGE)
     USE PARAM
     USE GLOBAL, ONLY : depth,MinDepthFrc,Csp
                        
# if defined (PARALLEL)
     USE GLOBAL, ONLY : n_west, n_east, n_suth, n_nrth,px,py,npx,npy,&
                        Mglob,Nglob
# endif
     IMPLICIT NONE
     INTEGER, INTENT(IN)::M,N,Nghost
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX,DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX,DY
# endif
     REAL(SP),INTENT(IN) :: &
                          Sponge_west_width,Sponge_east_width,&
                          Sponge_south_width,Sponge_north_width, &
                          R_sponge,A_sponge
     REAL(SP),DIMENSION(M,N),INTENT(INOUT)::SPONGE
     REAL(SP)::ri,lim
     INTEGER::Iwidth
     REAL(SP) :: DXg,DYg,xx
     REAL(SP),DIMENSION(M,N) :: tmp_2d_1,tmp_2d_2


     tmp_2d_2 = ZERO
     tmp_2d_1 = ZERO

# if defined (CARTESIAN)
     DXg=DX
     DYg=DY
# else
     DXg=DX(1,1)
     DYg=DY(1,1)
# endif

! west

     IF(Sponge_west_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_west_width/DX)+Nghost
# else
     Iwidth=INT(Sponge_west_width/DX(1,1))+Nghost
# endif
     DO J=1,N
     DO I=1,M
       IF(SPONGE(I,J)>0.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=0.0_SP
       ENDIF
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-i-npx*Mglob/px))
# else
       ri=MAX(0.0,REAL(Iwidth-I))
# endif
       tmp_2d_1(i,j)=MAX(Csp*TANH(ri/10.0),lim)

     ENDDO
     ENDDO
     ENDIF

! east

     IF(Sponge_east_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_east_width/DX)+Nghost
# else
     Iwidth=INT(Sponge_east_width/DX(1,1))+Nghost
# endif
     DO J=1,N
     DO I=1,M
       IF(SPONGE(I,J)>0.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=0.0_SP
       ENDIF
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-M+I-(px-npx-1)*Mglob/px))
# else
       ri=MAX(0.0,REAL(Iwidth-M+I))
# endif
       tmp_2d_2(i,j)=MAX(Csp*TANH(ri/10.0),lim)
     ENDDO
     ENDDO
     ENDIF

      DO J=1,N
      DO I=1,M
        IF(tmp_2d_1(I,J)>tmp_2d_2(I,J)) THEN
          Sponge(I,J)=tmp_2d_1(I,J)
        ELSE
          Sponge(I,J)=tmp_2d_2(I,J)
        ENDIF
      ENDDO
      ENDDO

! south

     IF(Sponge_south_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_south_width/DY)+Nghost
# else
     Iwidth=INT(Sponge_south_width/DY(1,1))+Nghost
# endif
     DO I=1,M
     DO J=1,N
       IF(SPONGE(I,J)>0.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=0.0_SP
       ENDIF
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-J-npy*Nglob/py))
# else
       ri=MAX(0.0,REAL(Iwidth-J))
# endif
       tmp_2d_1(i,j)=MAX(Csp*TANH(ri/10.0),lim)
     ENDDO
     ENDDO
     ENDIF


! north

     IF(Sponge_north_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_north_width/DY)+Nghost
# else
     Iwidth=INT(Sponge_north_width/DY(1,1))+Nghost
# endif
     DO I=1,M
     DO J=1,N
       IF(SPONGE(I,J)>0.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=0.0_SP
       ENDIF
# if defined(PARALLEL)
       ri=MAX(0.0,REAL(Iwidth-N+J-(py-npy-1)*Nglob/py))
# else
       ri=MAX(0.0,REAL(Iwidth-N+J))
# endif
       tmp_2d_2(i,j)=MAX(Csp*TANH(ri/10.0),lim)
     ENDDO
     ENDDO
     ENDIF

      DO J=1,N
      DO I=1,M
        IF(tmp_2d_1(I,J)>tmp_2d_2(I,J)) THEN
          Sponge(I,J)=tmp_2d_1(I,J)
        ELSE
          Sponge(I,J)=tmp_2d_2(I,J)
        ENDIF
      ENDDO
      ENDDO

END SUBROUTINE CALCULATE_DIFFUSION_SPONGE

!-------------------------------------------------------------------------------------
!
!    CALCULATE_SPONGE is subroutine of sponge layer to get coefficient
!
!    HISTORY: 
!     10/27/2010 Fengyan Shi
!
!-------------------------------------------------------------------------------------

SUBROUTINE CALCULATE_SPONGE(M,N,Nghost,DX,DY,&
                            Sponge_west_width,Sponge_east_width,&
                            Sponge_south_width,Sponge_north_width, &
                            R_sponge,A_sponge,SPONGE)

     USE PARAM
     USE GLOBAL, ONLY : depth,MinDepthFrc
# if defined (PARALLEL)
     USE GLOBAL, ONLY : n_west, n_east, n_suth, n_nrth,px,py,npx,npy,&
                        Mglob,Nglob
# endif
     IMPLICIT NONE
     INTEGER, INTENT(IN)::M,N,Nghost
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX,DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX,DY
# endif
     REAL(SP),INTENT(IN) :: &
                          Sponge_west_width,Sponge_east_width,&
                          Sponge_south_width,Sponge_north_width, &
                          R_sponge,A_sponge

     REAL(SP),DIMENSION(M,N),INTENT(INOUT)::SPONGE
     REAL(SP)::ri,lim
     INTEGER::Iwidth
     REAL(SP) :: DXg,DYg,xx
     REAL(SP),DIMENSION(M,N) :: tmp_2d_1,tmp_2d_2 

     tmp_2d_2 = ZERO
     tmp_2d_1 = ZERO

# if defined (CARTESIAN)
     DXg=DX
     DYg=DY
# else
     DXg=DX(1,1)
     DYg=DY(1,1)
# endif

! west

     IF(Sponge_west_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_west_width/DX)+Nghost
# else
     Iwidth=INT(Sponge_west_width/DX(1,1))+Nghost
# endif
     DO J=1,N
     DO I=1,M
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
# if defined(PARALLEL)
       ri = R_Sponge**(50*(i+npx*Mglob/px-1)/(Iwidth-1))
# else
       ri=R_sponge**(50*(I-1)/(Iwidth-1))
# endif
       tmp_2d_1(i,j)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO
     ENDIF

! east

     IF(Sponge_east_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_east_width/DX)+Nghost
# else
     Iwidth=INT(Sponge_east_width/DX(1,1))+Nghost
# endif
     DO J=1,N
     DO I=1,M
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
# if defined(PARALLEL)
       ri = R_Sponge**(50*(M-i+(px-npx-1)*Mglob/px)/(Iwidth-1))
# else
       ri=R_sponge**(50*(M-I)/(Iwidth-1))
# endif
       tmp_2d_2(i,j)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO
     ENDIF

      DO J=1,N
      DO I=1,M
        IF(tmp_2d_1(I,J)>tmp_2d_2(I,J)) THEN
          Sponge(I,J)=tmp_2d_1(I,J)
        ELSE
          Sponge(I,J)=tmp_2d_2(I,J)
        ENDIF
        IF(Sponge(I,J)<A_Sponge**(R_Sponge**50))Sponge(I,J)=1.0_SP
      ENDDO
      ENDDO

! south

     IF(Sponge_south_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_south_width/DY)+Nghost
# else
     Iwidth=INT(Sponge_south_width/DY(1,1))+Nghost
# endif
     DO I=1,M
     DO J=1,N
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
# if defined(PARALLEL)
       ri=R_sponge**(50*(J+npy*Nglob/py-1)/(Iwidth-1))
# else
       ri=R_sponge**(50*(J-1)/(Iwidth-1))
# endif
       tmp_2d_1(i,j)=MAX(A_Sponge**ri,lim)
     ENDDO
     ENDDO
     ENDIF


! north

     IF(Sponge_north_width>ZERO)THEN
# if defined (CARTESIAN)
     Iwidth=INT(Sponge_north_width/DY)+Nghost
# else
     Iwidth=INT(Sponge_north_width/DY(1,1))+Nghost
# endif
     DO I=1,M
     DO J=1,N
       IF(SPONGE(I,J)>1.0_SP)THEN
         lim=SPONGE(I,J)
       ELSE
         lim=1.0_SP
       ENDIF
# if defined(PARALLEL)
       ri = R_Sponge**(50*(N-J+(py-npy-1)*Nglob/py)/(Iwidth-1))
# else
       ri=R_sponge**(50*(N-J)/(Iwidth-1))	  
# endif
       tmp_2d_2(i,j)=MAX(A_Sponge**ri,lim)	  
     ENDDO
     ENDDO
     ENDIF

      DO J=1,N
      DO I=1,M
        IF(tmp_2d_1(I,J)>tmp_2d_2(I,J)) THEN
          Sponge(I,J)=tmp_2d_1(I,J)
        ELSE
          Sponge(I,J)=tmp_2d_2(I,J)
        ENDIF
        IF(Sponge(I,J)<A_Sponge**(R_Sponge**50))Sponge(I,J)=1.0_SP	  
      ENDDO
      ENDDO

END SUBROUTINE CALCULATE_SPONGE




