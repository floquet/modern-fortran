!------------------------------------------------------------------------------------
!
!      FILE breaker.F
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
!    WAVE_BREAKING is subroutine to do wave breaking 
!      using the artificial eddy viscosity scheme 
!      For the shock-capturing based breaking scheme 
!      this subroutine is only used for
!      demonstration or calculating bubbles or foam as 
!      SHOW_BREAKING = T.
!    
!    HISTORY: 
!      11/22/2010 Fengyan Shi
!      04/15/2015 Fengyan Shi, added viscosity breaking
!      09/19/2015 YoungKwang Chio, added viscosity distribution within WaveMaker region
!
!-------------------------------------------------------------------------------------

SUBROUTINE WAVE_BREAKING
     USE GLOBAL
     IMPLICIT NONE

      IF(SHOW_BREAKING)THEN
        CALL BREAKING(Mloc,Nloc,ETAx,ETAy,ETAT,Cbrk1,Cbrk2,H,MinDepthFrc,DT,&
               DX,DY,T_brk,AGE_BREAKING)

	ELSEIF( WAVEMAKER_VIS )THEN
	  ! ykchoi(08.19.2015)
	  !
        CALL VISCOSITY_WMAKER(Mloc,Nloc,ETAT,visbrk,H,MinDepthFrc,DX,DY)
      ENDIF


END SUBROUTINE WAVE_BREAKING

!-------------------------------------------------------------------------------------
!
!    BREAKING is subroutine to calculate viscosity and breaking age
!    for the artificial eddy viscosity breaking scheme
!    
!    HISTORY: 
!     11/22/2010 Fengyan Shi
!
!-------------------------------------------------------------------------------------

SUBROUTINE BREAKING(M,N,ETAx,ETAy,ETAT,Cbrk1,Cbrk2,H,MinDepthFrc,&
               DT,DX,DY,T_brk,AGE)
     USE PARAM
     USE GLOBAL,ONLY : Depth,nu_break,ETA,Nghost,Xc_WK,Yc_WK,Ibeg,Iend,&
                       Jbeg,Jend,Width_WK,Xc_WK,Yc_WK,Ywidth_WK,WAVEMAKER_Cbrk,nu_bkg
# if defined(PARALLEL)
     USE GLOBAL,ONLY : npx,npy
# endif
     IMPLICIT NONE
     INTEGER,INTENT(IN)::M,N
     REAL(SP),INTENT(IN)::Cbrk1,Cbrk2,MinDepthFrc,DT,T_brk
     REAL(SP) :: cap1,cap2
     REAL(SP) :: xmk,ymk,DXg,DYg
!     REAL(SP) :: lim_breaker=0.10
# if defined (CARTESIAN)
     REAL(SP),INTENT(IN)::DX,DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN)::DX,DY
# endif
     REAL(SP),DIMENSION(M,N),INTENT(IN)::ETAx,ETAy,ETAt,H
     REAL(SP),DIMENSION(M,N),INTENT(OUT)::AGE
     REAL(SP)::C,Angle,AGE1,AGE2,AGE3,propx,propy,propxy

     DO J=Nghost+1,N-Nghost
     DO I=Nghost+1,M-Nghost

     tmp3=SQRT(GRAV*MAX(MinDepthFrc,H(I,J)))
     tmp1=Cbrk1*tmp3
     tmp2=Cbrk2*tmp3
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
# if defined (CARTESIAN)
        DXg=DX
        DYg=DY
        propxy=SQRT(DX*DX+DY*DY)/MAX(C,SMALL)
        propx=SQRT(DX*DX)/MAX(C,SMALL)
        propy=SQRT(DY*DY)/MAX(C,SMALL)
# else
        DXg=DX(I,J)
        DYg=DY(I,J)
        propxy=SQRT(DX(I,J)*DX(I,J)+DY(I,J)*DY(I,J))/MAX(C,SMALL)
        propx=SQRT(DX(I,J)*DX(I,J))/MAX(C,SMALL)
        propy=SQRT(DY(I,J)*DY(I,J))/MAX(C,SMALL)
# endif
        ANGLE=ATAN2(ETAy(I,J),ETAx(I,J))

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

! set viscosity

! wavemaker
# if defined (PARALLEL)
            xmk=(I-Ibeg)*DXg+npx*(M-2*Nghost)*DXg
            ymk=(J-Jbeg)*DYg+npy*(N-2*Nghost)*DYg
# else
            xmk=(I-Ibeg)*DXg
            ymk=(J-Jbeg)*DYg
# endif

! wavemaker doesnt use breaker age

    IF(ABS(xmk-Xc_WK)<Width_WK.AND. &
            ABS(ymk-Yc_WK)<Ywidth_WK/2.0_SP)THEN

      IF(ETAt(I,J)>MIN(tmp2,WAVEMAKER_Cbrk*tmp3))THEN
         cap1=1.0*(MAX(Depth(I,J),MinDepthFrc)+ETA(I,J))
         nu_break(I,J)=cap1*WAVEMAKER_Cbrk*tmp3+nu_bkg
      ELSE
         nu_break(I,J)=ZERO+nu_bkg
      ENDIF

    ELSE ! outside wavemaker    

     IF(AGE(I,J)>ZERO.AND.AGE(I,J)<T_brk.AND.ETAt(I,J)>tmp2)THEN
       cap1=1.0*(MAX(Depth(I,J),MinDepthFrc)+ETA(I,J))
!   note 09/17/2016
!   Kennedy et al used a transition for nu, the transition is basically
!   used to avoid instabilities. Choi and I did some tests:
!   1) use a static nu instead of a function of eta_t which 
!      causes a numerical instability. However, instabilities still
!      occur for some 2D cases. 
!   2) use a static nu as a function of Cbrk2. 
!      nu_break(I,J) = cap1*tmp2 + nu_bkg
!      This is the setting
!      I used before the transition scheme. It worked well but
!      we should re-calibrate Cbrk2. 
!   
# if defined (VIS_KENNEDY)
        nu_break(I,J)=cap1*ABS(ETAt(I,J))+nu_bkg
# elif defined (VIS_STATIC_TRANS)
         nu_break(I,J)=cap1*tmp3*(Cbrk2+(Cbrk1-Cbrk2)*(T_brk-AGE(I,J))/T_brk)+nu_bkg
# else
        nu_break(I,J) = cap1*tmp2 + nu_bkg
# endif
     ELSE
       nu_break(I,J)=ZERO+nu_bkg
     ENDIF
          
   ENDIF ! end wavemaker

     ENDDO
     ENDDO

END SUBROUTINE BREAKING

