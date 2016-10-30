!------------------------------------------------------------------------------------
!
!      FILE wavemaker.F
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
!    WAVEMAKER_INITIALIZATION is subroutine for initialization of 
!    Wei and Kirbys internal wave maker
!
!    HISTORY: 
!      11/09/2010 Fengyan Shi
!      02/10/2016 Fengyan Shi, separated from INITIALIZATION 
!
! --------------------------------------------------

SUBROUTINE WAVEMAKER_INITIALIZATION
     USE GLOBAL
# if defined(INTEL)
     USE IFPORT
# endif
     IMPLICIT NONE

! internal wavemaker of wei and kirby

     IF(WaveMaker(1:6)=='WK_REG') THEN

! Reniel complaint about regular wave case with wrong Yc
! it turns out this part is a redefinition of Yc_WK, remove it (07/08/2016)

!# if defined (PARALLEL)
!# if defined (CARTESIAN)
!     Yc_WK=(INT((Nloc-1)/2)+Nghost)*DY
!# else
!     Yc_WK=(INT((Nloc-1)/2)+Nghost)*DY(1,1)
!# endif
!# else
!# if defined (CARTESIAN)
!     Yc_WK=INT(Nglob/2)*DY
!# else
!     Yc_WK=INT(Nglob/2)*DY(1,1)
!# endif
!# endif
    
     IF(PERIODIC)THEN

    ! calculate wave number
       tmp1 = -0.39_SP + 1.0_SP / 3.0_SP  ! alpha1
       tmp2 = 2.*pi/Tperiod               ! omgn
       tmp2 = tmp2*tmp2*DEP_WK/grav       ! tb
       tmp3 = 1.0_SP + tmp2*(-0.39_SP)    ! tc

      IF(DEP_WK==ZERO.OR.Tperiod==ZERO)THEN
# if defined (PARALLEL)
         if(myid.eq.0) write(*,*) 're-set depth, Tperiod for wavemaker, STOP!'
         call MPI_FINALIZE ( ier )
# else
         WRITE(*,*)'re-set depth, Tperiod for wavemaker, STOP!'
         STOP
# endif
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
# if defined (CARTESIAN)
          tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)
# else
          tmp2=I*2.0_SP*pi/DY(1,1)/(Nglob-1.0_SP)
# endif
         IF(tmp2.GE.tmp1)THEN
          tmp3=90.0

# if defined (PARALLEL)
         if(myid.eq.0) write(*,*) 'should enlarge domain for periodic boundary with this wave angle, STOP'
         call MPI_FINALIZE ( ier )
# else
          WRITE(*,*)'should enlarge domain for periodic boundary with this wave angle, STOP'
          STOP
# endif
         ELSE
           tmp3=ASIN(tmp2/tmp1)*180.0_SP/pi    ! theta, based on rlamda=wkn*sin(theta)
         ENDIF
         IF(I>1000)THEN
# if defined (PARALLEL)
         if(myid.eq.0) write(*,*)'could not find a wave angle for periodic boundary condition, STOP'
           call MPI_FINALIZE ( ier )
# else
           WRITE(*,*) 'could not find a wave angle for periodic boundary condition, STOP'
           STOP
# endif
         ENDIF
       ENDDO
      ELSEIF(Theta_WK.LT.ZERO)THEN
       tmp3=ZERO
       I=0
       Do WHILE (tmp3>Theta_WK)
         I=I+1
!         tmp2=I*2.0_SP*pi/DY/(Jend-Jbeg)     ! rlamda
# if defined (CARTESIAN)
         tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)     ! rlamda
# else
         tmp2=I*2.0_SP*pi/DY(1,1)/(Nglob-1.0_SP)     ! rlamda
# endif
         IF(tmp2.GE.tmp1)THEN
          tmp3=-90.0
# if defined (PARALLEL)
         if(myid.eq.0) write(*,*)'should enlarge domain for periodic boundary with this wave angle, STOP'
           call MPI_FINALIZE ( ier )
# else
          WRITE(*,*)'should enlarge domain for periodic boundary with this wave angle, STOP'
          STOP
# endif
         ELSE
           tmp3=-ASIN(tmp2/tmp1)*180.0_SP/pi    ! theta, based on rlamda=wkn*sin(theta)
         ENDIF
         IF(I>1000)THEN
# if defined (PARALLEL)
         if(myid.eq.0) write(*,*) 'could not find a wave angle for periodic boundary condition, STOP'
           call MPI_FINALIZE ( ier )
# else
           WRITE(*,*) 'could not find a wave angle for periodic boundary condition, STOP'
           STOP
# endif
         ENDIF
       ENDDO
      ENDIF

# if defined (PARALLEL)
         if(myid.eq.0)then
          WRITE(*,*) 'wave angle you set:', Theta_WK
          WRITE(*,*) 'wave angle in calculation to make periodic boundary:', tmp3
         endif
# else
       WRITE(*,*) 'wave angle you set:', Theta_WK
       WRITE(*,*) 'wave angle in calculation to make periodic boundary:', tmp3
# endif
   

       Theta_WK = tmp3
     ENDIF
    ENDIF ! end theta .ne.zero

       CALL WK_WAVEMAKER_REGULAR_WAVE & 
               (Tperiod,AMP_WK,Theta_WK,DEP_WK,Delta_WK,D_gen,rlamda,beta_gen,Width_WK)

     ENDIF
     
     IF(WaveMaker(1:9)=='WK_DATA2D')THEN
      OPEN(1,FILE=TRIM(WaveCompFile))
       READ(1,*)NumFreq,NumDir
       ALLOCATE (WAVE_COMP(NumFreq,NumDir),Beta_gen2D(NumFreq,NumDir),D_gen2D(NumFreq,NumDir),  &
          Phase2D(NumFreq,NumDir), &
          Freq(NumFreq),Dire(NumDir),rlamda2D(NumFreq,NumDir))

       ! define cm sm here to make consistence with irregular wave
       ALLOCATE (Cm(Mloc,Nloc,NumFreq),Sm(Mloc,Nloc,NumFreq))
       ALLOCATE (OMGN2D(NumFreq))
 
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
# if defined (INTEL)
          Phase2D(J,I)=rand()*2.0_SP*pi
# else
          Phase2D(J,I)=rand(0)*2.0_SP*pi
# endif
       ENDDO
       ENDDO


!      make efficient calculation fyshi 07/07/2016

       DO J=1,NumFreq
          OMGN2D(J) = 2.0_SP*PI*Freq(J)
       ENDDO

      FreqPeak = 1.0_SP/PeakPeriod

      CALL CALCULATE_Cm_Sm(Mloc,Nloc,DX,DY,Xc_WK,Ibeg,Jbeg,NumFreq,NumDir,&
               D_gen2D,Phase2D,Width_WK,rlamda2D,beta_gen2D,Cm,Sm)

      IF(SHOW_BREAKING)THEN
       T_brk=WAVE_COMP(NumWaveComp,1) 
      ENDIF

!  cannot deallocate phase2d, I could not find any reason why cannot.
!  Phase2D is only used here, deallocation will cause minor discontinuity 
!  at processor interface
!  leave it now 06/10/2016

       DEALLOCATE (WAVE_COMP,Beta_gen2D,D_gen2D,  &
!          Phase2D, &
          Freq,Dire,rlamda2D)


     ENDIF ! end wk_data2d
       
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

     ENDIF 

! wei and kirby, 1999, irregular wavemaker
      
     IF(WaveMaker(1:6)=='WK_IRR') THEN

!        move allocation here from init.F 06/07/2016

       ALLOCATE(D_gen_ir(Nfreq,Ntheta),rlamda_ir(Nfreq,Ntheta), &
                phase_ir(Nfreq,Ntheta),&
                Beta_gen_ir(Nfreq),omgn_ir(Nfreq), &
                Cm(Mloc,Nloc,Nfreq),Sm(Mloc,Nloc,Nfreq))

# if defined (CARTESIAN)
      CALL WK_WAVEMAKER_IRREGULAR_WAVE & 
       (Nfreq,Ntheta,delta_WK,DEP_WK,FreqPeak,FreqMax,FreqMin,GammaTMA,Hmo,ThetaPeak, &
         sigma_theta,rlamda_ir,beta_gen_ir,D_gen_ir,Phase_ir,Width_WK,omgn_ir,&
         Periodic,DY,Nglob)
      CALL CALCULATE_Cm_Sm(Mloc,Nloc,DX,DY,Xc_WK,Ibeg,Jbeg,Nfreq,Ntheta,&
               D_gen_ir,Phase_ir,Width_WK,rlamda_ir,beta_gen_ir,Cm,Sm)
# else
      CALL WK_WAVEMAKER_IRREGULAR_WAVE & 
       (Nfreq,Ntheta,delta_WK,DEP_WK,FreqPeak,FreqMax,FreqMin,GammaTMA,Hmo,ThetaPeak, &
         sigma_theta,rlamda_ir,beta_gen_ir,D_gen_ir,Phase_ir,Width_WK,omgn_ir,&
         Periodic,DY(1,1),Nglob)
      CALL CALCULATE_Cm_Sm(Mloc,Nloc,DX(1,1),DY(1,1),Xc_WK,Ibeg,Jbeg,Nfreq,Ntheta,&
               D_gen_ir,Phase_ir,Width_WK,rlamda_ir,beta_gen_ir,Cm,Sm)
# endif

       ! 06/07/2016 fyshi
       DEALLOCATE(D_gen_ir,rlamda_ir,phase_ir,&
                Beta_gen_ir)

      IF(SHOW_BREAKING)THEN
       T_brk=1.0_SP/FreqMax
      ENDIF

     ENDIF
!   now include spherical 


     IF(WAVEMAKER(1:3)=='ABS')THEN
      IF(WAVE_DATA)THEN
        CALL CALCULATE_DATA2D_Cm_Sm
      ELSE
        CALL CALCULATE_TMA_Cm_Sm &
     (Nfreq,Ntheta,DEP_Ser,FreqPeak,FreqMax,FreqMin,GammaTMA,Hmo,ThetaPeak, &
         sigma_theta)
      ENDIF
     ENDIF

END SUBROUTINE WAVEMAKER_INITIALIZATION


!-------------------------------------------------------------------------------------
!
!    WK_WAVEMAKER_2D_SPECTRAL_DATA is subroutine 
!     to generate directional spectrum for given 2D directional spectrum
!     for given 2D directional spectrum using
!     source function for Wei and Kirbys internal wave maker
!
!    HISTORY: 
!      10/17/2011 Fengyan Shi
!
!-------------------------------------------------------------------------------------
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
# if defined (CARTESIAN)
         tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)     ! rlamda
# else
         tmp2=I*2.0_SP*pi/DY(1,1)/(Nglob-1.0_SP)     ! rlamda
# endif
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
# if defined (CARTESIAN)
         angle1=ASIN((I-1)*2.0_SP*pi/DY/(Nglob-1.0_SP)/tmp1)
# else
         angle1=ASIN((I-1)*2.0_SP*pi/DY(I,J)/(Nglob-1.0_SP)/tmp1)
# endif
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
# if defined (CARTESIAN)
         tmp2=I*2.0_SP*pi/DY/(Nglob-1.0_SP)     ! rlamda
# else
         tmp2=I*2.0_SP*pi/DY(I,J)/(Nglob-1.0_SP)     ! rlamda
# endif
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
# if defined (CARTESIAN)
         angle1=-ASIN((I-1)*2.0_SP*pi/DY/(Nglob-1.0_SP)/tmp1)
# else
         angle1=-ASIN((I-1)*2.0_SP*pi/DY(1,1)/(Nglob-1.0_SP)/tmp1)
# endif

         if (abs(angle1-Dire(ndir))<abs(Dire(ndir)-tmp3))then
            angle2=angle1
         else
            angle2=tmp3
         endif
      ENDIF 

# if defined (PARALLEL)
# else

       WRITE(*,*) 'wave angle you set:', Dire(ndir)*180/pi
       WRITE(*,*) 'wave angle in calculation:', angle2*180/pi
       WRITE(*,*) 'chosen between', angle1*180/pi, 'and', tmp3*180/pi

       WRITE(3,*) 'wave angle you set:', Dire(ndir)*180/pi
       WRITE(3,*) 'wave angle in calculation:', angle2*180/pi
       WRITE(3,*) 'chosen between', angle1*180/pi, 'and', tmp3*180/pi
# endif

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

!-------------------------------------------------------------------------------------
!
!    CALCULATE_Cm_Sm is subroutine 
!     to calculate Cm Sm for Wei and Kirbys 
!     internal wave maker, irregular wave (TMA)
!
!    HISTORY: 
!      11/9/2010 Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE CALCULATE_Cm_Sm(M,N,DX,DY,Xc,Ibeg,Jbeg,mfreq,mtheta,D_gen,phi1, &
               width,rlamda,beta_gen,Cm,Sm)
     USE PARAM
# if defined (PARALLEL)
     USE GLOBAL, ONLY : myid,npx,npy,px,py,Mglob,Nglob
# endif
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

# if defined (PARALLEL)
            Cm(i,j,kf)=Cm(i,j,kf) &
             +D_gen(kf,ktheta)*exp(-beta_gen(kf)*((I-Ibeg +npx*Mglob/px)*DX-Xc)**2) &
          *cos(rlamda(kf,ktheta) &
          *((J-Jbeg  +npy*Nglob/py)*DY-ZERO)+phi1(kf,ktheta))

            Sm(i,j,kf)=Sm(i,j,kf) &
             +D_gen(kf,ktheta)*exp(-beta_gen(kf)*((I-Ibeg+ npx*Mglob/px)*DX-Xc)**2) &
          *sin(rlamda(kf,ktheta) &
          *((J-Jbeg +npy*Nglob/py)*DY-ZERO)+phi1(kf,ktheta))
# else
            Cm(i,j,kf)=Cm(i,j,kf) &
             +D_gen(kf,ktheta)*exp(-beta_gen(kf)*((I-Ibeg)*DX-Xc)**2) &
          *cos(rlamda(kf,ktheta) &
          *((J-Jbeg)*DY-ZERO)+phi1(kf,ktheta))

            Sm(i,j,kf)=Sm(i,j,kf) &
             +D_gen(kf,ktheta)*exp(-beta_gen(kf)*((I-Ibeg)*DX-Xc)**2) &
          *sin(rlamda(kf,ktheta) &
          *((J-Jbeg)*DY-ZERO)+phi1(kf,ktheta))
# endif

           enddo
           enddo

        enddo
        enddo

END SUBROUTINE CALCULATE_Cm_Sm

!-------------------------------------------------------------------------------------
!
!    CALCULATE_Cm_Sm is subroutine 
!     to calculate Cm Sm for Wei and Kirbys 
!     internal wave maker, irregular wave (DATA)
!
!    HISTORY: 
!      11/9/2010 Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE CALCULATE_DATA2D_Cm_Sm
     USE GLOBAL, ONLY : Mloc,Nloc,MASK,I,J,DX,DY,ZERO,Beta_ref
     USE GLOBAL, ONLY : SP,PI,Grav,TIME,Amp_Ser,Per_Ser,Phase_Ser,Dep_Ser,&
                       Theta_Ser, Ibeg,Iend,Jbeg,Jend,&
                       Wave_Number_Ser,Stokes_Drift_Ser,NumFreq,NumDir,&
                       Segma_Ser,&
                       Cm_eta,Sm_eta,Cm_u,Sm_u,Cm_v,Sm_v
# if defined (PARALLEL)
     USE GLOBAL, ONLY : myid,npx,npy,px,py,Mglob,Nglob
# endif
     IMPLICIT NONE
     INTEGER :: Iter,KK,K,KKK
     REAL(SP) :: Celerity,Wave_Length,Fk,Fkdif,Zlev,Fact,X_maker,Y_maker
     real(SP),DIMENSION(Mloc,Nloc) :: Ein2D,Din2D
     real(SP),DIMENSION(Mloc,Nloc) :: Uin2D,Vin2D
     REAL(SP) :: tmp_eta,tmp_u,tmp_v


       ALLOCATE(Cm_eta(Mloc,Nloc,Numfreq),Sm_eta(Mloc,Nloc,Numfreq), &
                Cm_u(Mloc,Nloc,Numfreq),Sm_u(Mloc,Nloc,Numfreq),&
                Cm_v(Mloc,Nloc,Numfreq),Sm_v(Mloc,Nloc,Numfreq) )

! wave_number

   DO I=1,NumFreq
     Segma_Ser(I) = 2.0*pi/Per_Ser(I)
     Celerity = sqrt(Grav*Dep_Ser)
     Wave_Length = Celerity*Per_Ser(I)
     Wave_Number_Ser(I) = 2.0*pi/Wave_Length
     
     Iter = 0
55   Fk = Grav*Wave_Number_Ser(I)*tanh(Wave_Number_Ser(I)*Dep_Ser)-Segma_Ser(I)**2
     if(abs(Fk)<=1.0e-8.or.Iter>1000) goto 65
     Fkdif = Grav*Wave_Number_Ser(I)*Dep_Ser*(1.0-tanh(Wave_Number_Ser(I)*Dep_Ser)**2)+  &
        Grav*tanh(Wave_Number_ser(I)*Dep_Ser) 
     Wave_Number_Ser(I) = Wave_Number_Ser(I)-Fk/Fkdif
     Iter = Iter+1
     goto 55
65   continue
     Wave_Length = 2.0*pi/Wave_Number_Ser(I)

    ENDDO ! end NumCompSer  

     Stokes_Drift_Ser = ZERO
     Fact = 1.0 

! Cm and Sm
     Cm_eta = ZERO
     Sm_eta = ZERO
     Cm_u = ZERO
     Sm_u = ZERO
     Cm_v = ZERO
     Sm_v = ZERO

     Zlev = ABS(Beta_ref)*Dep_Ser

     DO KK=1,NumFreq
       DO J=1,Nloc
       DO I=1,Mloc
# if defined (PARALLEL)
# if defined (CARTESIAN)
         X_maker=(I-Ibeg+npx*Mglob/px)*DX
         Y_maker=(J-Jbeg+npy*Nglob/py)*DY 
# else
         X_maker=(I-Ibeg+npx*Mglob/px)*DX(I,J)
         Y_maker=(J-Jbeg+npy*Nglob/py)*DY(I,J)
# endif

# else

# if defined (CARTESIAN)
         X_maker=(I-Ibeg)*DX
         Y_maker=(J-Jbeg)*DY
# else
         X_maker=(I-Ibeg)*DX(I,J)
         Y_maker=(J-Jbeg)*DY(I,J)
# endif

# endif

         DO KKK=1,NumDir
          Cm_eta(I,J,KK)=Cm_eta(I,J,KK)+Amp_Ser(KK,KKK)*COS( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Sm_eta(I,J,KK)=Sm_eta(I,J,KK)+Amp_Ser(KK,KKK)*SIN( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Cm_u(I,J,KK)=Cm_u(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *COS(Theta_Ser(KKK))*COS( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Cm_v(I,J,KK)=Cm_v(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *SIN(Theta_Ser(KKK))*COS( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )                   
          Sm_u(I,J,KK)=Sm_u(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *COS(Theta_Ser(KKK))*SIN( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Sm_v(I,J,KK)=Sm_v(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *SIN(Theta_Ser(KKK))*SIN( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )

         ENDDO

       ENDDO
       ENDDO
      ENDDO


END SUBROUTINE CALCULATE_DATA2D_Cm_Sm


!-------------------------------------------------------------------------------------
!
!    CALCULATE_Cm_Sm is subroutine 
!     to calculate Cm Sm for Wei and Kirbys 
!     internal wave maker, irregular wave (TMA)
!
!    HISTORY: 
!      11/09/2010 Fengyan Shi
!      05/12/2011 Fengyan Shi, removed conversion between Hrms and Hmo based on
!                              Joe Geiman test
!
!-------------------------------------------------------------------------------------
SUBROUTINE CALCULATE_TMA_Cm_Sm & 
               (mfreq,mtheta,h_gen,fm,fmax,fmin,gamma_spec,Hmo,theta_input,&
                sigma_theta_input)
     USE PARAM
! *** from data2d_cm_sm
     USE GLOBAL, ONLY : Mloc,Nloc,I,J,DX,DY,ZERO,Beta_ref
     USE GLOBAL, ONLY : Amp_Ser,Per_Ser,Phase_Ser,Dep_Ser,&
                       Theta_Ser, Ibeg,Iend,Jbeg,Jend,&
                       Wave_Number_Ser,Stokes_Drift_Ser,NumFreq,NumDir,&
                       Segma_Ser,&
                       Cm_eta,Sm_eta,Cm_u,Sm_u,Cm_v,Sm_v
! ***

# if defined (PARALLEL)
     USE GLOBAL, ONLY : myid,npx,npy,px,py,Mglob,Nglob
# endif
# if defined(INTEL)
     USE IFPORT
# endif
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: mfreq,mtheta
     REAL(SP),INTENT(IN) :: h_gen,fm,fmax,fmin,gamma_spec,Hmo,theta_input,&
                            sigma_theta_input

     REAL(SP),DIMENSION(mfreq):: Freq,omgn
     REAL(SP), DIMENSION(mtheta) :: Hmo_each,AG
     REAL(SP), DIMENSION(10000) :: Ef10000
     REAL(SP) :: Ef,fre,omiga_spec,phi,sigma_spec,Etma,Ef100,Ef_add,sigma_theta,&
                 theta_p,theta_m,theta_10,theta_11,theta_21,alpha_spec,ap,&
                 theta,alpha,alpha1,tb,tc,wkn,C_phase,wave_length,rl_gen,rI,theta_1,&
                 omgn_tmp
     INTEGER :: kf,kff,kb,N_spec,ktotal,k_n,ktheta,mcenter
     INTEGER :: Iter,KK,KKK
     REAL(SP) :: Celerity,Fkdif,Zlev,Fact,X_maker,Y_maker

       NumFreq = mfreq
       NumDir = mtheta

       ALLOCATE (Amp_Ser(NumFreq,NumDir), Wave_Number_Ser(NumFreq), &
          Per_Ser(NumFreq),Theta_Ser(NumDir),Segma_Ser(NumFreq), &
          Phase_Ser(NumFreq))
       ALLOCATE(Cm_eta(Mloc,Nloc,Numfreq),Sm_eta(Mloc,Nloc,Numfreq), &
                Cm_u(Mloc,Nloc,Numfreq),Sm_u(Mloc,Nloc,Numfreq),&
                Cm_v(Mloc,Nloc,Numfreq),Sm_v(Mloc,Nloc,Numfreq) )

       DO kf=1,NumFreq
# if defined (INTEL)
          Phase_Ser(kf)=rand()*2.0_SP*3.1415926
# else
          Phase_Ser(kf)=rand(0)*2.0_SP*3.1415926
# endif
       ENDDO

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
! -- it seems theres an inaccuracy caused by 100 freq if mfreq<100
!    should change to mfreq 29/10/2012
 
         Ef100=Ef/REAL(mfreq+1)
        kb=0
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

! in case wkn=0 02/08/2012  
        IF(wkn.eq.0.0)THEN
           wkn=SMALL
           C_phase=sqrt(grav*h_gen)
           wave_length=C_phase/fm
        ELSE
          C_phase=1.0_SP/wkn*fm*2.0_SP*pi
          wave_length=C_phase/fm
        ENDIF
!                          

        Amp_Ser(kf,ktheta)=ap
        Wave_Number_Ser(kf)=wkn
        Theta_Ser(ktheta)=theta
        Segma_Ser(kf)=omgn(kf)
        Per_Ser(kf)=1.0_SP/Freq(kf)

        enddo
        enddo

     Stokes_Drift_Ser = ZERO
     Fact = 1.0 

! Cm and Sm
     Cm_eta = ZERO
     Sm_eta = ZERO
     Cm_u = ZERO
     Sm_u = ZERO
     Cm_v = ZERO
     Sm_v = ZERO

     Zlev = ABS(Beta_ref)*Dep_Ser

     DO KK=1,NumFreq
       DO J=1,Nloc
       DO I=1,Mloc

# if defined (PARALLEL)
# if defined (CARTESIAN)
         X_maker=(I-Ibeg+npx*Mglob/px)*DX
         Y_maker=(J-Jbeg+npy*Nglob/py)*DY 
# else
         X_maker=(I-Ibeg+npx*Mglob/px)*DX(I,J)
         Y_maker=(J-Jbeg+npy*Nglob/py)*DY(I,J)
# endif

# else
# if defined (CARTESIAN)
         X_maker=(I-Ibeg)*DX
         Y_maker=(J-Jbeg)*DY
# else
         X_maker=(I-Ibeg)*DX(I,J)
         Y_maker=(J-Jbeg)*DY(I,J)
# endif
# endif

         DO KKK=1,NumDir
          Cm_eta(I,J,KK)=Cm_eta(I,J,KK)+Amp_Ser(KK,KKK)*COS( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Sm_eta(I,J,KK)=Sm_eta(I,J,KK)+Amp_Ser(KK,KKK)*SIN( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Cm_u(I,J,KK)=Cm_u(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *COS(Theta_Ser(KKK))*COS( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Cm_v(I,J,KK)=Cm_v(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *SIN(Theta_Ser(KKK))*COS( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )                   
          Sm_u(I,J,KK)=Sm_u(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *COS(Theta_Ser(KKK))*SIN( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )
          Sm_v(I,J,KK)=Sm_v(I,J,KK)+Amp_Ser(KK,KKK) &
                       *Segma_Ser(KK)*cosh(Wave_Number_Ser(KK)*Zlev)  &
                       /sinh(Wave_Number_Ser(KK)*Dep_Ser)  &
                   *SIN(Theta_Ser(KKK))*SIN( &
                   Wave_Number_Ser(KK)*SIN(Theta_Ser(KKK))*Y_maker &
                  +Wave_Number_Ser(KK)*COS(Theta_Ser(KKK))*X_maker )

         ENDDO

       ENDDO
       ENDDO
      ENDDO


END SUBROUTINE CALCULATE_TMA_Cm_Sm

!-------------------------------------------------------------------------------------
!
!    WK_WAVEMAKER_IRREGULAR_WAVE is subroutine 
!      to calculate source function for Wei and Kirbys 
!      internal wave maker, irregular wave (TMA)
!
!    HISTORY: 
!      11/8/2010 Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE WK_WAVEMAKER_IRREGULAR_WAVE & 
               (mfreq,mtheta,delta,h_gen,fm,fmax,fmin,gamma_spec,Hmo,theta_input,&
                sigma_theta_input,rlamda,beta_gen,D_gen,phi1,width,omgn, &
                Periodic,DY,Nglob)
     USE PARAM
#  if defined (PARALLEL)
     USE GLOBAL, only : myid, ier
# endif
# if defined(INTEL)
     USE IFPORT
# endif
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
! -- it seems theres an inaccuracy caused by 100 freq if mfreq<100
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

! in case wkn=0 02/08/2012  
        IF(wkn.eq.0.0)THEN
           wkn=SMALL
           C_phase=sqrt(grav*h_gen)
           wave_length=C_phase/fm
        ELSE
          C_phase=1.0_SP/wkn*fm*2.0_SP*pi
          wave_length=C_phase/fm
        ENDIF
!                          

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
        width=delta*wave_length/2.0_SP
! ---   create phi1

        do ktheta=1,mtheta
        do kf=1,mfreq
# if defined (INTEL)
          phi1(kf,ktheta)=rand()*2.0_SP*pi
# else
          phi1(kf,ktheta)=rand(0)*2.0_SP*pi
# endif
        enddo
        enddo

END SUBROUTINE WK_WAVEMAKER_IRREGULAR_WAVE

!-------------------------------------------------------------------------------------
!
!    WK_WAVEMAKER_REGULAR_WAVE is subroutine 
!      to calculate source function for Wei and Kirbys 
!      internal wave maker
!
!    HISTORY: 
!      10/22/2010 Fengyan Shi
!
!-------------------------------------------------------------------------------------
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

!-------------------------------------------------------------------------------------
!
!    WK_WAVEMAKER_TIME_SERIES is subroutine 
!      to generate time series using 
!      source function for Wei and Kirbys internal wave maker
!
!    HISTORY: 
!      04/13/2011 Fengyan Shi
!
!-------------------------------------------------------------------------------------
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


!-------------------------------------------------------------------------------------
!
!    VISCOSITY_WMAKER is subroutine 
!      to calculate viscosity inside wavemaker
!
!    HISTORY: 
!      08/19/2015 YoungKwang Choi
!
!-------------------------------------------------------------------------------------
SUBROUTINE VISCOSITY_WMAKER (M,N,ETAT,visbrk,H,MinDepthFrc,DX,DY)
     USE PARAM
     USE GLOBAL,ONLY : Depth,nu_break,ETA,Nghost,Xc_WK,Yc_WK,Ibeg,&
                       Jbeg,Width_WK,Xc_WK,Yc_WK,Ywidth_WK,WAVEMAKER_visbrk,nu_bkg
# if defined(PARALLEL)
     USE GLOBAL,ONLY : npx,npy
# endif
     IMPLICIT NONE
     INTEGER,INTENT(IN) :: M,N
     REAL(SP),INTENT(IN) :: visbrk,MinDepthFrc
     REAL(SP) :: cap1
     REAL(SP) :: xmk,ymk,DXg,DYg

# if defined (CARTESIAN)
     REAL(SP),INTENT(IN) :: DX,DY
# else
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: DX,DY
# endif
     REAL(SP),DIMENSION(M,N),INTENT(IN) :: ETAt,H
     
     DO J=Nghost+1,N-Nghost
     DO I=Nghost+1,M-Nghost

        tmp3=SQRT(GRAV*MAX(MinDepthFrc,H(I,J)))
        tmp2=visbrk*tmp3

# if defined (CARTESIAN)
        DXg=DX
		DYg=DY
# else
		DXg=DX(I,J)
		DYg=DY(I,J)
# endif

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

          IF(ETAt(I,J)>MIN(tmp2,WAVEMAKER_visbrk*tmp3))THEN
            cap1=1.0*(MAX(Depth(I,J),MinDepthFrc)+ETA(I,J))
            nu_break(I,J)=cap1*WAVEMAKER_visbrk*tmp3+nu_bkg
          ELSE
            nu_break(I,J)=ZERO+nu_bkg
          ENDIF
          
        ENDIF ! end wavemaker

     ENDDO
     ENDDO

END SUBROUTINE VISCOSITY_WMAKER
