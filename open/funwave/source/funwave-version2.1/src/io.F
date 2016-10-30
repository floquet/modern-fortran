! ----------------------------------------------------
!    This is subroutine to read input.txt
!  called by 
!        MAIN
!    Last Update: 09/26/2013 Babak Tehranirad, University of Delaware
! --------------------------------------------------

SUBROUTINE READ_INPUT
    USE GLOBAL
    USE Input_Util
    IMPLICIT NONE
    CHARACTER(LEN=80) FILE_NAME
    INTEGER::LINE
    INTEGER :: ierr

# if defined (PARALLEL)
    call MPI_COMM_RANK (MPI_COMM_WORLD, myid, ier)
# endif

      OPEN(3,FILE='LOG.txt')   

! read everything from input.txt
      FILE_NAME='input.txt'

! title
      CALL GET_STRING_VAL(TITLE,FILE_NAME,'TITLE',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No TITLE in ', FILE_NAME, 'use default'
        TITLE='---TEST RUN---'
      ENDIF
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)'---- LOG FILE ---'
      if (myid.eq.0) WRITE(3,*)TITLE
      if (myid.eq.0) WRITE(3,*)' --------------input start --------------'
# else
      WRITE(3,*)'---- LOG FILE ---'
      WRITE(3,*)TITLE
      WRITE(3,*)' --------------input start --------------'     
# endif

# if defined (PARALLEL)
! parallel info
      CALL GET_INTEGER_VAL(PX,FILE_NAME,'PX',line,ierr)
      CALL GET_INTEGER_VAL(PY,FILE_NAME,'PY',line,ierr)       
      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'PX   =',PX,'PY   =', PY
# endif
! dimension
      CALL GET_INTEGER_VAL(Mglob,FILE_NAME,'Mglob',line,ierr)
      CALL GET_INTEGER_VAL(Nglob,FILE_NAME,'Nglob',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'Mglob=',Mglob,'Nglob=', Nglob
# else
      WRITE(3,'(A7,I3,A7,I3)') 'Mglob=',Mglob,'Nglob=', Nglob
# endif
! grid 
# if defined (CARTESIAN)

      CALL GET_Float_VAL(DX,FILE_NAME,'DX',line,ierr)
      CALL GET_Float_VAL(DY,FILE_NAME,'DY',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A4,F12.2,A4,F12.2)')'DX=',DX,'DY=',DY
# else
      WRITE(3,'(A4,F12.2,A4,F12.2)')'DX=',DX,'DY=',DY
# endif

# else
      CALL GET_LOGICAL_VAL(StretchGrid,FILE_NAME,'StretchGrid',line,ierr)   
     if (StretchGrid) then
        CALL GET_STRING_VAL(DX_FILE,FILE_NAME,'DX_FILE',line,ierr)  
        CALL GET_STRING_VAL(DY_FILE,FILE_NAME,'DY_FILE',line,ierr)   
        CALL GET_STRING_VAL(Coriolis_FILE,FILE_NAME,'CORIOLIS_FILE',line,ierr)    
     else
      CALL GET_Float_VAL(Lon_West,FILE_NAME,'Lon_West',line,ierr)
      CALL GET_Float_VAL(Lat_South,FILE_NAME,'Lat_South',line,ierr)
      CALL GET_Float_VAL(Dphi,FILE_NAME,'Dphi',line,ierr)
      CALL GET_Float_VAL(Dtheta,FILE_NAME,'Dtheta',line,ierr)
     endif
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Lon_West=',Lon_West
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Lat_South=',Lat_South
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Dphi     =',Dphi
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Dtheta   =',Dtheta
# else
      WRITE(3,'(A10,F12.2)')'Lon_West=',Lon_West
      WRITE(3,'(A11,F12.2)')'Lat_South=',Lat_South
      WRITE(3,'(A11,F12.2)')'Dphi     =',Dphi
      WRITE(3,'(A11,F12.2)')'Dtheta   =',Dtheta
# endif

# endif

! result folder
      CALL GET_STRING_VAL(RESULT_FOLDER,FILE_NAME,'RESULT_FOLDER',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# else
      WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# endif
! station files
      CALL GET_INTEGER_VAL(NumberStations,FILE_NAME,'NumberStations',line,ierr)
      IF(NumberStations>0)THEN
      CALL GET_STRING_VAL(STATIONS_FILE,FILE_NAME,'STATIONS_FILE',line,ierr)
      ENDIF
! depth 
      CALL GET_STRING_VAL(DEPTH_TYPE,FILE_NAME,'DEPTH_TYPE',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_TYPE:', DEPTH_TYPE
# else
      WRITE(3,'(A12,A50)')'DEPTH_TYPE:', DEPTH_TYPE
# endif
      IF(DEPTH_TYPE(1:3)=='DAT')THEN
        CALL GET_STRING_VAL(DEPTH_FILE,FILE_NAME,'DEPTH_FILE',line,ierr)
        CALL GET_STRING_VAL(DepthFormat,FILE_NAME,'DepthFormat',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_FILE:', DEPTH_FILE
      if (myid.eq.0) WRITE(3,'(A14,A50)')'DEPTH_FORMAT:', DEPTHFORMAT
# else
      WRITE(3,'(A12,A50)')'DEPTH_FILE:', DEPTH_FILE
      WRITE(3,'(A14,A50)')'DEPTH_FORMAT:', DEPTHFORMAT
# endif
      ENDIF
      IF(DEPTH_TYPE(1:3)=='FLA')THEN
      CALL GET_Float_VAL(DEPTH_FLAT,FILE_NAME,'DEPTH_FLAT',line,ierr) 
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT  
# else
      WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT 
# endif
      ENDIF
      IF(DEPTH_TYPE(1:3)=='SLO')THEN
      CALL GET_Float_VAL(DEPTH_FLAT,FILE_NAME,'DEPTH_FLAT',line,ierr) 
      CALL GET_Float_VAL(SLP,FILE_NAME,'SLP',line,ierr) 
      CALL GET_Float_VAL(Xslp,FILE_NAME,'Xslp',line,ierr) 
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT 
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'SLP=', SLP
      if (myid.eq.0) WRITE(3,'(A6,F12.2)')'Xslp=', Xslp  
# else
      WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT 
      WRITE(3,'(A5,F12.2)')'SLP=', SLP
      WRITE(3,'(A6,F12.2)')'Xslp=', Xslp  
# endif
      ENDIF
! time
      CALL GET_Float_VAL(TOTAL_TIME,FILE_NAME,'TOTAL_TIME',line,ierr)
      CALL GET_Float_VAL(PLOT_INTV,FILE_NAME,'PLOT_INTV',line,ierr)
      CALL GET_Float_VAL(PLOT_INTV_STATION,FILE_NAME,'PLOT_INTV_STATION',line,ierr)
      CALL GET_Float_VAL(SCREEN_INTV,FILE_NAME,'SCREEN_INTV',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'TOTAL_TIME=', TOTAL_TIME
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'PLOT_INTV= ', PLOT_INTV
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'SCREEN_INTV=', SCREEN_INTV
# else
      WRITE(3,'(A12,F12.2)')'TOTAL_TIME=', TOTAL_TIME
      WRITE(3,'(A12,F12.2)')'PLOT_INTV= ', PLOT_INTV
      WRITE(3,'(A13,F12.2)')'SCREEN_INTV=', SCREEN_INTV
# endif

! initial uvz
      CALL GET_LOGICAL_VAL(INI_UVZ,FILE_NAME,'INI_UVZ',line,ierr)
      IF(INI_UVZ)THEN
        CALL GET_STRING_VAL(ETA_FILE,FILE_NAME,'ETA_FILE',line,ierr)
        CALL GET_STRING_VAL(U_FILE,FILE_NAME,'U_FILE',line,ierr)
        CALL GET_STRING_VAL(V_FILE,FILE_NAME,'V_FILE',line,ierr)
       ENDIF

# if defined (WIND)
! wind
      CALL GET_LOGICAL_VAL(WindForce,FILE_NAME,'WindForce',line,ierr)
      IF(WindForce)THEN
        CALL GET_STRING_VAL(WIND_FILE,FILE_NAME,'WIND_FILE',line,ierr)
        CALL GET_Float_VAL(Cdw,FILE_NAME,'Cdw',line,ierr)
        CALL GET_Float_VAL(WindCrestPercent,FILE_NAME,'WindCrestPercent',line,ierr)
      ENDIF
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A7,F12.5)')'Cdw = ', Cdw
      if (myid.eq.0) WRITE(3,'(A21,F12.5)')'WindCrestPercent = ', WindCrestPercent
# else
      WRITE(3,'(A7,F12.5)')'Cdw = ', Cdw
      WRITE(3,'(A21,F12.5)')'WindCrestPercent = ', WindCrestPercent
# endif  
# endif 

! wavemaker
      CALL GET_STRING_VAL(WaveMaker,FILE_NAME,'WAVEMAKER',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A11,A50)')'WAVEMAKER:', WAVEMAKER
# else
      WRITE(3,'(A11,A50)')'WAVEMAKER:', WAVEMAKER
# endif
        IF(WaveMaker(1:7)=='LEF_SOL')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(DEP_SOLI,FILE_NAME,'DEP',line,ierr)
          CALL GET_Float_VAL(LAG_SOLI,FILE_NAME,'LAGTIME',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'LAG_SOLI=', LAG_SOLI
# else
      WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI
      WRITE(3,'(A10,F12.2)')'LAG_SOLI=', LAG_SOLI
# endif
        ENDIF

        IF(WaveMaker(1:7)=='WK_TIME')THEN
        CALL GET_INTEGER_VAL(NumWaveComp,FILE_NAME,'NumWaveComp',line,ierr)
        CALL GET_Float_VAL(PeakPeriod,FILE_NAME,'PeakPeriod',line,ierr)
        CALL GET_STRING_VAL(WaveCompFile,FILE_NAME,'WaveCompFile',line,ierr)
          CALL GET_Float_VAL(Xc_WK,FILE_NAME,'Xc_WK',line,ierr)
          CALL GET_Float_VAL(DEP_WK,FILE_NAME,'DEP_WK',line,ierr)
          CALL GET_Float_VAL(Time_ramp,FILE_NAME,'Time_ramp',line,ierr)
          CALL GET_Float_VAL(Delta_WK,FILE_NAME,'Delta_WK',line,ierr)
          CALL GET_Float_VAL(Ywidth_WK,FILE_NAME,'Ywidth_WK',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc_WK   =', Xc_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_WK  =', DEP_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Time_ramp=', Time_ramp
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Delta_WK=', Delta_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
# else
      WRITE(3,'(A10,F12.2)')'Xc_WK   =', Xc_WK
      WRITE(3,'(A10,F12.2)')'DEP_WK  =', DEP_WK
      WRITE(3,'(A11,F12.2)')'Time_ramp=', Time_ramp
      WRITE(3,'(A11,F12.2)')'Delta_WK=', Delta_WK
      WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
# endif

        ENDIF

        IF(WaveMaker(1:7)=='INI_SOL')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(DEP_SOLI,FILE_NAME,'DEP',line,ierr)
          CALL GET_Float_VAL(XWAVEMAKER,FILE_NAME,'XWAVEMAKER',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI
# else
      WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI
# endif
        ENDIF
        IF(WaveMaker(1:6)=='N_WAVE')THEN
          CALL GET_Float_VAL(x1_Nwave,FILE_NAME,'x1_Nwave',line,ierr)
          CALL GET_Float_VAL(x2_Nwave,FILE_NAME,'x2_Nwave',line,ierr)
          CALL GET_Float_VAL(a0_Nwave,FILE_NAME,'a0_Nwave',line,ierr)
          CALL GET_Float_VAL(gamma_Nwave,FILE_NAME,'gamma_Nwave',line,ierr)
          CALL GET_Float_VAL(dep_Nwave,FILE_NAME,'dep_Nwave',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'x1_Nwave=', x1_Nwave
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'x2_Nwave=', x2_Nwave
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'a0_Nwave=', a0_Nwave
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'gamma_Nwave=', gamma_Nwave
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'dep_Nwave=', dep_Nwave
# else
      WRITE(3,'(A10,F12.2)')'x1_Nwave=', x1_Nwave
      WRITE(3,'(A10,F12.2)')'x2_Nwave=', x2_Nwave
      WRITE(3,'(A10,F12.2)')'a0_Nwave=', a0_Nwave
      WRITE(3,'(A13,F12.2)')'gamma_Nwave=', gamma_Nwave
      WRITE(3,'(A11,F12.2)')'dep_Nwave=', dep_Nwave
# endif
        ENDIF

        IF(WaveMaker(1:7)=='INI_REC')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(Xc,FILE_NAME,'Xc',line,ierr)
          CALL GET_Float_VAL(Yc,FILE_NAME,'Yc',line,ierr)
          CALL GET_Float_VAL(WID,FILE_NAME,'WID',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP     =', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc      =', Xc
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Yc      =', Yc
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'WID     =', WID
# else
      WRITE(3,'(A10,F12.2)')'AMP     =', AMP_SOLI
      WRITE(3,'(A10,F12.2)')'Xc      =', Xc
      WRITE(3,'(A10,F12.2)')'Yc      =', Yc
      WRITE(3,'(A10,F12.2)')'WID     =', WID
# endif
        ENDIF

        IF(WaveMaker(1:7)=='INI_GAU'.OR.&
           WaveMaker(1:7)=='INI_DIP')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(Xc,FILE_NAME,'Xc',line,ierr)
          CALL GET_Float_VAL(Yc,FILE_NAME,'Yc',line,ierr)
          CALL GET_Float_VAL(WID,FILE_NAME,'WID',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP     =', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc      =', Xc
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Yc      =', Yc
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'WID(gamma)=', WID
# else
      WRITE(3,'(A10,F12.2)')'AMP     =', AMP_SOLI
      WRITE(3,'(A10,F12.2)')'Xc      =', Xc
      WRITE(3,'(A10,F12.2)')'Yc      =', Yc
      WRITE(3,'(A12,F12.2)')'WID(gamma)=', WID
# endif
        ENDIF

        IF(WaveMaker(1:6)=='WK_REG')THEN
          CALL GET_Float_VAL(Xc_WK,FILE_NAME,'Xc_WK',line,ierr)
          CALL GET_Float_VAL(Tperiod,FILE_NAME,'Tperiod',line,ierr)
          CALL GET_Float_VAL(AMP_WK,FILE_NAME,'AMP_WK',line,ierr)
          CALL GET_Float_VAL(DEP_WK,FILE_NAME,'DEP_WK',line,ierr)
          CALL GET_Float_VAL(Theta_WK,FILE_NAME,'Theta_WK',line,ierr)
          CALL GET_Float_VAL(Time_ramp,FILE_NAME,'Time_ramp',line,ierr)
          CALL GET_Float_VAL(Delta_WK,FILE_NAME,'Delta_WK',line,ierr)
          CALL GET_Float_VAL(Ywidth_WK,FILE_NAME,'Ywidth_WK',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc_WK   =', Xc_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Tperiod =', Tperiod
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP_WK  =', AMP_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_WK  =', DEP_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Theta_WK=', Theta_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Time_ramp=', Time_ramp
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Delta_WK=', Delta_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
# else
      WRITE(3,'(A10,F12.2)')'Xc_WK   =', Xc_WK
      WRITE(3,'(A10,F12.2)')'Tperiod =', Tperiod
      WRITE(3,'(A10,F12.2)')'AMP_WK  =', AMP_WK
      WRITE(3,'(A10,F12.2)')'DEP_WK  =', DEP_WK
      WRITE(3,'(A10,F12.2)')'Theta_WK=', Theta_WK
      WRITE(3,'(A11,F12.2)')'Time_ramp=', Time_ramp
      WRITE(3,'(A11,F12.2)')'Delta_WK=', Delta_WK
      WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
# endif
        ENDIF
        IF(WaveMaker(1:6)=='WK_IRR')THEN
          CALL GET_Float_VAL(Xc_WK,FILE_NAME,'Xc_WK',line,ierr)
          CALL GET_Float_VAL(DEP_WK,FILE_NAME,'DEP_WK',line,ierr)
          CALL GET_Float_VAL(Time_ramp,FILE_NAME,'Time_ramp',line,ierr)
          CALL GET_Float_VAL(Delta_WK,FILE_NAME,'Delta_WK',line,ierr)
          CALL GET_Float_VAL(FreqPeak,FILE_NAME,'FreqPeak',line,ierr)
          CALL GET_Float_VAL(FreqMin,FILE_NAME,'FreqMin',line,ierr)
          CALL GET_Float_VAL(FreqMax,FILE_NAME,'FreqMax',line,ierr)
          CALL GET_Float_VAL(Hmo,FILE_NAME,'Hmo',line,ierr)
          CALL GET_Float_VAL(GammaTMA,FILE_NAME,'GammaTMA',line,ierr)
          CALL GET_Float_VAL(ThetaPeak,FILE_NAME,'ThetaPeak',line,ierr)
          CALL GET_Float_VAL(Sigma_Theta,FILE_NAME,'Sigma_Theta',line,ierr)
          CALL GET_Float_VAL(Ywidth_WK,FILE_NAME,'Ywidth_WK',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'FreqPeak=  ', FreqPeak
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'FreqMin =  ', FreqMin
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'FreqMax =  ', FreqMax
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Hmo     =  ', Hmo
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'GammaTMA=  ', GammaTMA
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'ThetaPeak= ', ThetaPeak
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'Sigma_Theta=', Sigma_Theta
# else
      WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
      WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
      WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
      WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
      WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
      WRITE(3,'(A12,F12.2)')'FreqPeak=  ', FreqPeak
      WRITE(3,'(A12,F12.2)')'FreqMin =  ', FreqMin
      WRITE(3,'(A12,F12.2)')'FreqMax =  ', FreqMax
      WRITE(3,'(A12,F12.2)')'Hmo     =  ', Hmo
      WRITE(3,'(A12,F12.2)')'GammaTMA=  ', GammaTMA
      WRITE(3,'(A12,F12.2)')'ThetaPeak= ', ThetaPeak
      WRITE(3,'(A13,F12.2)')'Sigma_Theta=', Sigma_Theta
# endif
        ENDIF
! ****
        IF(WaveMaker(1:9)=='WK_DATA2D')THEN
          CALL GET_Float_VAL(Xc_WK,FILE_NAME,'Xc_WK',line,ierr)
          CALL GET_Float_VAL(DEP_WK,FILE_NAME,'DEP_WK',line,ierr)
          CALL GET_Float_VAL(Time_ramp,FILE_NAME,'Time_ramp',line,ierr)
          CALL GET_Float_VAL(Delta_WK,FILE_NAME,'Delta_WK',line,ierr)
!          CALL GET_Float_VAL(PeakPeriod,FILE_NAME,'PeakPeriod',line,ierr)
!          CALL GET_INTEGER_VAL(NumFreq,FILE_NAME,'NumFreq',line,ierr)
!          CALL GET_INTEGER_VAL(NumDir,FILE_NAME,'NumDir',line,ierr)
          CALL GET_STRING_VAL(WaveCompFile,FILE_NAME,'WaveCompFile',line,ierr)
          CALL GET_Float_VAL(Ywidth_WK,FILE_NAME,'Ywidth_WK',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
!      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'PeakPeriod= ', PeakPeriod
!      if (myid.eq.0) WRITE(3,'(A12,I5)')'NumFreq=  ', NumFreq
!      if (myid.eq.0) WRITE(3,'(A12,I5)')'NumDir =  ', NumDir
# else
      WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
      WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
      WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
      WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
      WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
!      WRITE(3,'(A12,F12.2)')'FreqPeak=  ', FreqPeak
!      WRITE(3,'(A12,I5)')'NumFreq=  ', NumFreq
!      WRITE(3,'(A12,I5)')'NumDir =  ', NumDir
# endif
        ENDIF
! ******
# if defined (CARTESIAN)
! south-north periodic boundary condition
      CALL GET_LOGICAL_VAL(PERIODIC,FILE_NAME,'PERIODIC',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A11,A50)')'PERIODIC:', PERIODIC
# else
      WRITE(3,'(A11,A50)')'PERIODIC:', PERIODIC
# endif
# endif

! sponge layer
      CALL GET_LOGICAL_VAL(SPONGE_ON,FILE_NAME,'SPONGE_ON',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A11,A50)')'SPONGE_ON:', SPONGE_ON
# else
      WRITE(3,'(A11,A50)')'SPONGE_ON:', SPONGE_ON
# endif
      IF(SPONGE_ON)THEN
        CALL GET_Float_VAL(Sponge_west_width,FILE_NAME,'Sponge_west_width',line,ierr)
        CALL GET_Float_VAL(Sponge_east_width,FILE_NAME,'Sponge_east_width',line,ierr)
        CALL GET_Float_VAL(Sponge_south_width,FILE_NAME,'Sponge_south_width',line,ierr)
        CALL GET_Float_VAL(Sponge_north_width,FILE_NAME,'Sponge_north_width',line,ierr)
        CALL GET_Float_VAL(R_sponge,FILE_NAME,'R_sponge',line,ierr)
        CALL GET_Float_VAL(A_sponge,FILE_NAME,'A_sponge',line,ierr)
# if defined (PARALLEL)
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_west_width =', Sponge_west_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_east_width =', Sponge_east_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_south_width=', Sponge_south_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_north_width=', Sponge_north_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'R_sponge          =', R_sponge
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'A_sponge          =', A_sponge
# else
        WRITE(3,'(A20,F12.2)')'Sponge_west_width =', Sponge_west_width
        WRITE(3,'(A20,F12.2)')'Sponge_east_width =', Sponge_east_width
        WRITE(3,'(A20,F12.2)')'Sponge_south_width=', Sponge_south_width
        WRITE(3,'(A20,F12.2)')'Sponge_north_width=', Sponge_north_width
        WRITE(3,'(A20,F12.2)')'R_sponge          =', R_sponge
        WRITE(3,'(A20,F12.2)')'A_sponge          =', A_sponge
# endif
      ENDIF

! obstacle structures
      CALL GET_STRING_VAL(OBSTACLE_FILE,FILE_NAME,'OBSTACLE_FILE',line,ierr)
      IF(ierr==1)THEN
        OBSTACLE=.FALSE.
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A5)')'OBSTACLE_FILE:', 'NO'
# else
      WRITE(3,'(A15,A5)')'OBSTACLE_FILE:', 'NO'
# endif
      ELSE
        OBSTACLE=.TRUE.
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'OBSTACLE_FILE:', OBSTACLE_FILE
# else
      WRITE(3,'(A15,A50)')'OBSTACLE_FILE:', OBSTACLE_FILE
# endif
      ENDIF

! physics
          CALL GET_LOGICAL_VAL(DISPERSION,FILE_NAME,'DISPERSION',line,ierr)
          CALL GET_Float_VAL(Gamma1,FILE_NAME,'Gamma1',line,ierr)
# if defined (CARTESIAN)
          CALL GET_Float_VAL(Gamma2,FILE_NAME,'Gamma2',line,ierr)
          CALL GET_Float_VAL(Beta_ref,FILE_NAME,'Beta_ref',line,ierr)
# endif
# if defined (ZALPHA)
          CALL GET_Float_VAL(Beta_ref,FILE_NAME,'Beta_ref',line,ierr)
# endif
          CALL GET_Float_VAL(Gamma3,FILE_NAME,'Gamma3',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8)')'Physics'
# else
      WRITE(3,'(A8)')'Physics'
# endif

# if defined (PARALLEL)

       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Gamma1 = ', Gamma1
# if defined (CARTESIAN)
       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Gamma2 = ', Gamma2
       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Beta_ref= ', Beta_ref
       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Gamma3 = ', Gamma3
# endif
# else

       WRITE(3,'(A10,F12.2)')'Gamma1 = ', Gamma1
# if defined (CARTESIAN)
       WRITE(3,'(A10,F12.2)')'Gamma2 = ', Gamma2
       WRITE(3,'(A10,F12.2)')'Beta_ref= ', Beta_ref
# endif
       WRITE(3,'(A10,F12.2)')'Gamma3 = ', Gamma3
# endif

      CALL GET_Float_VAL(SWE_ETA_DEP,FILE_NAME,'SWE_ETA_DEP',line,ierr)
# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,'(A13,F12.2)')'SWE_ETA_DEP=', SWE_ETA_DEP
# else
       WRITE(3,'(A13,F12.2)')'SWE_ETA_DEP=', SWE_ETA_DEP
# endif
      CALL GET_LOGICAL_VAL(IN_Cd,FILE_NAME,'Friction_Matrix',line,ierr)
      CALL GET_STRING_VAL(CD_FILE,FILE_NAME,'Cd_file',line,ierr)  
      CALL GET_Float_VAL(Cd_fixed,FILE_NAME,'Cd',line,ierr)
     
# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,'(A13,F12.2)')'Cd_fixed         =', Cd_fixed
       if (myid.eq.0) WRITE(3,'(A15,A50)')'CD_FILE:', CD_FILE
# else
       WRITE(3,'(A13,F12.2)')'Cd_fixed         =', Cd_fixed
       WRITE(3,'(A15,A50)')'CD_FILE:', CD_FILE
# endif
! numerics schemes
      CALL GET_STRING_VAL(Time_Scheme,FILE_NAME,'Time_Scheme',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'Please define Time_Scheme in ', FILE_NAME
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,A50)')'TIME_SCHEME:', 'NOT DEFINED, STOP'
# else
      WRITE(3,'(A13,A50)')'TIME_SCHEME:', 'NOT DEFINED, STOP'
# endif
        STOP
      ENDIF
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,A50)')'TIME_SCHEME:', TIME_SCHEME
# else
      WRITE(3,'(A13,A50)')'TIME_SCHEME:', TIME_SCHEME
# endif
      CALL GET_STRING_VAL(CONSTR,FILE_NAME,'CONSTRUCTION',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No definition of CONSTRUCTION in ', FILE_NAME, 'use default'
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A14,A50)')'CONSTRUCTION', 'NOT DEFINED, USE DEFAULT'
# else
      WRITE(3,'(A14,A50)')'CONSTRUCTION', 'NOT DEFINED, USE DEFAULT'
# endif
        CONSTR='HLLC'
      ENDIF
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A14,A50)')'CONSTRUCTION:', CONSTR
# else
      WRITE(3,'(A14,A50)')'CONSTRUCTION:', CONSTR
# endif
      CALL GET_STRING_VAL(HIGH_ORDER,FILE_NAME,'HIGH_ORDER',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No definition of HIGH_ORDER in ', FILE_NAME, 'use default'
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'HIGH_ORDER', 'NOT DEFINED, USE DEFAULT'
# else
      WRITE(3,'(A12,A50)')'HIGH_ORDER', 'NOT DEFINED, USE DEFAULT'
# endif
        HIGH_ORDER='FOURTH'        
      ENDIF
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'HIGH_ORDER:', HIGH_ORDER
# else
      WRITE(3,'(A12,A50)')'HIGH_ORDER:', HIGH_ORDER
# endif
! CFL
      CALL GET_Float_VAL(CFL,FILE_NAME,'CFL',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'CFL=', CFL
# else
      WRITE(3,'(A5,F12.2)')'CFL=', CFL
# endif
! Froude Number Cap
      CALL GET_Float_VAL(FroudeCap,FILE_NAME,'FroudeCap',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'FroudeCap=', FroudeCap
# else
      WRITE(3,'(A5,F12.2)')'FroudeCap=', FroudeCap
# endif
! MinDepth etc
      CALL GET_Float_VAL(MinDepth,FILE_NAME,'MinDepth',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
# else
      WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
# endif
      CALL GET_Float_VAL(MinDepthFrc,FILE_NAME,'MinDepthFrc',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'MinDepthFrc=', MinDepthFrc
# else
      WRITE(3,'(A13,F12.2)')'MinDepthFrc=', MinDepthFrc
# endif

! show breaking
      CALL GET_LOGICAL_VAL(SHOW_BREAKING,FILE_NAME,'SHOW_BREAKING',line,ierr)
      IF(SHOW_BREAKING)THEN
      CALL GET_Float_VAL(Cbrk1,FILE_NAME,'Cbrk1',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk1 =', Cbrk1
# else
      WRITE(3,'(A8,F12.6)')'Cbrk1 =', Cbrk1
# endif
      CALL GET_Float_VAL(Cbrk2,FILE_NAME,'Cbrk2',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk2 =', Cbrk2
# else
      WRITE(3,'(A8,F12.6)')'Cbrk2 =', Cbrk2
# endif
      ENDIF
# if defined (MIXING)
      CALL GET_Float_VAL(T_INTV_mean,FILE_NAME,'T_INTV_mean',line,ierr)
      CALL GET_Float_VAL(C_smg,FILE_NAME,'C_smg',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A14,F12.6)')'T_INTV_mean =', T_INTV_mean
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'C_smg =', C_smg
# else
      WRITE(3,'(A14,F12.6)')'T_INTV_mean =', T_INTV_mean
      WRITE(3,'(A8,F12.6)')'C_smg =', C_smg
# endif
# endif

# if defined (COUPLING)
      CALL GET_STRING_VAL(COUPLING_FILE,FILE_NAME,'COUPLING_FILE',line,ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# else
      WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# endif
# endif

! output parameters
      CALL GET_INTEGER_VAL(OUTPUT_RES,FILE_NAME,'OUTPUT_RES',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_DEPTH,FILE_NAME,'DEPTH_OUT',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_U,FILE_NAME,'U',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_V,FILE_NAME,'V',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_ETA,FILE_NAME,'ETA',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Hmax,FILE_NAME,'Hmax',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Hmin,FILE_NAME,'Hmin',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Umax,FILE_NAME,'Umax',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_MFmax,FILE_NAME,'MFmax',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_VORmax,FILE_NAME,'VORmax',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_MASK,FILE_NAME,'MASK',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_MASK9,FILE_NAME,'MASK9',line,ierr)
# if defined (MIXING)
      CALL GET_LOGICAL_VAL(OUT_Umean,FILE_NAME,'Umean',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Vmean,FILE_NAME,'Vmean',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_ETAmean,FILE_NAME,'ETAmean',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_WaveHeight,FILE_NAME,'WaveHeight',line,ierr)
# endif
      CALL GET_LOGICAL_VAL(OUT_SXL,FILE_NAME,'SXL',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SXR,FILE_NAME,'SXR',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SYL,FILE_NAME,'SYL',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SYR,FILE_NAME,'SYR',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SourceX,FILE_NAME,'SourceX',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_SourceY,FILE_NAME,'SourceY',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_P,FILE_NAME,'P',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Q,FILE_NAME,'Q',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Fx,FILE_NAME,'Fx',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Fy,FILE_NAME,'Fy',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Gx,FILE_NAME,'Gx',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Gy,FILE_NAME,'Gy',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_AGE,FILE_NAME,'AGE',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_TMP,FILE_NAME,'TMP',line,ierr)

! 
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)' --------------input end --------------' 
# else
      WRITE(3,*)' --------------input end --------------' 
# endif

END SUBROUTINE READ_INPUT

# if defined (CARTESIAN)
! -------------
!    Writes station data
! Fengyan Shi modified based on Jeff Harris' for Spherical
! here simply specify grid number i and j instead of x and y
! later modified parallelization part at LSU
! 09/16/2011
! -------------
SUBROUTINE STATIONS
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: iunit
     REAL(SP) :: dum1,dum2
     CHARACTER(LEN=80)::FILE_NAME=''
     CHARACTER(LEN=80)::TMP_NAME=''
     CHARACTER(LEN=80)::FDIR=''

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (icount.eq.0) then
       ALLOCATE(ista(NumberStations),&
                jsta(NumberStations),&
                nsta(NumberStations))
! calculate how many output components
              
       open(100,FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(100,*) dum1,dum2
# if defined (PARALLEL)
          ista(i) = Nghost+dum1-npx*Mglob/px
          jsta(i) = Nghost+dum2-npy*Nglob/py
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif

# else
          ista(i) = Nghost+dum1
          jsta(i) = Nghost+dum2
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# endif
       enddo
     endif

! write to stations

     do i=1,NumberStations
       if (nsta(i).eq.1) then
          iunit=100+i
          write (iunit,'(20E16.5)') time, eta(ista(i),jsta(i)),&
                          u(ista(i),jsta(i)),v(ista(i),jsta(i))
       endif
     enddo

! close station files
     if (TIME.ge.TOTAL_TIME) then
       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=100+i
             close(iunit)
          endif
       enddo
     endif

END SUBROUTINE STATIONS

# else

SUBROUTINE STATIONS_SPHERICAL_IJ
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: iunit
     REAL(SP) :: dum1,dum2
     REAL(SP) :: eta_sta,u_sta,v_sta
     CHARACTER(LEN=80)::FILE_NAME=''
     CHARACTER(LEN=80)::TMP_NAME=''
     CHARACTER(LEN=80)::FDIR=''

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (icount.eq.0) then
       ALLOCATE(ista(NumberStations),&
                jsta(NumberStations),&
                nsta(NumberStations))
  
! calculate how many output components
              
       open(100,FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(100,*) dum1,dum2
# if defined (PARALLEL)
          ista(i) = Nghost+dum1-npx*Mglob/px
          jsta(i) = Nghost+dum2-npy*Nglob/py
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif

# else
          ista(i) = Nghost+dum1
          jsta(i) = Nghost+dum2
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# endif
       enddo
     endif

! write to stations

     do i=1,NumberStations
       if (nsta(i).eq.1) then
          iunit=100+i
          IF(mask(ista(i),jsta(i))<1)THEN
            eta_sta=ZERO
            u_sta=ZERO
            v_sta=ZERO
          ELSE  ! to avoid topography on nested water surface
            eta_sta=eta(ista(i),jsta(i))
            u_sta=u(ista(i),jsta(i))
            v_sta=v(ista(i),jsta(i)) 
          ENDIF
          write (iunit,'(20E16.5)') time, eta_sta,&
                          u_sta,v_sta
       endif
     enddo

! close station files
     if (TIME.ge.TOTAL_TIME) then
       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=100+i
             close(iunit)
          endif
       enddo
     endif

END SUBROUTINE STATIONS_SPHERICAL_IJ

! -------------
!    Writes station data,  works in spherical
! Jeff Harris did this 04/05/2011
! -------------
SUBROUTINE STATIONS
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: iunit
     REAL(SP) :: dum1,dum2
     REAL(SP) :: eta_sta,u_sta,v_sta
     CHARACTER(LEN=80)::FILE_NAME=''
     CHARACTER(LEN=80)::TMP_NAME=''
     CHARACTER(LEN=80)::FDIR=''

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (icount.eq.0) then
       ALLOCATE(ista(NumberStations),&
                jsta(NumberStations),&
                nsta(NumberStations))
!       ALLOCATE(envelope(Mloc,Nloc))
!       envelope = 0.d0
       open(100,FILE=TRIM(STATIONS_FILE))
       do i=1,NumberStations
          read(100,*) dum1,dum2
# if defined (PARALLEL)
          ista(i) = Nghost+1+nint((dum2-Lon_West)/Dphi) &
                     -npx*Mglob/px
          jsta(i) = Nghost+1+nint((dum1-Lat_South)/Dtheta) &
                     -npy*Nglob/py
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# else
          ista(i) = Nghost+1+nint((dum2-Lon_West)/Dphi) 
          jsta(i) = Nghost+1+nint((dum1-Lat_South)/Dtheta) 
          if ((ista(i).ge.Ibeg).and.(ista(i).le.Iend).and.&
              (jsta(i).ge.Jbeg).and.(jsta(i).le.Jend)) then
             nsta(i) = 1
             write(file_name(1:4),'(I4.4)') i
             TMP_NAME = TRIM(FDIR)//'sta_'//TRIM(FILE_NAME)
             iunit=100+i
             open(iunit,FILE=TMP_NAME)
          else
             nsta(i) = 0
          endif
# endif
       enddo
     endif

! write to stations
 
     do i=1,NumberStations
       if (nsta(i).eq.1) then
          iunit=100+i

! for coupling, make sure dry point
          IF(mask(ista(i),jsta(i))<1)THEN
            eta_sta=ZERO
            u_sta=ZERO
            v_sta=ZERO
          ELSE  ! to avoid topography on nested water surface
            eta_sta=eta(ista(i),jsta(i))
            u_sta=u(ista(i),jsta(i))
            v_sta=v(ista(i),jsta(i)) 
          ENDIF

          write (iunit,'(20E16.5)') time, eta_sta,&
                          u_sta,v_sta

!          write (iunit,*) time, eta(ista(i),jsta(i)),u(ista(i),jsta(i)),&
!                                v(ista(i),jsta(i))

       endif
     enddo

! close station files
     if (TIME.ge.TOTAL_TIME) then
!       TMP_NAME = TRIM(FDIR)//'envelope.out'
!       call PutFile(TMP_NAME,envelope)
       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=100+i
             close(iunit)
          endif
       enddo
     endif

END SUBROUTINE STATIONS

# endif

! ----------------------------------------------------
!    This is subroutine for preview
!  called by 
!        MAIN
!    Last Update: 09/26/2013 Babak Tehranirad, University of Delaware
! --------------------------------------------------
SUBROUTINE PREVIEW
     USE GLOBAL
     IMPLICIT NONE

     CHARACTER(LEN=80)::FILE_NAME=''
     CHARACTER(LEN=80)::TMP_NAME=''
     CHARACTER(LEN=80)::FDIR=''

     FDIR=TRIM(RESULT_FOLDER)

     ICOUNT=ICOUNT+1

# if defined (PARALLEL)
        if (myid.eq.0)then
        WRITE(3,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
        WRITE(*,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time        
        endif
# else
        WRITE(*,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
# endif
102     FORMAT(A20,I4,A14,F12.3,A2,F12.3)

        itmp1=mod(icount/1000,10)
        itmp2=mod(icount/100,10)
        itmp3=mod(icount/10,10)
        itmp4=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4

     IF(ICOUNT==1)THEN
     IF(OUT_DEPTH)THEN
        TMP_NAME = TRIM(FDIR)//'dep.out'
        call PutFile(TMP_NAME,DEPTH)
     ENDIF
     ENDIF

     IF(OUT_ETA)THEN
        TMP_NAME = TRIM(FDIR)//'eta_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Eta)
     ENDIF

     IF(OUT_Hmax)THEN
        TMP_NAME = TRIM(FDIR)//'hmax_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,HeightMax)
     ENDIF

     IF(OUT_Hmin)THEN
        TMP_NAME = TRIM(FDIR)//'hmin_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,HeightMin)
     ENDIF

     IF(OUT_Umax)THEN
        TMP_NAME = TRIM(FDIR)//'umax_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,VelocityMax)
     ENDIF
     
     IF(OUT_MFmax)THEN                                                                                            
        TMP_NAME = TRIM(FDIR)//'MFmax_'//TRIM(FILE_NAME)                                                          
        call PutFile(TMP_NAME,MomentumFluxMax)                                                                              
     ENDIF      
     
     IF(OUT_VORmax)THEN                                                                                            
        TMP_NAME = TRIM(FDIR)//'VORmax_'//TRIM(FILE_NAME)                                                          
        call PutFile(TMP_NAME,VorticityMax)                                                                              
     ENDIF            
     
# if defined (MIXING)
     IF(OUT_Umean)THEN
        TMP_NAME = TRIM(FDIR)//'umean_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Umean)
     ENDIF
     IF(OUT_Vmean)THEN
        TMP_NAME = TRIM(FDIR)//'vmean_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Vmean)
     ENDIF
     IF(OUT_ETAmean)THEN
        TMP_NAME = TRIM(FDIR)//'etamean_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,ETAmean)
     ENDIF
     IF(OUT_WaveHeight)THEN
        TMP_NAME = TRIM(FDIR)//'hrms_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,WaveHeightRMS)
        TMP_NAME = TRIM(FDIR)//'havg_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,WaveHeightAve)
     ENDIF

# endif

     IF(OUT_U)THEN
        TMP_NAME = TRIM(FDIR)//'u_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,U)
     ENDIF

     IF(OUT_V)THEN
        TMP_NAME = TRIM(FDIR)//'v_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,V)
     ENDIF

     IF(OUT_MASK)THEN
        TMP_NAME = TRIM(FDIR)//'mask_'//TRIM(FILE_NAME)
        Int2Flo=MASK
        call PutFile(TMP_NAME,Int2Flo)
     ENDIF

     IF(OUT_MASK9)THEN
        TMP_NAME = TRIM(FDIR)//'mask9_'//TRIM(FILE_NAME)
        Int2Flo=MASK9
        call PutFile(TMP_NAME,Int2Flo)
     ENDIF

210   FORMAT(5000I3)

     IF(OUT_P)THEN
        TMP_NAME = TRIM(FDIR)//'p_'//TRIM(FILE_NAME)
!        call PutFile(TMP_NAME,P)
     ENDIF

     IF(OUT_Q)THEN
        TMP_NAME = TRIM(FDIR)//'q_'//TRIM(FILE_NAME)
!        call PutFile(TMP_NAME,Q)
     ENDIF


     IF(OUT_AGE)THEN
        TMP_NAME = TRIM(FDIR)//'age_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,AGE_BREAKING)
     ENDIF

     IF(OUT_TMP)THEN
        TMP_NAME = TRIM(FDIR)//'tmp_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,tmp4preview)
     ENDIF

101   continue

END SUBROUTINE PREVIEW

# if defined (PARALLEL)
SUBROUTINE GetFile (FILE,PHI)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     ! could be max. procs
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT) :: PHI

! TEMP

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost+1,NGlob+NGhost
           READ(1,*)(PHIGLOB(I,J),I=Nghost+1,MGlob+Nghost)
        ENDDO
        CLOSE(1)
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

END SUBROUTINE Getfile
# endif

# if defined (PARALLEL)
SUBROUTINE PutFile (FILE,PHI)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     ! could be max. procs
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI


! first time call 
     IF(icount.EQ.1)THEN
! format length
        write(FORMAT_LEN(1:1),'(A1)') '('
        write(FORMAT_LEN(2:8),'(I7)') Mglob
        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN(14:14),'(A1)') ')'
     ENDIF

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

     do i=1,Mloc
     do j=1,Nloc
        call MPI_Gather(PHI(i,j),1,MPI_SP,&
             xx,1,MPI_SP,0,MPI_COMM_WORLD,ier)

        if (j.eq.1) call MPI_Barrier(MPI_COMM_WORLD,ier)

        if (myid.eq.0) then
           do l=1,px*py
              PHIGLOB(i+npxs(l)*(Iend-Ibeg+1),&
                   j+npys(l)*(Jend-Jbeg+1)) = xx(l)
           enddo
        endif
     enddo
     enddo

     if (myid.eq.0) then
        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost+1,NGlob+NGhost,OUTPUT_RES
           WRITE(1,FORMAT_LEN)(real(PHIGLOB(I,J)),I=Nghost+1,MGlob+Nghost,OUTPUT_RES)
        ENDDO
!100  FORMAT(5000E16.6)
!100   FORMAT(FORMAT_LEN)
        CLOSE(1)
     endif

END SUBROUTINE Putfile

# else
SUBROUTINE PutFile(FILE,PHI)
     USE PARAM
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI
     CHARACTER(LEN=80) FILE

! first time call 
     IF(icount.EQ.1)THEN
! format length
        write(FORMAT_LEN(1:1),'(A1)') '('
        write(FORMAT_LEN(2:8),'(I7)') Mglob
        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN(14:14),'(A1)') ')'
     ENDIF

        OPEN(1,FILE=TRIM(FILE))
        DO J=Nghost+1,Nloc-Nghost,OUTPUT_RES
           WRITE(1,FORMAT_LEN)(real(PHI(I,J)),I=Nghost+1,Mloc-Nghost,OUTPUT_RES)
        ENDDO
!100  FORMAT(5000E16.6)
!100   FORMAT(FORMAT_LEN)
        CLOSE(1)
END SUBROUTINE PutFile

# endif

