!------------------------------------------------------------------------------------
!
!      FILE io.F
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
!    OUTPUT is subroutine for screen, station, and field print-out
!
!    HISTORY:
!      01/10/2011  Fengyan SHi
!-------------------------------------------------------------------------------------
SUBROUTINE OUTPUT
    USE GLOBAL
    IMPLICIT NONE

     SCREEN_COUNT=SCREEN_COUNT+DT

     IF(SCREEN_COUNT>=SCREEN_INTV)THEN
      SCREEN_COUNT=SCREEN_COUNT-SCREEN_INTV
      CALL STATISTICS
     ENDIF

! stations
      IF(NumberStations>0)THEN
      PLOT_COUNT_STATION=PLOT_COUNT_STATION+DT
      IF(PLOT_COUNT_STATION>=PLOT_INTV_STATION)THEN
       PLOT_COUNT_STATION=PLOT_COUNT_STATION-PLOT_INTV_STATION
# if defined (SPHERICAL_IJ_STATION)
       ! use IJ for coupling purpose
       CALL STATIONS_SPHERICAL_IJ
# else
       CALL STATIONS
# endif
      ENDIF
      ENDIF
! preview

	PLOT_COUNT=PLOT_COUNT+DT
      IF(PLOT_COUNT>=PLOT_INTV)THEN
       PLOT_COUNT=PLOT_COUNT-PLOT_INTV
       CALL PREVIEW
      ENDIF

END SUBROUTINE OUTPUT

!-------------------------------------------------------------------------------------
!
!    READ_INPUT is subroutine to read from input.txt
!
!  HISTORY:
!  01/10/2011  Fengyan SHi
!  12/23/2014  Young-Kwang Choi, added option for intel compiler
!
!-------------------------------------------------------------------------------------

SUBROUTINE READ_INPUT
    USE GLOBAL
    USE INPUT_READ
![jychoi added this for intel compiler 14.12.23    
# if defined(INTEL)
     USE IFPORT
# endif

!jychoi 14.12.23]
    IMPLICIT NONE
    CHARACTER(LEN=80) FILE_NAME
    CHARACTER(LEN=80) MKFOLDER
    INTEGER::LINE
    INTEGER :: ierr
    INTEGER :: I_comp

![ykchoi
    CHARACTER(LEN=80)::FDIR=' '
!ykchoi]

# if defined (PARALLEL)
    CALL MPI_COMM_RANK (MPI_COMM_WORLD, myid, ier)
# endif

![ykchoi (15.03.25)
      FDIR=TRIM(RESULT_FOLDER)
	OPEN(10000,FILE='time_dt.out',STATUS='UNKNOWN')
	!OPEN(10000,FILE=TRIM( TRIM(FDIR)//'time.out' ),STATUS='UNKNOWN')
!ykchoi (15.03.25)]

      OPEN(3,FILE='LOG.txt')   

! read everything from input.txt
      FILE_NAME='input.txt'

! title
      CALL READ_STRING(TITLE,FILE_NAME,'TITLE',ierr)
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
      CALL READ_INTEGER(PX,FILE_NAME,'PX',ierr)
      CALL READ_INTEGER(PY,FILE_NAME,'PY',ierr)       
      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'PX   =',PX,'PY   =', PY
# endif
! dimension
      CALL READ_INTEGER(Mglob,FILE_NAME,'Mglob',ierr)
      CALL READ_INTEGER(Nglob,FILE_NAME,'Nglob',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'Mglob=',Mglob,'Nglob=', Nglob
# else
      WRITE(3,'(A7,I3,A7,I3)') 'Mglob=',Mglob,'Nglob=', Nglob
# endif
! grid 
# if defined (CARTESIAN)

      CALL READ_FLOAT(DX,FILE_NAME,'DX',ierr)
      CALL READ_FLOAT(DY,FILE_NAME,'DY',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A4,F12.2,A4,F12.2)')'DX=',DX,'DY=',DY
# else
      WRITE(3,'(A4,F12.2,A4,F12.2)')'DX=',DX,'DY=',DY
# endif

# else
      CALL READ_LOGICAL(StretchGrid,FILE_NAME,'StretchGrid',ierr)   
     if (StretchGrid) then
        CALL READ_STRING(DX_FILE,FILE_NAME,'DX_FILE',ierr)  
        CALL READ_STRING(DY_FILE,FILE_NAME,'DY_FILE',ierr)   
        CALL READ_STRING(Coriolis_FILE,FILE_NAME,'CORIOLIS_FILE',ierr)    
     else
      CALL READ_FLOAT(Lon_West,FILE_NAME,'Lon_West',ierr)
      CALL READ_FLOAT(Lat_South,FILE_NAME,'Lat_South',ierr)
      CALL READ_FLOAT(Dphi,FILE_NAME,'Dphi',ierr)
      CALL READ_FLOAT(Dtheta,FILE_NAME,'Dtheta',ierr)
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
      CALL READ_STRING(RESULT_FOLDER,FILE_NAME,'RESULT_FOLDER',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# else
      WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER
# endif

! create result folder
      MKFOLDER = "mkdir -p "//TRIM(RESULT_FOLDER)
# if defined (PARALLEL)
      IF (myid.eq.0) THEN
        CALL SYSTEM(TRIM(MKFOLDER))
      ENDIF
# else
      CALL SYSTEM(TRIM(MKFOLDER))
# endif

! station files
      CALL READ_INTEGER(NumberStations,FILE_NAME,'NumberStations',ierr)
      IF(NumberStations>0)THEN
      CALL READ_STRING(STATIONS_FILE,FILE_NAME,'STATIONS_FILE',ierr)
      ENDIF
! depth 
      CALL READ_STRING(DEPTH_TYPE,FILE_NAME,'DEPTH_TYPE',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_TYPE:', DEPTH_TYPE
# else
      WRITE(3,'(A12,A50)')'DEPTH_TYPE:', DEPTH_TYPE
# endif
      IF(DEPTH_TYPE(1:3)=='DAT')THEN
        CALL READ_STRING(DEPTH_FILE,FILE_NAME,'DEPTH_FILE',ierr)
        CALL READ_STRING(DepthFormat,FILE_NAME,'DepthFormat',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_FILE:', DEPTH_FILE
      if (myid.eq.0) WRITE(3,'(A14,A50)')'DEPTH_FORMAT:', DEPTHFORMAT
# else
      WRITE(3,'(A12,A50)')'DEPTH_FILE:', DEPTH_FILE
      WRITE(3,'(A14,A50)')'DEPTH_FORMAT:', DEPTHFORMAT
# endif
      ENDIF
      IF(DEPTH_TYPE(1:3)=='FLA')THEN
      CALL READ_FLOAT(DEPTH_FLAT,FILE_NAME,'DEPTH_FLAT',ierr) 
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT  
# else
      WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT 
# endif
      ENDIF
      IF(DEPTH_TYPE(1:3)=='SLO')THEN
      CALL READ_FLOAT(DEPTH_FLAT,FILE_NAME,'DEPTH_FLAT',ierr) 
      CALL READ_FLOAT(SLP,FILE_NAME,'SLP',ierr) 
      CALL READ_FLOAT(Xslp,FILE_NAME,'Xslp',ierr) 
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
      CALL READ_FLOAT(TOTAL_TIME,FILE_NAME,'TOTAL_TIME',ierr)
      CALL READ_FLOAT(PLOT_INTV,FILE_NAME,'PLOT_INTV',ierr)
      CALL READ_FLOAT(PLOT_INTV_STATION,FILE_NAME,'PLOT_INTV_STATION',ierr)
      CALL READ_FLOAT(SCREEN_INTV,FILE_NAME,'SCREEN_INTV',ierr)
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
      CALL READ_LOGICAL(INI_UVZ,FILE_NAME,'INI_UVZ',ierr)
      IF(INI_UVZ)THEN
        CALL READ_STRING(ETA_FILE,FILE_NAME,'ETA_FILE',ierr)
        CALL READ_STRING(U_FILE,FILE_NAME,'U_FILE',ierr)
        CALL READ_STRING(V_FILE,FILE_NAME,'V_FILE',ierr)
        CALL READ_STRING(MASK_FILE,FILE_NAME,'MASK_FILE',ierr)

        IF(ierr==1)THEN
          NO_MASK_FILE = .TRUE.
        ELSE
          NO_MASK_FILE = .FALSE.
        ENDIF

	![ykchoi(14.12.24.)
        !CALL READ_FLOAT(TIME,FILE_NAME,'HotStartTime',ierr)
	  CALL READ_FLOAT(HotStartTime,FILE_NAME,'HotStartTime',ierr)
	!ykchoi(14.12.24.)]
        CALL READ_INTEGER(icount,FILE_NAME,'OutputStartNumber',ierr)
        icount = icount-1
       ENDIF

! add water level 03/29/2016

        CALL READ_FLOAT(WaterLevel,FILE_NAME,'WaterLevel',ierr)
        IF(ierr==1)THEN
          WaterLevel = 0.0
        ENDIF

# if defined (WIND)
! wind
      CALL READ_LOGICAL(WindForce,FILE_NAME,'WindForce',ierr)
      IF(WindForce)THEN
        CALL READ_STRING(WIND_FILE,FILE_NAME,'WIND_FILE',ierr)
        CALL READ_FLOAT(Cdw,FILE_NAME,'Cdw',ierr)
        CALL READ_FLOAT(WindCrestPercent,FILE_NAME,'WindCrestPercent',ierr)
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
      CALL READ_STRING(WaveMaker,FILE_NAME,'WAVEMAKER',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A11,A50)')'WAVEMAKER:', WAVEMAKER
# else
      WRITE(3,'(A11,A50)')'WAVEMAKER:', WAVEMAKER
# endif
        IF(WaveMaker(1:7)=='LEF_SOL')THEN
          CALL READ_FLOAT(AMP_SOLI,FILE_NAME,'AMP',ierr)
          CALL READ_FLOAT(DEP_SOLI,FILE_NAME,'DEP',ierr)
          CALL READ_FLOAT(LAG_SOLI,FILE_NAME,'LAGTIME',ierr)
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
        CALL READ_INTEGER(NumWaveComp,FILE_NAME,'NumWaveComp',ierr)
        CALL READ_FLOAT(PeakPeriod,FILE_NAME,'PeakPeriod',ierr)
        CALL READ_STRING(WaveCompFile,FILE_NAME,'WaveCompFile',ierr)
          CALL READ_FLOAT(Xc_WK,FILE_NAME,'Xc_WK',ierr)
          CALL READ_FLOAT(Yc_WK,FILE_NAME,'Yc_WK',ierr)
          CALL READ_FLOAT(DEP_WK,FILE_NAME,'DEP_WK',ierr)
          CALL READ_FLOAT(Time_ramp,FILE_NAME,'Time_ramp',ierr)
          CALL READ_FLOAT(Delta_WK,FILE_NAME,'Delta_WK',ierr)
          CALL READ_FLOAT(Ywidth_WK,FILE_NAME,'Ywidth_WK',ierr)
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
          CALL READ_FLOAT(AMP_SOLI,FILE_NAME,'AMP',ierr)
          CALL READ_FLOAT(DEP_SOLI,FILE_NAME,'DEP',ierr)
          CALL READ_FLOAT(XWAVEMAKER,FILE_NAME,'XWAVEMAKER',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI
# else
      WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI
# endif
        ENDIF
        IF(WaveMaker(1:6)=='N_WAVE')THEN
          CALL READ_FLOAT(x1_Nwave,FILE_NAME,'x1_Nwave',ierr)
          CALL READ_FLOAT(x2_Nwave,FILE_NAME,'x2_Nwave',ierr)
          CALL READ_FLOAT(a0_Nwave,FILE_NAME,'a0_Nwave',ierr)
          CALL READ_FLOAT(gamma_Nwave,FILE_NAME,'gamma_Nwave',ierr)
          CALL READ_FLOAT(dep_Nwave,FILE_NAME,'dep_Nwave',ierr)
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
          CALL READ_FLOAT(AMP_SOLI,FILE_NAME,'AMP',ierr)
          CALL READ_FLOAT(Xc,FILE_NAME,'Xc',ierr)
          CALL READ_FLOAT(Yc,FILE_NAME,'Yc',ierr)
          CALL READ_FLOAT(WID,FILE_NAME,'WID',ierr)
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
          CALL READ_FLOAT(AMP_SOLI,FILE_NAME,'AMP',ierr)
          CALL READ_FLOAT(Xc,FILE_NAME,'Xc',ierr)
          CALL READ_FLOAT(Yc,FILE_NAME,'Yc',ierr)
          CALL READ_FLOAT(WID,FILE_NAME,'WID',ierr)
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
          CALL READ_FLOAT(Xc_WK,FILE_NAME,'Xc_WK',ierr)
          CALL READ_FLOAT(Yc_WK,FILE_NAME,'Yc_WK',ierr)
          CALL READ_FLOAT(Tperiod,FILE_NAME,'Tperiod',ierr)
          CALL READ_FLOAT(AMP_WK,FILE_NAME,'AMP_WK',ierr)
          CALL READ_FLOAT(DEP_WK,FILE_NAME,'DEP_WK',ierr)
          CALL READ_FLOAT(Theta_WK,FILE_NAME,'Theta_WK',ierr)
          CALL READ_FLOAT(Time_ramp,FILE_NAME,'Time_ramp',ierr)
          CALL READ_FLOAT(Delta_WK,FILE_NAME,'Delta_WK',ierr)
          CALL READ_FLOAT(Ywidth_WK,FILE_NAME,'Ywidth_WK',ierr)
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
          CALL READ_FLOAT(Xc_WK,FILE_NAME,'Xc_WK',ierr)
          CALL READ_FLOAT(Yc_WK,FILE_NAME,'Yc_WK',ierr)
          CALL READ_FLOAT(DEP_WK,FILE_NAME,'DEP_WK',ierr)
          CALL READ_FLOAT(Time_ramp,FILE_NAME,'Time_ramp',ierr)
          CALL READ_FLOAT(Delta_WK,FILE_NAME,'Delta_WK',ierr)
          CALL READ_FLOAT(FreqPeak,FILE_NAME,'FreqPeak',ierr)
          CALL READ_FLOAT(FreqMin,FILE_NAME,'FreqMin',ierr)
          CALL READ_FLOAT(FreqMax,FILE_NAME,'FreqMax',ierr)
          CALL READ_FLOAT(Hmo,FILE_NAME,'Hmo',ierr)
          CALL READ_FLOAT(GammaTMA,FILE_NAME,'GammaTMA',ierr)
          CALL READ_FLOAT(ThetaPeak,FILE_NAME,'ThetaPeak',ierr)
          CALL READ_FLOAT(Sigma_Theta,FILE_NAME,'Sigma_Theta',ierr)
          CALL READ_FLOAT(Ywidth_WK,FILE_NAME,'Ywidth_WK',ierr)
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

       CALL READ_LOGICAL(ETA_LIMITER,FILE_NAME,'ETA_LIMITER',ierr)
       IF(ETA_LIMITER)THEN
          CALL READ_FLOAT(CrestLimit,FILE_NAME,'CrestLimit',ierr)
          CALL READ_FLOAT(TroughLimit,FILE_NAME,'TroughLimit',ierr)
       ENDIF

        IF(WaveMaker(1:9)=='WK_DATA2D')THEN
          CALL READ_FLOAT(Xc_WK,FILE_NAME,'Xc_WK',ierr)
          CALL READ_FLOAT(Yc_WK,FILE_NAME,'Yc_WK',ierr)
          CALL READ_FLOAT(DEP_WK,FILE_NAME,'DEP_WK',ierr)
          CALL READ_FLOAT(Time_ramp,FILE_NAME,'Time_ramp',ierr)
          CALL READ_FLOAT(Delta_WK,FILE_NAME,'Delta_WK',ierr)
          CALL READ_STRING(WaveCompFile,FILE_NAME,'WaveCompFile',ierr)
          CALL READ_FLOAT(Ywidth_WK,FILE_NAME,'Ywidth_WK',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
# else
      WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
      WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
      WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
      WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
      WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
# endif
        ENDIF

! absorbing generating wavemaker
     IF(WaveMaker(1:3)=='ABS') THEN

       CALL READ_LOGICAL(WAVE_DATA,FILE_NAME,'WAVE_DATA',ierr)

       CALL READ_FLOAT(DEP_Ser,FILE_NAME,'DepthWaveMaker',ierr)
       CALL READ_FLOAT(WidthWaveMaker,FILE_NAME,'WidthWaveMaker',ierr) 
!  get R and A for sponge_wavemaker calculation  
        CALL READ_FLOAT(R_sponge_wavemaker,FILE_NAME,'R_sponge_wavemaker',ierr)
        CALL READ_FLOAT(A_sponge_wavemaker,FILE_NAME,'A_sponge_wavemaker',ierr)

      IF(WAVE_DATA)THEN
       CALL READ_STRING(WaveCompFile,FILE_NAME,'WaveCompFile',ierr)

      OPEN(1,FILE=TRIM(WaveCompFile))
       READ(1,*)NumFreq,NumDir
       ALLOCATE (Amp_Ser(NumFreq,NumDir),  &
          Per_Ser(NumFreq),Theta_Ser(NumDir))
       READ(1,*)PeakPeriod  ! useless for this application but should keep for consistency
       DO J=1,NumFreq
          READ(1,*)Per_Ser(J)  ! read in as frequency
       ENDDO
       DO I=1,NumDir
          READ(1,*)Theta_Ser(I)
       ENDDO
       DO I=1,NumDir
         READ(1,*)(Amp_Ser(J,I),J=1,NumFreq)
       ENDDO
     CLOSE(1)

       ALLOCATE(  Phase_Ser(NumFreq),&
                  Segma_Ser(NumFreq),Wave_Number_Ser(NumFreq) )
       DO J=1,NumFreq
          IF(Per_Ser(J).EQ.ZERO)THEN
          WRITE(*,*) 'wave frequency is zero, STOP'
          STOP
         ELSE
          Per_Ser(J)=1.0_SP/Per_Ser(J)
         ENDIF
# if defined (INTEL)
          Phase_Ser(J)=rand()*2.0_SP*3.1415926
# else
          Phase_Ser(J)=rand(0)*2.0_SP*3.1415926
# endif
       ENDDO
       DO I=1,NumDir
         Theta_Ser(I)=Theta_Ser(I)*DEG2RAD
       ENDDO  

# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A40)')'absorbing generating wave maker'
      if (myid.eq.0) WRITE(3,'(A40)')'use DATA'
# else
      WRITE(3,'(A40)')'absorbing generating wave maker'
      WRITE(3,'(A40)')'use DATA'
# endif
 
     ELSE ! use TMA 

          CALL READ_FLOAT(FreqPeak,FILE_NAME,'FreqPeak',ierr)
          CALL READ_FLOAT(FreqMin,FILE_NAME,'FreqMin',ierr)
          CALL READ_FLOAT(FreqMax,FILE_NAME,'FreqMax',ierr)
          CALL READ_FLOAT(Hmo,FILE_NAME,'Hmo',ierr)
          CALL READ_FLOAT(GammaTMA,FILE_NAME,'GammaTMA',ierr)
          CALL READ_FLOAT(ThetaPeak,FILE_NAME,'ThetaPeak',ierr)
          CALL READ_FLOAT(Sigma_Theta,FILE_NAME,'Sigma_Theta',ierr)


# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A40)')'absorbing generating wave maker'
      if (myid.eq.0) WRITE(3,'(A40)')'use TMA'
# else
      WRITE(3,'(A40)')'absorbing generating wave maker'
      WRITE(3,'(A40)')'use TMA'
# endif
      ENDIF ! end if data or tma
     ENDIF ! end absorbing-generating

# if defined (CARTESIAN)
! south-north periodic boundary condition
      CALL READ_LOGICAL(PERIODIC,FILE_NAME,'PERIODIC',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A11,A50)')'PERIODIC:', PERIODIC
# else
      WRITE(3,'(A11,A50)')'PERIODIC:', PERIODIC
# endif
# endif

      CALL READ_LOGICAL(DIFFUSION_SPONGE,FILE_NAME,'DIFFUSION_SPONGE',ierr)
      CALL READ_LOGICAL(DIRECT_SPONGE,FILE_NAME,'DIRECT_SPONGE',ierr)
      CALL READ_LOGICAL(FRICTION_SPONGE,FILE_NAME,'FRICTION_SPONGE',ierr)
      IF(DIFFUSION_SPONGE)THEN
        CALL READ_FLOAT(Csp,FILE_NAME,'Csp',ierr)
# if defined (PARALLEL)
        if (myid.eq.0) WRITE(3,'(A22,F12.2)')'DIFFUSION_SPONGE Csp=', Csp
# else
        WRITE(3,'(A22,F12.2)')'DIFFUSION_SPONGE Csp=', Csp
# endif
      ENDIF

      IF(FRICTION_SPONGE)THEN
        CALL READ_FLOAT(CDsponge,FILE_NAME,'CDsponge',ierr)
# if defined (PARALLEL)
        if (myid.eq.0) WRITE(3,'(A26,F12.2)')'FRICTION_SPONGE CDsponge=', CDsponge
# else
        WRITE(3,'(A26,F12.2)')'FRICTION_SPONGE CDsponge=', CDsponge
# endif
      ENDIF

      IF(DIFFUSION_SPONGE.OR.DIRECT_SPONGE.OR.FRICTION_SPONGE)THEN
        CALL READ_FLOAT(Sponge_west_width,FILE_NAME,'Sponge_west_width',ierr)
        CALL READ_FLOAT(Sponge_east_width,FILE_NAME,'Sponge_east_width',ierr)
        CALL READ_FLOAT(Sponge_south_width,FILE_NAME,'Sponge_south_width',ierr)
        CALL READ_FLOAT(Sponge_north_width,FILE_NAME,'Sponge_north_width',ierr)
        CALL READ_FLOAT(R_sponge,FILE_NAME,'R_sponge',ierr)
        CALL READ_FLOAT(A_sponge,FILE_NAME,'A_sponge',ierr)

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
      CALL READ_STRING(OBSTACLE_FILE,FILE_NAME,'OBSTACLE_FILE',ierr)
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
          CALL READ_LOGICAL(DISPERSION,FILE_NAME,'DISPERSION',ierr)
          CALL READ_FLOAT(Gamma1,FILE_NAME,'Gamma1',ierr)
# if defined (CARTESIAN)
          CALL READ_FLOAT(Gamma2,FILE_NAME,'Gamma2',ierr)
          CALL READ_FLOAT(Beta_ref,FILE_NAME,'Beta_ref',ierr)
# endif
# if defined (ZALPHA)
          CALL READ_FLOAT(Beta_ref,FILE_NAME,'Beta_ref',ierr)
# endif
          CALL READ_FLOAT(Gamma3,FILE_NAME,'Gamma3',ierr)
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

      CALL READ_LOGICAL(VISCOSITY_BREAKING,FILE_NAME,'VISCOSITY_BREAKING',ierr)
      IF(VISCOSITY_BREAKING)THEN
# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,*)'VISCOSITY_BREAKING'
# else
       WRITE(3,*)'VISCOSITY_BREAKING'
# endif    
      ENDIF

      CALL READ_FLOAT(SWE_ETA_DEP,FILE_NAME,'SWE_ETA_DEP',ierr)
# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,'(A13,F12.2)')'SWE_ETA_DEP=', SWE_ETA_DEP
# else
       WRITE(3,'(A13,F12.2)')'SWE_ETA_DEP=', SWE_ETA_DEP
# endif
      CALL READ_LOGICAL(IN_Cd,FILE_NAME,'Friction_Matrix',ierr)
      CALL READ_STRING(CD_FILE,FILE_NAME,'Cd_file',ierr)  
      CALL READ_FLOAT(Cd_fixed,FILE_NAME,'Cd',ierr)
     
# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,'(A13,F12.2)')'Cd_fixed         =', Cd_fixed
       if (myid.eq.0) WRITE(3,'(A15,A50)')'CD_FILE:', CD_FILE
# else
       WRITE(3,'(A13,F12.2)')'Cd_fixed         =', Cd_fixed
       WRITE(3,'(A15,A50)')'CD_FILE:', CD_FILE
# endif
! numerics schemes
      CALL READ_STRING(Time_Scheme,FILE_NAME,'Time_Scheme',ierr)
      IF(ierr==1)THEN
        !write(*,*) 'Please define Time_Scheme in ', FILE_NAME
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,A50)')'TIME_SCHEME:', 'NOT DEFINED, STOP'
       call MPI_FINALIZE ( ier )
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
      CALL READ_STRING(CONSTR,FILE_NAME,'CONSTRUCTION',ierr)
      IF(ierr==1)THEN
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
      CALL READ_STRING(HIGH_ORDER,FILE_NAME,'HIGH_ORDER',ierr)
      IF(ierr==1)THEN
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
      CALL READ_FLOAT(CFL,FILE_NAME,'CFL',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'CFL=', CFL
# else
      WRITE(3,'(A5,F12.2)')'CFL=', CFL
# endif
! Froude Number Cap
      CALL READ_FLOAT(FroudeCap,FILE_NAME,'FroudeCap',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'FroudeCap=', FroudeCap
# else
      WRITE(3,'(A5,F12.2)')'FroudeCap=', FroudeCap
# endif
! MinDepth etc
      CALL READ_FLOAT(MinDepth,FILE_NAME,'MinDepth',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
# else
      WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth
# endif
      CALL READ_FLOAT(MinDepthFrc,FILE_NAME,'MinDepthFrc',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'MinDepthFrc=', MinDepthFrc
# else
      WRITE(3,'(A13,F12.2)')'MinDepthFrc=', MinDepthFrc
# endif

! show breaking
      CALL READ_LOGICAL(SHOW_BREAKING,FILE_NAME,'SHOW_BREAKING',ierr)
	
      IF(VISCOSITY_BREAKING) SHOW_BREAKING = .TRUE.

      IF(SHOW_BREAKING)THEN
      CALL READ_FLOAT(Cbrk1,FILE_NAME,'Cbrk1',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk1 =', Cbrk1
# else
      WRITE(3,'(A8,F12.6)')'Cbrk1 =', Cbrk1
# endif
      CALL READ_FLOAT(Cbrk2,FILE_NAME,'Cbrk2',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk2 =', Cbrk2
# else
      WRITE(3,'(A8,F12.6)')'Cbrk2 =', Cbrk2
# endif
      CALL READ_FLOAT(WAVEMAKER_Cbrk,FILE_NAME,'WAVEMAKER_Cbrk',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A8,F17.6)')'WAVEMAKER_Cbrk =', WAVEMAKER_Cbrk
# else
      WRITE(3,'(A8,F17.6)')'WAVEMAKER_Cbrk =', WAVEMAKER_Cbrk
# endif
      ENDIF

	![ykchoi(08.18.2015) : for viscosity of wavemaker
      CALL READ_LOGICAL(WAVEMAKER_VIS,FILE_NAME,'WAVEMAKER_VIS',ierr)  
	
	IF( VISCOSITY_BREAKING .AND. WAVEMAKER_VIS ) THEN
# if defined (PARALLEL)
	  IF (myid.eq.0) then
	     WRITE(*,*) "==============================================="
	     WRITE(*,*)  "STOP :: VISCOSITY_BREAKING=T, WAVEMAKER_VIS=T"
	     WRITE(*,*) "==============================================="
          ENDIF
          call MPI_FINALIZE ( ier )
# else
	     WRITE(*,*) "==============================================="
	     WRITE(*,*)  "STOP :: VISCOSITY_BREAKING=T, WAVEMAKER_VIS=T"
	     WRITE(*,*) "==============================================="
	  STOP
# endif
      ENDIF

      IF(WAVEMAKER_VIS)THEN
# if defined (PARALLEL)
       if (myid.eq.0) WRITE(3,*)'WAVEMAKER_VIS'
# else
       WRITE(3,*)'WAVEMAKER_VIS'
# endif    
      ENDIF

      IF(WAVEMAKER_VIS)THEN
      CALL READ_FLOAT(visbrk,FILE_NAME,'visbrk',ierr)
	CALL READ_FLOAT(WAVEMAKER_visbrk,FILE_NAME,'WAVEMAKER_visbrk',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A14,F12.6)')'visbrk =', visbrk
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'WAVEMAKER_visbrk =', WAVEMAKER_visbrk
# else
      WRITE(3,'(A10,F12.6)')'visbrk =', visbrk
      WRITE(3,'(A20,F12.6)')'WAVEMAKER_visbrk =', WAVEMAKER_visbrk
# endif
	ENDIF
      CALL READ_FLOAT(T_INTV_mean,FILE_NAME,'T_INTV_mean',ierr)
      CALL READ_FLOAT(C_smg,FILE_NAME,'C_smg',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A14,F12.6)')'T_INTV_mean =', T_INTV_mean
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'C_smg =', C_smg
# else
      WRITE(3,'(A14,F12.6)')'T_INTV_mean =', T_INTV_mean
      WRITE(3,'(A8,F12.6)')'C_smg =', C_smg
# endif
	
      CALL READ_FLOAT(nu_bkg,FILE_NAME,'nu_bkg',ierr)

# if defined (COUPLING)
      CALL READ_STRING(COUPLING_FILE,FILE_NAME,'COUPLING_FILE',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# else
      WRITE(3,'(A15,A50)')'COUPLING_FILE:', COUPLING_FILE
# endif
# endif

! output parameters
      CALL READ_INTEGER(OUTPUT_RES,FILE_NAME,'OUTPUT_RES',ierr)
      CALL READ_LOGICAL(OUT_DEPTH,FILE_NAME,'DEPTH_OUT',ierr)
      CALL READ_LOGICAL(OUT_U,FILE_NAME,'U',ierr)
      CALL READ_LOGICAL(OUT_V,FILE_NAME,'V',ierr)
      CALL READ_LOGICAL(OUT_ETA,FILE_NAME,'ETA',ierr)
      CALL READ_LOGICAL(OUT_Hmax,FILE_NAME,'Hmax',ierr)
      CALL READ_LOGICAL(OUT_Hmin,FILE_NAME,'Hmin',ierr)
      CALL READ_LOGICAL(OUT_Umax,FILE_NAME,'Umax',ierr)
      CALL READ_LOGICAL(OUT_MFmax,FILE_NAME,'MFmax',ierr)
      CALL READ_LOGICAL(OUT_VORmax,FILE_NAME,'VORmax',ierr)
      CALL READ_LOGICAL(OUT_MASK,FILE_NAME,'MASK',ierr)
      CALL READ_LOGICAL(OUT_MASK9,FILE_NAME,'MASK9',ierr)
      CALL READ_LOGICAL(OUT_Umean,FILE_NAME,'Umean',ierr)
      CALL READ_LOGICAL(OUT_Vmean,FILE_NAME,'Vmean',ierr)
      CALL READ_LOGICAL(OUT_ETAmean,FILE_NAME,'ETAmean',ierr)
      CALL READ_LOGICAL(OUT_WaveHeight,FILE_NAME,'WaveHeight',ierr)
      CALL READ_LOGICAL(OUT_SXL,FILE_NAME,'SXL',ierr)
      CALL READ_LOGICAL(OUT_SXR,FILE_NAME,'SXR',ierr)
      CALL READ_LOGICAL(OUT_SYL,FILE_NAME,'SYL',ierr)
      CALL READ_LOGICAL(OUT_SYR,FILE_NAME,'SYR',ierr)
      CALL READ_LOGICAL(OUT_SourceX,FILE_NAME,'SourceX',ierr)
      CALL READ_LOGICAL(OUT_SourceY,FILE_NAME,'SourceY',ierr)
      CALL READ_LOGICAL(OUT_P,FILE_NAME,'P',ierr)
      CALL READ_LOGICAL(OUT_Q,FILE_NAME,'Q',ierr)
      CALL READ_LOGICAL(OUT_Fx,FILE_NAME,'Fx',ierr)
      CALL READ_LOGICAL(OUT_Fy,FILE_NAME,'Fy',ierr)
      CALL READ_LOGICAL(OUT_Gx,FILE_NAME,'Gx',ierr)
      CALL READ_LOGICAL(OUT_Gy,FILE_NAME,'Gy',ierr)
      CALL READ_LOGICAL(OUT_AGE,FILE_NAME,'AGE',ierr)
      CALL READ_LOGICAL(OUT_TMP,FILE_NAME,'TMP',ierr)

!ykchoi
!	CALL READ_FLOAT(EtaBlowVal,FILE_NAME,'EtaBlowVal',ierr)
!  fyshi set blowup value is 100xmax_depth in init.F  
	CALL READ_FLOAT(STEADY_TIME,FILE_NAME,'STEADY_TIME',ierr)

! 
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,*)' --------------input end --------------' 
# else
      WRITE(3,*)' --------------input end --------------' 
# endif

END SUBROUTINE READ_INPUT

# if defined (CARTESIAN)
!-------------------------------------------------------------------------------------
!
!    STATIONS is a subroutine to write station data
! Fengyan Shi modified based on Jeff Harris for Spherical
! here simply specify grid number i and j instead of x and y
!
! HISTORY: 
!    09/16/2011  Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE STATIONS
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: iunit
     REAL(SP) :: dum1,dum2
     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallStation = .TRUE.
     SAVE FirstCallStation

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (FirstCallStation) then
       FirstCallStation = .FALSE.
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

!-------------------------------------------------------------------------------------
!
!    STATIONS_SPHERICAL_IJ is a subroutine to write station data
!       Fengyan Shi modified based on Jeff Harris for Spherical
!       here simply specify grid number i and j instead of x and y
!
! HISTORY: 
!    09/16/2011  Fengyan Shi
!
!-------------------------------------------------------------------------------------

SUBROUTINE STATIONS_SPHERICAL_IJ
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: iunit
     REAL(SP) :: dum1,dum2
     REAL(SP) :: eta_sta,u_sta,v_sta
     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallStation = .TRUE.
     SAVE FirstCallStation

! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (FirstCallStation) then
       FirstCallStation = .FALSE.
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

!-------------------------------------------------------------------------------------
!
!    STATIONS is a subroutine to write station data,  works in spherical
!
!    HISTORY:
!   04/05/2011  Jeff Harris
!
!-------------------------------------------------------------------------------------
SUBROUTINE STATIONS
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: iunit
     REAL(SP) :: dum1,dum2
     REAL(SP) :: eta_sta,u_sta,v_sta
     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     LOGICAL :: FirstCallStationSP = .TRUE.
     SAVE FirstCallStationSP


! initialize stations
     FDIR=TRIM(RESULT_FOLDER)
     if (FirstCallStationSP) then
       FirstCallStationSP = .FALSE.
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

       do i=1,NumberStations
          if (nsta(i).eq.1) then
             iunit=100+i
             close(iunit)
          endif
       enddo
     endif

END SUBROUTINE STATIONS

# endif

!-------------------------------------------------------------------------------------
!
!    PREVIEW is subroutine for print-out of field data
!
!  HISTORY:
!    05/01/2010  Fengyan Shi
!    06/01/2015  Young-Kwang Choi, change file number to 5 digits, 
!                        such as eta_00001
!
!-------------------------------------------------------------------------------------
SUBROUTINE PREVIEW
     USE GLOBAL
# if defined (VESSEL)
     USE VESSEL_MODULE
# endif

     IMPLICIT NONE

     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::FILE_NAME_MEAN=' '
     CHARACTER(LEN=80)::TMP_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '

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

102     FORMAT(A20,I6,A14,F12.3,A2,F12.3)

        !ykchoi
	  !itmp1=mod(icount/1000,10)
        !itmp2=mod(icount/100,10)
        !itmp3=mod(icount/10,10)
        !itmp4=mod(icount,10)
	   itmp1=mod(icount/10000,10)
	   itmp2=mod(icount/1000,10)
	   itmp3=mod(icount/100,10)
	   itmp4=mod(icount/10,10)
	   itmp5=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
    	  write(file_name(5:5),'(I1)')itmp5   !ykchoi

     IF(ICOUNT==1)THEN
     IF(OUT_DEPTH)THEN
        TMP_NAME = TRIM(FDIR)//'dep.out'
        call PutFile(TMP_NAME,DEPTH)
     ENDIF
     ENDIF
![ykchoi
     write(10000,*)time, dt
!ykchoi]

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
        call PutFile(TMP_NAME,P(1:Mloc,1:Nloc))
     ENDIF

     IF(OUT_Q)THEN
        TMP_NAME = TRIM(FDIR)//'q_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,Q(1:Mloc,1:Nloc))
     ENDIF


     IF(OUT_AGE)THEN
      IF(SHOW_BREAKING)THEN
        TMP_NAME = TRIM(FDIR)//'age_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,AGE_BREAKING)
      ENDIF
      IF(VISCOSITY_BREAKING)THEN
        TMP_NAME = TRIM(FDIR)//'nubrk_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,nu_break)
        TMP_NAME = TRIM(FDIR)//'etat_'//TRIM(FILE_NAME)
         call PutFile(TMP_NAME,etat) 
      ENDIF
     
     ENDIF

# if defined (VESSEL)
     IF(OUT_VESSEL)THEN
        TMP_NAME = TRIM(FDIR)//'Pves_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,VesselPressureTotal)
     ENDIF
# endif

     IF(OUT_TMP)THEN
        TMP_NAME = TRIM(FDIR)//'tmp_'//TRIM(FILE_NAME)
        call PutFile(TMP_NAME,tmp4preview)
     ENDIF

101   continue

END SUBROUTINE PREVIEW

!-------------------------------------------------------------------------------------
!
!    PREVIEW_MEAN is subroutine for print-out of mean field data
!
!  HISTORY:
!    03/22/2016  Fengyan Shi
!-------------------------------------------------------------------------------------
SUBROUTINE PREVIEW_MEAN
     USE GLOBAL
     IMPLICIT NONE

     CHARACTER(LEN=80)::FILE_NAME=' '
     CHARACTER(LEN=80)::FDIR=' '
     CHARACTER(LEN=80)::TMP_NAME=' '

     FDIR=TRIM(RESULT_FOLDER)

     ICOUNT_MEAN=ICOUNT_MEAN+1

# if defined (PARALLEL)
        if (myid.eq.0)then
        WRITE(3,102)'PRINTING MEAN FILE', icount_mean
        WRITE(*,102)'PRINTING MEAN FILE', icount_mean
        endif
# else
        WRITE(*,102)'PRINTING MEAN FILE', icount_mean
# endif

102     FORMAT(A20,I6)

	   itmp1=mod(icount_mean/10000,10)
	   itmp2=mod(icount_mean/1000,10)
	   itmp3=mod(icount_mean/100,10)
	   itmp4=mod(icount_mean/10,10)
	   itmp5=mod(icount_mean,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
    	write(file_name(5:5),'(I1)')itmp5  

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
          TMP_NAME = TRIM(FDIR)//'Hrms_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,WaveHeightRMS)
          TMP_NAME = TRIM(FDIR)//'Havg_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,WaveHeightAve)
          TMP_NAME = TRIM(FDIR)//'Hsig_'//TRIM(FILE_NAME)
          call PutFile(TMP_NAME,SigWaveHeight)
        ENDIF
                    

END SUBROUTINE PREVIEW_MEAN


# if defined (PARALLEL)
!-------------------------------------------------------------------------------------
!
!    GetFile is subroutine for reading field data
!
!    HISTORY:
!    05/01/2010  Fengyan Shi
!
!-------------------------------------------------------------------------------------
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
!-------------------------------------------------------------------------------------
!
!    PutFile is subroutine for print-out of field data
!
!    HISTORY:
!      05/01/2010  Fengyan Shi
!
!-------------------------------------------------------------------------------------

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
     LOGICAL :: FirstCallPutFile = .TRUE.
     SAVE  FirstCallPutFile

! first time call 
     IF(FirstCallPutFile)THEN
        FirstCallPutFile = .FALSE.
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
!-------------------------------------------------------------------------------------
!
!    GetFile is subroutine for print-out of field data
!
!    HISTORY:
!      05/01/2010  Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE PutFile(FILE,PHI)
     USE PARAM
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI
     CHARACTER(LEN=80) FILE
     LOGICAL :: FirstCallPutFile = .TRUE.
     SAVE  FirstCallPutFile

! first time call 
     IF(FirstCallPutFile)THEN
        FirstCallPutFile = .FALSE.
! format length
        write(FORMAT_LEN(1:1),'(A1)') '('
        write(FORMAT_LEN(2:8),'(I7)') Mglob
        write(FORMAT_LEN(9:13),'(A5)') 'E16.6'
        write(FORMAT_LEN(14:14),'(A1)') ')'
     ENDIF

        OPEN(1,FILE=TRIM(FILE))
# if defined(DEBUG)
        DO J=1,Nloc
           WRITE(1,100)(real(PHI(I,J)),I=1,Mloc)
        ENDDO
# else
        DO J=Nghost+1,Nloc-Nghost,OUTPUT_RES
           WRITE(1,FORMAT_LEN)(real(PHI(I,J)),I=Nghost+1,Mloc-Nghost,OUTPUT_RES)
        ENDDO
# endif
100  FORMAT(5000E16.6)
!100   FORMAT(FORMAT_LEN)
        CLOSE(1)
END SUBROUTINE PutFile

# endif

