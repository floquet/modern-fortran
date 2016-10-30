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


    call MPI_COMM_RANK (MPI_COMM_WORLD, myid, ier)


      OPEN(3,FILE='LOG.txt')   

! read everything from input.txt
      FILE_NAME='input.txt'

! title
      CALL GET_STRING_VAL(TITLE,FILE_NAME,'TITLE',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No TITLE in ', FILE_NAME, 'use default'
        TITLE='---TEST RUN---'
      ENDIF

      if (myid.eq.0) WRITE(3,*)'---- LOG FILE ---'
      if (myid.eq.0) WRITE(3,*)TITLE
      if (myid.eq.0) WRITE(3,*)' --------------input start --------------'







! parallel info
      CALL GET_INTEGER_VAL(PX,FILE_NAME,'PX',line,ierr)
      CALL GET_INTEGER_VAL(PY,FILE_NAME,'PY',line,ierr)       
      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'PX   =',PX,'PY   =', PY

! dimension
      CALL GET_INTEGER_VAL(Mglob,FILE_NAME,'Mglob',line,ierr)
      CALL GET_INTEGER_VAL(Nglob,FILE_NAME,'Nglob',line,ierr)

      if (myid.eq.0) WRITE(3,'(A7,I3,A7,I3)') 'Mglob=',Mglob,'Nglob=', Nglob



! grid 


      CALL GET_Float_VAL(DX,FILE_NAME,'DX',line,ierr)
      CALL GET_Float_VAL(DY,FILE_NAME,'DY',line,ierr)

      if (myid.eq.0) WRITE(3,'(A4,F12.2,A4,F12.2)')'DX=',DX,'DY=',DY






! result folder
      CALL GET_STRING_VAL(RESULT_FOLDER,FILE_NAME,'RESULT_FOLDER',line,ierr)

      if (myid.eq.0) WRITE(3,'(A15,A50)')'RESULT_FOLDER:', RESULT_FOLDER



! station files
      CALL GET_INTEGER_VAL(NumberStations,FILE_NAME,'NumberStations',line,ierr)
      IF(NumberStations>0)THEN
      CALL GET_STRING_VAL(STATIONS_FILE,FILE_NAME,'STATIONS_FILE',line,ierr)
      ENDIF
! depth 
      CALL GET_STRING_VAL(DEPTH_TYPE,FILE_NAME,'DEPTH_TYPE',line,ierr)

      if (myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_TYPE:', DEPTH_TYPE



      IF(DEPTH_TYPE(1:3)=='DAT')THEN
        CALL GET_STRING_VAL(DEPTH_FILE,FILE_NAME,'DEPTH_FILE',line,ierr)
        CALL GET_STRING_VAL(DepthFormat,FILE_NAME,'DepthFormat',line,ierr)

      if (myid.eq.0) WRITE(3,'(A12,A50)')'DEPTH_FILE:', DEPTH_FILE
      if (myid.eq.0) WRITE(3,'(A14,A50)')'DEPTH_FORMAT:', DEPTHFORMAT




      ENDIF
      IF(DEPTH_TYPE(1:3)=='FLA')THEN
      CALL GET_Float_VAL(DEPTH_FLAT,FILE_NAME,'DEPTH_FLAT',line,ierr) 

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT  



      ENDIF
      IF(DEPTH_TYPE(1:3)=='SLO')THEN
      CALL GET_Float_VAL(DEPTH_FLAT,FILE_NAME,'DEPTH_FLAT',line,ierr) 
      CALL GET_Float_VAL(SLP,FILE_NAME,'SLP',line,ierr) 
      CALL GET_Float_VAL(Xslp,FILE_NAME,'Xslp',line,ierr) 

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEPTH_FLAT=', DEPTH_FLAT 
      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'SLP=', SLP
      if (myid.eq.0) WRITE(3,'(A6,F12.2)')'Xslp=', Xslp  





      ENDIF
! time
      CALL GET_Float_VAL(TOTAL_TIME,FILE_NAME,'TOTAL_TIME',line,ierr)
      CALL GET_Float_VAL(PLOT_INTV,FILE_NAME,'PLOT_INTV',line,ierr)
      CALL GET_Float_VAL(PLOT_INTV_STATION,FILE_NAME,'PLOT_INTV_STATION',line,ierr)
      CALL GET_Float_VAL(SCREEN_INTV,FILE_NAME,'SCREEN_INTV',line,ierr)

      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'TOTAL_TIME=', TOTAL_TIME
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'PLOT_INTV= ', PLOT_INTV
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'SCREEN_INTV=', SCREEN_INTV






! initial uvz
      CALL GET_LOGICAL_VAL(INI_UVZ,FILE_NAME,'INI_UVZ',line,ierr)
      IF(INI_UVZ)THEN
        CALL GET_STRING_VAL(ETA_FILE,FILE_NAME,'ETA_FILE',line,ierr)
        CALL GET_STRING_VAL(U_FILE,FILE_NAME,'U_FILE',line,ierr)
        CALL GET_STRING_VAL(V_FILE,FILE_NAME,'V_FILE',line,ierr)
       ENDIF



! wavemaker
      CALL GET_STRING_VAL(WaveMaker,FILE_NAME,'WAVEMAKER',line,ierr)

      if (myid.eq.0) WRITE(3,'(A11,A50)')'WAVEMAKER:', WAVEMAKER



        IF(WaveMaker(1:7)=='LEF_SOL')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(DEP_SOLI,FILE_NAME,'DEP',line,ierr)
          CALL GET_Float_VAL(LAG_SOLI,FILE_NAME,'LAGTIME',line,ierr)

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'LAG_SOLI=', LAG_SOLI





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

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc_WK   =', Xc_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_WK  =', DEP_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Time_ramp=', Time_ramp
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Delta_WK=', Delta_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK








        ENDIF

        IF(WaveMaker(1:7)=='INI_SOL')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(DEP_SOLI,FILE_NAME,'DEP',line,ierr)
          CALL GET_Float_VAL(XWAVEMAKER,FILE_NAME,'XWAVEMAKER',line,ierr)

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP_SOLI=', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_SOLI=', DEP_SOLI




        ENDIF
        IF(WaveMaker(1:6)=='N_WAVE')THEN
          CALL GET_Float_VAL(x1_Nwave,FILE_NAME,'x1_Nwave',line,ierr)
          CALL GET_Float_VAL(x2_Nwave,FILE_NAME,'x2_Nwave',line,ierr)
          CALL GET_Float_VAL(a0_Nwave,FILE_NAME,'a0_Nwave',line,ierr)
          CALL GET_Float_VAL(gamma_Nwave,FILE_NAME,'gamma_Nwave',line,ierr)
          CALL GET_Float_VAL(dep_Nwave,FILE_NAME,'dep_Nwave',line,ierr)

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'x1_Nwave=', x1_Nwave
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'x2_Nwave=', x2_Nwave
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'a0_Nwave=', a0_Nwave
      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'gamma_Nwave=', gamma_Nwave
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'dep_Nwave=', dep_Nwave







        ENDIF

        IF(WaveMaker(1:7)=='INI_REC')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(Xc,FILE_NAME,'Xc',line,ierr)
          CALL GET_Float_VAL(Yc,FILE_NAME,'Yc',line,ierr)
          CALL GET_Float_VAL(WID,FILE_NAME,'WID',line,ierr)

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP     =', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc      =', Xc
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Yc      =', Yc
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'WID     =', WID






        ENDIF

        IF(WaveMaker(1:7)=='INI_GAU'.OR.&
           WaveMaker(1:7)=='INI_DIP')THEN
          CALL GET_Float_VAL(AMP_SOLI,FILE_NAME,'AMP',line,ierr)
          CALL GET_Float_VAL(Xc,FILE_NAME,'Xc',line,ierr)
          CALL GET_Float_VAL(Yc,FILE_NAME,'Yc',line,ierr)
          CALL GET_Float_VAL(WID,FILE_NAME,'WID',line,ierr)

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP     =', AMP_SOLI
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc      =', Xc
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Yc      =', Yc
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'WID(gamma)=', WID






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

      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Xc_WK   =', Xc_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Tperiod =', Tperiod
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'AMP_WK  =', AMP_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'DEP_WK  =', DEP_WK
      if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Theta_WK=', Theta_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Time_ramp=', Time_ramp
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Delta_WK=', Delta_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK

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

      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Xc_WK   =  ', Xc_WK
      if (myid.eq.0) WRITE(3,'(A11,F12.2)')'Ywidth_WK=', Ywidth_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'DEP_WK  =  ', DEP_WK
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Time_ramp= ', Time_ramp
      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'Delta_WK=  ', Delta_WK
!      if (myid.eq.0) WRITE(3,'(A12,F12.2)')'PeakPeriod= ', PeakPeriod
!      if (myid.eq.0) WRITE(3,'(A12,I5)')'NumFreq=  ', NumFreq
!      if (myid.eq.0) WRITE(3,'(A12,I5)')'NumDir =  ', NumDir

        ENDIF
! ******

! south-north periodic boundary condition
      CALL GET_LOGICAL_VAL(PERIODIC,FILE_NAME,'PERIODIC',line,ierr)

      if (myid.eq.0) WRITE(3,'(A11,A50)')'PERIODIC:', PERIODIC





! sponge layer
      CALL GET_LOGICAL_VAL(SPONGE_ON,FILE_NAME,'SPONGE_ON',line,ierr)

      if (myid.eq.0) WRITE(3,'(A11,A50)')'SPONGE_ON:', SPONGE_ON



      IF(SPONGE_ON)THEN
        CALL GET_Float_VAL(Sponge_west_width,FILE_NAME,'Sponge_west_width',line,ierr)
        CALL GET_Float_VAL(Sponge_east_width,FILE_NAME,'Sponge_east_width',line,ierr)
        CALL GET_Float_VAL(Sponge_south_width,FILE_NAME,'Sponge_south_width',line,ierr)
        CALL GET_Float_VAL(Sponge_north_width,FILE_NAME,'Sponge_north_width',line,ierr)
        CALL GET_Float_VAL(R_sponge,FILE_NAME,'R_sponge',line,ierr)
        CALL GET_Float_VAL(A_sponge,FILE_NAME,'A_sponge',line,ierr)

        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_west_width =', Sponge_west_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_east_width =', Sponge_east_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_south_width=', Sponge_south_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'Sponge_north_width=', Sponge_north_width
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'R_sponge          =', R_sponge
        if (myid.eq.0) WRITE(3,'(A20,F12.2)')'A_sponge          =', A_sponge








      ENDIF

! obstacle structures
      CALL GET_STRING_VAL(OBSTACLE_FILE,FILE_NAME,'OBSTACLE_FILE',line,ierr)
      IF(ierr==1)THEN
        OBSTACLE=.FALSE.

      if (myid.eq.0) WRITE(3,'(A15,A5)')'OBSTACLE_FILE:', 'NO'



      ELSE
        OBSTACLE=.TRUE.

      if (myid.eq.0) WRITE(3,'(A15,A50)')'OBSTACLE_FILE:', OBSTACLE_FILE



      ENDIF

! physics
          CALL GET_LOGICAL_VAL(DISPERSION,FILE_NAME,'DISPERSION',line,ierr)
          CALL GET_Float_VAL(Gamma1,FILE_NAME,'Gamma1',line,ierr)

          CALL GET_Float_VAL(Gamma2,FILE_NAME,'Gamma2',line,ierr)
          CALL GET_Float_VAL(Beta_ref,FILE_NAME,'Beta_ref',line,ierr)




          CALL GET_Float_VAL(Gamma3,FILE_NAME,'Gamma3',line,ierr)

      if (myid.eq.0) WRITE(3,'(A8)')'Physics'






       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Gamma1 = ', Gamma1

       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Gamma2 = ', Gamma2
       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Beta_ref= ', Beta_ref
       if (myid.eq.0) WRITE(3,'(A10,F12.2)')'Gamma3 = ', Gamma3


      CALL GET_Float_VAL(SWE_ETA_DEP,FILE_NAME,'SWE_ETA_DEP',line,ierr)

       if (myid.eq.0) WRITE(3,'(A13,F12.2)')'SWE_ETA_DEP=', SWE_ETA_DEP



      CALL GET_LOGICAL_VAL(IN_Cd,FILE_NAME,'Friction_Matrix',line,ierr)
      CALL GET_STRING_VAL(CD_FILE,FILE_NAME,'Cd_file',line,ierr)  
      CALL GET_Float_VAL(Cd_fixed,FILE_NAME,'Cd',line,ierr)
     

       if (myid.eq.0) WRITE(3,'(A13,F12.2)')'Cd_fixed         =', Cd_fixed
       if (myid.eq.0) WRITE(3,'(A15,A50)')'CD_FILE:', CD_FILE




! numerics schemes
      CALL GET_STRING_VAL(Time_Scheme,FILE_NAME,'Time_Scheme',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'Please define Time_Scheme in ', FILE_NAME

      if (myid.eq.0) WRITE(3,'(A13,A50)')'TIME_SCHEME:', 'NOT DEFINED, STOP'



        STOP
      ENDIF

      if (myid.eq.0) WRITE(3,'(A13,A50)')'TIME_SCHEME:', TIME_SCHEME



      CALL GET_STRING_VAL(CONSTR,FILE_NAME,'CONSTRUCTION',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No definition of CONSTRUCTION in ', FILE_NAME, 'use default'

      if (myid.eq.0) WRITE(3,'(A14,A50)')'CONSTRUCTION', 'NOT DEFINED, USE DEFAULT'



        CONSTR='HLLC'
      ENDIF

      if (myid.eq.0) WRITE(3,'(A14,A50)')'CONSTRUCTION:', CONSTR



      CALL GET_STRING_VAL(HIGH_ORDER,FILE_NAME,'HIGH_ORDER',line,ierr)
      IF(ierr==1)THEN
        !write(*,*) 'No definition of HIGH_ORDER in ', FILE_NAME, 'use default'

      if (myid.eq.0) WRITE(3,'(A12,A50)')'HIGH_ORDER', 'NOT DEFINED, USE DEFAULT'



        HIGH_ORDER='FOURTH'        
      ENDIF

      if (myid.eq.0) WRITE(3,'(A12,A50)')'HIGH_ORDER:', HIGH_ORDER



! CFL
      CALL GET_Float_VAL(CFL,FILE_NAME,'CFL',line,ierr)

      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'CFL=', CFL



! Froude Number Cap
      CALL GET_Float_VAL(FroudeCap,FILE_NAME,'FroudeCap',line,ierr)

      if (myid.eq.0) WRITE(3,'(A5,F12.2)')'FroudeCap=', FroudeCap



! MinDepth etc
      CALL GET_Float_VAL(MinDepth,FILE_NAME,'MinDepth',line,ierr)

      if (myid.eq.0) WRITE(3,'(A10,F12.6)')'MinDepth=', MinDepth



      CALL GET_Float_VAL(MinDepthFrc,FILE_NAME,'MinDepthFrc',line,ierr)

      if (myid.eq.0) WRITE(3,'(A13,F12.2)')'MinDepthFrc=', MinDepthFrc




! show breaking
      CALL GET_LOGICAL_VAL(SHOW_BREAKING,FILE_NAME,'SHOW_BREAKING',line,ierr)
      IF(SHOW_BREAKING)THEN
      CALL GET_Float_VAL(Cbrk1,FILE_NAME,'Cbrk1',line,ierr)

      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk1 =', Cbrk1



      CALL GET_Float_VAL(Cbrk2,FILE_NAME,'Cbrk2',line,ierr)

      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'Cbrk2 =', Cbrk2



      ENDIF

      CALL GET_Float_VAL(T_INTV_mean,FILE_NAME,'T_INTV_mean',line,ierr)
      CALL GET_Float_VAL(C_smg,FILE_NAME,'C_smg',line,ierr)

      if (myid.eq.0) WRITE(3,'(A14,F12.6)')'T_INTV_mean =', T_INTV_mean
      if (myid.eq.0) WRITE(3,'(A8,F12.6)')'C_smg =', C_smg















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

      CALL GET_LOGICAL_VAL(OUT_Umean,FILE_NAME,'Umean',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Vmean,FILE_NAME,'Vmean',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_ETAmean,FILE_NAME,'ETAmean',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_WaveHeight,FILE_NAME,'WaveHeight',line,ierr)

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

      if (myid.eq.0) WRITE(3,*)' --------------input end --------------' 




END SUBROUTINE READ_INPUT


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


        if (myid.eq.0)then
        WRITE(3,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time
        WRITE(*,102)'PRINTING FILE NO.', icount, ' TIME/TOTAL: ', TIME,'/',Total_Time        
        endif



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




