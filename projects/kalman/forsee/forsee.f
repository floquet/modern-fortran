C     *****************************************************************
C     *                                                               *
C     *      PROGRAM FORSEE - A FILTERING AND PREDICTION CODE FOR     *
C     *                       DISCRETE TIME-HISTORY DATA              *
C     *                                                               *
C     *****************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1)
     1,QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC
     2,ICOUNT,TFACTR
      CHARACTER FNAME*30
      DATA IIN,IOUT,IPLT1,IPLT2,IPLT3/1,2,8,9,10/
C
C     ASSIGN FILENAMES AND READ INITIAL INPUT DATA
C
      WRITE(*,*)'INPUT FILENAME IS? (w/o .inp extension)'
      READ(*,'(A30)') FNAME
      CALL DCHARP(FNAME,30,K1,K2)
      OPEN(IIN,FILE=FNAME(K1:K2)//'.INP')
      OPEN(IOUT,FILE=FNAME(K1:K2)//'.OUT',STATUS='UNKNOWN')
      OPEN(IPLT1,FILE=FNAME(K1:K2)//'.T',STATUS='UNKNOWN')
      OPEN(IPLT2,FILE=FNAME(K1:K2)//'.P',STATUS='UNKNOWN')
      OPEN(IPLT3,FILE=FNAME(K1:K2)//'.E',STATUS='UNKNOWN')
      CALL READIN(1,IIN,IOUT,IPLT1,IPLT2,IPLT3)      
C
C     INITIALIZE VARIABLES
C
      CALL INITAL(TEST1)
C
C     READ NEXT DATA POINT, COMPUTE PREDICTION, AND PRINT OUTPUT
C
 1    CALL READIN(2,IIN,IOUT,IPLT1,IPLT2,IPLT3)
      CALL KALMAN(TEST1)
      CALL OUTPUT(IOUT,IPLT1,IPLT2,IPLT3)
      GOTO 1
C
 9999 STOP
      END
C **********************************************************************
C *                                                                    *
C *   SUBROUTINE DCHARP - TO DETERMINE THE CHARACTER POSITIONS         * 
C *                                                                    *
C **********************************************************************
      subroutine dcharp(cstring,length,k1,k2)
      character*(*) cstring
      k1=1
      k2=length
      do i=1,length
      if (cstring(i:i).ne.' ') then
          k1=i
          goto 10
        endif
      enddo
   10 do i=length,1,-1
      if (cstring(i:i).ne.' ') then
          k2=i
          goto 20
        endif
      enddo
   20 return
      end
C
C     *****************************************************************
C     *                                                               *
C     *      SUBROUTINE INITAL - TO INITIALIZE VARIABLES              *
C     *                                                               *
C     *****************************************************************
      SUBROUTINE INITAL(TEST1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1)
     1,QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC
     2,ICOUNT,TFACTR
C
      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          PCM_P(I,J) = 0.0
        ENDDO
      ENDDO
C
      DO I=1,IFILEN,1
        FV_F(I)=0.0
        GV_K(I)=0.0
        PCM_P(I,I)=1.0
      ENDDO
      TEST1=IFILEN
C
      RETURN
      END
C
C
C     *****************************************************************
C     *                                                               *
C     *      SUBROUTINE KALMAN - FORTRAN IMPLEMENTATION OF THE        *
C     *                          RECURSIVE KALMAN FILTER              *
C     *                                                               *
C     *****************************************************************
      SUBROUTINE KALMAN(TEST1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1)
     1,QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC
     2,ICOUNT,TFACTR
      REAL TSCALR,TVECT(IMP1),TMAT(IMP1,IMP1)
C
C     UPDATE THE PREDICTED COVARIANCE MATRIX (1st UPDATE)
C
      DO I=1,IFILEN,1
        PCM_P(I,I) = PCM_P(I,I) + QCOEFF
      ENDDO
C
C     UPDATE THE GAIN VECTOR
C
      TSCALR = 0.0
      TEST0 = 0.0
      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          TSCALR = TSCALR + DV_X(I)*PCM_P(I,J)*DV_X(J)
          TEST0 = TEST0 + ABS(PCM_P(I,J))
        ENDDO
      ENDDO
      TEST1 = TEST1 + IFILEN*(QCOEFF)
C
C      WRITE(*,*)'ICOUNT,TSCALR,T0,T1',ICOUNT,TSCALR,TEST0,TEST1
      IF(TEST0.GT.(TFACTR*TEST1))THEN
        IF(QCOEFF.GT.1.E-10)QCOEFF=QCOEFF*(REAL(IFILEN)/TEST0)
        IF(RCOEFF.LT.1.E+10)RCOEFF=RCOEFF*(TEST0/REAL(IFILEN))
        TEST1=TEST0
      END IF
C
      TSCALR = TSCALR + RCOEFF
      DO I=1,IFILEN,1
        GV_K(I) = 0.0
        DO J=1,IFILEN,1
          GV_K(I) =  GV_K(I) + PCM_P(I,J)*DV_X(J)/TSCALR
        ENDDO
      ENDDO
C
C     UPDATE THE PREDICTED COVARIANCE MATRIX (2nd UPDATE)
C
      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          TMAT(I,J) =  GV_K(I)*DV_X(J)
        ENDDO
      ENDDO
C
      DO I=1,IFILEN,1
        DO J=1,IFILEN,1
          DO K=1,IFILEN,1
            PCM_P(I,J) =  PCM_P(I,J) - PCM_P(I,K)*TMAT(K,J)
          ENDDO
        ENDDO
      ENDDO
C
C     UPDATE THE FILTER VECTOR
C
      PRED_X = 0.0
      DO I=1,IFILEN,1
        PRED_X = PRED_X + FV_F(I)*DV_X(I)
      ENDDO
C
      DO I=1,IFILEN,1
        FV_F(I) =  FV_F(I) + GV_K(I)*(TRUE_X-PRED_X)
      ENDDO
C
      RETURN
      END
C
C     *****************************************************************
C     *                                                               *
C     *      SUBROUTINE OUTPUT - TO PRINT RESULTS TO THE OUTPUT FILE  *
C     *                                                               *
C     *****************************************************************
      SUBROUTINE OUTPUT(IOUT,IPLT1,IPLT2,IPLT3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1)
     1,QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC
     2,ICOUNT,TFACTR
      ERROR = TRUE_X-PRED_X
C
      WRITE(IOUT,10)ICOUNT,TRUE_X,PRED_X,ERROR,QCOEFF,RCOEFF
      WRITE(*,10)ICOUNT,TRUE_X,PRED_X,ERROR,QCOEFF,RCOEFF 
      WRITE(IPLT1,10)ICOUNT,TRUE_X
      WRITE(IPLT2,10)ICOUNT,PRED_X
      WRITE(IPLT3,10)ICOUNT,ERROR
 10   FORMAT(2X,I5,2X,E12.5,2X,E12.5,2X,E12.5,2X,E12.5,2X,E12.5)
C
  999 RETURN
      END
C
C     *****************************************************************
C     *                                                               *
C     *      SUBROUTINE READIN - TO READ AND ECHO-PRINT INPUT DATA    *
C     *                                                               *
C     *****************************************************************
      SUBROUTINE READIN(IFLAG,IIN,IOUT,IPLT1,IPLT2,IPLT3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (IMP1=50,IP=50)
      COMMON/FILTER/PCM_P(IMP1,IMP1),GV_K(IMP1),FV_F(IMP1),DV_X(IMP1)
     1,QCOEFF,RCOEFF,PRED_X,TRUE_X,IFILEN,BASEL,QA,QB,QC,RA,RB,RC
     2,ICOUNT,TFACTR
      DIMENSION BUFFER(IP)
      CHARACTER TITLE*80
      SAVE
C
      IF(IFLAG.EQ.1)THEN       ! READ AND ECHO INITIAL INPUT DATA !
        READ(IIN,'(A80)') TITLE
        WRITE(IOUT,'(A80)') TITLE
        READ(IIN,*)QCOEFF,RCOEFF,IFILEN,IPREDP,BASEL,TFACTR
        IFILEN = MIN(IFILEN,IMP1)
        IFILEN = MAX(IFILEN,5)
        IPREDP = MIN(IPREDP,IP+1)
        IPREDP = MAX(IPREDP,1)
        TFACTR = MIN(TFACTR,1.E+10)
        TFACTR = MAX(TFACTR,1.01)
        WRITE(IOUT,1000)QCOEFF,RCOEFF,IFILEN,IPREDP,BASEL,TFACTR
 1000   FORMAT(/,5X,'THE VALUE OF Q IS:',2X,E12.5,/,5X
     1         ,'THE VALUE OF R IS:',2X,E12.5,/,5X
     2         ,'THE FILTER LENGTH IS:',2X,I5,/,5X
     3         ,'THE PREDICTION LENGTH IS:',2X,I5,/,5X
     4         ,'THE BASELINE VALUE IS:',2X,E12.5,/,5X
     5         ,'THE TEST FACTOR VALUE IS:',2X,E12.5,//)
C
C        READ(IIN,*)QA,QB,QC,RA,RB,RC
C        WRITE(IOUT,1000)QA,QB,QC,RA,RB,RC
C 1000   FORMAT(/,5X,'THE VALUE OF QA IS:',2X,E12.5,/,5X
C     1         ,'THE VALUE OF QB IS:',2X,E12.5,/,5X
C     2         ,'THE VALUE OF QC IS:',2X,E12.5,/,5X
C     3         ,'THE VALUE OF RA IS:',2X,E12.5,/,5X
C     4         ,'THE VALUE OF RB IS:',2X,E12.5,/,5X
C     5         ,'THE VALUE OF RC IS:',2X,E12.5,//)
C
        DO I=1,IFILEN,1
        DV_X(I) = BASEL
        ENDDO
C
        IF(IPREDP.GT.1)THEN
        DO I=1,IPREDP-1,1
        BUFFER(I) = BASEL
        ENDDO
        END IF
C
        TRUE_X = BASEL
        ICOUNT=0
        WRITE(IOUT,*)'     I      TRUE_X        PRED_X        ERROR'
     1              ,'            Q             R   '
        WRITE(*,*)'     I      TRUE_X        PRED_X        ERROR'
     1              ,'            Q             R   '

C        WRITE(IPLT1,*)'     I      TRUE_X'
C        WRITE(IPLT2,*)'     I      PRED_X'
C        WRITE(IPLT3,*)'     I      ERROR'
      ELSE IF(IFLAG.EQ.2)THEN   ! READ TIME HISTORY DATA !
        DO I=1,IFILEN-1,1
        DV_X(I)=DV_X(I+1)
        ENDDO
C
        IF(IPREDP.EQ.1)THEN
          DV_X(IFILEN)=TRUE_X
        ELSE
          DV_X(IFILEN)=BUFFER(1)
          DO I=1,IPREDP-2,1
          BUFFER(I)=BUFFER(I+1)
          ENDDO
          BUFFER(IPREDP-1)=TRUE_X
        END IF
C
        READ(IIN,*,END=999,ERR=999)TRUE_X
        ICOUNT=ICOUNT+1
      END IF
C
      RETURN
C
  999 STOP
      END
