!------------------------------------------------------------------------------------
!
!      FILE vessle.F
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
!  VESSEL is a module to model ship-wakes    
!
!  HISTORY :
!    10/07/2016  Fengyan Shi
!
!-------------------------------------------------------------------------------------

# if defined (VESSEL)

MODULE VESSEL_MODULE
  USE PARAM
  USE GLOBAL,ONLY : Mloc,Nloc,Nghost,Ibeg,Iend,Jbeg,Jend,DX,DY, &
                    H
  USE INPUT_READ
#if defined (PARALLEL)
  USE GLOBAL,ONLY : myid,ier, npx,npy,PX,PY
  USE MPI
# endif
IMPLICIT NONE
  SAVE

    INTEGER :: NumVessel,Kves
    REAL(SP),DIMENSION(:),ALLOCATABLE :: Xvessel1,Yvessel1,Xvessel2,Yvessel2, &
                                       LengthVessel,WidthVessel, &
                                       AlphaVessel,BetaVessel,Pvessel, &
                                       TimeVessel1,TimeVessel2,ThetaVessel
    REAL(SP),DIMENSION(:,:),ALLOCATABLE :: VesselPressureTotal,VesselPressureEach, &
                                       VesselPressureX,VesselPressureY
    REAL(SP),DIMENSION(:),ALLOCATABLE ::  Xco,Yco
    LOGICAL :: OUT_VESSEL = .TRUE.


!INTERFACE READ_FOUR_TYPE_VALUES
!  Module Procedure VESSEL_INITIAL
!  Module Procedure VESSEL_FORCING
!END INTERFACE

CONTAINS
  
! READ VESSEL

SUBROUTINE VESSEL_INITIAL
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4
                    
  USE INPUT_READ
  IMPLICIT NONE
  CHARACTER(LEN=80)::FILE_NAME=' '
  CHARACTER(LEN=80)::FILE_VESSEL=' '
  CHARACTER(LEN=80) :: VESSEL_FOLDER=' '
  CHARACTER(LEN=80)::TMP_NAME=' '
  INTEGER :: Ifile,ierr
  CHARACTER(LEN=80):: VesselName

! read vessel number and folder from input.txt
      FILE_NAME='input.txt'

! vessel folder
      CALL READ_STRING(VESSEL_FOLDER,FILE_NAME,'VESSEL_FOLDER',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A15,A50)')'VESSEL_FOLDER:', VESSEL_FOLDER
# else
      WRITE(3,'(A15,A50)')'VESSEL_FOLDER:', VESSEL_FOLDER
# endif

      CALL READ_INTEGER(NumVessel,FILE_NAME,'NumVessel',ierr)
# if defined (PARALLEL)
      if (myid.eq.0) WRITE(3,'(A12,I3)') 'NumVessel = ',NumVessel
# else
      WRITE(3,'(A12,I3)') 'NumVessel = ',NumVessel
# endif

      CALL READ_LOGICAL(OUT_VESSEL,FILE_NAME,'OUT_VESSEL',ierr)

      ALLOCATE (Xvessel1(NumVessel),Yvessel1(NumVessel),  &
                Xvessel2(NumVessel),Yvessel2(NumVessel),  &
                TimeVessel1(NumVessel),TimeVessel2(NumVessel), &
                LengthVessel(NumVessel),WidthVessel(NumVessel), &
                AlphaVessel(NumVessel),BetaVessel(NumVessel),  &
                Pvessel(NumVessel),ThetaVessel(NumVessel) )
      ALLOCATE (VesselPressureTotal(Mloc,Nloc), VesselPressureEach(Mloc,Nloc),&
                 Xco(Mloc),Yco(Nloc), VesselPressureX(Mloc,Nloc), &
                 VesselPressureY(Mloc,Nloc) )


! Xco, and Yco

# if defined (PARALLEL)
     Xco(Ibeg) = npx*(Mloc-2*Nghost)*DX
# else
     Xco(Ibeg) = ZERO
# endif
     DO I = Ibeg+1,Mloc
       Xco(I) = Xco(I-1)+DX
     ENDDO
     DO I = Ibeg-1,Ibeg-Nghost,-1
       Xco(I) = Xco(I+1)-DX
     ENDDO

# if defined (PARALLEL)
     Yco(Jbeg) = npy*(Nloc-2*Nghost)*DY
# else
     Yco(Jbeg) = ZERO
# endif
     DO J = Jbeg+1,Nloc
       Yco(J) = Yco(J-1)+DY
     ENDDO
     DO J = Jbeg-1,Jbeg-Nghost,-1
       Yco(J) = Yco(J+1)-DY
     ENDDO

  DO Kves = 1, NumVessel

!  file name
    itmp1=mod(Kves/1000,10)
    itmp2=mod(Kves/100,10)
    itmp3=mod(Kves/10,10)
    itmp4=mod(Kves,10)
    write(FILE_VESSEL(1:1),'(I1)')itmp1
    write(FILE_VESSEL(2:2),'(I1)')itmp2
    write(FILE_VESSEL(3:3),'(I1)')itmp3
    write(FILE_VESSEL(4:4),'(I1)')itmp4

    TMP_NAME = TRIM(VESSEL_FOLDER)//'vessel_'//TRIM(FILE_VESSEL)

! check existing

 INQUIRE(FILE=TRIM(TMP_NAME),EXIST=FILE_EXIST)
  IF(.NOT.FILE_EXIST)THEN
# if defined (PARALLEL)
   IF(MYID==0)  &
   WRITE(*,*) TRIM(TMP_NAME), ' specified in ', TRIM(VESSEL_FOLDER), ' but CANNOT BE FOUND. STOP'
   CALL MPI_FINALIZE (ier)
   STOP
# else
    WRITE(*,*) TRIM(TMP_NAME), ' specified in ', TRIM(VESSEL_FOLDER), ' but CANNOT BE FOUND. STOP'
    STOP
# endif
  ENDIF

! open file
  Ifile=Kves+200
  OPEN(Ifile,FILE=TRIM(TMP_NAME))

! read file
         READ(Ifile,*)  ! title
         READ(Ifile,*)  VesselName  ! vessel name
         READ(Ifile,*)  ! length and width
         READ(Ifile,*)  LengthVessel(Kves), WidthVessel(Kves), &
                      AlphaVessel(Kves),BetaVessel(Kves),Pvessel(Kves)
         READ(Ifile,*)  ! t, x, y
         READ(Ifile,*)  TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)

         TimeVessel1(Kves) = TimeVessel2(Kves)
         Xvessel1(Kves) = Xvessel2(Kves)
         Yvessel1(Kves) = Yvessel2(Kves)

         AlphaVessel(Kves) = Max(SMALL, AlphaVessel(Kves))
         BetaVessel(Kves) = Max(SMALL, BetaVessel(Kves))
         AlphaVessel(Kves) = Min(1.0_SP, AlphaVessel(Kves))
         BetaVessel(Kves) = Min(1.0_SP, BetaVessel(Kves))

# if defined (PARALLEL)
   IF(MYID==0)THEN
   WRITE(3,*) 'Vessel Name: ',  TRIM(VesselName)
   WRITE(3,*) 'Vessel Length', LengthVessel(Kves)
   WRITE(3,*) 'Vessel Width', WidthVessel(Kves)
   WRITE(3,*) 'Vessel Alpha', AlphaVessel(Kves)
   WRITE(3,*) 'Vessel Beta', BetaVessel(Kves)
   WRITE(3,*) 'Vessel P', PVessel(Kves)
   WRITE(3,*) 'Initial Time, X, Y', TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)
   ENDIF
# else
   WRITE(3,*) 'Vessel Name: ',  TRIM(VesselName)
   WRITE(3,*) 'Vessel Length', LengthVessel(Kves)
   WRITE(3,*) 'Vessel Width', WidthVessel(Kves)
   WRITE(3,*) 'Vessel Alpha', AlphaVessel(Kves)
   WRITE(3,*) 'Vessel Beta', BetaVessel(Kves)
   WRITE(3,*) 'Vessel P', PVessel(Kves)
   WRITE(3,*) 'Initial Time, X, Y', TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)
# endif

  ENDDO  ! end Kves

End SUBROUTINE VESSEL_INITIAL

SUBROUTINE VESSEL_FORCING
  USE GLOBAL,ONLY : Mloc,Nloc,tmp1,tmp2,SMALL,TIME,ZERO
  USE INPUT_READ
  IMPLICIT NONE
  INTEGER :: Ifile,ierr,I,J
  REAL(SP) :: Xves,Yves,Lves,Wves

  VesselPressureTotal = ZERO

  DO Kves = 1,NumVessel

    IF(TIME>TimeVessel1(Kves).AND.TIME>TimeVessel2(Kves)) THEN

         TimeVessel1(Kves)=TimeVessel2(Kves)
         Xvessel1(Kves) = Xvessel2(Kves)
         Yvessel1(Kves) = Yvessel2(Kves)

    Ifile = 200 + Kves

    READ(Ifile,*,END=120)  TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)

    ThetaVessel(Kves) = ATAN2(Yvessel2(Kves)-Yvessel1(Kves),  &
                              Xvessel2(Kves)-Xvessel1(Kves))

# if defined (PARALLEL)
   IF(MYID==0)THEN
     WRITE(3,*)'Read Vessel # ', Kves
     WRITE(3,*)'T,X,Y = ', TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)
   ENDIF
# else
     WRITE(3,*)'Read Vessel # ', Kves
     WRITE(3,*)'T,X,Y = ', TimeVessel2(Kves),Xvessel2(Kves),Yvessel2(Kves)
# endif

    ENDIF ! end time > timevessel2

! calculate force
    tmp2=ZERO
    tmp1=ZERO

    IF(TIME>TimeVessel1(Kves))THEN
      IF(TimeVessel1(Kves).EQ.TimeVessel2(Kves))THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
      tmp2=(TimeVessel2(Kves)-TIME) &
            /MAX(SMALL, ABS(TimeVessel2(Kves)-TimeVessel1(Kves)))
      tmp1=1.0_SP - tmp2;
      ENDIF  ! no more data?
    ENDIF ! time>time_1

    Xves = Xvessel2(Kves)*tmp1 +Xvessel1(Kves)*tmp2
    Yves = Yvessel2(Kves)*tmp1 +Yvessel1(Kves)*tmp2

! rectangular
    VesselPressureEach = ZERO
    DO J=1,Nloc
    DO I=1,Mloc
      Lves=(Xco(I)-Xves)*COS(ThetaVessel(Kves)) + (Yco(J)-Yves)*SIN(ThetaVessel(Kves))
      Wves=-(Xco(I)-Xves)*SIN(ThetaVessel(Kves)) + (Yco(J)-Yves)*COS(ThetaVessel(Kves))
      IF(ABS(Lves)<=0.5_SP*LengthVessel(Kves).AND. &
         ABS(Wves)<=0.5_SP*WidthVessel(Kves)) THEN
         VesselPressureEach(I,J) = Pvessel(Kves)  &
                  *COS(PI*Lves/(AlphaVessel(Kves)*LengthVessel(Kves)))**2 &
                  *COS(PI*Wves/(BetaVessel(Kves)*WidthVessel(Kves)))**2                  
      ENDIF
    ENDDO
    ENDDO

120 CONTINUE  ! no more data for vessel Kves

    VesselPressureTotal = VesselPressureTotal+VesselPressureEach

  ENDDO  ! end Kves

! sourceX and sourceY

    DO J=Jbeg,Jend
    DO I=Ibeg,Iend
       VesselPressureX(I,J) = Grav*H(I,J)*  &
               (VesselPressureTotal(I+1,J)-VesselPressureTotal(I-1,J))/2.0_SP  &
               /DX
       VesselPressureY(I,J) = Grav*H(I,J)*  &
               (VesselPressureTotal(I,J+1)-VesselPressureTotal(I,J-1))/2.0_SP  &
               /DY

    ENDDO
    ENDDO


END SUBROUTINE VESSEL_FORCING

END MODULE VESSEL_MODULE

# endif 
! end vessel
