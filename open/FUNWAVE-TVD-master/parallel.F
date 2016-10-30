!------------------------------------------------------------------------------------
!
!      FILE parallel.F
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
# if defined (PARALLEL)
!-------------------------------------------------------------------------------------
!
!    phi_exch is the subroutine to exchange variable at processor interface
!    
!    HISTORY: 
!      02/14/2011 Jeff Harris
!      05/01/2011 Fengyan Shi, implemented into the TVD code
!
!-------------------------------------------------------------------------------------
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
# endif

# if defined(PARALLEL)
!-------------------------------------------------------------------------------------
!
!    phi_exch_variable_length is the subroutine 
!    to exchange variable at processor interface, variable length
!    
!    HISTORY: 02/14/2011 Jeff Harris 
!             05/01/2011 Fengyan Shi, implemented into the TVD code
!
!-------------------------------------------------------------------------------------

! ******* variable length
SUBROUTINE phi_exch_variable_length (PHI,Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost)
    USE PARAM
    USE GLOBAL,ONLY : n_west,n_east,n_suth,n_nrth,npx,npy,px,py,comm2d,ier,ndims,dims,coords,periods
    IMPLICIT NONE
    REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc)
    INTEGER,INTENT(IN) :: Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost

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

END SUBROUTINE phi_exch_variable_length

# endif


# if defined(PARALLEL)
!-------------------------------------------------------------------------------------
!
!    phi_exch is the subroutine to exchange variable at processor interface
!    
!    HISTORY: 02/14/2011 Jeff Harris
!             05/01/2011 Fengyan Shi, implemented into the TVD code
!
!-------------------------------------------------------------------------------------

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

# endif

# if defined (PARALLEL)
!-------------------------------------------------------------------------------------
!
!    DISTRIBUTE_VarGlob is subroutine to distribute global variable into local variable 
!
!    HISTORY: 09/24/2011 Fengyan Shi
!
!-------------------------------------------------------------------------------------
SUBROUTINE DISTRIBUTE_VarGlob (VarGlob,PHI)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     REAL(SP),DIMENSION(NumberProcessor) :: xx
     REAL(SP),DIMENSION(MGlob,NGlob),INTENT(IN) :: VarGlob
     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost) :: PHIGLOB
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT) :: PHI

! TEMP

     if (myid.eq.0) then
        DO J=Nghost+1,NGlob+NGhost
         DO I=Nghost+1,MGlob+Nghost
           PHIGLOB(I,J) = VarGlob(I-Nghost,J-Nghost)
         ENDDO
        ENDDO
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


END SUBROUTINE DISTRIBUTE_VarGlob
# endif

  
