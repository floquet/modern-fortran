
! ---------------------------------------------------
!    This is subroutine to solve PERIODIC tridiagonal matrix
!    Last Update: 01/17/2011 Fengyan Shi, University of Delaware
! --------------------------------------------------
!solve the following cyclic tridiagonal equation
!*************************************************************!
!*                                                           *!
!*         (B1 C1                      A1 )   (x1)     (b1)  *!
!*         (A2 B2 C2                      )   (x2)     (b2)  *!
!*         (   A3 B3 C3                   )   (x3)     (b3)  *!
!*         (      A4 B4 C4                )   (x4)====       *!
!*         (         A5 B5  C5            )   ... ====  ...  *!
!*         (            ... ... ...       )   ...       ...  *!
!*         (                AN-1 BN-1 CN-1)   (xN-1)   (bN-1)*!
!*         (CN                   AN   BN  )   (xN)     (bN)  *!
!*                                                           *!
!*                                                           *!
!*************************************************************!
!    where A are alpha, B are beta, C are gama
!  CALL TRIG_PERIODIC(Aper,Bper,Cper,Dper,Vper,Jend-Jbeg+1)

SUBROUTINE TRIG_PERIODIC (alpha, beta,gama, b, x, N)  
         USE PARAM
         IMPLICIT NONE
!----dummy variables--
         INTEGER,INTENT(IN) :: N
         REAL(SP),DIMENSION(N),INTENT(IN) :: alpha, beta,gama,b
         REAL(SP),DIMENSION(N),INTENT(OUT) ::  x
!----local variables--
          REAL(SP),DIMENSION(N) :: betaPrime,u,z
          REAL(SP) :: c,fact
          INTEGER :: II

          c=-beta(1)   !the minus sign makes sure betaPrime(1) non zero

          betaPrime(1) = beta(1) - c     !the first tirdiagonal element
          do II=2,N-1,1
           betaPrime(II)=beta(II)
          enddo
   
          betaPrime(N) = beta(N) - alpha(1) * gama(N) / c    !the last tridiagonal element

          call tri_ge(alpha, betaPrime, gama, b, x, N)       !solve for x  !first equation (13) on notes page 148-4-3

          u(1) = c;                                          !first u,
          do II=2,N-1
           u(II)=0.d0
          enddo
          u(N) = gama(N)                                    !last U is same as alpha

        call tri_ge(alpha, betaPrime, gama, u, z, N)       !solve for z  !second equation (13) on notes page 148-4-3

        fact = (x(1) + alpha(1) * x(N) / c)   &
             / (1.0 + z(1) + alpha(1) * z(N) / c)

          do II=1,N
             x(II) =x(II)- fact * z(II)       !construct final results
          enddo

END SUBROUTINE TRIG_PERIODIC


SUBROUTINE TRI_GE(alpha,beta,gama,b, x, N)
!----basically same as subroutine trig()----but allows diagonal variables not equal to unit
!*************************************************************!
!*                                                           *!
!*         (B1 C1                         )   (x1)     (b1)  *!
!*         (A2 B2 C2                      )   (x2)     (b2)  *!
!*         (   A3 B3 C3                   )   (x3)     (b3)  *!
!*         (      A4 B4 C4                )   (x4)====       *!
!*         (         A5 B5  C5            )   ... ====  ...  *!
!*         (            ... ... ...       )   ...       ...  *!
!*         (                An-1 Bn-1 Cn-1)   (xn-1)   (bn-1)*!
!*         (                     An   Bn  )   (xn)     (bn)  *!
!*                                                           *!
!*                                                           *!
!*************************************************************!
! where A are alpha, B are beta, C are gama
        USE PARAM
        IMPLICIT NONE
!----dummy variables---
        INTEGER,INTENT(IN) :: N
        REAL(SP),DIMENSION(N),INTENT(IN) :: alpha,beta, gama, b
        REAL(SP),DIMENSION(N),INTENT(OUT) :: x
!-----local variables
        REAL(SP),DIMENSION(N) :: betaPrime, bPrime
        REAL(SP) :: coeff
        INTEGER :: II
!Perform forward elimination
         betaPrime(1) = beta(1)
         bPrime(1) = b(1)
 
         do II=2,N
               coeff = alpha(II) / betaPrime(II-1)
               betaPrime(II) = beta(II) - coeff * gama(II-1)
               bPrime(II) = b(II) - coeff * bPrime(II-1)
         enddo

! Perform back substitution
         x(N) = bPrime(N) / betaPrime(N)
         do II=N-1,1,-1
              x(II) = (bPrime(II) - gama(II) * x(II+1)) / betaPrime(II)
         enddo

END SUBROUTINE TRI_GE








SUBROUTINE ScatterVariable(PHIGLOB,PHI)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: l,len,iglob,jglob
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     INTEGER,DIMENSION(1) :: req
     integer,dimension(MPI_STATUS_SIZE,1) :: status
     REAL(SP),DIMENSION(Mloc,Nloc) :: xx
     REAL(SP),DIMENSION(MGlob+2*Nghost,NGlob+2*Nghost),INTENT(IN) :: PHIGLOB
     CHARACTER(LEN=80) FILE
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(OUT) :: PHI
     REAL(SP),DIMENSION(Mloc,Nloc) :: PHItmp

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

      len=Mloc*Nloc

    if(myid==0) then
      do j = 1,Nloc
      do i = 1,Mloc
        phi(i,j)=phiglob(i,j)
      enddo
      enddo
      DO l=1,NumberProcessor-1
! send from master
        do j = 1,Nloc
        do i = 1,Mloc
          iglob = npxs(l+1)*(Mloc-2*Nghost)+i
          jglob = npys(l+1)*(Nloc-2*Nghost)+j
          PHItmp(i,j)=PHIGLOB(iglob,jglob)          
        enddo
        enddo
        call MPI_SEND(PHItmp,len,MPI_SP,l,0,MPI_COMM_WORLD,ier)
       ENDDO  ! end of l
    endif ! end myid=0

      DO l=1,NumberProcessor-1
        if(myid==l)then
        call MPI_IRECV(xx,len,MPI_SP,0,0,MPI_COMM_WORLD,req(1),ier)
        call MPI_WAITALL(1,req,status,ier)
          DO J=1,Nloc
          DO I=1,Mloc
            PHI(I,J)=xx(I,J)
          ENDDO
          ENDDO
        endif
      ENDDO     

! old one
!     do i=1,Mloc
!     do j=1,Nloc
!        if (myid.eq.0) then
!           do l=1,px*py
!              xx(l) = PHIGLOB(i+npxs(l)*(Iend-Ibeg+1),&
!                   j+npys(l)*(Jend-Jbeg+1))
!           enddo
!        endif
!        call MPI_Scatter(xx,1,MPI_SP,&
!             PHI(i,j),1,MPI_SP,0,MPI_COMM_WORLD,ier)
!     enddo
!     enddo


END SUBROUTINE ScatterVariable



SUBROUTINE GatherVariable(PHI,PHIGLOB)
     USE GLOBAL
     IMPLICIT NONE

     INTEGER :: len,l,iglob,jglob
     ! could be max. procs
     INTEGER,DIMENSION(NumberProcessor) :: npxs,npys
     INTEGER,DIMENSION(1) :: req
     integer,dimension(MPI_STATUS_SIZE,1) :: status
     REAL(SP),DIMENSION(Mloc,Nloc) :: xx
     REAL(SP),DIMENSION(MGlob,NGlob) :: PHIGLOB
     REAL(SP),DIMENSION(Mloc,Nloc),INTENT(IN) :: PHI

     call MPI_Gather(npx,1,MPI_INTEGER,npxs,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)
     call MPI_Gather(npy,1,MPI_INTEGER,npys,1,MPI_INTEGER,&
          0,MPI_COMM_WORLD,ier)

    ! put the data in master processor into the global var
    if(myid==0) then
      do j = Jbeg,Jend
      do i = Ibeg,Iend
        iglob = i-Nghost
        jglob = j-Nghost
        phiglob(iglob,jglob) = Phi(i,j)
      enddo
      enddo
    endif

    ! collect data from other processors into the master processor
    len = Mloc*Nloc

    do l = 1,NumberProcessor-1
      if(myid==0) then
        call MPI_IRECV(xx,len,MPI_SP,l,0,MPI_COMM_WORLD,req(1),ier)
        call MPI_WAITALL(1,req,status,ier)
        do j = Jbeg,Jend
        do i = Ibeg,Iend
          iglob = npxs(l+1)*(Iend-Ibeg+1)+i-Nghost
          jglob = npys(l+1)*(Jend-Jbeg+1)+j-Nghost
          phiglob(iglob,jglob) = xx(i,j)
        enddo
        enddo
      endif

      if(myid==l) then
        call MPI_SEND(phi,len,MPI_SP,0,0,MPI_COMM_WORLD,ier)
      endif
    enddo   

! old one

!     do i=1,Mloc
!     do j=1,Nloc
!        call MPI_Gather(PHI(i,j),1,MPI_SP,&
!             xx,1,MPI_SP,0,MPI_COMM_WORLD,ier)

!        if (j.eq.1) call MPI_Barrier(MPI_COMM_WORLD,ier)

!        if (myid.eq.0) then
!           do l=1,px*py
!              PHIGLOB(i+npxs(l)*(Iend-Ibeg+1),&
!                   j+npys(l)*(Jend-Jbeg+1)) = xx(l)
!           enddo
!        endif
!     enddo
!     enddo

END SUBROUTINE GatherVariable




SUBROUTINE TRIDx ( a, c, d, f, Ibeg, Iend, Jbeg, Jend )
        USE PARAM
        USE GLOBAL, ONLY : n_east, n_west, comm2d, ier, Mloc, Nloc
        IMPLICIT NONE

        integer Ibeg, Iend, Jbeg, Jend
        REAL(SP), dimension(Mloc,Nloc) :: a, c, d, f
        REAL(SP), dimension(Nloc,2) :: rmsg, smsg
        integer status(MPI_STATUS_SIZE), req
        
! forward sweep

        if ( n_west .ne. MPI_PROC_NULL ) then
           call MPI_IRECV( rmsg, 2*Nloc, MPI_SP,&
                n_west, 0, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
           do j = Jbeg, Jend
           if (a(Ibeg,j).ne.ZERO) then
              c(Ibeg,j) = c(Ibeg,j) / a(Ibeg,j) / (1.0_SP/a(Ibeg,j) - rmsg(j,2))
              d(Ibeg,j) = (d(Ibeg,j) / a(Ibeg,j) - rmsg(j,1)) &
                   / (1.0_SP/a(Ibeg,j) - rmsg(j,2))
           endif
           enddo
        endif   

        do j = Jbeg, Jend
        do i = Ibeg+1, Iend
           if (a(i,j).ne.ZERO) then
              c(i,j) = c(i,j) / a(i,j) / (1.0_SP/a(i,j) - c(i-1,j))
              d(i,j) = (d(i,j) / a(i,j) - d(i-1,j)) &
                   / (1.0_SP/a(i,j) - c(i-1,j))
           endif
        enddo
        enddo

        if ( n_east .ne. MPI_PROC_NULL ) then
           do j = Jbeg, Jend
              smsg(j,1) = d(Iend,j)
              smsg(j,2) = c(Iend,j)
           enddo
           call MPI_ISEND( smsg, 2*Nloc, MPI_SP,&
                n_east, 0, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
        endif

! back substitution

        if ( n_east .ne. MPI_PROC_NULL ) then
           call MPI_IRECV( rmsg, 2*Nloc, MPI_SP,&
                n_east, 1, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
           do j = Jbeg, Jend
              f(Iend,j) = d(Iend,j) - c(Iend,j) * rmsg(j,1)
           enddo
        else
           do j = Jbeg, Jend
              f(Iend,j) = d(Iend,j)
           enddo
        endif   

        do j = Jbeg, Jend
        do i = Iend-1, Ibeg, -1
           f(i,j) = d(i,j) - c(i,j) * f(i+1,j)
        enddo
        enddo

        if ( n_west .ne. MPI_PROC_NULL ) then
           do j = Jbeg, Jend
              smsg(j,1) = f(Ibeg,j)
           enddo
           call MPI_ISEND( smsg, 2*Nloc, MPI_SP,&
                n_west, 1, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
        endif

END SUBROUTINE TRIDx





SUBROUTINE TRIDy ( a, c, d, f, Ibeg, Iend, Jbeg, Jend )
        USE PARAM
        USE GLOBAL, ONLY : n_suth, n_nrth, comm2d, ier, Mloc, Nloc,myid
        IMPLICIT NONE

        integer Ibeg, Iend, Jbeg, Jend
        REAL(SP), dimension(Mloc,Nloc) :: a, c, d, f
        REAL(SP), dimension(Mloc,2) :: rmsg, smsg
        integer status(MPI_STATUS_SIZE), req
!        integer i, j
        
! forward sweep

        if ( n_suth .ne. MPI_PROC_NULL ) then
           call MPI_IRECV( rmsg, 2*Mloc, MPI_SP,&
                n_suth, 0, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
           do i = Ibeg, Iend
           if (a(i,Jbeg).ne.ZERO) then
              c(i,Jbeg) = c(i,Jbeg) / a(i,Jbeg) / (1.0_SP/a(i,Jbeg) - rmsg(i,2))
              d(i,Jbeg) = (d(i,Jbeg) / a(i,Jbeg) - rmsg(i,1)) &
                   / (1.0_SP/a(i,Jbeg) - rmsg(i,2))
           endif
           enddo
        endif   

        do j = Jbeg+1, Jend
        do i = Ibeg, Iend
           if (a(i,j).ne.ZERO) then
              c(i,j) = c(i,j) / a(i,j) / (1.0_SP/a(i,j) - c(i,j-1))
              d(i,j) = (d(i,j) / a(i,j) - d(i,j-1)) &
                   / (1.0_SP/a(i,j) - c(i,j-1))
           endif
        enddo
        enddo

        if ( n_nrth .ne. MPI_PROC_NULL ) then
           do i = Ibeg, Iend
              smsg(i,1) = d(i,Jend)
              smsg(i,2) = c(i,Jend)
           enddo
           call MPI_ISEND( smsg, 2*Mloc, MPI_SP,&
                n_nrth, 0, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
        endif

! back substitution

        if ( n_nrth .ne. MPI_PROC_NULL ) then
           call MPI_IRECV( rmsg, 2*Mloc, MPI_SP,&
                n_nrth, 1, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
           do i = Ibeg, Iend
              f(i,Jend) = d(i,Jend) - c(i,Jend) * rmsg(i,1)
           enddo
        else
           do i = Ibeg, Iend
              f(i,Jend) = d(i,Jend)
           enddo
        endif   

        do j = Jend-1, Jbeg, -1
        do i = Ibeg, Iend
           f(i,j) = d(i,j) - c(i,j) * f(i,j+1)
        enddo
        enddo

        if ( n_suth .ne. MPI_PROC_NULL ) then
           do i = Ibeg, Iend
              smsg(i,1) = f(i,Jbeg)
           enddo
           call MPI_ISEND( smsg, 2*Mloc, MPI_SP,&
                n_suth, 1, comm2d, req, ier )
           call MPI_WAIT( req, status, ier )
        endif

END SUBROUTINE TRIDy

