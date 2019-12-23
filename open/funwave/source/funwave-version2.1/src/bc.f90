
!-------------------------------------------------------------------
!   This subroutine is used to collect data into ghost cells                                                         
!   Called by
!      TVD_SOLVER
!
!   Call PHI_COLL
!
!   Update: 07/09/2010 Fengyan Shi, 
!     1) use dummy variables 2) add vtype=3
!-------------------------------------------------------------------
SUBROUTINE EXCHANGE_DISPERSION
    USE GLOBAL
    IMPLICIT NONE
    INTEGER :: VTYPE


    VTYPE=2
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Uxx,VTYPE,PERIODIC)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUxx,VTYPE,PERIODIC)
    VTYPE=3
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vyy,VTYPE,PERIODIC)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVyy,VTYPE,PERIODIC)

    VTYPE=1
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Uxy,VTYPE,PERIODIC)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUxy,VTYPE,PERIODIC)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vxy,VTYPE,PERIODIC)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVxy,VTYPE,PERIODIC)

    IF(Gamma2>ZERO)THEN

      IF(DISP_TIME_LEFT)THEN
        VTYPE=1 ! symetric in both direction
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,ETAT,VTYPE,PERIODIC)
        VTYPE=2  ! like u
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,ETATx,VTYPE,PERIODIC)
        VTYPE=3  ! like v
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,ETATy,VTYPE,PERIODIC) 
      ELSE
        VTYPE=2
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Ut,VTYPE,PERIODIC)
        VTYPE=3
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vt,VTYPE,PERIODIC)

        VTYPE=1
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Utx,VTYPE,PERIODIC)
        VTYPE=1
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vty,VTYPE,PERIODIC)

        VTYPE=2
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Utxx,VTYPE,PERIODIC)
        VTYPE=3
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vtyy,VTYPE,PERIODIC)

        VTYPE=1
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Utxy,VTYPE,PERIODIC) 
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vtxy,VTYPE,PERIODIC) 

        VTYPE=2
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUtxx,VTYPE,PERIODIC)
        VTYPE=3
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVtyy,VTYPE,PERIODIC)

        VTYPE=1
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUtxy,VTYPE,PERIODIC) 
        CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVtxy,VTYPE,PERIODIC) 
    
      ENDIF

      VTYPE=1  ! symetric in both direction
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Ux,VTYPE,PERIODIC)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUx,VTYPE,PERIODIC)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vy,VTYPE,PERIODIC)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVy,VTYPE,PERIODIC)
      VTYPE=3  !like v
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Uy,VTYPE,PERIODIC)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUy,VTYPE,PERIODIC)
      Vtype=2  !like u
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vx,VTYPE,PERIODIC)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVx,VTYPE,PERIODIC)
      VTYPE=2  ! like u
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,ETAx,VTYPE,PERIODIC)
      VTYPE=3  ! like v
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,ETAy,VTYPE,PERIODIC)   

    ENDIF

   
    
END SUBROUTINE EXCHANGE_DISPERSION

!-------------------------------------------------------------------
!   This subroutine is used to collect data into ghost cells                                                         
!   Called by
!      TVD_SOLVER
!
!   Call PHI_COLL
!
!   Update: 07/09/2010 Fengyan Shi, 
!     1) use dummy variables 2) add vtype=3
!   Update: 05/27/2010 Gangfeng Ma, University of Delaware                                         
!-------------------------------------------------------------------
SUBROUTINE EXCHANGE
    USE GLOBAL
    IMPLICIT NONE
    INTEGER :: VTYPE

    REAL(SP),DIMENSION(Mloc,Nloc) :: rMASK



    VTYPE=1
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Eta,VTYPE,PERIODIC)

    rMASK = MASK ! for periodic boundary condition
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,rMASK,VTYPE,PERIODIC)  
    MASK = rMASK  

    VTYPE=2
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,U,VTYPE,PERIODIC)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,HU,VTYPE,PERIODIC)
    VTYPE=3
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,V,VTYPE,PERIODIC)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,HV,VTYPE,PERIODIC)


! etaR x mask is a wrong idea
!    Eta=Eta*MASK

    U=U*MASK
    V=V*MASK
    HU=HU*MASK
    HV=HV*MASK
    
END SUBROUTINE EXCHANGE

!-------------------------------------------------------------------
!   This subroutine is used to collect data into ghost cells
!   Called by
!      Exchange
!
!   Update: 09/07/2010 Fengyan Shi, fix:
!   1) u v symmetric problem, 2) remove use global 3) fix bug
!   Update: 05/27/2010 Gangfeng Ma, University of Delaware
!-------------------------------------------------------------------

SUBROUTINE PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,PHI,VTYPE,PERIODIC)



    USE PARAM








    USE GLOBAL, ONLY : n_east,n_west,n_suth,n_nrth


    IMPLICIT NONE
    INTEGER,INTENT(IN) :: VTYPE
    INTEGER,INTENT(IN) :: Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost
    REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc)

    LOGICAL :: PERIODIC


! I added coupling condition 10/14/2012

! for Eta
    IF(VTYPE==1) THEN  ! for eta
      ! x-direction

    if ( n_west .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend  
      DO K=1,Nghost
        PHI(K,J)=PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO




    endif



    if ( n_east .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend  
      DO K=1,Nghost
        PHI(Iend+K,J)=PHI(Iend-K+1,J)
      ENDDO
      ENDDO




    endif



      ! y-direction and corners

    if ( n_suth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO




    endif



    if ( n_nrth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=PHI(I,Jend-K+1)
      ENDDO
      ENDDO




    endif








! for u
    ELSEIF(VTYPE==2) THEN  ! for u (x-mirror condition)
      ! x-direction

    if ( n_west .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=-PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO




    endif



    if ( n_east .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=-PHI(Iend-K+1,J)
      ENDDO
      ENDDO




    endif



      ! y-direction and corners

    if ( n_suth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO




    endif



    if ( n_nrth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=PHI(I,Jend-K+1)
      ENDDO
      ENDDO




    endif







    ELSEIF(VTYPE==3) THEN ! for v (y-mirror condition)
! for v
      ! x-direction

    if ( n_west .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO




    endif



    if ( n_east .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=PHI(Iend-K+1,J)
      ENDDO
      ENDDO




    endif



      ! y-direction and corners

    if ( n_suth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=-PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO




    endif



    if ( n_nrth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=-PHI(I,Jend-K+1)
      ENDDO
      ENDDO




    endif







! for cross-derivatives
    ELSEIF(VTYPE==4) THEN ! VTYPE==4 for u and v cross-mirror
     ! x-direction

    if ( n_west .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=ZERO
      ENDDO
      ENDDO




    endif



    if ( n_east .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=ZERO
      ENDDO
      ENDDO




    endif


      ! y-direction and corners, this one is not an exact solution

    if ( n_suth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=ZERO
      ENDDO
      ENDDO




    endif



    if ( n_nrth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=ZERO
      ENDDO
      ENDDO




    endif



! for symmetric
    ELSEIF(VTYPE==5)THEN
      ! x-direction

    if ( n_west .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=PHI(Ibeg+Nghost-K,J)
       ENDDO
      ENDDO




    endif



    if ( n_east .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=PHI(Iend-K+1,J)
      ENDDO
      ENDDO




    endif


      ! y-direction and corners


    if ( n_suth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO




    endif



    if ( n_nrth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=PHI(I,Jend-K+1)
      ENDDO
      ENDDO




    endif



! for anti-symmetric
      ELSE
      ! x-direction

    if ( n_west .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=-PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO 




    endif



    if ( n_east .eq. MPI_PROC_NULL ) then




      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=-PHI(Iend-K+1,J)
      ENDDO
      ENDDO 




    endif


      ! y-direction and corners

    if ( n_suth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=-PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO   




    endif



    if ( n_nrth .eq. MPI_PROC_NULL ) then




      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=-PHI(I,Jend-K+1)
      ENDDO
      ENDDO     




    endif



    ENDIF


    call phi_exch (PHI)


END SUBROUTINE PHI_COLL

! ---------------------------------------------------
!    This is subroutine to provide boundary conditions at edges of domain
!    Last Update: 05/06/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE BOUNDARY_CONDITION
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP)::Xi,Deps


! four sides of computational domain


        if ( n_west .eq. MPI_PROC_NULL ) then



     DO J=Jbeg,Jend
      P(Ibeg,J)=ZERO
      Xi=EtaRxR(Ibeg,J)
      Deps=Depthx(Ibeg,J)
      Fx(Ibeg,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Ibeg,J)=ZERO
      ENDDO



      endif




        if ( n_east .eq. MPI_PROC_NULL ) then



     DO J=Jbeg,Jend
      P(Iend1,J)=ZERO
      Xi=EtaRxL(Iend1,J)
      Deps=Depthx(Iend1,J)
      Fx(Iend1,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Iend1,J)=ZERO
     ENDDO



      endif



! y direction
   IF(PERIODIC)THEN
!   do nothing
   ELSE



      if ( n_suth .eq. MPI_PROC_NULL ) then



     DO I=Ibeg,Iend
      Q(I,Jbeg)=ZERO
      Fy(I,Jbeg)=ZERO
      Xi=EtaRyR(I,Jbeg)
      Deps=Depthy(I,Jbeg)
      Gy(I,Jbeg)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      ENDDO



      endif


      if ( n_nrth .eq. MPI_PROC_NULL ) then

     DO I=Ibeg,Iend
      Q(I,Jend1)=ZERO
      Fy(I,Jend1)=ZERO
      Xi=EtaRyL(I,Jend1)
      Deps=Depthy(I,Jend1)
      Gy(I,Jend1)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
     ENDDO


     endif



    ENDIF


! mask points
! Jeff pointed out the loop should be Jbeg-1, Jend+1
! The problem is that the fluxes on the inter-processor boundaries may be
!modified if the point next to the boundary (e.g., in the ghost cells,
!managed by a different processor) is land, but as is the routine doesn't
!check for this. 

     DO j=Jbeg-1,Jend+1
     DO i=Ibeg-1,Iend+1
      IF(MASK(I,J)<1)THEN
        P(I,J)=ZERO
! Jeff reported a bug here for parallel version

        IF((I/=Ibeg).or.(n_west.ne.MPI_PROC_NULL))THEN



!         Fx(I,J)=0.5_SP*GRAV*HxL(I,J)*HxL(I,J)*MASK(I-1,J)
!new splitting method
      Xi=EtaRxL(I,J)
      Deps=Depthx(I,J)
         Fx(I,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)*MASK(I-1,J)
        ELSE
         Fx(I,J)=ZERO
        ENDIF
        Gx(I,J)=ZERO

        P(I+1,J)=ZERO
! Jeff also here

        IF((I/=Iend).or.(n_east.ne.MPI_PROC_NULL))THEN



!         Fx(I+1,J)=0.5_SP*GRAV*HxR(I+1,J)*HxR(I+1,J)*MASK(I+1,J)
! new splitting method
      Xi=EtaRxR(I+1,J)
      Deps=Depthx(I+1,J)
         Fx(I+1,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)*MASK(I+1,J)
        ELSE
         Fx(I+1,J)=ZERO
        ENDIF
        Gx(I+1,J)=ZERO

        Q(I,J)=ZERO
        Fy(I,J)=ZERO
! Jeff also here

        IF((J/=Jbeg).or.(n_suth.ne.MPI_PROC_NULL))THEN



!         Gy(I,J)=0.5_SP*GRAV*HyL(I,J)*HyL(I,J)*MASK(I,J-1)
! new splitting method
      Xi=EtaRyL(I,J)
      Deps=Depthy(I,J)
         Gy(I,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)*MASK(I,J-1)
        ELSE
         Gy(I,J)=ZERO
        ENDIF

        Q(I,J+1)=ZERO
        Fy(I,J+1)=ZERO
! Jeff also here

        IF((J/=Jend).or.(n_nrth.ne.MPI_PROC_NULL))THEN



!         Gy(I,J+1)=0.5_SP*GRAV*HyR(I,J+1)*HyR(I,J+1)*MASK(I,J+1)
! new splitting method
      Xi=EtaRyR(I,J+1)
      Deps=Depthy(I,J+1)
         Gy(I,J+1)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)*MASK(I,J+1)
        ELSE
         Gy(I,J+1)=ZERO
        ENDIF
      ENDIF
     ENDDO
     ENDDO

END SUBROUTINE BOUNDARY_CONDITION


! --------------------------------------------------
!    This is subroutine to provide solitary wave at left boundary
!    it can be specified in input.txt giving 'SOL'
!    called by
!       - MAIN
!    Last Update: 05/28/2010 Fengyan Shi, University of Delaware
! --------------------------------------------------
SUBROUTINE SOLITARY_WAVE_LEFT_BOUNDARY
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP):: aa,h00,c1,tex,tlag,zz
     INTEGER::Iwavemaker

       Iwavemaker=Ibeg
       aa=AMP_SOLI
       h00=DEP_SOLI
       c1=sqrt(GRAV*h00*(1.0_SP+aa/h00))
       DO J=1,Nloc
         tex=sqrt(0.75_SP*aa/h00**3)
         tlag=4.0_SP*h00/sqrt(aa/h00)
         zz=aa/COSH(tex*(Lag_soli-c1*TIME))**2
         Eta(Iwavemaker,J)=zz
         H(Iwavemaker,J)=Eta(Iwavemaker,J)+Depth(Iwavemaker,J) 
! note: can not provide u and hu at boundary for dispersive equations!
!         U(Iwavemaker,J)= SQRT(grav/h00)*zz
!         HU(Iwavemaker,J)=h00*U(Iwavemaker,J)       
       enddo   
     
END SUBROUTINE SOLITARY_WAVE_LEFT_BOUNDARY

