# if defined (COUPLING)
!-------------------------------------------------------------------
!   This subroutine is used to pass coupling variables into ghost cells                                                         
!   Called by
!      TVD_SOLVER
!   Update: 04/28/2012 Fengyan Shi, University of Delaware   
!   1) used format I/O  2) read through read(11,119)                                      
!-------------------------------------------------------------------
SUBROUTINE OneWayCoupling
    USE GLOBAL
    IMPLICIT NONE

119      FORMAT(5E16.6)  ! this is a fixed format for I/O
 
! determine time slot

    IF(TIME>TIME_COUPLING_1.AND.TIME>TIME_COUPLING_2) THEN
         TIME_COUPLING_1=TIME_COUPLING_2

         READ(11,*,END=120) TIME_COUPLING_2 
! east
         IF(N_COUPLING_EAST.GT.0)THEN
             READ(11,*,END=120)   ! east

             U_COUPLING_EAST(:,1)=U_COUPLING_EAST(:,2)
             V_COUPLING_EAST(:,1)=V_COUPLING_EAST(:,2)
             Z_COUPLING_EAST(:,1)=Z_COUPLING_EAST(:,2)

             READ(11,119,END=120)(U_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119,END=120)(V_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119,END=120)(Z_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
         ELSE
             READ(11,*,END=120)   ! east            
         ENDIF
! west
         IF(N_COUPLING_WEST.GT.0)THEN
             READ(11,*,END=120)   ! west

             U_COUPLING_WEST(:,1)=U_COUPLING_WEST(:,2)
             V_COUPLING_WEST(:,1)=V_COUPLING_WEST(:,2)
             Z_COUPLING_WEST(:,1)=Z_COUPLING_WEST(:,2)


             READ(11,119,END=120)(U_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119,END=120)(V_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119,END=120)(Z_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
         ELSE
             READ(11,*,END=120)   ! west            
         ENDIF
! south
         IF(N_COUPLING_SOUTH.GT.0)THEN
             READ(11,*,END=120)   ! south

             U_COUPLING_SOUTH(:,1)=U_COUPLING_SOUTH(:,2)
             V_COUPLING_SOUTH(:,1)=V_COUPLING_SOUTH(:,2)
             Z_COUPLING_SOUTH(:,1)=Z_COUPLING_SOUTH(:,2)


             READ(11,119,END=120)(U_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119,END=120)(V_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119,END=120)(Z_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
         ELSE
             READ(11,*,END=120)   ! south            
         ENDIF
! north
         IF(N_COUPLING_NORTH.GT.0)THEN
             READ(11,*,END=120)   ! north

             U_COUPLING_NORTH(:,1)=U_COUPLING_NORTH(:,2)
             V_COUPLING_NORTH(:,1)=V_COUPLING_NORTH(:,2)
             Z_COUPLING_NORTH(:,1)=Z_COUPLING_NORTH(:,2)

             READ(11,119,END=120)(U_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119,END=120)(V_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119,END=120)(Z_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
         ELSE
             READ(11,*,END=120)   ! north            
         ENDIF


    ENDIF  ! time>time_2 and time_1

120 CONTINUE

    tmp2=ZERO
    tmp1=ZERO

    IF(TIME>TIME_COUPLING_1)THEN
      IF(TIME_COUPLING_1.EQ.TIME_COUPLING_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
      tmp2=(TIME_COUPLING_2-TIME) &
            /MAX(SMALL, ABS(TIME_COUPLING_2-TIME_COUPLING_1))
      tmp1=1.0_SP - tmp2;
      ENDIF  ! no more data?
    ENDIF ! time>time_1


!$$$$

!    IF(TIME>TIME_COUPLING(icount_coupling).AND.  &
!                          icount_coupling<N_COUPLING_DATA)THEN
!      icount_coupling=icount_coupling+1
!    ENDIF
!    IF(TIME>TIME_COUPLING(icount_coupling))THEN
!      tmp2=ZERO
!      tmp1=ZERO   ! when no data use zero
!    ELSE
!      tmp2=(TIME_COUPLING(icount_coupling)-TIME) &
!            /(TIME_COUPLING(icount_coupling)-TIME_COUPLING(icount_coupling-1))
!      tmp1=1.0_SP - tmp2;
!    ENDIF

!$$$$

! west boundary
   IF(N_COUPLING_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_WEST)THEN

      DO J=Kstart_WEST,Kend_WEST 
      DO I=1,Nghost
        ETA(I,J)=Z_COUPLING_WEST(J-Nghost+Kshift_WEST,2)*tmp1&
                +Z_COUPLING_WEST(J-Nghost+Kshift_WEST,1)*tmp2
        U(I,J)=U_COUPLING_WEST(J-Nghost+Kshift_WEST,2)*tmp1&
                +U_COUPLING_WEST(J-Nghost+Kshift_WEST,1)*tmp2
        V(I,J)=V_COUPLING_WEST(J-Nghost+Kshift_WEST,2)*tmp1&
                +V_COUPLING_WEST(J-Nghost+Kshift_WEST,1)*tmp2
        HU(I,J)=(Depth(I,J)+ETA(I,J))*U(I,J)
        HV(I,J)=(Depth(I,J)+ETA(I,J))*V(I,J)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_west>0

! east boundary
   IF(N_COUPLING_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_EAST)THEN
      DO J=Kstart_EAST,Kend_EAST  
      DO I=Iend+1,Iend+Nghost
        ETA(I,J)=Z_COUPLING_EAST(J-Nghost+Kshift_EAST,2)*tmp1&
                +Z_COUPLING_EAST(J-Nghost+Kshift_EAST,1)*tmp2
        U(I,J)=U_COUPLING_EAST(J-Nghost+Kshift_EAST,2)*tmp1&
                +U_COUPLING_EAST(J-Nghost+Kshift_EAST,1)*tmp2
        V(I,J)=V_COUPLING_EAST(J-Nghost+Kshift_EAST,2)*tmp1&
                +V_COUPLING_EAST(J-Nghost+Kshift_EAST,1)*tmp2
        HU(I,J)=(Depth(I,J)+ETA(I,J))*U(I,J)
        HV(I,J)=(Depth(I,J)+ETA(I,J))*V(I,J)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_east>0

! south boundary
   IF(N_COUPLING_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_SOUTH)THEN
      DO I=Kstart_SOUTH,Kend_SOUTH  
      DO J=1,Nghost
        ETA(I,J)=Z_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,2)*tmp1&
                +Z_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,1)*tmp2
        U(I,J)=U_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,2)*tmp1&
                +U_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,1)*tmp2
        V(I,J)=V_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,2)*tmp1&
                +V_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,1)*tmp2
        HU(I,J)=(Depth(I,J)+ETA(I,J))*U(I,J)
        HV(I,J)=(Depth(I,J)+ETA(I,J))*V(I,J)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_south>0

! north boundary
   IF(N_COUPLING_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_NORTH)THEN
      DO I=Kstart_NORTH,Kend_NORTH  
      DO J=Jend+1,Jend+Nghost
        ETA(I,J)=Z_COUPLING_NORTH(I-Nghost+Kshift_NORTH,2)*tmp1&
                +Z_COUPLING_NORTH(I-Nghost+Kshift_NORTH,1)*tmp2
        U(I,J)=U_COUPLING_NORTH(I-Nghost+Kshift_NORTH,2)*tmp1&
                +U_COUPLING_NORTH(I-Nghost+Kshift_NORTH,1)*tmp2
        V(I,J)=V_COUPLING_NORTH(I-Nghost+Kshift_NORTH,2)*tmp1&
                +V_COUPLING_NORTH(I-Nghost+Kshift_NORTH,1)*tmp2
        HU(I,J)=(Depth(I,J)+ETA(I,J))*U(I,J)
        HV(I,J)=(Depth(I,J)+ETA(I,J))*V(I,J)
      ENDDO
      ENDDO
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif
    ENDIF ! end of n_coupling_north>0

END SUBROUTINE OneWayCoupling
# endif 

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

# if defined (CARTESIAN)
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
# else
    VTYPE=2
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Uxx,VTYPE)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUxx,VTYPE)
    VTYPE=3
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vyy,VTYPE)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVyy,VTYPE)

    VTYPE=1
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Uxy,VTYPE)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUxy,VTYPE)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vxy,VTYPE)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVxy,VTYPE)

      VTYPE=1  ! symetric in both direction
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Ux,VTYPE)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUx,VTYPE)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vy,VTYPE)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVy,VTYPE)
      VTYPE=3  !like v
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Uy,VTYPE)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DUy,VTYPE)
      Vtype=2  !like u
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Vx,VTYPE)
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,DVx,VTYPE)
      VTYPE=2  ! like u
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,ETAx,VTYPE)
      VTYPE=3  ! like v
      CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,ETAy,VTYPE)   
# endif
   
    
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
# if defined (CARTESIAN)
    REAL(SP),DIMENSION(Mloc,Nloc) :: rMASK
# endif

# if defined (CARTESIAN)
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
# else
    VTYPE=1
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,Eta,VTYPE)

    VTYPE=2
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,U,VTYPE)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,HU,VTYPE)
    VTYPE=3
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,V,VTYPE)
    CALL PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,HV,VTYPE)
# endif

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
# if defined (CARTESIAN)
SUBROUTINE PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,PHI,VTYPE,PERIODIC)
# else
SUBROUTINE PHI_COLL(Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost,PHI,VTYPE)
# endif
    USE PARAM
# if defined (COUPLING)
    USE GLOBAL,ONLY : IN_DOMAIN_WEST,IN_DOMAIN_EAST,IN_DOMAIN_SOUTH,IN_DOMAIN_NORTH
# endif
# if defined (PARALLEL)
# if defined (COUPLING)
    USE GLOBAL, ONLY : n_east,n_west,n_suth,n_nrth,&
        IN_DOMAIN_WEST,IN_DOMAIN_EAST,IN_DOMAIN_SOUTH,IN_DOMAIN_NORTH
# else
    USE GLOBAL, ONLY : n_east,n_west,n_suth,n_nrth
# endif
# endif
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: VTYPE
    INTEGER,INTENT(IN) :: Mloc,Nloc,Ibeg,Iend,Jbeg,Jend,Nghost
    REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc)
# if defined (CARTESIAN)
    LOGICAL :: PERIODIC
# endif

! I added coupling condition 10/14/2012

! for Eta
    IF(VTYPE==1) THEN  ! for eta
      ! x-direction
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_WEST)THEN
# endif
      DO J=Jbeg,Jend  
      DO K=1,Nghost
        PHI(K,J)=PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_EAST)THEN
# endif
      DO J=Jbeg,Jend  
      DO K=1,Nghost
        PHI(Iend+K,J)=PHI(Iend-K+1,J)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif


      ! y-direction and corners
# if defined (CARTESIAN)
# ifndef PARALLEL
      IF(PERIODIC)THEN
        DO I=1,Mloc
        DO K=1,Nghost
          PHI(I,K)=PHI(I,Jend-Nghost+K)
          PHI(I,Jend+K)=PHI(I,Nghost+K)
        ENDDO
        ENDDO        
      ELSE
# endif 
# endif 
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_SOUTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_NORTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=PHI(I,Jend-K+1)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif
# if defined (CARTESIAN)
# ifndef PARALLEL
      ENDIF
# endif
# endif


! for u
    ELSEIF(VTYPE==2) THEN  ! for u (x-mirror condition)
      ! x-direction
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_WEST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=-PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_EAST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=-PHI(Iend-K+1,J)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif


      ! y-direction and corners
# if defined (CARTESIAN)
# ifndef PARALLEL
      IF(PERIODIC)THEN
        DO I=1,Mloc
        DO K=1,Nghost
          PHI(I,K)=PHI(I,Jend-Nghost+K)
          PHI(I,Jend+K)=PHI(I,Nghost+K)
        ENDDO
        ENDDO        
      ELSE
# endif 
# endif 
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_SOUTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_NORTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=PHI(I,Jend-K+1)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif
# if defined (CARTESIAN)
# ifndef PARALLEL
      ENDIF
# endif 
# endif

    ELSEIF(VTYPE==3) THEN ! for v (y-mirror condition)
! for v
      ! x-direction
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_WEST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_EAST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=PHI(Iend-K+1,J)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif


      ! y-direction and corners
# if defined (CARTESIAN)
# ifndef PARALLEL
      IF(PERIODIC)THEN
        DO I=1,Mloc
        DO K=1,Nghost
          PHI(I,K)=PHI(I,Jend-Nghost+K)
          PHI(I,Jend+K)=PHI(I,Nghost+K)
        ENDDO
        ENDDO        
      ELSE
# endif 
# endif 
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_SOUTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=-PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_NORTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=-PHI(I,Jend-K+1)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif
# if defined (CARTESIAN)
# ifndef PARALLEL
      ENDIF
# endif
# endif

! for cross-derivatives
    ELSEIF(VTYPE==4) THEN ! VTYPE==4 for u and v cross-mirror
     ! x-direction
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_WEST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=ZERO
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_EAST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=ZERO
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

      ! y-direction and corners, this one is not an exact solution
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_SOUTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=ZERO
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_NORTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=ZERO
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif


! for symmetric
    ELSEIF(VTYPE==5)THEN
      ! x-direction
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_WEST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=PHI(Ibeg+Nghost-K,J)
       ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_EAST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=PHI(Iend-K+1,J)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

      ! y-direction and corners

# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_SOUTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_NORTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=PHI(I,Jend-K+1)
      ENDDO
      ENDDO
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif


! for anti-symmetric
      ELSE
      ! x-direction
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_WEST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(K,J)=-PHI(Ibeg+Nghost-K,J)
      ENDDO
      ENDDO 
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_EAST)THEN
# endif
      DO J=Jbeg,Jend
      DO K=1,Nghost
        PHI(Iend+K,J)=-PHI(Iend-K+1,J)
      ENDDO
      ENDDO 
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif

      ! y-direction and corners
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_SOUTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,K)=-PHI(I,Jbeg+Nghost-K)
      ENDDO
      ENDDO   
# if defined (COUPLING)
    ENDIF
# endif  
# if defined (PARALLEL)
    endif
# endif

# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
    IF(.NOT.IN_DOMAIN_NORTH)THEN
# endif
      DO I=1,Mloc
      DO K=1,Nghost
        PHI(I,Jend+K)=-PHI(I,Jend-K+1)
      ENDDO
      ENDDO     
# if defined (COUPLING)
    ENDIF
# endif
# if defined (PARALLEL)
    endif
# endif


    ENDIF

# if defined (PARALLEL)
    call phi_exch (PHI)
# endif

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

# if defined (PARALLEL)
        if ( n_west .eq. MPI_PROC_NULL ) then
# endif

# if defined (COUPLING)
   IF(IN_DOMAIN_WEST)THEN
     DO J=Jbeg,Kstart_WEST-1
      P(Ibeg,J)=ZERO
      Xi=EtaRxR(Ibeg,J)
      Deps=Depthx(Ibeg,J)
      Fx(Ibeg,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Ibeg,J)=ZERO
      ENDDO
     DO J=Kend_WEST+1,Jend
      P(Ibeg,J)=ZERO
      Xi=EtaRxR(Ibeg,J)
      Deps=Depthx(Ibeg,J)
      Fx(Ibeg,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Ibeg,J)=ZERO
      ENDDO
   ENDIF
# else
     DO J=Jbeg,Jend
      P(Ibeg,J)=ZERO
      Xi=EtaRxR(Ibeg,J)
      Deps=Depthx(Ibeg,J)
      Fx(Ibeg,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Ibeg,J)=ZERO
      ENDDO
# endif 

# if defined (PARALLEL)
      endif
# endif


# if defined (PARALLEL)
        if ( n_east .eq. MPI_PROC_NULL ) then
# endif

# if defined (COUPLING)
   IF(IN_DOMAIN_EAST)THEN
     DO J=Jbeg,Kstart_EAST-1
      P(Iend1,J)=ZERO
      Xi=EtaRxL(Iend1,J)
      Deps=Depthx(Iend1,J)
      Fx(Iend1,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Iend1,J)=ZERO
     ENDDO
     DO J=Kend_EAST+1,Jend
      P(Iend1,J)=ZERO
      Xi=EtaRxL(Iend1,J)
      Deps=Depthx(Iend1,J)
      Fx(Iend1,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Iend1,J)=ZERO
     ENDDO
   ENDIF
# else
     DO J=Jbeg,Jend
      P(Iend1,J)=ZERO
      Xi=EtaRxL(Iend1,J)
      Deps=Depthx(Iend1,J)
      Fx(Iend1,J)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      Gx(Iend1,J)=ZERO
     ENDDO
# endif 

# if defined (PARALLEL)
      endif
# endif

# if defined (CARTESIAN)
! y direction
   IF(PERIODIC)THEN
!   do nothing
   ELSE
# endif

# if defined (PARALLEL)
      if ( n_suth .eq. MPI_PROC_NULL ) then
# endif

# if defined (COUPLING)
   IF(IN_DOMAIN_SOUTH)THEN
     DO I=Ibeg,Kstart_SOUTH-1
      Q(I,Jbeg)=ZERO
      Fy(I,Jbeg)=ZERO
      Xi=EtaRyR(I,Jbeg)
      Deps=Depthy(I,Jbeg)
      Gy(I,Jbeg)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
     ENDDO
     DO I=Kend_SOUTH+1,Iend
      Q(I,Jbeg)=ZERO
      Fy(I,Jbeg)=ZERO
      Xi=EtaRyR(I,Jbeg)
      Deps=Depthy(I,Jbeg)
      Gy(I,Jbeg)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
     ENDDO
   ENDIF
# else
     DO I=Ibeg,Iend
      Q(I,Jbeg)=ZERO
      Fy(I,Jbeg)=ZERO
      Xi=EtaRyR(I,Jbeg)
      Deps=Depthy(I,Jbeg)
      Gy(I,Jbeg)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
      ENDDO
# endif  

# if defined (PARALLEL)
      endif
# endif
# if defined (PARALLEL)
      if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
# if defined (COUPLING)
   IF(IN_DOMAIN_NORTH)THEN
     DO I=Ibeg,Kstart_NORTH-1
      Q(I,Jend1)=ZERO
      Fy(I,Jend1)=ZERO
      Xi=EtaRyL(I,Jend1)
      Deps=Depthy(I,Jend1)
      Gy(I,Jend1)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
     ENDDO
     DO I=Kend_NORTH+1,Iend
      Q(I,Jend1)=ZERO
      Fy(I,Jend1)=ZERO
      Xi=EtaRyL(I,Jend1)
      Deps=Depthy(I,Jend1)
      Gy(I,Jend1)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
     ENDDO
   ENDIF
# else
     DO I=Ibeg,Iend
      Q(I,Jend1)=ZERO
      Fy(I,Jend1)=ZERO
      Xi=EtaRyL(I,Jend1)
      Deps=Depthy(I,Jend1)
      Gy(I,Jend1)=0.5_SP*GRAV*(Xi*Xi*Gamma3+2.0_SP*Xi*Deps)
     ENDDO
# endif 
# if defined (PARALLEL)
     endif
# endif

# if defined (CARTESIAN)
    ENDIF
# endif

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
# if defined(PARALLEL)
        IF((I/=Ibeg).or.(n_west.ne.MPI_PROC_NULL))THEN
# else
        IF(I/=Ibeg)THEN
# endif
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
# if defined(PARALLEL)
        IF((I/=Iend).or.(n_east.ne.MPI_PROC_NULL))THEN
# else
        IF(I/=Iend)THEN
# endif
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
# if defined(PARALLEL)
        IF((J/=Jbeg).or.(n_suth.ne.MPI_PROC_NULL))THEN
# else
        IF(J/=Jbeg)THEN
# endif
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
# if defined(PARALLEL)
        IF((J/=Jend).or.(n_nrth.ne.MPI_PROC_NULL))THEN
# else
        IF(J/=Jend)THEN
# endif
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

# if defined(SAMPLES)
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
# endif
