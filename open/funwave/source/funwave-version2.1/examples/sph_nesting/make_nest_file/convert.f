         parameter (Ntotal=10000,nsta_tot=1000)
         parameter (Nb=2000)
         real west_z(nsta_tot,ntotal),west_u(nsta_tot,ntotal),
     &       time(ntotal),
     &        west_v(nsta_tot,ntotal),
     &        east_z(nsta_tot,ntotal),
     &        east_u(nsta_tot,ntotal),east_v(nsta_tot,ntotal),
     &        south_z(nsta_tot,ntotal),south_u(nsta_tot,ntotal),
     &        south_v(nsta_tot,ntotal),
     &        north_z(nsta_tot,ntotal),north_u(nsta_tot,ntotal),
     &        north_v(nsta_tot,ntotal)

         real U_COUPLING_WEST(Nb,Ntotal),
     &       V_COUPLING_WEST(Nb,Ntotal),
     &       Z_COUPLING_WEST(Nb,Ntotal),
     &       U_COUPLING_EAST(Nb,Ntotal),
     &       V_COUPLING_EAST(Nb,Ntotal),
     &       Z_COUPLING_EAST(Nb,Ntotal),
     &       U_COUPLING_SOUTH(Nb,Ntotal),
     &       V_COUPLING_SOUTH(Nb,Ntotal),
     &       Z_COUPLING_SOUTH(Nb,Ntotal),
     &       U_COUPLING_NORTH(Nb,Ntotal),
     &       V_COUPLING_NORTH(Nb,Ntotal),
     &       Z_COUPLING_NORTH(Nb,Ntotal)

         CHARACTER(LEN=80) FDIR
         CHARACTER(LEN=4) FILE_NAME
         integer icount

         icount=0

! input parameters

         nsta=3
         nwest=3
         nsouth = 0
         neast=0
         nnorth=0

    
         time_cut=0.0
!         time_cut=60*60*10     
         iscale = 2

        J_START_WEST = 1
        J_START_EAST = 1
        J_START_SOUTH = 1
        J_START_NORTH = 1

        J_TRIM_WEST = 0
        J_TRIM_EAST = 0
        J_TRIM_SOUTH = 0
        J_TRIM_NORTH = 0

! end input paramters

         Nsm_west=(nwest-1)*iscale +1
         Nsm_south=(nsouth-1)*iscale +1
         Nsm_east=(neast-1)*iscale +1
         Nsm_north=(nnorth-1)*iscale +1

         print*,'Nsm_west',Nsm_west,'Nsm_east',Nsm_east
         print*,'Nsm_north',Nsm_north,'Nsm_south',Nsm_south


         fdir='../grid_A/'

! order should be east, west, south, north
! east
        IF(Neast>0)THEN
         do k=1,neast

          icount=icount+1
        itmp1=mod(icount/1000,10)
        itmp2=mod(icount/100,10)
        itmp3=mod(icount/10,10)
        itmp4=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
         open(1,file=TRIM(FDIR)//'sta_'//TRIM(FILE_NAME))
          do ktime=1,ntotal
          read(1,*,end=301)time(ktime),east_z(k,ktime),east_u(k,ktime),
     &                     east_v(k,ktime)

          enddo
301      continue
          Ndata=ktime-1
          print*,'east station',k,'total time', Ndata

         close(1)
         enddo
        ENDIF
! west
        IF(Nwest.GT.0)THEN
         do k=1,nwest

          icount=icount+1
        itmp1=mod(icount/1000,10)
        itmp2=mod(icount/100,10)
        itmp3=mod(icount/10,10)
        itmp4=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
         open(1,file=TRIM(FDIR)//'sta_'//TRIM(FILE_NAME))

          do ktime=1,ntotal
          read(1,*,end=101)time(ktime),west_z(k,ktime),west_u(k,ktime),
     &                     west_v(k,ktime)

          enddo
101      continue
          Ndata=ktime-1
          print*,'west station',k,'total time', Ndata

         close(1)
         enddo
        ENDIF


! south
       IF(Nsouth>0)THEN
         do k=1,nsouth

          icount=icount+1
        itmp1=mod(icount/1000,10)
        itmp2=mod(icount/100,10)
        itmp3=mod(icount/10,10)
        itmp4=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
         open(1,file=TRIM(FDIR)//'sta_'//TRIM(FILE_NAME))
          do ktime=1,ntotal
          read(1,*,end=200)time(ktime),south_z(k,ktime),
     &     south_u(k,ktime),
     &     south_v(k,ktime)
          enddo
200      continue
         Ndata=ktime-1
          print*,'south station',k,'total time', Ndata

         close(1)
         enddo
         ENDIF
! north
       IF(Nnorth>0)THEN
         do k=1,Nnorth

          icount=icount+1
        itmp1=mod(icount/1000,10)
        itmp2=mod(icount/100,10)
        itmp3=mod(icount/10,10)
        itmp4=mod(icount,10)

        write(file_name(1:1),'(I1)')itmp1
        write(file_name(2:2),'(I1)')itmp2
        write(file_name(3:3),'(I1)')itmp3
        write(file_name(4:4),'(I1)')itmp4
         open(1,file=TRIM(FDIR)//'sta_'//TRIM(FILE_NAME))
          do ktime=1,ntotal
          read(1,*,end=201)time(ktime),north_z(k,ktime),
     &     north_u(k,ktime),
     &     north_v(k,ktime)
          enddo
201      continue
         Ndata=ktime-1
          print*,'north station',k,'total time', Ndata

         close(1)
         enddo
         ENDIF

!  --------------  construct data

        DO K=1,Ndata

       IF(nwest>0)THEN 
         DO I=1,Nwest-1
           II=(I-1)*iscale +1
           DO III=II,II+iscale
             tmp=(III-II)/iscale
             Z_COUPLING_WEST(III,K)=west_z(I,K)*(1-tmp)+
     &                   west_z(I+1,K)*tmp
             U_COUPLING_WEST(III,K)=west_u(I,K)*(1-tmp)+
     &                   west_u(I+1,K)*tmp
             V_COUPLING_WEST(III,K)=west_v(I,K)*(1-tmp)+
     &                   west_v(I+1,K)*tmp
           ENDDO
         ENDDO
       ENDIF

       IF(Neast>0)THEN 
         DO I=1,Neast-1
           II=(I-1)*iscale +1
           DO III=II,II+iscale
             tmp=(III-II)/iscale
             Z_COUPLING_east(III,K)=east_z(I,K)*(1-tmp)+
     &                   east_z(I+1,K)*tmp
             U_COUPLING_east(III,K)=east_u(I,K)*(1-tmp)+
     &                   east_u(I+1,K)*tmp
             V_COUPLING_east(III,K)=east_v(I,K)*(1-tmp)+
     &                   east_v(I+1,K)*tmp
           ENDDO
         ENDDO
       ENDIF

       IF(nsouth>0)THEN
         DO I=1,Nsouth-1
           II=(I-1)*iscale +1
           DO III=II,II+iscale
             tmp=(III-II)/iscale
             Z_COUPLING_SOUTH(III,K)=south_z(I,K)*(1-tmp)+
     &                   south_z(I+1,K)*tmp
             U_COUPLING_SOUTH(III,K)=south_u(I,K)*(1-tmp)+
     &                   south_u(I+1,K)*tmp
             V_COUPLING_SOUTH(III,K)=south_v(I,K)*(1-tmp)+
     &                   south_v(I+1,K)*tmp
           ENDDO
         ENDDO
       ENDIF

       IF(nnorth>0)THEN
         DO I=1,Nnorth-1
           II=(I-1)*iscale +1
           DO III=II,II+iscale
             tmp=(III-II)/iscale
             Z_COUPLING_north(III,K)=north_z(I,K)*(1-tmp)+
     &                   north_z(I+1,K)*tmp
             U_COUPLING_north(III,K)=north_u(I,K)*(1-tmp)+
     &                   north_u(I+1,K)*tmp
             V_COUPLING_north(III,K)=north_v(I,K)*(1-tmp)+
     &                   north_v(I+1,K)*tmp
           ENDDO
         ENDDO
       ENDIF

        ENDDO

! write out ! change format 04/28 

! deal with time cut

        Kstart=1
        DO WHILE (TIME(Kstart)<time_cut)
          Kstart=Kstart+1
        ENDDO
        print*,'start_time',TIME(Kstart),Kstart,time_cut

        OPEN(1,FILE='coupling.txt')
         WRITE(1,*) 'coupling data'
         WRITE(1,*) 'boundary info: num of points, start point'
         WRITE(1,*) 'EAST'
         WRITE(1,*) Nsm_east-J_TRIM_EAST-J_START_EAST+1, J_START_EAST
         WRITE(1,*) 'WEST'
         WRITE(1,*) Nsm_west-J_TRIM_WEST-J_START_WEST+1, J_START_WEST
         WRITE(1,*) 'SOUTH'
         WRITE(1,*) Nsm_south-J_TRIM_SOUTH-J_START_SOUTH+1, J_START_SOUTH
         WRITE(1,*) 'NORTH'
         WRITE(1,*) Nsm_north-J_TRIM_NORTH-J_START_NORTH+1, J_START_NORTH

119      FORMAT(5E16.6)

         WRITE(1,*) 'TIME SERIES'
         DO K=Kstart,Ndata
           WRITE(1,*)TIME(K)
! east
           WRITE(1,*)'EAST SIDE'
          IF(Neast>0)THEN
             WRITE(1,119)(U_COUPLING_east(I,K),I=J_START_EAST,
     &         Nsm_east-J_TRIM_EAST)
             WRITE(1,119)(V_COUPLING_east(I,K),I=J_START_EAST,
     &         Nsm_east-J_TRIM_EAST)
             WRITE(1,119)(Z_COUPLING_east(I,K),I=J_START_EAST,
     &         Nsm_east-J_TRIM_EAST)
          ENDIF
! west
           WRITE(1,*)'WEST SIDE'
          IF(NWEST>0)THEN
             WRITE(1,119)(U_COUPLING_WEST(I,K),I=J_START_WEST,
     &         Nsm_WEST-J_TRIM_WEST)
             WRITE(1,119)(V_COUPLING_WEST(I,K),I=J_START_WEST,
     &         Nsm_WEST-J_TRIM_WEST)
             WRITE(1,119)(Z_COUPLING_WEST(I,K),I=J_START_WEST,
     &         Nsm_WEST-J_TRIM_WEST)
          ENDIF

! south
           WRITE(1,*)'SOUTH SIDE'
          IF(NSOUTH>0)THEN
             WRITE(1,119)(U_COUPLING_SOUTH(I,K),I=J_START_SOUTH,
     &         Nsm_SOUTH-J_TRIM_SOUTH)
             WRITE(1,119)(V_COUPLING_SOUTH(I,K),I=J_START_SOUTH,
     &         Nsm_SOUTH-J_TRIM_SOUTH)
             WRITE(1,119)(Z_COUPLING_SOUTH(I,K),I=J_START_SOUTH,
     &         Nsm_SOUTH-J_TRIM_SOUTH)
          ENDIF

! north
           WRITE(1,*)'NORTH SIDE'
          IF(NNORTH>0)THEN
             WRITE(1,119)(U_COUPLING_NORTH(I,K),I=J_START_NORTH,
     &         Nsm_NORTH-J_TRIM_NORTH)
             WRITE(1,119)(V_COUPLING_NORTH(I,K),I=J_START_NORTH,
     &         Nsm_NORTH-J_TRIM_NORTH)
             WRITE(1,119)(Z_COUPLING_NORTH(I,K),I=J_START_NORTH,
     &         Nsm_NORTH-J_TRIM_NORTH)
          ENDIF

         ENDDO ! end time

100      format(999E16.5)

         CLOSE(1)

         end









