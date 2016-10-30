	parameter(nx=702,ny=602)

	real depth(nx,ny)

        open(1,file='dep.out')
       
   !     do j=1,ny
        read(1,*)((depth(j,i),i=1,ny),j=1,nx)
!        enddo
	close(1)


	open(2,file='depth.txt')
	do j=1,ny
	  write(2,110)(depth(i,j),i=1,nx)
	enddo
	close(2)
110	format(501f12.6)

	end



