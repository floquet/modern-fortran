          parameter(m=702,n=602)
          real depth(m,n)
          open(1,file='depth.txt')
          do j=1,n
            read(1,*) (depth(i,j),i=1,m)
          enddo
          close(1)

          do j=1,n
          do i=1,m
            depth(i,j)=depth(i,j)*100.0
          enddo
          enddo

          open(2,file='depth_blowup.txt')
          do j=1,n
            write(2,100)(depth(i,j),i=1,m)
          enddo
100       format(1000f12.3)

          close(2)
          end

          
