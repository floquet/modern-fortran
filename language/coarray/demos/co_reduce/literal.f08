          program test
            integer :: val
            val = this_image ()
            call co_reduce (val, result_image=1, operator=myprod)
            if (this_image() == 1) then
              write(*,*) "Product value", val  ! prints num_images() factorial
            end if
          contains
            pure function myprod(a, b)
              integer, value :: a, b
              integer :: myprod
              myprod = a * b
            end function myprod
          end program test

!  16:21 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib literal.f08 -L/opt/local/lib/ -lcaf_mpi -o literal
!  16:27 dan-topas-pro-2 rditldmt $ mpirun -np 2 ./literal
!  Product value  1467410496
!  16:27 dan-topas-pro-2 rditldmt $ mpirun -np 2 ./literal
!  Product value  2095491456

!  16:28 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib literal.f08 
!  16:28 dan-topas-pro-2 rditldmt $ cafrun -np 2 ./a.out
!  Product value  1118024576
!  16:28 dan-topas-pro-2 rditldmt $ cafrun -np 2 ./a.out
!  Product value  1046312128
