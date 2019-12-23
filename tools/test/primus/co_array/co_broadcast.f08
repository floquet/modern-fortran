! https://gcc.gnu.org/onlinedocs/gfortran/CO_005fBROADCAST.html#CO_005fBROADCAST
! CO_BROADCAST copies the value of argument A on the image with image index SOURCE_IMAGE to all images in the current team
program co_broadcast
    implicit none

    integer :: val ( 3 )
        if ( this_image ( ) == 1 ) then
            val = [ 1, 5, 3 ]
            print *, 'broadcasting the vector ', val
        end if

        call co_broadcast ( val, source_image = 1 )
        print *, 'image ', this_image ( ), ":", val

end program co_broadcast

!  16:18 dan-topas-pro-2 rditldmt $ date
! Tue Mar 15 16:18:08 CDT 2016
!  16:18 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib co_broadcast.f08
!  16:18 dan-topas-pro-2 rditldmt $ cafrun -np 5 ./a.out
!            1 :           1           5           3
!            2 :           1           5           3
!            3 :           1           5           3
!            5 :           1           5           3
!            4 :           1           5           3
!  16:18 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib co_broadcast.f08 -L/opt/local/lib/ -lcaf_mpi -o co_broadcast
!  16:18 dan-topas-pro-2 rditldmt $ mpirun -np 5 ./co_broadcast
!            1 :           1           5           3
!            3 :           1           5           3
!            4 :           1           5           3
!            5 :           1           5           3
!            2 :           1           5           3
