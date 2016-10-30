! https://gcc.gnu.org/onlinedocs/gfortran/CO_005fREDUCE.html#CO_005fREDUCE
! CO_REDUCE determines element-wise the reduction of the value of A on all images of the current team
program co_reduce

    implicit none

    integer :: val = 0
        val = this_image ( )
        call co_reduce ( val, result_image = 1, operator = myProd )
        if ( this_image ( ) == 1 ) then
            write ( * , 100 ) val  ! prints num_images() factorial
            write ( * , 110 )
        end if

        stop

    100 format ( "Product  value = ", g0 )
    110 format ( "Expected value = num_images()!", /, " 2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320 " )

    contains

        pure function myProd ( a, b ) result ( rslt )
            integer, value :: a, b
            integer        :: rslt

                rslt = a * b

        end function myProd

end program co_reduce

!  16:38 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib co_reduce.f08
!  16:38 dan-topas-pro-2 rditldmt $ cafrun -np 2 ./a.out
! Product  value = -885468160
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:38 dan-topas-pro-2 rditldmt $ cafrun -np 2 ./a.out
! Product  value = -764292096
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:38 dan-topas-pro-2 rditldmt $ cafrun -np 2 ./a.out
! Product  value = -1662709760
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:38 dan-topas-pro-2 rditldmt $ cafrun -np 3 ./a.out
! Product  value = -375438336
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:39 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib co_reduce.f08 -L/opt/local/lib/ -lcaf_mpi -o co_reduce
!  16:39 dan-topas-pro-2 rditldmt $ mpirun -np 2 ./co_reduce
! Product  value = -1954993024
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:39 dan-topas-pro-2 rditldmt $ mpirun -np 2 ./co_reduce
! Product  value = 1500375424
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:39 dan-topas-pro-2 rditldmt $ mpirun -np 3 ./co_reduce
! Product  value = 909948928
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
