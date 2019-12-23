! https://gcc.gnu.org/onlinedocs/gfortran/CO_005fREDUCE.html#CO_005fREDUCE
! co_reduce_example determines element-wise the reduction of the value of A on all images of the current team
program co_reduce_example

    implicit none

    integer :: value[ * ]
    integer :: k
        value = this_image ( )
        call co_reduce ( value, result_image = 1, operator = myProd )
        if ( this_image ( ) == 1 ) then
            write ( * , '( "Number of images = ", g0 )' ) num_images ( )
            do k = 1, num_images ( )
                write ( * , '( 2( a, i0 ) )' ) 'value [ ', k, ' ] is ', value [ k ]
            end do
            write ( * , '( "Product  value = ", g0 )' ) value  ! prints num_images() factorial
            write ( * , 100 )
        end if

    100 format ( "Expected value = num_images()!", /, " 2! = 2, 3! = 6, 4! = 24, ..." )

    contains

        pure function myProd ( a, b ) result ( rslt )
            integer, value :: a, b
            integer        :: rslt

                rslt = a * b

        end function myProd

end program co_reduce_example

!  16:38 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib co_reduce_example.f08
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
!  16:39 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib co_reduce_example.f08 -L/opt/local/lib/ -lcaf_mpi -o co_reduce_example
!  16:39 dan-topas-pro-2 rditldmt $ mpirun -np 2 ./co_reduce_example
! Product  value = -1954993024
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:39 dan-topas-pro-2 rditldmt $ mpirun -np 2 ./co_reduce_example
! Product  value = 1500375424
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
!  16:39 dan-topas-pro-2 rditldmt $ mpirun -np 3 ./co_reduce_example
! Product  value = 909948928
! Expected value = num_images()!
!  2! = 2, 3! = 6, 4! = 24, 5! = 120, 6! = 720, 7! = 5040, 8! = 40320
