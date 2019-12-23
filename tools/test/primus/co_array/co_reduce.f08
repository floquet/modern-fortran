! https://gcc.gnu.org/onlinedocs/gfortran/CO_005fREDUCE.html#CO_005fREDUCE
! CO_REDUCE determines element-wise the reduction of the value of A on all images of the current team
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
