!  http://gcc.gnu.org/onlinedocs/gfortran/RESHAPE.html#RESHAPE
          PROGRAM test_reshape
            INTEGER, DIMENSION(4) :: x
            integer, dimension ( 2, 2 ) :: y
            WRITE(*,*) SHAPE(x)                       ! prints "4"
            y = reshape ( x, [ 2, 2] )
            WRITE(*,*) SHAPE(RESHAPE(x, [ 2, 2 ]))    ! prints "2 2"
            WRITE(*,*) 'shape = ', SHAPE( y )    ! prints "2 2"
          END PROGRAM
