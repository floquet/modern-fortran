PROGRAM test_unpack
! http://gcc.gnu.org/onlinedocs/gfortran/UNPACK.html#UNPACK

            integer :: vector ( 2 )    = [ 1, 1 ]
            logical :: mask   ( 4 )    = [ .TRUE., .FALSE., .FALSE., .TRUE. ]
            integer :: field  ( 2, 2 ) = 0
            integer :: unity  ( 2, 2 )
          
            ! result: unity matrix
            unity = unpack( vector, reshape ( mask, [ 2, 2 ] ), field )
            
            print *, 'unity = ', unity

END PROGRAM test_unpack