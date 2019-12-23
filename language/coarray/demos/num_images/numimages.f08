! https://gcc.gnu.org/onlinedocs/gfortran/NUM_005fIMAGES.html
program numimages

    implicit none
    integer :: value [ * ]
    integer :: i

        value = this_image ( )
        !sync all
        if ( this_image ( ) == 1 ) then
            write ( *, '( "number of images = ", g0 )' ) num_images ( )
            do i = 1, num_images ( )
                write ( * , '( 2( a, i0 ) )' ) 'value [ ', i, ' ] is ', value [ i ]
            end do
        end if

end program numimages
