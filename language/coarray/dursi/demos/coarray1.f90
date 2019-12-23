!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! http://www.dursi.ca/coarray-fortran-goes-mainstream-gcc-5-1/

program coarray1

    implicit none
    integer :: me, right, i
    integer, dimension ( 3 ), codimension [ * ] :: a

        me = this_image( )

        right = me + 1
        if ( right > num_images( ) ) right = 1

        a ( : ) = [ ( me**i, i=1, 3 ) ]

        sync all

        print *, "Image ", me, " has a(2) = ", a( 2 )[ me ], "; neighbour has ", a( 2 )[ right ]

      stop 'successful program execution for coarray1'

end program coarray1