!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! https://stackoverflow.com/questions/19669412/array-of-arrays-in-fortran

program main

    implicit none

    type :: my_type
        ! integer, pointer :: my_size( : )      ! F95
        integer, allocatable :: my_size ( : ) ! F95 + TR 15581 or F2003
    end type my_type

    type ( my_type ), allocatable :: x ( : )

        allocate ( x ( 3 ) )

        allocate ( x ( 1 ) % my_size ( 3 ) )
        allocate ( x ( 2 ) % my_size ( 2 ) )
        allocate ( x ( 3 ) % my_size ( 1 ) )

        print *, 'x( 3 ) % my_size = ', x ( 1 ) % my_size
        print *, 'x( 2 ) % my_size = ', x ( 2 ) % my_size
        print *, 'x( 1 ) % my_size = ', x ( 3 ) % my_size

        deallocate ( x ( 3 ) % my_size , x ( 2 ) % my_size , x ( 1 ) % my_size )
        deallocate ( x )

end program main