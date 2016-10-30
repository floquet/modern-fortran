program test

    implicit none

    type :: dan
        real :: a, b, c
    end type dan

    type ( dan ), parameter :: bob = dan ( 1.0, 2.0, 3.0 )
    type ( dan ), parameter :: rob ( 1 : 2 ) = [ dan ( 1.0, 2.0, 3.0 ), dan ( 2.0, 4.0, 6.0 ) ]

    write ( * , * ) bob
    write ( * , * ) rob

end program test
