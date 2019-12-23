include 'myClass.f08'

program class_pass

    use myClass
    implicit none

    type ( test ) :: myTest

        myTest % x = 2.0
!         call myTest % action ( square )
!         call myTest % action ( double )
        call myTest % check ()
        call myTest % action ( square_sub )

end program class_pass
