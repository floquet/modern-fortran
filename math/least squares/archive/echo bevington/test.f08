program test

    implicit none

    type :: results
        real :: x ( 1 : 10 )
    end type results

    type, extends ( results ) :: data
        real :: y ( 1 : 10 )
    end type data

    type ( results ) :: myResults
    type ( data ) :: myData

    type, extends ( data ) :: data
        real :: z
    end type

        myResults % x ( : ) = 1.0
        myData % y ( : ) = 2.0

        print *, 'results % x ( 1 ) = ', myResults % x ( 1 )
        print *, 'data % y ( 1 ) = ', myData % y ( 1 )
        print *, 'data % x ( 1 ) = ', myData % x ( 1 )


end program test
