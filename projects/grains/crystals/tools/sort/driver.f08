program driver

    use, intrinsic :: iso_fortran_env,  only : REAL64
    use mSorter,                        only : ascend

    implicit none

    integer, parameter :: nElements = 10, rp = REAL64

    real ( rp ) :: myList ( 1 : nElements ) = 0.0_rp
    integer     :: k = 0
    logical     :: passed = .true.

        ! create an array
        myList = [ ( k, k = nElements, 1, -1 ) ]
        print *, 'myList in = ', myList
        ! sort array
        call ascend ( myList )
        ! check array
        do k = 1, nElements - 1
            if ( myList ( k ) > myList ( k + 1 ) ) passed = .false.
        end do

        print *, 'myList out = ', myList

        write  ( *, 100 ) passed
    100 format ( 'Is the list sorted in ascending order? ', g0 )

end program driver
