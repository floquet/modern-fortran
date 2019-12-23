module mSorter

    use, intrinsic :: iso_fortran_env,  only : REAL64

    implicit none
    integer, parameter :: rp = REAL64

    contains

        subroutine ascend ( list )
            real ( rp ), intent ( inout ) :: list ( : )
            real ( rp )                   :: swap
            integer                       :: nElements, left, right

                nElements = size ( list )
                print *, 'Number of elements = ', nElements

                do left = 1, nElements - 1
                    do right = left + 1, nElements
                        if ( list ( left ) < list ( right ) ) cycle
                        swap           = list ( left )
                        list ( left )  = list ( right )
                        list ( right ) = swap
                    end do
                end do

        end subroutine ascend

end module mSorter
