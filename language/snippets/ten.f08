program one
    implicit none
    integer :: j
        do j = 0, 1
            write ( *, 100 ) j, 10 ** j
        end do
    100 format ( 'j = ', g0, '; 10 ** j = ', g0 )
end program one
