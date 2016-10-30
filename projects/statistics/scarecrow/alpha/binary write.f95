!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! write (filename, "(A5,I2)") "hello", 10
!include 'modules/mod precision definitions.f90'

program write

    use iso_fortran_env

    implicit NONE

    integer, parameter               :: ip = selected_int_kind  ( INT8 )
    integer ( ip ), allocatable      :: events ( : , : )
    integer ( ip )                   :: j = 0, k = 0
    integer ( ip )                   :: count = 1_ip
    integer ( ip )                   :: io = 0, io_status = 0, alloc_status = 0

    character ( len = 512 )          :: io_msg = "", alloc_msg = ""
    character ( len = * ), parameter :: file_name = "test.int8"

        allocate ( events ( 1 : 100, 1 : 2 ), stat = alloc_status, errmsg = alloc_msg )

        count = 0
        do j = 1, 10
            do k = 1, 10
                count = count + 1_ip
                events ( count, : ) = [ k, j ]
            end do
        end do

        write ( *, '( "size ( events, 1 ) = ", g0, "; size ( events, 2 ) = ", g0 )') size ( events, 1 ), size ( events, 2 )
        write ( *, '( "bytes needed to store events = ", g0 )' ) sizeof ( events )

        open ( newunit = io, file = 'data/' // file_name, action = 'write', status = 'unknown', form = 'unformatted', &
                access = 'stream', iostat = io_status, iomsg = io_msg )
        write ( unit = io, iostat = io_status, iomsg = io_msg ) events
        close ( unit = io, iostat = io_status, iomsg = io_msg )

        deallocate ( events, stat = alloc_status, errmsg = alloc_msg )

        stop

end program write

! gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 runner.f95