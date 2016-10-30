!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

program deluxe_mask

    use iso_fortran_env
    implicit NONE

    integer ( int64 )                :: narg = 0, alloc_status = 0, total_even = 0, total_odd = 0, bound = 0, i = 0
    integer ( int64 )                :: io_unit_target = 0, io_status = 0
    integer ( int64 ), parameter     :: bound_default = 1000000_int64
    integer ( int64 ), allocatable   :: iarray ( : )

    real    ( real64 )               :: u0, u1, time0, time1, time_mask, time_do

    character ( len = * ), parameter :: me   = 'deluxe_mask' ! Metcalf, Reid, Cohen: p. 309
    character ( len = 255 )          :: alloc_msg = " ", argument = " ", cmd = " ", io_msg = " ", file_name = "run_times.txt"

        call cpu_time ( u0 ) ! global cpu time

        ! check for command line shape parameters
        call get_command ( cmd )
        write ( *, '( "command line = ", g0, / )' ) trim ( cmd )

        narg = command_argument_count ( )

        if ( narg >= 1 ) then
            call getarg ( 1, argument )
            read  ( argument, * ) bound
            write ( *, '( "bound set to ", g0 )' ) bound
        else
            bound = bound_default
            write ( *, '( "using default bound of ", g0 )' ) bound
        end if
        write ( *, * )

        ! allocate large array
        call cpu_time ( time0 )
        write ( *, '( "Allocating int64 array of size ", g0, "..." )' ) bound
        allocate ( iarray ( 1 : bound ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 110 ) ""
            write ( *, 120 ) "", bound
            write ( *, 130 ) alloc_status
            write ( *, 140 ) trim ( alloc_msg )
        end if
        call cpu_time ( time1 )
        write ( *, 100 ) time1 - time0, "s, allocation time"

        ! assignment
        call cpu_time ( time0 )
        iarray ( 1 : bound ) = [ ( i, i = 1, bound ) ]
        call cpu_time ( time1 )
        write ( *, 100 ) time1 - time0, "s, load with implied do"

        ! summation with do loop
        call cpu_time ( time0 )
        total_even = 0
        total_odd  = 0
        do i = 1, bound
            if ( MOD ( i, 2_int64 ) == 0 ) then
                total_even = total_even + iarray ( i )
            else
                total_odd  = total_odd  + iarray ( i )
            end if
        end do
        call cpu_time ( time1 )
        write ( *, 150 ) total_even, total_odd
        time_do = time1 - time0
        write ( *, 100 ) time_do, "s, even, odd totals with do loop"

        ! summation with mask
        call cpu_time ( time0 )
        total_even = sum ( iarray, MASK = MOD ( iarray, 2_int64 ) == 0 )
        total_odd  = sum ( iarray, MASK = MOD ( iarray, 2_int64 ) == 1 )
        call cpu_time ( time1 )
        write ( *, 150 ) total_even, total_odd
        time_mask = time1 - time0
        write ( *, 100 ) time_mask, "s, even, odd totals with mask"

        ! deallocation
        call cpu_time ( time0 )
        deallocate ( iarray, stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 110 ) ""
            write ( *, 120 ) "", bound
            write ( *, 130 ) alloc_status
            write ( *, 140 ) trim ( alloc_msg )
        end if
        call cpu_time ( time1 )
        write ( *, 100 ) time1 - time0, "s, deallocation time"

        call cpu_time ( u1 )
        write ( *, 100 ) u1 - u0, "s, total cpu time"

!       post results to file
        open ( newunit = io_unit_target, file = trim ( file_name ), action = 'WRITE', status = 'OLD', position = "APPEND", &
                iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then                                              ! can't open file
            write ( *, 100 )
            write ( *, 110 ) 'open', trim ( file_name ), ' for'
            write ( *, 120 )
            write ( *, 130 ) io_unit_target
            write ( *, 140 ) io_status
            write ( *, 150 ) io_msg
            stop  'error during file open'
        else
            write ( *, 260 ) trim ( file_name ), " OPEN", io_unit_target
        end if

        write ( io_unit_target, '( I12, 3( 2X, E10.3 ) )' ) bound, time_do, time_mask, time_do / time_mask
        flush ( io_unit_target )

        close ( unit = io_unit_target, iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then                                              ! can't close file
            write ( *, 200 )
            write ( *, 210 ) 'close', trim ( file_name ), ' after'
            write ( *, 220 )
            write ( *, 230 ) io_unit_target
            write ( *, 240 ) io_status
            write ( *, 250 ) io_msg
            stop  'error during file close'
        else
            write ( *, 260 ) trim ( file_name ), " CLOSED", io_unit_target
        end if

        stop "successful completion for program " // me  ! must reduce to constant expression

  100   format ( F20.6, 2x, A, / )
  110   format ( "Memory ", A, "allocation failure" )
  120   format ( "Attempting to ", A, "allocate int64 array of ", g0, " elements" )
  130   format ( "Status code: ", g0 )
  140   format ( "Error message: ", A )
  150   format ( "Total of evens: ", g0, /, "Total of odds:  ", g0 )

  200   format ( "" )
  210   format ( "unable to ", A, " file ", A, A, " write." )
  220   format ( "trying to write compiler characterization" )
  230   format ( "io unit = ", g0 )
  240   format ( "iostat  = ", g0 )
  250   format ( "iomsg   = ", A, "." )
  260   format ( "file ", A, A, " as unit ", g0, "." )

end program deluxe_mask