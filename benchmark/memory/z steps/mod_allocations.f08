module mAllocations

    !use iso_fortran_env
    use mPrecisionDefinitions,  only : ip, zint, ascii
    use mParameters,            only : megabytes
    use mTimerCPU
    use mTimerClock
    use mShared,                only : alloc_status, alloc_msg, io_status, io_msg, alert_io, alert_alloc, open_file_output
    use mBlocks,                only : announce, nBlock
    use mUnitValues

    integer ( ip ) :: myIO

    contains

        subroutine allocator ( io_unit, MBytes, file_out )

            integer ( ip ),        intent ( in )           :: io_unit
            integer ( zint ),      intent ( in )           :: MBytes
            character ( len = * ), intent ( in ), optional :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_allocator ( io_block, MBytes )
                close ( io_block, iostat = io_status, iomsg = io_msg )
            else
                call print_allocator ( io_unit, MBytes )
            end if

        end subroutine allocator

        subroutine print_allocator ( myIO, MBytes )  !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   print_allocator

            integer ( zint ), intent ( in ) :: MBytes
            integer ( zint )                :: size

            ! instantiate timer instances for allocations
            type ( timer_cpu   ) ::   cpu_all_allocations
            type ( timer_clock ) :: clock_all_allocations

                call announce ( 'Memory allocations', myIO ) ! annunciation

                ! start timers
                call cpu_all_allocations   % timer_start_cpu   ( ) ! CPU time in measurement
                call clock_all_allocations % timer_start_clock ( ) ! clock time in measurement

                size = MBytes * megabytes
                write ( myIO, 100 ) size, MBytes, megabytes

                call allocations ( size, myIO )

                write  ( myIO, 110 ) cpu_all_allocations   % time_elapsed_cpu   ( ) !  record clock and CPU times
                write  ( myIO, 120 ) clock_all_allocations % time_elapsed_clock ( )

                return

            100 format ( 'size = MBytes * mega : ', g0, ' = ', g0, ' * ', g0 )
            110 format ( /, 'total cpu   time for all allocations = ', g0, ' s' )
            120 format (    'total clock time for all allocations = ', g0, ' s', / )

        end subroutine print_allocator

        subroutine allocations ( size, myIO ) !   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +  ALLOCATIONS

            integer,                                    dimension ( : ), allocatable :: arrayidef
            integer ( selected_int_kind ( INT8 ) ),     dimension ( : ), allocatable :: arrayi08
            integer ( selected_int_kind ( INT16 ) ),    dimension ( : ), allocatable :: arrayi16
            integer ( selected_int_kind ( INT32 ) ),    dimension ( : ), allocatable :: arrayi32
            integer ( selected_int_kind ( INT64 ) ),    dimension ( : ), allocatable :: arrayi64

            real,                                       dimension ( : ), allocatable :: arrayxdef
            real ( selected_real_kind ( REAL32 ) ),     dimension ( : ), allocatable :: arrayx032
            real ( selected_real_kind ( REAL64 ) ),     dimension ( : ), allocatable :: arrayx064
            real ( selected_real_kind ( REAL128 ) ),    dimension ( : ), allocatable :: arrayx128

            complex,                                    dimension ( : ), allocatable :: arraycdef
            complex ( selected_real_kind ( REAL32  ) ), dimension ( : ), allocatable :: arrayc032
            complex ( selected_real_kind ( REAL64  ) ), dimension ( : ), allocatable :: arrayc064
            complex ( selected_real_kind ( REAL128 ) ), dimension ( : ), allocatable :: arrayc128

            ! instantiate timer instances for allocations
            type ( timer_cpu   ) :: cpu_allocate
            type ( timer_clock ) :: clock_allocate

            integer   ( zint ), intent ( in ) :: size, myIO

            character ( kind = ascii, len = 255 )       :: precision_type = ' '

                ! announce allocation size
                write ( *,    100 ) size / megabytes
                write ( myIO, 100 ) size / megabytes

                precision_type = 'default integer' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'integer', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayidef ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayidef', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayidef = idef
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayidef )
                write ( myIO, 150 ) dot_product ( arrayidef, arrayidef )

                ! DEALLOCATE
                deallocate ( arrayidef, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayidef', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'INT8' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'integer', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayi08 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayi08', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayi08 = i08
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayi08 )
                write ( myIO, 150 ) dot_product ( arrayi08, arrayi08 )

                ! DEALLOCATE
                deallocate ( arrayi08, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayi08', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'INT16' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'integer', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayi16 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayi16', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayi16 = i16
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayi16 )
                write ( myIO, 150 ) dot_product ( arrayi16, arrayi16 )

                ! DEALLOCATE
                deallocate ( arrayi16, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayi16', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'INT32' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'integer', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayi32 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayi32', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayi32 = i32
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayi32 )
                write ( myIO, 150 ) dot_product ( arrayi32, arrayi32 )

                ! DEALLOCATE
                deallocate ( arrayi32, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayi32', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'INT64' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'integer', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayi64 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayi64', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayi64 = i64
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayi64 )
                write ( myIO, 150 ) dot_product ( arrayi64, arrayi64 )

                ! DEALLOCATE
                deallocate ( arrayi64, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayi64', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'default real' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'real', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayxdef ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayxdef', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayxdef = xdef
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayxdef )
                write ( myIO, 150 ) dot_product ( arrayxdef, arrayxdef )

                ! DEALLOCATE
                deallocate ( arrayxdef, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayxdef', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'REAL32' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'real', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayx032 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayx032', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayx032 = x032
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayx032 )
                write ( myIO, 150 ) dot_product ( arrayx032, arrayx032 )

                ! DEALLOCATE
                deallocate ( arrayx032, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayx032', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'REAL64' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'real', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayx064 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayx064', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayx064 = x064
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayx064 )
                write ( myIO, 150 ) dot_product ( arrayx064, arrayx064 )

                ! DEALLOCATE
                deallocate ( arrayx064, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayx064', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'REAL128' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'real', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayx128 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayx128', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayx128 = x128
                write ( myIO, 130 ) size
                write ( myIO, 140 ) sum ( arrayx128 )
                write ( myIO, 150 ) dot_product ( arrayx128, arrayx128 )

                ! DEALLOCATE
                deallocate ( arrayx128, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayx128', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'default complex' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'complex', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arraycdef ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arraycdef', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arraycdef = cdef
                write ( myIO, 130 ) size
                write ( myIO, 160 ) sum ( arraycdef )
                write ( myIO, 170 ) dot_product ( arraycdef, arraycdef )

                ! DEALLOCATE
                deallocate ( arraycdef, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arraycdef', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'REAL32' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'complex', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayc032 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayc032', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayc032 = c032
                write ( myIO, 130 ) size
                write ( myIO, 160 ) sum ( arrayc032 )
                write ( myIO, 170 ) dot_product ( arrayc032, arrayc032 )

                ! DEALLOCATE
                deallocate ( arrayc032, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayc032', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'REAL64' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'complex', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayc064 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayc064', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayc064 = c064
                write ( myIO, 130 ) size
                write ( myIO, 160 ) sum ( arrayc064 )
                write ( myIO, 170 ) dot_product ( arrayc064, arrayc064 )

                ! DEALLOCATE
                deallocate ( arrayc064, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayc064', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                precision_type = 'REAL128' !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
                write ( *,    110, advance = 'no' ) 'complex', trim ( precision_type )
                write ( myIO, 110 )                 trim ( precision_type )

                call cpu_allocate   % timer_start_cpu   ( )
                call clock_allocate % timer_start_clock ( )

                ! ALLOCATE
                allocate ( arrayc128 ( 1 : size ), stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = '', myArray = 'arrayc128', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )
                write ( * , 120 )

                ! POPULATE
                arrayc128 = c128
                write ( myIO, 130 ) size
                write ( myIO, 160 ) sum ( arrayc128 )
                write ( myIO, 170 ) dot_product ( arrayc128, arrayc128 )

                ! DEALLOCATE
                deallocate ( arrayc128, stat = alloc_status, errmsg = alloc_msg )
                call alert_alloc ( de = 'de', myArray = 'arrayc128', nElements = size, data_type = trim ( precision_type ), &
                                         myIO = myIO, fatal = .false. )

                ! record times
                write ( myIO, 180 ) cpu_allocate   % time_elapsed_cpu   ( )
                write ( myIO, 190 ) clock_allocate % time_elapsed_clock ( )
                flush ( myIO )

                return

        100     format ( /, '+ + + Requesting allocation of ', g0, ' megabytes' )
        110     format ( /, 'attempting to allocate rank one ', g0,' array of precision ', A, '.' )
        120     format ( /, ' ... allocation completed' )
        130     format ( 'expected total of all unit elements = ', g0 )
        140     format ( 'actual total of all unit elements   = ', g0 )
        150     format ( 'actual total of real elements       = ', g0, ', ', g0 )
        160     format ( 'dot product total                   = ', g0 )
        170     format ( 'dot product total                   = ', g0, ', ', g0 )
        180     format ( 'cpu time                            = ', E9.3, ' s' )
        190     format ( 'clock time                          = ', E9.3, ' s' )

        end subroutine allocations

end module mAllocations
