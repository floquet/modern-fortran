module mTimes

    use mPrecisionDefinitions,  only : ip
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg
    use mTimerCPU,              only : timer_cpu
    use mTimerClock,            only : timer_clock

    implicit none

    contains

        subroutine times ( cpu_global, clock_global, io_unit, file_out )

            integer ( ip ),        intent ( in )            :: io_unit
            type ( timer_cpu ),    intent ( in )            :: cpu_global
            type ( timer_clock ),  intent ( in )            :: clock_global
            character ( len = * ), intent ( in ), optional  :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_times ( cpu_global, clock_global, io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
                call print_times ( cpu_global, clock_global, io_unit )
            else
                call print_times ( cpu_global, clock_global, io_unit )
            end if

        end subroutine times

        subroutine print_times ( cpu_global, clock_global, myIO ) !   +   +   +   +   +   +   +   +   +   +   +   +   +  print_times

            integer ( ip ),       intent ( in ) :: myIO

            type ( timer_cpu ),   intent ( in ) :: cpu_global
            type ( timer_clock ), intent ( in ) :: clock_global

            integer ( ip )                      :: count_stop = 0, count_rate = 0, count_max = 0

                call announce ( "Check CPU and system clocks", myIO ) !  annunciation
                call system_clock ( count_stop, count_rate, count_max ) ! intrinsic

                write  ( myIO, 100 )
                write  ( myIO, 110 ) count_rate
                write  ( myIO, 120 ) count_max, huge ( count_rate )

                write  ( myIO, 180 ) cpu_global   % time_elapsed_cpu   ( ) !  record clock and CPU times
                write  ( myIO, 190 ) clock_global % time_elapsed_clock ( )

                return

            100 format ( 'system clock (implementation not standardized - compiler dependent)' )
            110 format ( 'count rate = ', g0, ': system dependent and can vary depending on the int_kind of the arguments' )
            120 format ( 'count max  = ', g0, ': typically [ HUGE ( count max ) = ', g0, ' ]', / )
            180 format ( 'total cpu time   = ', g0, ' s' )
            190 format ( 'total clock time = ', g0, ' s' )

        end subroutine print_times

end module mTimes
