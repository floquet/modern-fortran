module mAccumulator

    use mPrecisionDefinitions,  only : ip, zint, sp, dp, qp, ascii
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg
    use mTimerCPU,              only : timer_cpu
    use mTimerClock,            only : timer_clock
    use mUnitValues,            only : xdef, x032, x064, x128

    implicit none

    contains

        subroutine accumulator ( io_unit, file_out )  !   +   +   +   +   +   +   +   +   +   +   +      accumulator

            integer ( ip ),        intent ( in )            :: io_unit
            character ( len = * ), intent ( in ), optional  :: file_out

            integer ( ip )                                  :: io_block

            nBlock = nBlock + 1 ! block counter

            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call announce ( 'Accumulation tests', io_block ) ! annunciation
                call print_accumulator ( io_block, 1000000,            'million' )
                call print_accumulator ( io_block, 1000000000,         'billion' )
                call print_accumulator ( io_block, 1024 * 1024,        '2**20' )
                call print_accumulator ( io_block, 1024 * 1024 * 1024, '2**30' )
                !call print_accumulator ( io_block, how_many, name )
                close ( io_block, iostat = io_status, iomsg = io_msg )
                call print_accumulator ( io_unit, 1000000,            'million' )
                call print_accumulator ( io_unit, 1000000000,         'billion' )
                call print_accumulator ( io_unit, 1024 * 1024,        '2**20' )
                call print_accumulator ( io_unit, 1024 * 1024 * 1024, '2**30' )
            else
                call announce ( 'Accumulation tests', io_unit ) ! annunciation
                call print_accumulator ( io_unit, 1000000,            'million' )
                call print_accumulator ( io_unit, 1000000000,         'billion' )
                call print_accumulator ( io_unit, 1024 * 1024,        '2**20' )
                call print_accumulator ( io_unit, 1024 * 1024 * 1024, '2**30' )
            end if

        end subroutine accumulator

        subroutine print_accumulator ( io_unit, how_many, name )  !   +   +   +   +   +   +   +   +   +   +   +    print_accumulator

            integer ( ip ) ,                     intent ( in )  :: io_unit
            integer ( zint ),                    intent ( in )  :: how_many
            character ( kind = ascii, len = * ), intent ( in )  :: name

            integer ( ip )                                      :: k

            real                                                :: incdef, sumdef
            real ( sp )                                         :: inc032, sum032
            real ( dp )                                         :: inc064, sum064
            real ( qp )                                         :: inc128, sum128

            ! instantiate timer instances for entire program
            type ( timer_cpu )                                  :: cpu_accumulate
            type ( timer_clock )                                :: clock_accumulate

                ! start timers
                call cpu_accumulate   % timer_start_cpu   ( )                        ! CPU time in measurement
                call clock_accumulate % timer_start_clock ( )                        ! clock time in measurement

                incdef = xdef / how_many                                             ! interval size
                inc032 = x032 / how_many
                inc064 = x064 / how_many
                inc128 = x128 / how_many

                sumdef = 0.0                                                         ! clear summation registers
                sum032 = 0.0_sp
                sum064 = 0.0_dp
                sum128 = 0.0_qp

                do k = 1, how_many                                                   ! increment increase
                    sumdef = sumdef + incdef
                    sum032 = sum032 + inc032
                    sum064 = sum064 + inc064
                    sum128 = sum128 + inc128
                end do

                write  ( io_unit, 100 ) trim ( name )                                ! size of increment
                write  ( io_unit, 110 )
                write  ( io_unit, 120 ) "default  ", incdef
                write  ( io_unit, 120 ) "single   ", inc032
                write  ( io_unit, 120 ) "double   ", inc064
                write  ( io_unit, 120 ) "quadruple", inc128
                write  ( io_unit,  *  )

                write  ( io_unit, 150 )                                              ! summation value
                write  ( io_unit, 120 ) "default  ", sumdef
                write  ( io_unit, 120 ) "single   ", sum032
                write  ( io_unit, 120 ) "double   ", sum064
                write  ( io_unit, 120 ) "quadruple", sum128
                write  ( io_unit,  *  )

                write  ( io_unit, 140 )                                              ! increment error
                write  ( io_unit, 120 ) "default  ", xdef - sumdef
                write  ( io_unit, 120 ) "single   ", x032 - sum032
                write  ( io_unit, 120 ) "double   ", x064 - sum064
                write  ( io_unit, 120 ) "quadruple", x128 - sum128

                write  ( io_unit, 200 )                                              ! increment error
                write  ( io_unit, 120 ) "default  ", ( xdef - sumdef ) / incdef
                write  ( io_unit, 120 ) "single   ", ( x032 - sum032 ) / inc032
                write  ( io_unit, 120 ) "double   ", ( x064 - sum064 ) / inc064
                write  ( io_unit, 120 ) "quadruple", ( x128 - sum128 ) / inc128

                write  ( io_unit, 160 ) cpu_accumulate   % time_elapsed_cpu   ( )   !  record clock and CPU times
                write  ( io_unit, 170 ) clock_accumulate % time_elapsed_clock ( )
                write  ( io_unit, * )

                return

            100 format ( "one ", A, " intervals" )
            110 format ( "interval size:" )
            120 format ( A, " precision: ", g0 )

            140 format ( "accumulation errors: 1 - increment size * number of increments (ideal answer = 0)" )
            150 format ( "accumulation values: (ideal answer = 1)" )
            160 format ( /, 'total cpu   time for accumulation = ', g0, ' s' )
            170 format (    'total clock time for accumulation = ', g0, ' s', / )

            200 format ( /, "relative errors in accumulation (expressed in terms of the increment)" )

        end subroutine print_accumulator

end module mAccumulator
