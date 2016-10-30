! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
submodule ( mAxes ) smAxesIO

    use, intrinsic :: iso_fortran_env, only : iostat_end ! EOF

    use mFileHandling,                 only : safeopen_readonly

    integer ( ip ) :: io_stat
    integer ( ip ) :: jAxis, kRead

    character ( len = 64 )            :: fmt_parts
    character ( len = *  ), parameter :: fmt_matrix_rows = '( "( I5, 3X, ", g0, " ( I4, 2X ) )" )'
    character ( len = *  ), parameter :: fmt_read_array  = '( /, "Array of ", g0, " elements read in from ", g0, "." )'

contains

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +    export_solution

    module subroutine export_solution_sub ( me )

        class ( axis ), target  :: me

        integer ( ip )          :: values ( 1 : 8 ), my_io_unit

        character ( len = 512 ) :: myFile

            myFile = trim ( me % path_case ) // 'results.txt'
            my_io_unit = safeopen_writereplace (  filename = myFile )

            ! write file
            write  ( my_io_unit, '( "data set = ", g0, "." )' ) trim ( me % iam )
            write  ( my_io_unit, 100 ) 'intercept', me % results % intercept, me % results % err_intercept
            write  ( my_io_unit, 100 ) 'gap      ', me % results % slope,     me % results % err_slope
            write  ( my_io_unit, 100 ) 'slope    ', me % results % gap,       me % results % err_gap

            call date_and_time ( VALUES = values )

            write  ( my_io_unit, '( /, I5, 2 ( "-", I2.2 ) )', advance = 'no' ) values ( 1 : 3 )
            write  ( my_io_unit, '( I3, 2 ( ":", I2.2 ) )' )                    values ( 5 : 7 )

            ! close file
            close ( unit = my_io_unit )

            return

        100 format ( g0, " = ", g0, " +/- ", g0, "." )

    end subroutine export_solution_sub

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +    read_members

    module subroutine read_members_sub ( me )

        class ( axis ), target  :: me

        integer ( ip )          :: io_debug, io_members
        character ( len = 512 ) :: myFile

            call allocator_rank_1_ints_sub ( array = me % members, rows = me % nPoints )

            myFile = trim ( me % path_case ) // 'members.txt'
            io_members = safeopen_readonly ( filename = myFile )

            read_loop : do kRead = 1, me % nPoints
                read  ( io_members, *, iostat = io_stat ) me % members ( kRead )
                if ( io_stat == iostat_end ) exit !EOF
            end do read_loop

            ! close file
            close ( unit = io_members )

            ! diagnostic print
            myFile = trim ( me % path_case ) // 'members output.txt'
            io_debug = safeopen_writereplace ( filename = myFile )
            write ( unit = io_debug,  fmt = fmt_read_array  ) me % nPoints, trim ( myFile )
            write ( unit = fmt_parts, fmt = fmt_matrix_rows ) me % nPoints

            write ( io_debug , '( /, "Points:" )' )
            first = 1
            do kAxis = 1, me % nParts
                last = first + me % partition ( kAxis ) - 1
                write ( unit = fmt_parts, fmt = 100 ) me % partition ( kAxis )
                write ( io_debug , fmt = fmt_parts ) kAxis, ( ( me % members ( jAxis ) ), jAxis = first, last )
                first = last + 1
            end do
            write ( io_debug , '( "Total number of points = ", g0, / )' ) me % nPoints

            close ( unit = io_debug )

        100 format ( "( I5, 3X, ", g0, " ( I4, 2X ) )" )

    end subroutine read_members_sub

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +     read_partition

    module subroutine read_partition_sub ( me )

        class ( axis ), target  :: me

        integer ( ip )          :: io_debug, io_unit
        character ( len = 512 ) :: myFile

            ! open partitions file for reading: list of points for each row
            myFile = trim ( me % path_case ) // 'partition.txt'
            io_unit = safeopen_readonly ( filename = myFile )
                ! count elements in the partition file
                me % nParts = 0
                count_loop : do
                    read  ( io_unit, *, iostat = io_stat ) kAxis
                    if ( io_stat == iostat_end ) exit !EOF
                    me % nParts = me % nParts + 1
                end do count_loop

                call allocator_rank_1_ints_sub ( array = me % partition, rows = me % nParts )

                ! read elements in the partition file
                me % nPoints = 0
                rewind io_unit
                read_loop : do kRead = 1, me % nParts
                    read  ( io_unit, *, iostat = io_stat ) kAxis
                    if ( io_stat == iostat_end ) exit !EOF
                    me % partition ( kRead ) = kAxis
                    me % nPoints = me % nPoints + kAxis
                end do read_loop
            close ( unit = io_unit )

            myFile = trim ( me % path_case ) // 'partition output.txt'
            io_debug = safeopen_writereplace ( filename = myFile )
                write ( unit = io_debug , fmt = 100 ) me % nParts, trim ( myFile )
                write ( unit = fmt_parts, fmt = fmt_matrix_rows ) me % nParts

                write ( io_debug , '( /, "Partitions:" )' )
                write ( io_debug , fmt = fmt_parts ) ( ( me % partition ( kAxis ) ), kAxis = 1, me % nParts )
                write ( io_debug , '( "Total number of points = ", g0 )' ) me % nPoints
            close ( unit = io_debug )

            return

        100 format ( /, "Array of ", g0, " elements read in from ", g0, "." )

    end subroutine read_partition_sub

end submodule smAxesIO
