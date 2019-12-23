! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mAxes

    use iso_fortran_env,       only : iostat_end
    use mPrecisionDefinitions, only : ip, rp, zero, one
    use mParametersSimulation, only : path_data
    use mFormatDescriptors
    use mShared
    use mReadLAMMPS
    use mLSQ

    implicit none

    integer ( ip )                             :: kAxis = 0, jAxis = 0
    integer ( ip )                             :: start = 0, end = 0

    character ( len = 64 )                     :: fmt_parts = ''
    character ( len = * ), parameter, private  :: me_mAxes = 'module mAxes'  ! self-identification

    type :: axis
        integer ( ip )              :: nParts = 0, nPoints = 0, index = 0
        integer ( ip ), allocatable :: partition ( : ) ! number of points in each row
        integer ( ip ), allocatable :: members   ( : ) ! list of points in all rows
        type ( lsq_fit )            :: results
        real ( rp )                 :: phi_total = 0, phi_mean = 0
        real ( rp ),    allocatable :: matrix_A ( : , : )
        real ( rp ),    allocatable :: X ( : ), Y ( : ), ones ( : ), J ( : ), residual_errors ( : )
        character ( len = 8 )       :: iam = ''
        character ( len = 512 )     :: path_case = ''
    contains
        private
        procedure, public :: read_members                => read_members_sub
        procedure, public :: read_partition              => read_partition_sub
        procedure, public :: export_solution             => export_solution_sub
        procedure, public :: least_squares_solution      => least_squares_solution_sub
        procedure, public :: allocate_column_vectors_sub => allocate_column_vectors_sub
        procedure, public :: populate_column_vectors_sub => populate_column_vectors_sub
    end type axis

    private :: read_members_sub
    private :: read_partition_sub
    private :: export_solution_sub
    private :: least_squares_solution_sub
    private :: allocate_column_vectors_sub
    private :: populate_column_vectors_sub

contains

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++          least_squares_solution

    subroutine least_squares_solution_sub ( me, iam, subdir_case, xyphi )

        class ( axis ), target                 :: me
        type ( LAMMPS_data ),  intent ( in )   :: xyphi ! LAMMPS data
        character ( len = * ), intent ( in )   :: iam, subdir_case

            me % iam = trim ( iam ) ! e.g. 'axis I'
            me % path_case = path_data // subdir_case

            call read_partition_sub ( me )
            call read_members_sub ( me )
            call allocate_column_vectors_sub ( me )
            call populate_column_vectors_sub ( me, xyphi )
            call solve_linear_system_sub ( me )
            call export_solution_sub ( me )

    end subroutine least_squares_solution_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             solve_linear_system

    subroutine solve_linear_system_sub ( me )

        use mMatrixWriter
        class ( axis ), target                 :: me

        ! rank 2
        real ( rp ) :: Winv ( 1 : 3, 1 : 3 ) = zero
        ! rank 1
        real ( rp ) :: beta ( 1 : 3 ) = zero, solution ( 1 : 3 ) = zero, solution_errors ( 1 : 3 ) = zero
        ! rank 0
        real ( rp ) :: a = zero, b = zero, c = zero, d = zero, e = zero, f = zero
        real ( rp ) :: det = zero, SSE = zero

            ! unique elements in product matrix A'A
            a = dot_product ( me % ones, me % ones )
            b = dot_product ( me % ones, me % J )
            c = dot_product ( me % ones, me % X )
            d = dot_product ( me % J, me % J )
            e = dot_product ( me % J, me % X )
            f = dot_product ( me % X, me % X )

            ! determinant
            det = 2 * b * c * e + a * d * f - a * e ** 2 - d * c ** 2 - f * b ** 2

            ! data vector
            beta ( 1 ) = dot_product ( me % ones, me % Y )
            beta ( 2 ) = dot_product ( me % J,    me % Y )
            beta ( 3 ) = dot_product ( me % X,    me % Y )

            ! populate unique elements in inverse matrix
            Winv (   :  , 1 ) = [ d * f - e ** 2, c * e - b * f,  b * e - c * d ]
            Winv ( 2 : 3, 2 ) = [ a * f - c ** 2, b * c - a * e ]
            Winv ( 3 : 3, 3 ) = [ a * d - b ** 2 ]

            ! populate repeated elements using symmetry
            Winv ( 1, 2 ) = Winv ( 2, 1 )
            Winv ( 1, 3 ) = Winv ( 3, 1 )
            Winv ( 2, 3 ) = Winv ( 3, 2 )

            Winv = Winv / det                                              ! inverse matrix
            call print_matrix ( Winv, 'E10.3', 2, 'Winv' )

            solution = matmul ( Winv, beta )                               ! solution vector

            me % matrix_A ( : , 1 ) = me % ones
            me % matrix_A ( : , 2 ) = me % J
            me % matrix_A ( : , 3 ) = me % X
            call print_matrix ( me % matrix_A, 'F10.3', 2, 'System matrix A' )

            me % residual_errors = matmul ( me % matrix_A, solution ) - me % Y
            SSE = dot_product ( me % residual_errors, me % residual_errors ) ! sum of squared errors

!               Data analysis and error analysis for the physical sciences, 1e
!               Philip Bevingtion, section 6.4 "Estimation of errors"
            solution_errors = [ ( Winv ( kAxis, kAxis ), kAxis = 1, 3 ) ]  ! diagonal elements of Winv
!               scale by estimated parent standard deviation ( SSE )
            solution_errors = solution_errors * SSE / ( me % nPoints - 3 )
            solution_errors = sqrt ( solution_errors )

!           map to explicit names
            me % results % intercept = solution ( 1 )
            me % results % gap       = solution ( 2 )
            me % results % slope     = solution ( 3 )

            me % results % err_intercept = solution_errors ( 1 )
            me % results % err_gap       = solution_errors ( 2 )
            me % results % err_slope     = solution_errors ( 3 )

    end subroutine solve_linear_system_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     export_solution

    subroutine export_solution_sub ( me )

        class ( axis ), target                 :: me

        integer ( ip )                         :: values ( 1 : 8 ) = 0

        ! character ( len = 8 )                  :: date = ''
        ! character ( len = 10 )                 :: time = ''
        ! character ( len = 5 )                  :: zone = ''

        character ( len = 512 )                :: myFile    = ''
        character ( len = * ), parameter       :: myAction  = 'write', myAccess = 'stream', myForm = 'formatted'
        character ( len = * ), parameter       :: myRoutine = 'subroutine read_members_sub'  ! self-identification
        character ( len = * ), parameter       :: stop_msg  = 'Execution ending due to error in ' // me_mAxes // &
                                                                                            ', ' // myRoutine
            myFile = trim ( me % path_case ) // 'results.txt'

            ! open file for writing
            open ( newunit = io_unit, file = myFile, action = myAction, access = myAccess, form = myForm, &
                    iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror    ) 'OPEN', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat     ) io_stat
                write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                stop stop_msg
            end if

            ! write file
            write  ( io_unit, '( "data set = ", g0, "." )', iostat = io_stat, iomsg = io_msg ) trim( me % iam )
            write  ( io_unit, fmt_results, iostat = io_stat, iomsg = io_msg ) 'intercept', me % results % intercept, &
                                                                                           me % results % err_intercept
            write  ( io_unit, fmt_results, iostat = io_stat, iomsg = io_msg ) 'gap      ', me % results % slope, &
                                                                                           me % results % err_slope
            write  ( io_unit, fmt_results, iostat = io_stat, iomsg = io_msg ) 'slope    ', me % results % gap, &
                                                                                           me % results % err_gap

            ! call date_and_time ( date, time, zone, values )
            ! call date_and_time ( DATE = date, ZONE = zone )
            ! call date_and_time ( TIME = time )
            call date_and_time ( VALUES = values )

            !write  ( io_unit, '( g0, 2x, g0, 2x, g0 )', iostat = io_stat, iomsg = io_msg ) date, time, zone
            write  ( io_unit, '( /, I5, 2 ( "-", I2.2 ) )', advance = 'no', iostat = io_stat, iomsg = io_msg ) values ( 1 : 3 )
            write  ( io_unit, '( I3, 2 ( ":", I2.2 ) )',                    iostat = io_stat, iomsg = io_msg ) values ( 5 : 7 )

            ! close file
            close ( unit = io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

    end subroutine export_solution_sub

!   ############################################################################################
!   #                                                                                          #
!   #  Allocation and population                                                               #
!   #                                                                                          #
!   ############################################################################################

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             allocate_column_vectors

    subroutine allocate_column_vectors_sub ( me )

        class ( axis ), target :: me

            ! allocate matrix_A
            if ( allocated ( me % matrix_A ) ) deallocate ( me % matrix_A, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'me % matrix_A', size ( me % matrix_A ), 'real ( rp )' )

            allocate ( me % matrix_A ( 1 : me % nPoints, 1 : 3 ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', ' me % matrix_A ( 1 : me % nPoints, 1 : 3 )', me % nPoints, 'real ( rp )' )

            ! allocate vector of x locations
            if ( allocated ( me % X ) ) deallocate ( me % X, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'me % X', size ( me % X ), 'real ( rp )' )

            allocate ( me % X ( 1 : me % nPoints ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', ' me % X ( 1 : me % nPoints )', me % nPoints, 'real ( rp )' )

            ! allocate vector of y locations
            if ( allocated ( me % Y ) ) deallocate ( me % Y, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'me % Y', size ( me % Y ), 'real ( rp )' )

            allocate ( me % Y ( 1 : me % nPoints ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', ' me % Y ( 1 : me % nPoints )', me % nPoints, 'real ( rp )' )

            ! allocate vector of residual errors
            if ( allocated ( me % residual_errors ) ) deallocate ( me % residual_errors, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'me % residual_errors', size ( me % residual_errors ), 'real ( rp )' )

            allocate ( me % residual_errors ( 1 : me % nPoints ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', ' me % residual_errors ( 1 : me % nPoints )', me % nPoints, 'real ( rp )' )

            ! allocate J vector
            if ( allocated ( me % J ) ) deallocate ( me % J, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'me % J', size ( me % J ), 'real ( rp )' )

            allocate ( me % J ( 1 : me % nPoints ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', ' me % J ( 1 : me % nPoints )', me % nPoints, 'real ( rp )' )

            ! allocate ones vector
            if ( allocated ( me % ones ) ) deallocate ( me % ones, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'me % ones', size ( me % ones ), 'real ( rp )' )

            allocate ( me % ones ( 1 : me % nPoints ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', ' me % ones ( 1 : me % nPoints )', me % nPoints, 'real ( rp )' )

    end subroutine allocate_column_vectors_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             populate_column_vectors

    subroutine populate_column_vectors_sub ( me, xyphi )

        class ( axis ), target                 :: me
        type ( LAMMPS_data )                   :: xyphi

            me % ones = one
            me % residual_errors = zero

            start = 1
            do kAxis = 1, me % nParts
                end = start + me % partition ( kAxis ) - 1
                write ( * , '( "Partition ", g0, " has ", g0, " members:" )' ) kAxis, me % partition ( kAxis )
                write ( * , * ) me % members ( start : end )
                me % J ( start : end ) = kAxis - 1
                me % X ( start : end ) = xyphi % x ( me % members ( start : end ) )
                me % Y ( start : end ) = xyphi % y ( me % members ( start : end ) )
                start = end + 1
            end do

    end subroutine populate_column_vectors_sub

!   ############################################################################################
!   #                                                                                          #
!   #  Read *.txt                                                                              #
!   #                                                                                          #
!   ############################################################################################

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      read_partition

    subroutine read_members_sub ( me )

        class ( axis ), target                 :: me

        character ( len = * ), parameter       :: myAction  = 'read', myAccess = 'stream', myForm = 'formatted'
        character ( len = * ), parameter       :: myRoutine = 'subroutine read_members_sub'  ! self-identification
        character ( len = * ), parameter       :: stop_msg  = 'Execution ending due to error in ' // me_mAxes // &
                                                                                            ', ' // myRoutine
        character ( len = 512 )                :: myFile = ''

            ! open file for reading
            myFile = trim ( me % path_case ) // 'members.txt'
            open ( newunit = io_unit, file = myFile, action = myAction, access = myAccess, form = myForm, &
                    iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror    ) 'OPEN', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat     ) io_stat
                write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                stop stop_msg
            end if

            ! allocate members
            if ( allocated ( me % members ) ) deallocate ( me % members, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'members ( 1 : nPoints )', size ( me % members ), 'integer ( ip )' )
            allocate ( me % members ( 1 : me % nPoints ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', 'members ( 1 : nPoints )', me % nPoints, 'integer ( ip )' )

            ! read file
            read_loop : do kRead = 1, me % nPoints
                read  ( io_unit, *, iostat = io_stat, iomsg = io_msg ) me % members ( kRead )
                if ( io_stat == iostat_end ) exit !EOF
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror    ) 'READ', trim ( myFile ), io_unit
                    write ( * , fmt = fmt_iostat     ) io_stat
                    write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                    stop stop_msg
                end if
            end do read_loop

            ! close file
            close ( unit = io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

            write ( * , fmt = fmt_read_array ) me % nPoints, trim ( myFile )
            write ( unit = fmt_parts, fmt = fmt_x ) me % nPoints
            !write ( unit = fmt_parts, fmt = '( "(", g0, "( I4, 2X ) )" )', iostat = io_stat, iomsg = io_msg ) &
            !        me % nParts
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror    ) 'WRITE', 'fmt_parts', 'unit = 6'
                write ( * , fmt = fmt_iostat     ) io_stat
                write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                stop stop_msg
            end if

            write ( * , '( /, "Points:" )' )
            start = 1
            do kAxis = 1, me % nParts
                end = start + me % partition ( kAxis ) - 1
                write ( unit = fmt_parts, fmt = fmt_x ) me % partition ( kAxis )
                write ( * , fmt = fmt_parts ) ( ( me % members ( jAxis ) ), jAxis = start, end )
                start = end + 1
            end do
            write ( * , '( "Total number of points = ", g0, / )' ) me % nPoints

    end subroutine read_members_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      read_partition

    subroutine read_partition_sub ( me )

        class ( axis ), target                 :: me

        character ( len = * ), parameter       :: myAction = 'read', myAccess = 'stream', myForm = 'formatted'
        character ( len = * ), parameter       :: myRoutine = 'subroutine read_partition_sub'  ! self-identification
        character ( len = * ), parameter       :: stop_msg = 'Execution ending due to error in ' // me_mAxes // &
                                                                                            ', ' // myRoutine
        character ( len = 512 )                :: myFile = ''

            ! open file for reading
            myFile = trim ( me % path_case ) // 'partition.txt'
            open ( newunit = io_unit, file = myFile, action = myAction, access = myAccess, form = myForm, &
                    iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror    ) 'OPEN', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat     ) io_stat
                write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                stop stop_msg
            end if

            ! count elements in the partition file
            me % nParts = 0
            count_loop : do
                read  ( io_unit, *, iostat = io_stat, iomsg = io_msg ) kAxis
                if ( io_stat == iostat_end ) exit !EOF
                me % nParts = me % nParts + 1
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror    ) 'READ', trim ( myFile ), io_unit
                    write ( * , fmt = fmt_iostat     ) io_stat
                    write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                    stop stop_msg
                end if
            end do count_loop

            ! allocate partition
            if ( allocated ( me % partition ) ) deallocate ( me % partition, stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( 'de', 'partition ( 1 : nParts )', size ( me % partition ), 'integer ( ip )' )
            allocate ( me % partition ( 1 : me % nParts ), stat = alloc_status, errmsg = alloc_msg )
            call alloc_alert ( '', 'partition ( 1 : nParts )', me % nParts, 'integer ( ip )' )

            ! read file
            me % nPoints = 0
            rewind io_unit
            read_loop : do kRead = 1, me % nParts
                read  ( io_unit, *, iostat = io_stat, iomsg = io_msg ) kAxis
                if ( io_stat == iostat_end ) exit !EOF
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror    ) 'READ', trim ( myFile ), io_unit
                    write ( * , fmt = fmt_iostat     ) io_stat
                    write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                    stop stop_msg
                end if
                me % partition ( kRead ) = kAxis
                me % nPoints = me % nPoints + kAxis
            end do read_loop

            ! close file
            close ( unit = io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

            write ( * , fmt = fmt_read_array ) me % nParts, trim ( myFile )
            write ( unit = fmt_parts, fmt = fmt_x ) me % nParts
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror    ) 'WRITE', 'fmt_parts', 'unit = 6'
                write ( * , fmt = fmt_iostat     ) io_stat
                write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                stop stop_msg
            end if

            write ( * , '( /, "Partitions:" )' )
            write ( * , fmt = fmt_parts ) ( ( me % partition ( kAxis ) ), kAxis = 1, me % nParts )
            write ( * , '( "Total number of points = ", g0 )' ) me % nPoints

            return

    end subroutine read_partition_sub

end module mAxes
