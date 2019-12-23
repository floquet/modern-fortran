module mCompareToMM

    ! https://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90
    use, intrinsic :: iso_fortran_env, only : stdout => output_unit
    use mPrecisionDefinitions,         only : ip, rp, zero, one
    use mAxes
    use mParametersSimulation,         only : path_data
    use mShared,                       only : io_stat, io_msg
    use mFormatDescriptors
    use mMathematicaOutput
    use mAngle

    implicit none

    character ( len = * ), parameter, private  :: me_mCompareToMM = 'module mCompareToMM'  ! self-identification

    type :: comparison
        integer ( ip )      :: index = 0
        type ( lsq_fit )    :: result_fortran, result_mathematica, result_difference
        type ( angle )      :: apex_angle_fortran, apex_angle_mathematica, apex_angle_difference
    contains
        private
        procedure, public   :: print_engine_apex         => print_engine_apex_sub
        procedure, public   :: print_engine_lines        => print_engine_lines_sub
        !procedure, public   :: print_comparison_lines    => print_comparison_lines_sub
        procedure, public   :: compare_mm_apex  => compare_mm_apex_sub
        procedure, public   :: compare_mm_lines => compare_mm_lines_sub
    end type comparison

    private :: print_engine_apex_sub
    private :: print_engine_lines_sub
    !private :: print_comparison_lines_sub
    private :: compare_mm_apex_sub
    private :: compare_mm_lines_sub

contains

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   compare_mm_apex

    subroutine compare_mm_apex_sub ( me, apex_angle_fortran, index )

        class ( comparison ), target          :: me

        integer ( ip ), intent ( in )         :: index
        type ( angle ), intent ( in )         :: apex_angle_fortran
        integer ( ip )                        :: values ( 1 : 8 ) = 0

        type ( angle ), pointer               :: p_f, p_m, p_d

        logical                               :: file_exists = .false.

        character ( len = 512 )               :: myStatus    = 'old'
        character ( len = * ), parameter      :: myFile      = trim ( path_data // 'validation apex.txt' )
        character ( len = * ), parameter      :: myAction    = 'write'
        character ( len = * ), parameter      :: myPosition  = 'append'
        character ( len = * ), parameter      :: myRoutine   = 'compare_mm_apex_sub'
        character ( len = * ), parameter      :: stop_msg    = 'Execution ending due to error in ' // me_mCompareToMM // &
                                                                                              ', ' // myRoutine // '.'

            me % index = index
            p_f => me % apex_angle_fortran
            p_m => me % apex_angle_mathematica
            p_d => me % apex_angle_difference

                p_f = apex_angle_fortran
                p_m = mm_angles ( index )

                p_d %  th = p_f %  th - p_m %  th
                p_d % dth = p_f % dth - p_m % dth

            p_f => null ( )
            p_m => null ( )
            p_d => null ( )

            ! open file for writing
            ! https://stackoverflow.com/questions/15526203/single-command-to-open-a-file-or-create-it-and-the-append-data
            inquire ( file = myFile, exist = file_exists )

            if ( file_exists ) then
                myStatus = "old"
                open ( newunit = io_unit, file = myFile, status = myStatus, action = myAction, position = myPosition )
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror     ) 'OPEN', trim ( myFile ), io_unit
                    write ( * , fmt = fmt_iostat      ) io_stat
                    write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings3 ) myStatus, myAction, myPosition
                    stop stop_msg
                end if
                ! time and date stamp
                call date_and_time ( VALUES = values )
                write  ( io_unit, '( I5, 2 ( "-", I2.2 ) )', advance = 'no', iostat = io_stat, iomsg = io_msg ) values ( 1 : 3 )
                write  ( io_unit, '( I3, 2 ( ":", I2.2 ), / )',              iostat = io_stat, iomsg = io_msg ) values ( 5 : 7 )
            else
                myStatus = "new"
                open ( newunit = io_unit, file = myFile, status = myStatus, action = myAction )
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror     ) 'OPEN', trim ( myFile ), io_unit
                    write ( * , fmt = fmt_iostat      ) io_stat
                    write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings4 ) myStatus, myAction
                    stop stop_msg
                end if
            end if

            ! write file
            call print_engine_apex_sub ( me, io_unit )

            ! close file
            close ( unit = io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

    end subroutine compare_mm_apex_sub

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 print_engine_apex

    subroutine print_engine_apex_sub ( me, io_unit )

        class ( comparison ), target          :: me
        integer ( ip ), intent ( in )         :: io_unit
        type ( angle ), pointer               :: p_f, p_m, p_d

            write ( io_unit , '( /, "Validation exercise for apex angles, case ", g0 )' ) me % index

            p_f => me % apex_angle_fortran
            p_m => me % apex_angle_mathematica
            p_d => me % apex_angle_difference

                p_d %  th = p_f %  th - p_m %  th
                p_d % dth = p_f % dth - p_m % dth

                write ( io_unit, fmt_fortran )     'apex angle,', p_f % th, p_f % dth
                write ( io_unit, fmt_mathematica ) 'apex angle,', p_m % th, p_m % dth
                write ( io_unit, fmt_difference )  p_d % th, p_d % dth

            p_d => null ( )
            p_f => null ( )
            p_m => null ( )

            return

  end subroutine print_engine_apex_sub

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  compare_mm_lines

    subroutine compare_mm_lines_sub ( me, result_fortran, index )

        class ( comparison ), target          :: me

        type ( lsq_fit ), intent ( in )       :: result_fortran

        integer ( ip ),   intent ( in )       :: index
        integer ( ip )                        :: values ( 1 : 8 ) = 0

        type ( lsq_fit ), pointer             :: p_f, p_m, p_d

        logical                               :: file_exists = .false.

        character ( len = 512 )               :: myStatus    = 'old'
        character ( len = * ), parameter      :: myFile      = trim ( path_data // 'validation lines.txt' )
        character ( len = * ), parameter      :: myAction    = 'write'
        character ( len = * ), parameter      :: myPosition  = 'append'
        character ( len = * ), parameter      :: myRoutine   = 'compare_mm_lines_sub'
        character ( len = * ), parameter      :: stop_msg    = 'Execution ending due to error in ' // me_mCompareToMM // &
                                                                                              ', ' // myRoutine // '.'

            me % index = index
            p_f => me % result_fortran
            p_m => me % result_mathematica
            p_d => me % result_difference
                p_f = result_fortran
                p_m = mm_lines ( index )

                p_d % intercept      = p_f % intercept     - p_m % intercept
                p_d % gap            = p_f % gap           - p_m % gap
                p_d % slope          = p_f % slope         - p_m % slope

                p_d % err_intercept  = p_f % err_intercept - p_m % err_intercept
                p_d % err_gap        = p_f % err_gap       - p_m % err_gap
                p_d % err_slope      = p_f % err_slope     - p_m % err_slope
            p_d => null ( )
            p_f => null ( )
            p_m => null ( )

            !call print_comparison_lines_sub ( me, stdout )

            ! open file for writing
            ! https://stackoverflow.com/questions/15526203/single-command-to-open-a-file-or-create-it-and-the-append-data
            inquire ( file = myFile, exist = file_exists )

            if ( file_exists ) then
                myStatus = "old"
                open ( newunit = io_unit, file = myFile, status = myStatus, action = myAction, position = myPosition )
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror     ) 'OPEN', trim ( myFile ), io_unit
                    write ( * , fmt = fmt_iostat      ) io_stat
                    write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings3 ) myStatus, myAction, myPosition
                    stop stop_msg
                end if
                ! time and date stamp
                call date_and_time ( VALUES = values )
                write  ( io_unit, '( I5, 2 ( "-", I2.2 ) )', advance = 'no', iostat = io_stat, iomsg = io_msg ) values ( 1 : 3 )
                write  ( io_unit, '( I3, 2 ( ":", I2.2 ), / )',              iostat = io_stat, iomsg = io_msg ) values ( 5 : 7 )
            else
                myStatus = "new"
                open ( newunit = io_unit, file = myFile, status = myStatus, action = myAction )
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror     ) 'OPEN', trim ( myFile ), io_unit
                    write ( * , fmt = fmt_iostat      ) io_stat
                    write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings4 ) myStatus, myAction
                    stop stop_msg
                end if
            end if

            ! write file
            call print_comparison_lines_sub ( me, io_unit )

            ! close file
            close ( unit = io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', trim ( myFile ), io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

    end subroutine compare_mm_lines_sub

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                print_engine_lines

    subroutine print_engine_lines_sub ( me, io_unit )

        class ( comparison ), target          :: me
        integer ( ip ), intent ( in )         :: io_unit
        real ( rp )                           :: a = zero, b = zero, c = zero, d = zero, diff1 = zero, diff2 = zero
        type ( lsq_fit ), pointer             :: p_f, p_m, p_d

            write ( io_unit , '( /, "Validation exercise for lines, case ", g0 )' ) me % index

            p_f => me % result_fortran
            p_m => me % result_mathematica
            p_d => me % result_difference

                a = p_f %     intercept
                b = p_f % err_intercept
                c = p_m %     intercept
                d = p_m % err_intercept
                diff1 = a - c
                diff2 = b - d

                write ( io_unit, fmt_fortran )     'intercept,', a, b
                write ( io_unit, fmt_mathematica ) 'intercept,', c, d
                write ( io_unit, fmt_difference )   diff1, diff2

                a = p_f %     gap
                b = p_f % err_gap
                c = p_m %     gap
                d = p_m % err_gap
                diff1 = a - c
                diff2 = b - d

                write ( io_unit, fmt_fortran )     'gap,      ', a, b
                write ( io_unit, fmt_mathematica ) 'gap,      ', c, d
                write ( io_unit, fmt_difference )   diff1, diff2

                a = p_f %     slope
                b = p_f % err_slope
                c = p_m %     slope
                d = p_m % err_slope
                diff1 = a - c
                diff2 = b - d

                write ( io_unit, fmt_fortran )     'slope,    ', a, b
                write ( io_unit, fmt_mathematica ) 'slope,    ', c, d
                write ( io_unit, fmt_difference )   diff1, diff2

            p_d => null ( )
            p_f => null ( )
            p_m => null ( )

            return

    end subroutine print_engine_lines_sub

end module mCompareToMM
