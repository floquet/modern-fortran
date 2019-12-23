module mValidateApex

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

    character ( len = * ), parameter, private  :: me_mValidateApex = 'module mValidateApex'  ! self-identification

    type :: comparison_apex
        integer ( ip )      :: index = 0
        type ( angle )      :: apex_angle_fortran     = angle ( th = zero, dth = zero ), &
                               apex_angle_mathematica = angle ( th = zero, dth = zero ), &
                               apex_angle_difference  = angle ( th = zero, dth = zero )
    contains
        private
        procedure, public   :: validate_apex        => validate_apex_sub
        procedure, public   :: print_engine_apex    => print_engine_apex_sub
    end type comparison_apex

    private :: validate_apex_sub
    private :: print_engine_apex_sub

contains

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     validate_apex

    subroutine validate_apex_sub ( me, apex_angle_fortran, index )

        class ( comparison_apex ), target     :: me

        integer ( ip ), intent ( in )         :: index
        type ( angle ), intent ( in )         :: apex_angle_fortran
        integer ( ip )                        :: values ( 1 : 8 ) = 0, my_io_unit = 0

        type ( angle ), pointer               :: p_f => null ( ), p_m => null ( ), p_d => null ( )

        !character ( len = 512 )               :: myStatus    = 'old'
        character ( len = * ), parameter      :: myFile      = trim ( path_data // 'validation apex.txt' )
        !character ( len = * ), parameter      :: myAction    = 'write'
        !character ( len = * ), parameter      :: myPosition  = 'rewind'!'append'
        character ( len = * ), parameter      :: myRoutine   = 'validate_apex_sub'
        character ( len = * ), parameter      :: stop_msg    = 'Execution ending due to error in ' // me_mValidateApex // &
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
            call open_file_output ( myFile, my_io_unit, stop_msg )

            ! write file
            call print_engine_apex_sub ( me, my_io_unit )
            ! time and date stamp
            call date_and_time ( VALUES = values )
            write  ( my_io_unit, '( I5, 2 ( "-", I2.2 ) )', advance = 'no', iostat = io_stat, iomsg = io_msg ) values ( 1 : 3 )
            write  ( my_io_unit, '( I3, 2 ( ":", I2.2 ), / )',              iostat = io_stat, iomsg = io_msg ) values ( 5 : 7 )

            ! close file
            close ( unit = my_io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', trim ( myFile ), my_io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

    end subroutine validate_apex_sub

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 print_engine_apex

    subroutine print_engine_apex_sub ( me, io_unit )

        class ( comparison_apex ), target     :: me
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


end module mValidateApex
