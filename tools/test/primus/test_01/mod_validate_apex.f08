module mValidateApex

    use mPrecisionDefinitions,         only : ip, rp
    use mConstants,                    only : zero, one, stdout, fmt_fortran, fmt_mathematica, fmt_difference, machine_epsilon

    use mAngle,                        only : angle
    use mMathematicaOutput,            only : mm_angles
    use mParametersSimulation,         only : path_data

    implicit none

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

    !   +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  validate_apex

    subroutine validate_apex_sub ( me, apex_angle_fortran, index, my_io_unit )

        class ( comparison_apex ), target     :: me

        integer ( ip ), intent ( in )         :: index, my_io_unit
        type ( angle ), intent ( in )         :: apex_angle_fortran
        integer ( ip )                        :: values ( 1 : 8 ) = 0

        type ( angle ), pointer               :: p_f => null ( ), p_m => null ( ), p_d => null ( )

        !character ( len = * ), parameter      :: myFile = trim ( path_data // 'validation apex.txt' )

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

            !my_io_unit = safeopen_writereplace ( filename = myFile )

            call print_engine_apex_sub ( me, my_io_unit ) ! write file
            ! time and date stamp
            call date_and_time ( VALUES = values )
            write  ( my_io_unit, '( I5, 2 ( "-", I2.2 ) )', advance = 'no' ) values ( 1 : 3 )
            write  ( my_io_unit, '( I3, 2 ( ":", I2.2 ), / )' )              values ( 5 : 7 )

            !close ( unit = my_io_unit )

    end subroutine validate_apex_sub

    !   +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +    print_engine_apex

    subroutine print_engine_apex_sub ( me, io_unit )

        class ( comparison_apex ), target   :: me
        integer ( ip ), intent ( in )       :: io_unit
        type ( angle ), pointer             :: p_f => null ( ), p_m => null ( ), p_d => null ( )

            write ( io_unit , '( /, "Validation exercise for apex angles, case ", g0 )' ) me % index

            p_f => me % apex_angle_fortran
            p_m => me % apex_angle_mathematica
            p_d => me % apex_angle_difference

                p_d %  th = p_f %  th - p_m %  th
                p_d % dth = p_f % dth - p_m % dth

                write ( io_unit, fmt_fortran )     'apex angle,', p_f % th, p_f % dth
                write ( io_unit, fmt_mathematica ) 'apex angle,', p_m % th, p_m % dth
                write ( io_unit, fmt_difference )   p_d % th, p_d % dth

            p_d => null ( )
            p_f => null ( )
            p_m => null ( )

    end subroutine print_engine_apex_sub

end module mValidateApex
