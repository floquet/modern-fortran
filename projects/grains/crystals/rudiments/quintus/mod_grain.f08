! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mGrain

    use mPrecisionDefinitions, only : ip, rp, pi, rad_to_deg
    use mAxes
    use mAngle

    implicit none

    integer ( ip ), private                     :: kGrain = 0
    character ( len = * ), parameter, private   :: me_mAxes = 'module mGrain'  ! self-identification

    type :: grain
        type ( axis )  :: axes       ( 1 : 3 )
        type ( angle ) :: apex_angle ( 1 : 3 )
        real ( rp )    :: theta_line ( 1 : 3 ), err_theta_line ( 1 : 3 )
    contains
        private
        procedure, nopass, public   :: diff_angle           => diff_angle_fcn
        procedure, public           :: load_grain           => load_grain_sub
        procedure, public           :: compute_angles       => compute_angles_sub
        procedure, public           :: print_apex_angles    => print_apex_angles_sub
        procedure, public           :: compute_apex_angles  => compute_apex_angles_sub
    end type grain

    private :: diff_angle_fcn
    private :: load_grain_sub
    private :: compute_angles_sub
    private :: print_apex_angles_sub
    private :: compute_apex_angles_sub

contains

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++               compute_apex_angles

    subroutine compute_apex_angles_sub ( me, axisa, axisb, axisc )

        class ( grain ), target                 :: me

        type ( axis ), intent ( in )            :: axisa, axisb, axisc

            call load_grain_sub ( me, axisa, axisb, axisc )
            call compute_angles_sub ( me )
            call print_apex_angles_sub ( me )

    end subroutine compute_apex_angles_sub

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 print_apex_angles

    subroutine print_apex_angles_sub ( me )

        class ( grain ), target                 :: me

            write ( * , '( /, "Apex angles (ideal answer = 60 deg):" )' )
            do kGrain = 1, 3 ! convert the slopes into angles
                write ( * , 100 ) kGrain, me % apex_angle ( kGrain ) % th, &
                                          me % apex_angle ( kGrain ) % dth, &
                                          me % apex_angle ( kGrain ) % th  * rad_to_deg, &
                                          me % apex_angle ( kGrain ) % dth * rad_to_deg
            end do

            return

    100     format ( I3, 2X, F10.5, ' +/- ', F10.5, 5X, '(', F10.5, ' +/- ', F10.5,' ) degrees')

    end subroutine print_apex_angles_sub

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                    compute_angles

    subroutine compute_angles_sub ( me )

        class ( grain ), target                 :: me

            do kGrain = 1, 3 ! convert the slopes into angles
                me %     theta_line ( kGrain ) = atan ( me % axes ( kGrain ) % results %     slope )
                me % err_theta_line ( kGrain ) = atan ( me % axes ( kGrain ) % results % err_slope ) &
                                               / ( 1 + me % theta_line ( kGrain )**2 )
            end do

            ! sweep pairs of rows to find apex angles: rows 1,2; 1,3; 2,3
            me % apex_angle ( 1 ) % th = diff_angle_fcn ( me % theta_line ( 1 ), me % theta_line ( 2 ) )
            me % apex_angle ( 2 ) % th = diff_angle_fcn ( me % theta_line ( 1 ), me % theta_line ( 3 ) )
            me % apex_angle ( 3 ) % th = diff_angle_fcn ( me % theta_line ( 2 ), me % theta_line ( 3 ) )

            me % apex_angle ( 1 ) % dth = hypot ( me % err_theta_line ( 1 ), me % err_theta_line ( 2 ) )
            me % apex_angle ( 2 ) % dth = hypot ( me % err_theta_line ( 1 ), me % err_theta_line ( 3 ) )
            me % apex_angle ( 3 ) % dth = hypot ( me % err_theta_line ( 2 ), me % err_theta_line ( 3 ) )

    end subroutine compute_angles_sub

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                        diff_angle

        function diff_angle_fcn ( theta1, theta2 ) result ( diff )

            real ( rp ), intent ( in )  :: theta1, theta2
            real ( rp )                 :: diff

                diff = modulo ( theta1 - theta2, pi / 3 )

        end function diff_angle_fcn

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                    load_grain

    subroutine load_grain_sub ( me, axisa, axisb, axisc )

        class ( grain ), target                 :: me
        type ( axis ), intent ( in )            :: axisa, axisb, axisc

            me % axes ( 1 ) = axisa
            me % axes ( 2 ) = axisb
            me % axes ( 3 ) = axisc

    end subroutine load_grain_sub

end module mGrain
