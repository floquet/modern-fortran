! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mZones

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero, pi
    !use mSimulationParameters,  only : nMaxZonesRadial
    use mThermodynamics
    !use mPhotons
    use mPopulations

    implicit none

    !integer, private :: kZone = 0

    character ( len = * ), parameter, private :: myModule = 'module mZones'  ! self-identification

    type                        :: zonesRadial
        real ( rp )             :: radius_inner = zero
        real ( rp )             :: radius_outer = zero
        real ( rp )             :: area         = zero
        real ( rp )             :: thickness    = zero
        type ( thermodynamics ) :: thermo
        type ( populations )    :: population

        contains                                                                                             ! type bound procedures
            ! functions
            procedure, public :: print_zone   => print_zone_sub
            procedure, public :: annulus_area => annulus_area_sub

    end type zonesRadial

    !type ( zonesRadial ), parameter :: zonesRadial0 = zonesRadial ( zero, zero, zero, thermodynamics0 )

    private :: print_zone_sub
    private :: annulus_area_sub

    contains                                                                                    ! methods: subroutines and functions

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   annulus_area

        subroutine annulus_area_sub ( me )

            class ( zonesRadial ), target :: me

                me % area = pi * ( ( me % radius_outer )**2 - ( me % radius_inner )**2 )

        end subroutine annulus_area_sub

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +     print_zone

        subroutine print_zone_sub ( me, whoami )

            class ( zonesRadial ), target        :: me
            character ( len = * ), intent ( in ) :: whoami

                write ( * , 100 ) whoami, me % radius_inner, me % radius_outer, me % thickness, me % thermo % intensive % mfp

                return

            100 format (  g0, ': ', f6.3, ' <= r < ', f6.3, '; thickness = ', f6.3, ', mean free path = ', f6.3, '.' )

        end subroutine print_zone_sub

end module mZones
