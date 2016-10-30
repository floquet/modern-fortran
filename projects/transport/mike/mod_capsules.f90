! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mCapsules

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero

    use mPhotons,               only : photons
    use mPopulations,           only : populations
    use mSimulationParameters,  only : numDims, nMaxZonesRadial
    use mZones,                 only : zonesRadial

    implicit none

    integer, private :: kCap = 0

    character ( len = * ), parameter, private :: me_module_Cap = 'module mCapsules'  ! self-identification

    type                      :: capsules
        integer ( ip )        :: numZonesRadial = 0
        type ( zonesRadial )  :: myZonesRadial ( 1 : nMaxZonesRadial )
        real ( rp )           :: radius = zero
        real ( rp )           :: area = zero
        !type ( zones )        :: limbicZones ( 1 : nMaxZonesLimbic )
        type ( populations )  :: population

        real ( rp )           :: origin ( 1 : numDims ) = zero

        contains                                                                                             ! type bound procedures
            ! functions
            ! subroutines
            procedure, public :: capsule_area             => capsule_area_sub
            procedure, public :: determine_radial_zone    => determine_radial_zone_sub

            procedure, public :: load_capsule_structure           => load_capsule_structure_sub
            procedure, public :: load_capsule_population          => load_capsule_population_sub

            procedure, public :: print_capsule_structure          => print_capsule_structure_sub
            procedure, public :: print_capsule_population         => print_capsule_population_sub
            procedure, public :: print_capsule_density            => print_capsule_density_sub
            procedure, public :: print_capsule_population_details => print_capsule_population_details_sub

    end type capsules

    !type ( capsules ), parameter :: capsule0 = capsules ( 0, zone0, population0, zero )  ! capsule constructor

    !private :: locate_population_radial
    private :: capsule_area_sub
    private :: print_capsule_density_sub
    private :: determine_radial_zone_sub
    private :: print_capsule_structure_sub
    private :: print_capsule_population_sub
    private :: print_capsule_population_details_sub
    private :: load_capsule_structure_sub
    private :: load_capsule_population_sub

    interface capsule_load

        module subroutine load_capsule_structure_sub ( me, bound_radial, thick_radial, mfp, origin )
            !import mPrecisionDefinitions,  only : rp
            class ( capsules ), target              :: me
            real ( rp ),    intent ( in ), optional :: bound_radial ( : )
            real ( rp ),    intent ( in ), optional :: thick_radial ( : )
            real ( rp ),    intent ( in ), optional :: origin       ( : )
            real ( rp ),    intent ( in ), optional :: mfp          ( : )
        end subroutine load_capsule_structure_sub

        module subroutine load_capsule_population_sub ( me, numPhotons )
            class ( capsules ), target    :: me
            integer ( ip ), intent ( in ) :: numPhotons
            real ( rp )                   :: r_inner, r_outer
        end subroutine load_capsule_population_sub

    end interface capsule_load

    contains                                                                                    ! methods: subroutines and functions

!       ############################################################################################
!       #                                                                                          #
!       #  Computation                                                                             #
!       #                                                                                          #
!       ############################################################################################

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   capsule_area

        subroutine capsule_area_sub ( me )

            use mConstants,   only : pi

            class ( capsules ), target  :: me

                me % area = pi * ( me % radius )**2

        end subroutine capsule_area_sub

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   determine_radial_zone

        subroutine determine_radial_zone_sub ( me )

            class ( capsules ), target      :: me
            integer ( ip )                  :: k

            real ( rp )                     :: nrm
            type ( populations ), pointer   :: pop => null ( )

                ! assign radial zones
                do kCap = 1, me % population % numPhotons
                    nrm = norm2 ( me % population % photon ( kCap ) % loc ( : ) )

                    assign_zone : do k = 1, me % numZonesRadial
                        if ( nrm <= me % myZonesRadial ( k ) % radius_outer ) then
                            me % population % photon ( kCap  ) % zoneRadialIndex = k
                            pop => me % myZonesRadial ( k ) % population
                                pop % numPhotons = pop % numPhotons + 1
                            pop => null ( )
                            exit assign_zone
                        end if
                    end do assign_zone

                end do

                return

        end subroutine determine_radial_zone_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Print                                                                                   #
!       #                                                                                          #
!       ############################################################################################

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +    print_capsule_structure

        subroutine print_capsule_structure_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( ip ), intent ( in ) :: capsuleID
            character ( len = 64 )        :: whoami

                write ( * , 100 ) capsuleID, me % numZonesRadial
                do kCap = 1, me % numZonesRadial
                    write ( whoami, '( "radial zone", I3 )' ) kCap
                    call me % myZonesRadial ( kCap ) % print_zone ( trim ( whoami ) )
                end do

                return

            100 format ( /, 'Capsule ', g0, ' has ', g0, ' radial zones:' )

        end subroutine print_capsule_structure_sub

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   print_capsule_population

        subroutine print_capsule_population_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( ip ), intent ( in ) :: capsuleID

                write ( * , 100 ) capsuleID, me % population % numPhotons / 1000000
                call me % population % print_population_polar ( )

                return

            100 format ( /, 'Capsule ', g0, ' has ', g0, 'M photons:' )

        end subroutine print_capsule_population_sub

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +    print_capsule_details_population

        subroutine print_capsule_population_details_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( ip ), intent ( in ) :: capsuleID

            type ( photons ), pointer     :: g => null ( )
            integer ( ip )                :: nPhotons = 0
            real ( rp )                   :: pnt ( 1 : numDims ) = zero
            real ( rp )                   :: nrm = zero

                nPhotons = me % population % numPhotons
                write ( * , 100 ) capsuleID, nPhotons

                do kCap = 1, nPhotons
                    g => me % population % photon ( kCap )
                        pnt = g % loc ( : )
                        nrm = norm2 ( pnt )
                        write ( * , 110 ) kCap, pnt, nrm, g % zoneRadialIndex
                    g => null ( )
                end do

                return

            100 format ( /, 'Capsule ', g0, ' has ', g0, ' photons:' )
            110 format ( I5, '. loc = ', 2( 2X, F8.3 ), ', norm2 = ', g0, ', zone = ', g0, '.' )

        end subroutine print_capsule_population_details_sub

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   print_capsule_density

        subroutine print_capsule_density_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( ip ), intent ( in ) :: capsuleID
            integer ( ip )                :: cap_count, zone_count, zone_total
            type ( zonesRadial ), pointer :: zone => null ( )
            character ( len = 1 )         :: s

                cap_count = me % population % numPhotons
                write ( * , 100 ) capsuleID, cap_count / 1000000, cap_count / me % area
                zone_total = 0
                do kCap = 1, me % numZonesRadial
                    zone => me % myZonesRadial ( kCap )
                        call zone % annulus_area ( )
                        zone_count = zone % population % numPhotons
                        s = 's'  ! plural
                        if ( zone_count == 1 ) s = '' ! singular
                        zone_total = zone_total + zone_count
                        write ( * , 110 ) kCap, zone_count, s, zone_count / zone % area
                    zone => null ( )
                end do
                write ( * , 120 ) zone_total / 1000000, cap_count - zone_total

                return

            100 format ( /, 'Capsule      ', g0, ' has ', g0, 'M photons (', E9.3, ' photons per unit area).' )
            110 format (    'Radial zone', I3, ' has ', g0, ' photon', g0,' (', E9.3, ' photons per unit area).' )
            120 format (    'Total photons in all zones = ', g0, 'M; discrepancy = ', g0, '.' )

        end subroutine print_capsule_density_sub

    end module mCapsules
