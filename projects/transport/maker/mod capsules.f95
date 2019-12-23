! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mCapsules

    use precision_definitions, only : is, wp, zero, pi
    use param_simulation
    use mPopulations
    use mZones
    use mPhotons

    implicit none

    integer, private :: kCap = 0

    character ( len = * ), parameter, private :: me_module_Cap = 'module mCapsules'  ! self-identification

    type                      :: capsules
        integer ( is )        :: numZonesRadial = 0
        type ( zonesRadial )  :: myZonesRadial ( 1 : nMaxZonesRadial ) 
        real ( wp )           :: radius
        real ( wp )           :: area
        !type ( zones )        :: limbicZones ( 1 : nMaxZonesLimbic ) 
        type ( populations )  :: population 

        real ( wp )           :: origin ( 1 : numDims ) = zero

        contains                                                                                             ! type bound procedures
!           functions
!           subroutines
            procedure, public :: capsule_area             => capsule_area_sub
            procedure, public :: determine_radial_zone    => determine_radial_zone_sub
            procedure, public :: load_capsule_structure   => load_capsule_structure_sub
            procedure, public :: load_capsule_population  => load_capsule_population_sub

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
    private :: load_capsule_structure_sub
    private :: load_capsule_population_sub
    private :: print_capsule_structure_sub
    private :: print_capsule_population_sub
    private :: print_capsule_population_details_sub

    contains                                                                                    ! methods: subroutines and functions

!       ############################################################################################
!       #                                                                                          #
!       #  Computation                                                                             #
!       #                                                                                          #
!       ############################################################################################

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 capsule_area

        subroutine capsule_area_sub ( me )

            class ( capsules ), target              :: me

                me % area = pi * ( me % radius )**2

        end subroutine capsule_area_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        determine_radial_zone

        subroutine determine_radial_zone_sub ( me )

            class ( capsules ), target       :: me
            integer ( is )                   :: k

            real ( wp )                      :: nrm
            type ( populations ), pointer        :: pop

!               assign radial zones
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
!       #  Load structures                                                                         #
!       #                                                                                          #
!       ############################################################################################

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       load_capsule_structure

        subroutine load_capsule_structure_sub ( me, bound_radial, thick_radial, mfp, origin )

            class ( capsules ), target              :: me

            real ( wp ),    intent ( in ), optional :: origin       ( : )
            real ( wp ),    intent ( in ), optional :: mfp          ( : )
            real ( wp ),    intent ( in ), optional :: bound_radial ( : )
            real ( wp ),    intent ( in ), optional :: thick_radial ( : )

            integer ( is )                          :: nZones = 0

            type ( zonesRadial ), pointer           :: zone

            character ( len = * ), parameter :: myRoutine = 'subroutine load_capsule_sub'  ! self-identification
            character ( len = * ), parameter :: stop_msg = 'Execution ending due to error in subroutine ' // myRoutine // &
                                                                                             ', module: ' // me_module_Cap

                me % myZonesRadial ( 1 ) % radius_inner = zero

                if ( present ( mfp ) ) then
                    nZones = size ( mfp )
                    if ( nZones > nMaxZonesRadial ) then
                        write ( * , 100 ) 
                        write ( * , 110 ) nZones
                        write ( * , 120 ) nMaxZonesRadial
                        stop stop_msg
                    end if
                    me % numZonesRadial= nZones
                    me % myZonesRadial ( 1 : nZones ) % thermo % intensive % mfp = mfp
                end if

                if ( present ( bound_radial ) ) then
                    nZones = size ( bound_radial )
                    if ( nZones > nMaxZonesRadial ) then
                        write ( * , 100 ) 
                        write ( * , 110 ) nZones
                        write ( * , 120 ) nMaxZonesRadial
                        stop stop_msg
                    end if
                    me % numZonesRadial = nZones
                    me % myZonesRadial ( 1 : nZones ) % radius_outer = bound_radial
                    me % myZonesRadial ( 1 ) % thickness = bound_radial ( 1 ) - zero
                    do concurrent ( kCap = 2 : nZones )
                        zone => me % myZonesRadial ( kCap )
                            zone % radius_inner = me % myZonesRadial ( kCap - 1 ) % radius_outer
                            zone % thickness = bound_radial ( kCap ) - bound_radial ( kCap - 1 )
                        zone => null ( )
                    end do
                    do kCap = 1, nZones
                        call me % myZonesRadial ( kCap ) % annulus_area ( )
                    end do
                    me % radius = me % myZonesRadial ( nZones ) % radius_outer
                    call me % capsule_area ( )
                    return
                end if

                if ( present ( thick_radial ) ) then
                    nZones = size ( thick_radial )
                    if ( nZones > nMaxZonesRadial ) then
                        write ( * , 100 ) 
                        write ( * , 110 ) nZones
                        write ( * , 120 ) nMaxZonesRadial
                        stop stop_msg
                    end if
                    me % numZonesRadial = nZones
                    me % myZonesRadial ( 1 : nZones ) % thickness = thick_radial
                    me % myZonesRadial ( 1 ) % radius_outer = thick_radial ( 1 )
                    do kCap = 2, nZones
                        zone => me % myZonesRadial ( kCap )
                            zone % radius_inner = me % myZonesRadial ( kCap - 1 ) % radius_outer
                            zone % radius_outer = zone % radius_inner + thick_radial ( kCap )
                            zone % area = pi * ( ( zone % radius_outer )**2 -  ( zone % radius_inner )**2 )
                        zone => null ( )
                    end do
                    me % radius = me % myZonesRadial ( nZones ) % radius_outer
                    call me % capsule_area ( )
                    return  ! redundant - included in case the paragraph is moved
                end if

                if ( present ( origin ) ) me % origin = origin

            return

  100       format ( /, 'Data error: ' )
  110       format ( 'You are trying to load a capsule with ', g0, ' zones (nZones)' )
  120       format ( 'A maximum of ', g0, ' zones are allowed (nMaxZonesRadial set in module param_simulation)' )

!   200       format ( 'List of outer radii is not ordered. Outer radius (', g0, ') = ', g0, &
!                                                       ' < outer radius (', g0, ') = ', g0, '.', /,
!                                                       'Complete list = ', 100F8.3 )

        end subroutine load_capsule_structure_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      load_capsule_population

        subroutine load_capsule_population_sub ( me, numPhotons )

            class ( capsules ), target    :: me
            integer ( is ), intent ( in ) :: numPhotons
            real ( wp )                   :: r_inner, r_outer
            !type ( photons ), pointer     :: pPhoton

!               assign locations
                r_inner = zero
                r_outer = me % myZonesRadial ( me % numZonesRadial ) % radius_outer
                call me % population % create_population_polar ( numPhotons, r_inner, r_outer)

                do kCap = 1, me % population % numPhotons
                    me % population % photon ( kCap ) % loc ( : ) = me % origin + randomAnnularList ( kCap, : )
                end do

!               assign directions
                do kCap = 1, me % population % numPhotons
                    me % population % photon ( kCap ) % dir ( : ) = random_direction_fcn ( )
                end do

                return

        end subroutine load_capsule_population_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Print                                                                                   #
!       #                                                                                          #
!       ############################################################################################

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      print_capsule_structure

        subroutine print_capsule_structure_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( is ), intent ( in ) :: capsuleID
            character ( len = 64 )        :: whoami

                write ( * , 100 ) capsuleID, me % numZonesRadial
                do kCap = 1, me % numZonesRadial
                    write ( whoami, '( "radial zone", I3 )' ) kCap
                    call me % myZonesRadial ( kCap ) % print_zone ( trim ( whoami ) )
                end do

                return

  100   format ( /, 'Capsule ', g0, ' has ', g0, ' radial zones:' )

        end subroutine print_capsule_structure_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     print_capsule_population

        subroutine print_capsule_population_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( is ), intent ( in ) :: capsuleID

                write ( * , 100 ) capsuleID, me % population % numPhotons / 1000
                call me % population % print_population_polar ( )

                return

  100           format ( /, 'Capsule ', g0, ' has ', g0, 'K photons:' )

        end subroutine print_capsule_population_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   print_capsule_details_population

        subroutine print_capsule_population_details_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( is ), intent ( in ) :: capsuleID

            type ( photons ), pointer     :: g
            integer ( is )                :: nPhotons
            real ( wp )                   :: pnt ( 1 : numDims )
            real ( wp )                   :: nrm

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

  100           format ( /, 'Capsule ', g0, ' has ', g0, ' photons:' )
  110           format ( I5, '. loc = ', 2( 2X, F8.3 ), ', norm2 = ', g0, ', zone = ', g0, '.' )


        end subroutine print_capsule_population_details_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        print_capsule_density

        subroutine print_capsule_density_sub ( me, capsuleID )

            class ( capsules ), target    :: me
            integer ( is ), intent ( in ) :: capsuleID
            integer ( is )                :: cap_count, zone_count, zone_total
            type ( zonesRadial ), pointer :: zone
            character ( len = 1 )         :: s 

                cap_count = me % population % numPhotons
                write ( * , 100 ) capsuleID, cap_count / 1000, cap_count / me % area
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
                write ( * , 120 ) zone_total / 1000, cap_count - zone_total

                return

  100           format ( /, 'Capsule      ', g0, ' has ', g0, 'K photons (', E9.3, ' photons per unit area).' )
  110           format (    'Radial zone', I3, ' has ', g0, ' photon', g0,' (', E9.3, ' photons per unit area).' )
  120           format (    'Total photons in all zones = ', g0, 'K; discrepancy = ', g0, '.' )

        end subroutine print_capsule_density_sub

    end module mCapsules
