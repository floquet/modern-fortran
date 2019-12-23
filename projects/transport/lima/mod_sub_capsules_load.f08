submodule ( mCapsules ) smCapsulesLoader

    ! private :: load_capsule_structure_sub
    ! private :: load_capsule_population_sub

    integer :: kkCap = 0

contains

!       ############################################################################################
!       #                                                                                          #
!       #  Load structures                                                                         #
!       #                                                                                          #
!       ############################################################################################

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  load_capsule_structure

        module subroutine load_capsule_structure_sub ( me, bound_radial, thick_radial, mfp, origin )

            class ( capsules ), target              :: me

            real ( rp ),    intent ( in ), optional :: origin       ( : )
            real ( rp ),    intent ( in ), optional :: mfp          ( : )
            real ( rp ),    intent ( in ), optional :: bound_radial ( : )
            real ( rp ),    intent ( in ), optional :: thick_radial ( : )

            integer ( ip )                          :: nZones = 0

            type ( zonesRadial ), pointer           :: zone

            character ( len = * ), parameter :: myRoutine = 'module subroutine load_capsule_sub'  ! self-identification
            character ( len = * ), parameter :: stop_msg = 'Execution ending due to error in module subroutine ' // myRoutine // &
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
                    zone => me % myZonesRadial ( kCap )
                    do concurrent ( kCap = 2 : nZones )
                            zone % radius_inner = me % myZonesRadial ( kCap - 1 ) % radius_outer
                            zone % thickness = bound_radial ( kCap ) - bound_radial ( kCap - 1 )
                    end do
                    zone => null ( )
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

            100 format ( /, 'Data error: ' )
            110 format ( 'You are trying to load a capsule with ', g0, ' zones (nZones)' )
            120 format ( 'A maximum of ', g0, ' zones are allowed (nMaxZonesRadial set in module param_simulation)' )

            !   200       format ( 'List of outer radii is not ordered. Outer radius (', g0, ') = ', g0, &
            !                                                       ' < outer radius (', g0, ') = ', g0, '.', /,
            !                                                       'Complete list = ', 100F8.3 )

        end subroutine load_capsule_structure_sub

!       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  + load_capsule_population

        module subroutine load_capsule_population_sub ( me, numPhotons )

            class ( capsules ), target    :: me
            integer ( ip ), intent ( in ) :: numPhotons
            real ( rp )                   :: r_inner, r_outer
            !type ( photons ), pointer     :: pPhoton

                ! assign locations
                r_inner = zero
                r_outer = me % myZonesRadial ( me % numZonesRadial ) % radius_outer
                call me % population % create_population_polar ( numPhotons, r_inner, r_outer)

                do kkCap = 1, me % population % numPhotons
                    me % population % photon ( kkCap ) % loc ( : ) = me % origin + randomAnnularList ( kkCap, : )
                end do
                !do kCap = 1, me % population % numPhotons
                !    me % population % photon ( kCap ) % loc ( : ) = me % origin + randomAnnularList ( kCap, : )
                !end do

                ! assign directions
                do kCap = 1, me % population % numPhotons
                    me % population % photon ( kCap ) % dir ( : ) = random_direction_fcn ( )
                end do

                return

        end subroutine load_capsule_population_sub

end submodule smCapsulesLoader
