! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mSimulations

    use precision_definitions, only : is, wp, zero, one

    !use mTrajectories
    use mCapsules
    use param_simulation
    use mPopulations
    use mUtilities

    implicit none

    integer, private             :: jSim = 0, kSim = 0

    type                         :: simulations
        integer                  :: numCapsules = 0
        type ( capsules )        :: myCapsules ( 1 : nMaxCapsules )! = capsule0

        contains                                                                                             ! type bound procedures
!           subroutines
            procedure, public    :: go               => go_sub
            procedure, public    :: exporter         => exporter_sub
            !procedure, public    :: read_name_list   => read_name_list_sub
            procedure, public    :: print_population => print_population_sub

    end type simulations

    private :: go_sub
    private :: exporter_sub
    !private :: read_name_list_sub
    private :: print_population_sub

    contains                                                                                    ! methods: subroutines and functions

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                           go

        subroutine go_sub ( me, fPrintLoad, fPrintPopulate, fExportPhotons )

            class ( simulations ), target    :: me

            logical, optional, intent ( in ) :: fPrintLoad, fPrintPopulate, fExportPhotons
            type ( capsules ), pointer       :: cap

!               load zone structure
                jSim = 1
!                 call me % myCapsules ( jSim ) % load_capsule_structure ( bound_radial = [ 0.5_wp, 0.75_wp, one ], &
!                    mfp = one * [ 1, 2, 3 ], origin = [ zero, zero ] )
!                 jSim = jSim + 1
!                 call me % myCapsules ( jSim ) % load_capsule_structure ( thick_radial = [ 0.5_wp, 0.25_wp, 0.25_wp ], &
!                    mfp = one * [ 3, 2, 1 ], origin = [ one, one ] )
!                 jSim = jSim + 1
                call me % myCapsules ( jSim ) % load_capsule_structure ( bound_radial = one * [ ( kSim, kSim = 1, 10 ) ], &
                   mfp = one * [ ( 2 * kSim, kSim = 1, 10 ) ], origin = [ one, one ] * 2 )
                me % numCapsules = jSim
!                 call me % myCapsules ( 2 ) % load_capsule ( bound_radial = [ 0.25_wp, 0.33_wp, 0.67_wp, 0.75_wp, one ], &
!                                                                      mfp = [ one, 0.01_wp, 3.0_wp, 10.0_wp, one ] )

!               print capsule structure
                if ( present ( fPrintLoad ) ) then
                    if ( fPrintLoad ) then
                        do kSim = 1, me % numCapsules
                            cap => me % myCapsules ( kSim )
                                call cap % print_capsule_structure ( kSim )
                            cap => null ( )
                        end do
                    end if
                endif

!               populate capsules
                do kSim = 1, me % numCapsules
                    call me % myCapsules ( kSim ) % load_capsule_population ( kSim * 10000 )
                    call me % myCapsules ( kSim ) % determine_radial_zone ( )
                end do

!               print population data to screen
                if ( present ( fPrintPopulate ) ) then
!                     jSim = 1
!                     call export_generic_anyrank_sub ( 'population locs', rank1 = &
!                         [ ( me % myCapsules ( jSim ) % population % photon ( kSim ) % loc ( : ), &
!                           kSim = 1, me % myCapsules ( jSim ) % population % numPhotons ) ] )
                    !if ( fPrintPopulate ) call me % myCapsules ( jSim ) % print_capsule_population_details ( jSim )
                    if ( fPrintPopulate ) call me % myCapsules ( jSim ) % print_capsule_density ( jSim )
                endif

!                export to binary files
                if ( present ( fExportPhotons ) ) then
                    if ( fExportPhotons ) call exporter_sub ( me )
                endif

                return

        end subroutine go_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     exporter

        subroutine print_population_sub ( me )

            class ( simulations ), target :: me

            type ( capsules ), pointer    :: cap

                do kSim = 1, me % numCapsules
                    cap => me % myCapsules ( kSim )
                        !call cap % print_capsule_population ( kSim )
                        call cap % print_capsule_density ( kSim )
                    cap => null ( )
                end do

        end subroutine print_population_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     exporter

        subroutine exporter_sub ( me )

            class ( simulations ), target :: me

            type ( populations ), pointer :: pop
            character ( len = 512 )       :: myDescriptor  ! file descriptors

                do kSim = 1, me % numCapsules
                    pop => me % myCapsules ( kSim ) % population

                        write ( myDescriptor, 100 ) 'capsule', kSim, ' locs', '.r64'
                        call export_generic_anyrank_sub ( myDescriptor, &
                          rank1 = [ ( pop % photon ( kSim ) % loc ( : ), kSim = 1, pop % numPhotons ) ] )

                        write ( myDescriptor, 100 ) 'capsule', kSim, ' dirs', '.r64'
                        call export_generic_anyrank_sub ( myDescriptor, &
                          rank1 = [ ( pop % photon ( kSim ) % dir ( : ), kSim = 1, pop % numPhotons ) ] )

                    pop => null ( )
                end do

                return

  100           format ( g0, I3, g0, g0 )

        end subroutine exporter_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++               read_name_list
!
!         subroutine read_name_list_sub ( me )
!
!             class ( simulations ), target :: me
!
!                 return
!
!         end subroutine read_name_list_sub

end module mSimulations
