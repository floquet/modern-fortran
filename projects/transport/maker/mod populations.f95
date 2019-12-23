! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mPopulations

    !use precision_definitions, only : is, wp, zero
    use param_simulation
    use mPhotons
    use mRandom
    use mUtilities

    implicit none

    integer, private                  :: kPop!, jPop
    integer, private                  :: alloc_status

    character ( len = 512 ), private  :: alloc_msg
    character ( len = * ), parameter  :: me_module_Pop = 'module mPopulations'  ! self-identification

    type                              :: populations

        integer ( is )                :: numPhotons
        type ( photons ), allocatable :: photon ( : )
            !type ( spectrums ) :: spectrum
        contains                                                                                             ! type bound procedures
!           subroutines
            procedure, public :: allocation              => allocation_sub
            !procedure, public :: create_population       => create_population_sub
!             procedure, public :: export_generic_rank1    => export_generic_rank1_sub
!             procedure, public :: export_generic_anyrank  => export_generic_anyrank_sub
            procedure, public :: print_population_polar  => print_population_polar_sub
            procedure, public :: create_population_polar => create_population_polar_sub

    end type populations

    !type ( populations ), parameter :: population0 = populations ( 0, photon0 )  ! population constructor

    private :: allocation_sub
!     private :: export_generic_rank1_sub
!     private :: export_generic_anyrank_sub
    private :: print_population_polar_sub
    private :: create_population_polar_sub

    contains

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   allocation

        subroutine allocation_sub ( me )

            class ( populations ), target :: me

            character ( len = 512 ), parameter :: me_subroutine = 'subroutine allocation_sub'  ! self-identification
            character ( len = * ),   parameter :: stop_msg = 'Halting on execution error in ' // me_module_Pop // ', ' &
                                                                                              // me_subroutine // '.'

!               allocate and populate
                allocate ( me % photon ( 1 : me % numPhotons ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( * , 100 ) "", "type ( photons )", "photon"
                    write ( * , 110 ) me % numPhotons
                    write ( * , 120 ) alloc_status
                    write ( * , 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

                me % photon ( : ) = photon0

                return

    100         format ( /, "Error ", g0, "allocating memory for ", g0, " array ", g0, "." )
    110         format (    "  requested size is ", g0, " elements" )
    120         format (    "  stat = ", g0 )
    130         format (    "  errmsg = ", g0, "." )

        end subroutine allocation_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      create_population_polar

        subroutine create_population_polar_sub ( me, numPhotons, r_inner, r_outer )

            class ( populations ), target :: me

            integer ( is ), intent ( in ) :: numPhotons
            real ( wp ), intent ( in )    :: r_inner, r_outer

                !write ( * , 110 ) numPhotons, r_inner, r_outer
                me % numPhotons = numPhotons
                call me % allocation ( )

                !print *, 'random_distribution_annulus_sub ( numPhotons, r_inner, r_outer ) ', r_outer
                call random_distribution_annulus_sub ( numPhotons, r_inner, r_outer )
!                 if ( allocated ( randomAnnularList ) ) then
!                     call export_generic_anyrank_sub ( 'randomAnnularList', rank2 = randomAnnularList )
!                     !print *, 'randomAnnularList exported'
!                 end if
                do concurrent ( kPop = 1 : numPhotons )
                    me % photon ( kPop ) % loc ( : ) = randomAnnularList ( kPop, : )
                end do

                call random_vector_of_points_sub ( numPhotons )
                do concurrent ( kPop = 1 : numPhotons )
                    me % photon ( kPop ) % dir ( : ) = randomList ( kPop, : )
                end do

                return

  !100           format ( I9, '. setting location = ', 2( 2X, F8.3 ) )
  !110           format ( /, 'attempting to create ', g0, ' photons on the range ', F8.3, ' to ', F8.3 )

        end subroutine create_population_polar_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Print                                                                                   #
!       #                                                                                          #
!       ############################################################################################

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       print_population_polar

        subroutine print_population_polar_sub ( me )

            class ( populations ), target :: me
            type ( photons ), pointer     :: g

                do kPop = 1, me % numPhotons
                    g => me % photon ( kPop )
                        write ( * , 100 ) kPop, g % loc ( : ), g % dir ( : )
                    g => null ( )
                end do

                return

  100           format ( I9, '. location = ', 2( 2X, F8.3 ), '; direction = ', 2( 2X, F8.3 ) )

        end subroutine print_population_polar_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Export                                                                                  #
!       #                                                                                          #
!       ############################################################################################

end module mPopulations
