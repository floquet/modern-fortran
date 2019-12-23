! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mThermodynamics

    use precision_definitions, only : wp, zero

    implicit none

    type                       :: intensives
        real ( wp )            :: mfp
        real ( wp )            :: density
        real ( wp )            :: pressure
        real ( wp )            :: temperature
        character ( len = 64 ) :: mfp_units
        character ( len = 64 ) :: density_units
        character ( len = 64 ) :: pressure_units
        character ( len = 64 ) :: temperature_units
    end type intensives

    type                       :: extensives
        real ( wp )            :: mass
        real ( wp )            :: energy
        real ( wp )            :: volume
        real ( wp )            :: entropy
        character ( len = 64 ) :: mass_units
        character ( len = 64 ) :: energy_units
        character ( len = 64 ) :: volume_units
        character ( len = 64 ) :: entropy_units
    end type extensives

    type                    :: thermodynamics
        type ( intensives ) :: intensive
        type ( extensives ) :: extensive
    contains
        private
        procedure, public :: print_intensives     => print_intensives_sub
        procedure, public :: print_extensives     => print_extensives_sub
        procedure, public :: print_thermodynamics => print_thermodynamics_sub
    end type thermodynamics

    type ( intensives ),     parameter :: intensives0 = intensives ( zero, zero, zero, zero, " ", " ", " ", " " )  ! constructor
    type ( extensives ),     parameter :: extensives0 = extensives ( zero, zero, zero, zero, " ", " ", " ", " " )  ! constructor
    type ( thermodynamics ), parameter :: thermodynamics0 = thermodynamics ( intensives0, extensives0 )  ! constructor

    private :: print_intensives_sub
    private :: print_extensives_sub
    private :: print_thermodynamics_sub

    contains                                                                                    ! methods: subroutines and functions

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++         print_intensives_sub

        subroutine print_intensives_sub ( me )

            class ( thermodynamics ), target :: me
            class ( intensives ), pointer    :: int

                int => me % intensive
                    write ( * , 100 ) int % mfp,         int % mfp_units, &
                                      int % density,     int % density_units, &
                                      int % pressure,    int % pressure_units , &
                                      int % temperature, int % temperature_units
                int => null ( )

                return

  100           format ( 'mean free path = ', g0, ' ', g0, ', ', &
                         'density        = ', g0, ' ', g0, ', ', &
                         'pressure       = ', g0, ' ', g0, ', ', &
                         'temperature    = ', g0, ' ', g0, '.'  )

        end subroutine print_intensives_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++         print_extensives_sub

        subroutine print_extensives_sub ( me )

            class ( thermodynamics ), target :: me
            class ( extensives ), pointer    :: ext

                ext => me % extensive
                    write ( * , 100 ) ext % mass,    ext % mass_units, &
                                      ext % energy,  ext % energy_units, &
                                      ext % volume,  ext % volume_units, &
                                      ext % entropy, ext % entropy_units
                ext => null ( )
                return

  100           format ( 'mass           = ', g0, ' ', g0, ', ', &
                         'energy         = ', g0, ' ', g0, ', ', &
                         'volume         = ', g0, ' ', g0, ', ', &
                         'entropy        = ', g0, ' ', g0, '.'  )

        end subroutine print_extensives_sub

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     print_thermodynamics_sub

        subroutine print_thermodynamics_sub ( me, whoami )

            class ( thermodynamics ), target     :: me
            character ( len = * ), intent ( in ) :: whoami

                write ( * , 100 ) whoami
                call me % print_intensives ( )
                call me % print_extensives ( )

                return

  100   format ( 'Thermodynamic properties for ', g0, ':' )

        end subroutine print_thermodynamics_sub

end module mThermodynamics
