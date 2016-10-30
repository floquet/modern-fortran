! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mPhotons

    use precision_definitions, only : wp, is
    use param_simulation

    implicit none

    type               :: photons
        integer ( is ) :: zoneRadialIndex
        integer ( is ) :: zoneLimbicIndex
        real ( wp )    :: loc ( 1 : numDims )
        real ( wp )    :: dir ( 1 : numDims )
        real ( wp )    :: energy
        real ( wp )    :: probability
        real ( wp )    :: travel
    end type photons

    type ( photons ), parameter :: photon0 = photons ( 0, 0, zero, zero, zero, zero, zero )  ! photon constructor

end module mPhotons
