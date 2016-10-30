! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mPhotons

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero
    use mSimulationParameters,  only : numDims

    implicit none

    type               :: photons
        integer ( ip ) :: zoneRadialIndex
        integer ( ip ) :: zoneLimbicIndex
        real ( rp )    :: loc ( 1 : numDims )
        real ( rp )    :: dir ( 1 : numDims )
        real ( rp )    :: energy
        real ( rp )    :: probability
        real ( rp )    :: travel
    end type photons

    type ( photons ), parameter :: photon0 = photons ( 0, 0, zero, zero, zero, zero, zero )  ! photon constructor

end module mPhotons
