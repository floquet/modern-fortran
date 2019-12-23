!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mSimulationParameters

    use mPrecisionDefinitions,  only : ip, rp
    use mConstants,             only : zero

    implicit none

    integer ( ip ), parameter :: nMaxScatters    = 100
    integer ( ip ), parameter :: nMaxZonesRadial =  10
    integer ( ip ), parameter :: nMaxCapsules    =   5
    integer ( ip ), parameter :: numDims         =   2

    integer ( ip ), private   :: k

    !real ( rp ),    parameter :: origin ( 1 : numDims ) =  [ zero, zero ]
    real ( rp ),    parameter :: origin ( 1 : numDims ) =  [ ( zero, k = 1, numDims ) ]

end module mSimulationParameters
