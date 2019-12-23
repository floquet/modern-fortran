! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 30

module mResults

    use mPrecisionDefinitions, only : ip, rp, one
    use mMeasurements
    use mSolnsLinear
    implicit none

    type                       :: results
        type ( measurements )  :: measurement
        type ( solns_linear )  :: soln_linear ( 1 : 5 )
        integer ( ip )         :: numSolnsLinear
        character ( len = 64 ) :: descriptor_64
    contains
        private
!       subroutines
        procedure, public :: print_measurement   => print_measurement_sub
    end type results

    integer ( ip ), private                   :: kResult = 0
    character ( len = * ), parameter, private :: myModule = 'module mResults'  ! self-identification

    private :: print_measurement_sub

    contains

!       =============================================================================================              print_measurement

        subroutine print_measurement_sub ( me )

            class ( results ), target :: me

!               headers
                write ( * , 100 ) me % descriptor_64
                write ( * , 110 ) me % measurement % descriptor_64

!               print data
                do kResult = 1, me % measurement % m
                    write ( * , 200 ) kResult, me % measurement % x ( kResult ), me % measurement % y ( kResult )
                end do

            return

    100     format ( /, g0 )
    110     format (    g0, / )

    200     format ( I5, '. ', T10, g0, T30, g0 )

        end subroutine print_measurement_sub


end module mResults
