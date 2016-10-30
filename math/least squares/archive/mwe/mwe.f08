! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 12 01

include 'myIncludes.f08'

program mwe

!     use mQueries
!     use mMeasurements
!     use mSolnsLinear
    use mValidate

    implicit none

    type ( comparison ) :: myCompare

    character ( len = * ), parameter :: myProgram = 'program mwe'  ! self-identification

        call myCompare % validate_bevington_6_1 ( myCompare )

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

end program mwe