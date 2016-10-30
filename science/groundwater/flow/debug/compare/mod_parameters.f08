module mParameters  ! shared variables
                    ! module initializations in declarations happen only once ( first use )

    use mPrecisionDefinitions,  only : ip
    
    implicit none

    integer ( ip ), parameter :: nmax = 11

end module mParameters
