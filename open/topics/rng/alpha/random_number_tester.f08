program random_number_tester

    use mSetPrecision,  only : ip
    use mRandoms,       only : random_integer_fcn, init_random_seed_sub

    implicit none

    integer ( ip ) :: myRandomInteger = 0, UpperBound = 10
    !integer        :: mySeed ( : )

    ! interface
    !     subroutine init_random_seed_sub
    !         integer, intent ( IN ),  optional :: SeedIn  ( : )
    !         integer, intent ( OUT ), optional :: SeedOut ( : )
    !         logical, intent ( IN ),  optional :: FlagCheckOS
    !         integer, allocatable  :: seed ( : )
    !         integer        :: array_size = 0, io_urandom = 0, io_stat = 0, pid = 0!, nBytes = 0
    !         integer ( ip ) :: myCount = 0, input = 0
    !         character ( len = 512 )          :: io_msg ! for now this message is not reported
    !         character ( len = * ), parameter :: me_subroutine = 'subroutine init_random_seed_sub'  ! self-identification
    !         character ( len = * ), parameter :: stop_msg = 'Fatal error: ' // me_module // ', ' // me_subroutine
    !     end subroutine init_random_seed_sub
    ! end interface

        myRandomInteger = random_integer_fcn ( UpperBound )
        print *, 'myRandomInteger = ', myRandomInteger, '; upper bound = ', UpperBound

        call init_random_seed_sub ( FlagCheckOS = .true. )!, SeedOut = mySeed )
        !print *, 'rng seeded with ', mySeed

        stop 'successful completion for random_number_tester...'

end program random_number_tester
