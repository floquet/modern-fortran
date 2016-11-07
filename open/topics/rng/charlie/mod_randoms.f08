! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 11 06

module mRandoms

    use, intrinsic :: iso_fortran_env, only : INT64 ! can't be changed gracefully
    use mSetPrecision,                 only : rp

    implicit NONE

    integer, parameter, private :: ip = INT64 ! large integers in this scope

    integer :: seed_size = 0
    integer, allocatable :: SeedUsed ( : )

    integer,        private          :: k = 0              ! counter for this scope
    integer ( ip ), private          :: alloc_status  = 0  ! error handling
    character ( len = 512 ), private :: alloc_message = '' ! error handling
    character ( len = * ), parameter :: me_module = 'module mRandoms'  ! self-identification

    private :: random_integer_fcn

    procedure, public :: random_integer => random_integer_fcn

contains ! methods: subroutines and functions

    function byte_flipper ( input ) result ( byte_flipped )  ! flip bytes: sequences with small changes become
                                                             !             sequences with large changes
        integer ( ip ), intent ( in ) :: input

        integer ( ip ) :: byte_flipped
        integer        :: nBytes

            nBytes = bit_size ( input ) / 8 ! count bytes
            do k = 0, nBytes - 1  !  reverse byte order: move LSB in myCount to MSB in byte_flipped
                call mvbits ( FROM = input, FROMPOS = k * 8, LEN = 8, TO = byte_flipped, TOPOS = ( nBytes - k - 1 ) * 8 )
            end do

    end function byte_flipper

    !   https://stackoverflow.com/questions/23057213/how-to-generate-integer-random-number-in-fortran-90-in-the-range-0-5

    function random_integer_fcn ( UpperBound ) result ( RandomInteger ) ! 1 <= RandomInteger <= UpperBound

        integer ( ip ), intent ( in ) :: UpperBound

        integer ( ip ) :: RandomInteger ! output
        real    ( rp ) :: RandomReal    ! input

            call random_number ( RandomReal )                 ! 0 <= r < 1
            RandomReal = RandomReal * real ( UpperBound, rp ) ! 0 <= r < UpperBound
            RandomInteger = ceiling ( RandomReal, ip )        ! 1 <= RandomInteger <= UpperBound

    end function random_integer_fcn

    subroutine seed_across_processors_sub ( rank ) ! allows control of seed for each process

        integer, intent ( in ) :: rank

            do k = 1, seed_size
                SeedUsed ( k ) = ieor ( SeedUsed ( k ), rank )
            end do

    end subroutine seed_across_processors_sub

    subroutine init_random_seed_sub ( SeedIn, FlagCheckOS )
    ! If SeedIn is passed, use it to seed rng and return.
    ! Otherwise, create a new seed. First check for a system level seed.

        integer, intent ( IN ),  optional :: SeedIn  ( : )
        logical, intent ( IN ),  optional :: FlagCheckOS

        ! rank 0
        real ( rp ) :: myCPU_real = 0.0_rp

        integer        :: io_urandom = 0, io_stat = 0
        integer ( ip ) :: lcg_input = 0, myCount = 0, myCPU_integer = 0, flipped_Count = 0, flipped_CPU = 0

        character ( len = 512 )          :: io_msg ! for now this message is not reported
        character ( len = * ), parameter :: me_subroutine = 'subroutine init_random_seed_sub'  ! self-identification
        character ( len = * ), parameter :: stop_msg = 'Fatal error: ' // me_module // ', ' // me_subroutine

            if ( present ( SeedIn ) ) then  ! check for user-supplied seed
                call random_seed ( put = SeedIn )  ! rng is now seeded
                SeedUsed = SeedIn
                return
            end if  ! present ( SeedIn )

            ! interogate for size of seed vector and allocate memory
            call random_seed ( size = seed_size )  ! measure seed size for allocation
            if ( .not. allocated ( SeedUsed ) ) then
                allocate ( SeedUsed ( 1 : seed_size ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "integer", "seed"
                    write ( *, 110 ) seed_size
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_message )
                    stop stop_msg
                end if ! error
            end if ! allocated

            ! attempt to get seed from OS
            present_FlagCheckOS: if ( present ( FlagCheckOS ) ) then
                if ( FlagCheckOS ) then
                    open ( newunit = io_urandom, file = "/dev/urandom", access = "stream", form = "unformatted", action = "read", &
                                                 status = "old",  iostat = io_stat, iomsg = io_msg )
                    if ( io_stat == 0 ) then ! able to access urandom
                        read  ( io_urandom ) SeedUsed
                        close ( io_urandom )
                        call random_seed ( put = SeedUsed )  ! rng is now seeded
                        return  ! success - use seed generated by urandom
                    else
                        exit present_FlagCheckOS ! can't use urandom; use the system clock to create a seed
                    end if ! io_stat == 0

                end if  ! FlagCheckOS
            end if present_FlagCheckOS ! present ( FlagCheckOS )

            ! Fallback to XORing the current wall time with CPU_time
            call system_clock ( count = myCount )  ! quanta is ticks, not necessarily seconds
            call cpu_time ( time = myCPU_real )    ! milliseconds
            myCPU_integer = int ( myCPU_real * 10000_rp, ip )

            ! flip bytes to disperse first random numbers
            flipped_Count = byte_flipper ( myCount )
            flipped_CPU   = byte_flipper ( myCPU_integer )

            lcg_input = ieor ( flipped_Count, flipped_CPU )

            do k = 1, seed_size
                SeedUsed ( k ) = lcg ( lcg_seed = lcg_input )
            end do

            call random_seed ( put = SeedUsed )

            return

            100  format ( /, "Error allocating memory for ", g0, " array ", g0, "." )
            110  format (    "  requested size is ", g0, " elements" )
            120  format (    "  stat = ", g0 )
            130  format (    "  errmsg = ", g0, "." )

        contains
            ! https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
            ! Rustic linear congruential generator, adequate for seeding a better PRNG.
            function lcg ( lcg_seed ) result ( random_integer )
                integer        :: random_integer
                integer ( ip ) :: lcg_seed

                    if ( lcg_seed == 0 ) then
                        lcg_seed = 104729
                    else
                        lcg_seed = mod ( lcg_seed, 4294967296_ip )
                    end if

                    lcg_seed = mod ( lcg_seed * 279470273_ip, 4294967291_ip )
                    random_integer = int ( mod ( lcg_seed, int ( huge ( 0 ), ip ) ), kind ( 0 ) )
            end function lcg

    end subroutine init_random_seed_sub

end module mRandoms
