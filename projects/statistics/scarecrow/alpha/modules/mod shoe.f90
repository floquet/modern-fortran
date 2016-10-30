!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mShoe

    use mDeck
    use house_parameters
    use shared_variables

    implicit NONE

    integer ( is ), private       :: kShoe = 0

    type, public       :: spectra
        integer ( is ) :: initial   ( 1 : nofPipUnique )
        integer ( is ) :: in_shoe   ( 1 : nofPipUnique )
        integer ( is ) :: from_shoe ( 1 : nofPipUnique )
    end type

    type, public                  :: shoes

        integer ( is )            :: cardStop    = 0
        integer ( is )            :: cardCurrent = 0
        integer ( is )            :: numCards =  nofDecks * ninDeck
        integer ( is )            :: index ( 1 : nofDecks * ninDeck ) = [ ( deckTag,   kShoe = 1, nofDecks ) ]
        integer ( is )            :: value ( 1 : nofDecks * ninDeck ) = [ ( deckValue, kShoe = 1, nofDecks ) ]
        integer ( is )            :: order ( 1 : nofDecks * ninDeck ) = [ ( kShoe,     kShoe = 1, nofDecks * ninDeck ) ]
        integer ( is )            :: p     ( 1 : nofDecks * ninDeck ) = [ ( 0,         kShoe = 1, nofDecks * ninDeck ) ]

        type ( spectra )          :: spectrum
        integer ( is )            :: spectrum_held    ( 1 : nofPipUnique )
        integer ( is )            :: spectrum_dealt   ( 1 : nofPipUnique )

        logical                   :: fPastCutCard = .false.

        type ( decks )            :: myDeck

    contains

        private
!       functions
        procedure, public         :: next_card         =>    next_card_fcn
        procedure, public         :: random_integer    =>    random_integer_fcn

!       subroutines
        procedure, public         :: shuffle           =>    shuffle_sub
        procedure, public         :: initial_spectrum  =>    initial_spectrum_sub
        procedure, nopass, public :: init_random_seed  =>    init_random_seed_sub

    end type shoes

!   functions
    private                       :: next_card_fcn
    private                       :: random_integer_fcn

!   subroutines
    private                       :: shuffle_sub
    private                       :: init_random_seed_sub, initial_spectrum_sub

    contains                                                                                    ! methods: subroutines and functions

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                initial_spectrum

        subroutine initial_spectrum_sub ( self )

            class ( shoes ), target :: self

                self % spectrum_initial = pipDensity * nofSuits * nofDecks
                self % spectrum_held    = self % spectrum_initial
                self % spectrum_dealt   = [ ( 0, kShoe = 1, nofPipUnique ) ]

        end subroutine initial_spectrum_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      next_card

        function next_card_fcn ( self ) result ( pip )

            class ( shoes ), target :: self
            integer ( is )          :: pip

                pip = self % value ( self % p ( self % cardCurrent ) )
                self % cardCurrent = self % cardCurrent + 1

        end function next_card_fcn

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      next_card

        function next_card_update_spectrum_fcn ( self ) result ( pip )

            class ( shoes ), target :: self
            integer ( is )          :: pip

                pip = self % value ( self % p ( self % cardCurrent ) )
                self % cardCurrent = self % cardCurrent + 1

                self % spectrum_dealt ( pip ) = self % spectrum_dealt ( pip ) - 1
                self % spectrum_held  ( pip ) = self % spectrum_held  ( pip ) + 1

        end function next_card_update_spectrum_fcn

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         shuffle

!       https : // en.wikipedia.org/wiki/Fisher-Yates_shuffle

        subroutine shuffle_sub ( self )

            use precision_definitions, only : is

            class ( shoes ), target :: self

            integer ( is )          :: j = 0, k = 0, hold = 0

                self % p = self % order

                do k = self % numCards, 1, -1
                    j = random_integer_fcn ( self )
                    if ( j == k ) cycle
                    hold           = self % p ( k )
                    self % p ( k ) = self % p ( j )
                    self % p ( j ) = hold
                end do

                self % cardCurrent = 1
                self % cardStop    = stopAfter * ninDeck

        end subroutine shuffle_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  random_integer

!       https://stackoverflow.com/questions/23057213/how-to-generate-integer-random-number-in-fortran-90-in-the-range-0-5

        function random_integer_fcn ( self ) result ( r_int )

            use precision_definitions, only : wp, is

            class ( shoes ), target :: self

            integer ( is )          :: r_int

            real    ( wp )          :: r

                call random_number ( r )     ! 0 <= r < 1
                r = r * ( self % numCards )  ! 0 <= r < uBound
                r_int = ceiling ( r, is )    ! 1 <= r_int <= uBound

        end function random_integer_fcn

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                init_random_seed

!       https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html

        subroutine init_random_seed_sub ( checkOS, mySeed )

            use iso_fortran_env, only : int64

            integer, intent ( IN ), optional :: mySeed ( : )
            logical, intent ( IN ), optional :: checkOS
            integer, allocatable             :: seed ( : )
            integer                          :: k = 0, n = 0, un = 0, istat = 0, pid = 0, bytes = 0
            integer                          :: dt ( 8 ) = 0
            integer ( int64 )                :: t = 0, u = 0

            character ( len = * ), parameter :: me = 'subroutine init_random_seed_sub'  ! self-identification

                ! check for user-supplied seed
                if ( present ( mySeed ) ) then
                    call random_seed ( put = mySeed )
                    return
                end if  ! present ( mySeed )

                ! generate seed
                call random_seed ( size = n )  ! measure seed size for allocation
                allocate ( seed ( n ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "integer", "seed"
                    write ( *, 110 ) n
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg // me
                end if

                ! attempt to get seed from OS
                present_checkOS: if ( present ( checkOS ) ) then
                    if ( checkOS ) then
                        open ( newunit = un, file = "/dev/urandom", access = "stream", form = "unformatted", action = "read", &
                                                                    status = "old",  iostat = istat )
                        if ( istat == 0 ) then
                            read  ( un ) seed
                            close ( un )
                            call random_seed ( put = seed )
                            return
                        else
                            exit present_checkOS
                        end if ! istat == 0

                    end if  ! checkOS
                end if present_checkOS ! present ( checkOS )

                ! Fallback to XOR:ing the current time and pid. The PID is
                ! useful in case one launches multiple instances of the same
                ! program in parallel.
                call system_clock ( t )

                if ( t == 0 ) then
                    call date_and_time ( values = dt )  ! convert time to milliseconds from t0
                    t = ( dt ( 1 ) - 1970 ) * 365_int64 * 24 * 60 * 60 * 1000 &  ! year
                        + dt ( 2 ) * 30_int64 * 24 * 60 * 60 * 1000           &  ! month
                        + dt ( 3 ) * 24_int64 * 60 * 60 * 1000                &  ! day
                        + dt ( 5 ) * 60 * 60 * 1000                           &  ! hour
                        + dt ( 6 ) * 60 * 1000                                &  ! minutes
                        + dt ( 7 ) * 1000                                     &  ! seconds
                        + dt ( 8 )                                               ! milliseconds
                end if  ! t == 0

                bytes = bit_size ( t ) / 8
                do k = 0, bytes - 1  ! move LSB in t to MSB in u
                    call mvbits( t, k * 8, 8, u, ( bytes - k - 1 ) * 8 )
                end do

                pid = getpid ( )  ! get process id
                u = ieor ( u, int ( pid, kind ( u ) ) )
                do k = 1, n
                    seed ( k ) = lcg ( u )
                end do

                call random_seed ( put = seed )

                return

    100         format ( /, "Error allocating memory for ", g0, " array ", g0, "." )
    110         format (    "  requested size is ", g0, " elements" )
    120         format (    "  stat = ", g0 )
    130         format (    "  errmsg = ", g0, "." )

                    contains                                                                                              ! CONTAINS
                        ! This simple PRNG might not be good enough for real work, but is
                        ! sufficient for seeding a better PRNG.
                        function lcg ( s )
                            integer           :: lcg
                            integer ( int64 ) :: s
                                if ( s == 0 ) then
                                    s = 104729
                                else
                                    s = mod ( s, 4294967296_int64 )
                                end if
                                s   = mod ( s * 279470273_int64, 4294967291_int64 )
                                lcg = int ( mod ( s, int ( huge ( 0 ), int64 ) ), kind ( 0 ) )
                        end function lcg

        end subroutine init_random_seed_sub

end module mShoe