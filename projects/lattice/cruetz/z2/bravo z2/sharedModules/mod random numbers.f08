! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 10 04

module mRandoms

    use mPrecisionDefinitions, only : ip, rp, zero

    implicit NONE

    integer,        private          :: k = 0
    integer ( ip ), private          :: alloc_status  = 0
    character ( len = 512 ), private :: alloc_message = ''
    character ( len = * ), parameter :: me_module = 'module mRandoms'  ! self-identification

    contains                                                                                    ! methods: subroutines and functions

!       =============================================================================================                 random_integer

!       https://stackoverflow.com/questions/23057213/how-to-generate-integer-random-number-in-fortran-90-in-the-range-0-5

        function random_integer_fcn ( uBound ) result ( r_int )

            integer ( ip ), intent ( in ) :: uBound
            integer ( ip )                :: r_int

            real    ( rp )                :: r

                call random_number ( r )     ! 0 <= r < 1
                r = r * uBound               ! 0 <= r < uBound
                r_int = ceiling ( r, ip )    ! 1 <= r_int <= uBound

        end function random_integer_fcn

!       =============================================================================================               init_random_seed

!       https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html

        subroutine init_random_seed_sub ( checkOS, mySeed )

            use iso_fortran_env, only: int64

            integer, intent ( IN ), optional :: mySeed ( : )
            logical, intent ( IN ), optional :: checkOS
            integer, allocatable             :: seed ( : )
            integer                          :: n = 0, un = 0, istat = 0, pid = 0, bytes = 0
            integer                          :: dt ( 8 ) = 0
            integer ( int64 )                :: t = 0, u = 0

            character ( len = * ), parameter :: me_subroutine = 'subroutine init_random_seed_sub'  ! self-identification
            character ( len = * ), parameter :: stop_msg = 'Fatal error: ' // me_module // ', ' // me_subroutine

                ! check for user-supplied seed
                if ( present ( mySeed ) ) then
                    call random_seed ( put = mySeed )
                    return
                end if  ! present ( mySeed )

                ! generate seed
                call random_seed ( size = n )  ! measure seed size for allocation
                allocate ( seed ( n ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "integer", "seed"
                    write ( *, 110 ) n
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_message )
                    stop stop_msg
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

                ++++++++66
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

end module mRandoms