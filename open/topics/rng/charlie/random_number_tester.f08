program random_number_tester

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options

    use mSetPrecision,  only : ip, rp
    use mRandoms,       only : random_integer_fcn, init_random_seed_sub, SeedUsed, seed_size

    implicit none

    real ( rp ) :: xi = 0.0_rp

    integer        :: k = 0
    integer ( ip ) :: myRandomInteger = 0, UpperBound = 10

        myRandomInteger = random_integer_fcn ( UpperBound )
        print *, 'myRandomInteger = ', myRandomInteger, '; upper bound = ', UpperBound

        call init_random_seed ( FlagCheckOS = .true. )

        print *, '10 random numbers:'
        do k = 1, 10
            call random_number ( xi )
            write ( *, '( I4, ". ", g0 )' ) k, xi
        end do

        print *, 'seed_size = ', seed_size
        print *, 'rng seeded with OS seed'
        do k = 1, seed_size
            write ( *, 100 ) k, SeedUsed ( k )
        end do

        call init_random_seed ( )

        print *
        print *, '10 more random numbers:'
        do k = 1, 10
            call random_number ( xi )
            write ( *, '( I4, ". ", g0 )' ) k, xi
        end do

        print *, 'rng seeded with native routine'
        do k = 1, seed_size
            write ( *, 100 ) k, SeedUsed ( k )
        end do

        write ( *, 200 ) 'compiler version: ', compiler_version ()
        write ( *, 200 ) 'compiler options: ', compiler_options ()

        stop 'successful completion for random_number_tester...'

    100 format ( "seed (", g0, ") = ", g0 )
    200 format ( g0, g0 )

end program random_number_tester
