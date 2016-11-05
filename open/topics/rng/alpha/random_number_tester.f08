program random_number_tester

    use mSetPrecision,  only : ip
    use mRandoms,       only : random_integer_fcn

    implicit none

    integer ( ip ) :: myRandomInteger = 0, UpperBound = 10

        myRandomInteger = random_integer_fcn ( UpperBound )
        print *, 'myRandomInteger = ', myRandomInteger

        stop 'successful completion for random_number_tester...'

end program random_number_tester
