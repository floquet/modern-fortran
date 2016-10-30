! https://people.sc.fsu.edu/~jburkardt/f_src/prime_mpi/prime_mpi.f90
module mTimeStamp

    implicit none

contains

    subroutine timestamp ( )

        ! rank 1
        integer :: values (  1 : 8 )
<<<<<<< HEAD
        character ( len = 9 ), parameter, dimension ( 1 : 12 ) :: month = (/ &
        'January  ', 'February ', 'March    ', 'April    ', &
        'May      ', 'June     ', 'July     ', 'August   ', &
        'September', 'October  ', 'November ', 'December ' /)
=======
        character ( len = 9 ), parameter, dimension ( 1 : 12 ) :: month = [ &
        'January  ', 'February ', 'March    ', 'April    ', &
        'May      ', 'June     ', 'July     ', 'August   ', &
        'September', 'October  ', 'November ', 'December ' ]
>>>>>>> 9bb5e4725597a709f3c8f265e56bae45176185a5
        ! rank 0
        integer :: d = 0, h = 0, m = 0, mm = 0, n = 0, s = 0, y = 0
        character ( len = 8 ) :: ampm = ''

            call date_and_time ( values = values )

            y  = values ( 1 )
            m  = values ( 2 )
            d  = values ( 3 )
            h  = values ( 5 )
            n  = values ( 6 )
            s  = values ( 7 )
            mm = values ( 8 )

            if ( h < 12 ) then
                ampm = 'AM'
            else if ( h == 12 ) then
                if ( n == 0 .and. s == 0 ) then
                    ampm = 'Noon'
                else
                    ampm = 'PM'
                end if
            else
                h = h - 12
            if ( h < 12 ) then
                ampm = 'PM'
            else if ( h == 12 ) then
                if ( n == 0 .and. s == 0 ) then
                    ampm = 'Midnight'
                    else
                        ampm = 'AM'
                    end if
                end if
            end if

            write ( *, '( I2, 1x, A, 1x, I4, 2x, I2, A1, I2.2, A1, I2.2, A1, I3.3, 1x, A )' ) &
            d, trim ( month ( m ) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

            return

    end subroutine timestamp

end module mTimeStamp
