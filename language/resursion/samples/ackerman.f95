!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! http://fortranwiki.org/fortran/show/recursion
! https://en.wikipedia.org/wiki/Ackermann_function
program ackermann

    implicit NONE

    integer                :: n = 0, m = 0
    integer                :: ack

    real                   :: cpu_0 = 0, cpu_1 = 0

    character ( len = 16 ) :: c_arg

    character ( len = * ), parameter :: me_program = 'program ackermann'  ! self-identification

        call cpu_time ( cpu_0 ) ! global cpu time - start

            call get_command_argument ( 1, c_arg )
            if ( len_trim ( c_arg ) == 0 ) then
                n = 3
                m = 4
            else
                call get_command_argument ( 2, c_arg )
                read ( c_arg, '( I10 )' ) n
                call get_command_argument ( 3, c_arg )
                read ( c_arg, '( I10 )' ) m
            end if

            write ( *, 100 ) n, m, ack ( n, m )

        call cpu_time ( cpu_1 ) ! global cpu time - start
        write ( *, '( /, "total cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

  100   format ( "ackerman (", g0, ", ", g0, ") = ", g0 )

end program ackermann

recursive function ack ( m, n ) result ( a )

    implicit NONE

     integer, intent ( in ) :: m, n
     integer                :: a

        if ( m == 0 ) then
            a = n + 1
        else if (n == 0) then
            a = ack ( m-  1, 1 )
            else
                a = ack ( m - 1, ack ( m, n - 1 ) )
            end if

end function ack

! dan-topas-pro-2:recursion rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/recursion
! dan-topas-pro-2:recursion rditldmt$ date
! Wed Sep  2 17:30:51 CDT 2015
! dan-topas-pro-2:recursion rditldmt$ ./a.out 3 3
! ackerman (3, 0) = 5
!
! total cpu time used = .829999335E-04 seconds
!
! STOP successful completion for program ackermann.
! dan-topas-pro-2:recursion rditldmt$