!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! http://fortranwiki.org/fortran/show/recursion
! https://en.wikipedia.org/wiki/Ackermann_function
program loop_ackermann

    use iso_fortran_env

    implicit NONE

    integer, parameter :: ikind = selected_int_kind ( INT64 ) 

    integer ( ikind )  :: n = 0, m = 0, value = 0
    integer ( ikind )  :: ack

    real               :: cpu_0 = 0, cpu_1 = 0, t_0 = 0, t_1 = 0

    character ( len = * ), parameter :: me_program = 'program loop_ackermann'  ! self-identification

        call cpu_time ( cpu_0 ) ! global cpu time - start

            do n = 0, 4
                do m = 0, 4
                    call cpu_time ( t_0 ) ! local cpu time - start
                        value = ack ( n, m )
                    call cpu_time ( t_1 ) ! local cpu time - end
                    write ( *, 100 ) n, m, value, t_1 - t_0
                end do
            end do

        call cpu_time ( cpu_1 ) ! global cpu time - end
        write ( *, '( /, "total cpu time used = ", g0, " seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

  100   format ( "ackerman (", g0, ", ", g0, ") = ", I15, "; time = ", g0, " s" )

end program loop_ackermann

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
! Wed Sep  2 17:44:39 CDT 2015
! dan-topas-pro-2:recursion rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fcoarray=none loop.f95
! dan-topas-pro-2:recursion rditldmt$ ./a.out
! ackerman (0, 0) =               1; time = .100000761E-05 s
! ackerman (0, 1) =               2; time = .00000000 s
! ackerman (0, 2) =               3; time = .100000761E-05 s
! ackerman (0, 3) =               4; time = .00000000 s
! ackerman (0, 4) =               5; time = .100000761E-05 s
! ackerman (1, 0) =               2; time = .00000000 s
! ackerman (1, 1) =               3; time = .00000000 s
! ackerman (1, 2) =               4; time = .00000000 s
! ackerman (1, 3) =               5; time = .100000761E-05 s
! ackerman (1, 4) =               6; time = .00000000 s
! ackerman (2, 0) =               3; time = .00000000 s
! ackerman (2, 1) =               5; time = .100000761E-05 s
! ackerman (2, 2) =               7; time = .100000761E-05 s
! ackerman (2, 3) =               9; time = .100000761E-05 s
! ackerman (2, 4) =              11; time = .100000761E-05 s
! ackerman (3, 0) =               5; time = .100000761E-05 s
! ackerman (3, 1) =              13; time = .200001523E-05 s
! ackerman (3, 2) =              29; time = .300002284E-05 s
! ackerman (3, 3) =              61; time = .120000914E-04 s
! ackerman (3, 4) =             125; time = .539999455E-04 s
! ackerman (4, 0) =              13; time = .200001523E-05 s
! ackerman (4, 1) =           65533; time = 17.0830441 s
! Segmentation fault: 11
! dan-topas-pro-2:recursion rditldmt$