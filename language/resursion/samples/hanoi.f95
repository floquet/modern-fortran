!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! http://math.scu.edu/~dsmolars/ma60/notesforrec.html

program hanoi_main  ! recursive towers of hanoi program in fortran 90

    implicit none

    integer                :: num_pegs = 0
    character ( len = 16 ) :: c_arg

        call get_command_argument ( 1, c_arg )
        if ( len_trim ( c_arg ) == 0 ) then
            num_pegs = 3
        else
            read ( c_arg, '( I10 )' ) num_pegs
        end if

        call hanoi ( num_pegs, 'a', 'c', 'b' )

end program hanoi_main

recursive subroutine hanoi ( n, frompeg, auxpeg, topeg )

    implicit none

    integer,               intent ( in ) :: n
    character ( len = 1 ), intent ( in ) :: frompeg, auxpeg, topeg

        if ( n == 1 ) then
            write ( 6, * ) "Move disk 1 from peg ", frompeg, " to peg ", topeg
        else
            call hanoi ( n - 1, frompeg, auxpeg,   topeg )
            write ( *, 100 ) n, frompeg,  topeg
            call hanoi ( n - 1,  auxpeg,  topeg, frompeg )
        endif

        return

    100 format ( 1x, "Move disk", I2, " from peg ", A, " to peg ", A)

end subroutine

! dan-topas-pro-2:recursion rditldmt$ date
! Wed Sep  2 16:15:07 CDT 2015
! dan-topas-pro-2:recursion rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/recursion
! dan-topas-pro-2:recursion rditldmt$ ./a.out 4
!  Move disk 1 from peg a to peg b
!  Move disk 2 from peg a to peg b
!  Move disk 1 from peg c to peg a
!  Move disk 3 from peg a to peg b
!  Move disk 1 from peg c to peg a
!  Move disk 2 from peg c to peg a
!  Move disk 1 from peg b to peg c
!  Move disk 4 from peg a to peg b
!  Move disk 1 from peg c to peg a
!  Move disk 2 from peg c to peg a
!  Move disk 1 from peg b to peg c
!  Move disk 3 from peg c to peg a
!  Move disk 1 from peg b to peg c
!  Move disk 2 from peg b to peg c
!  Move disk 1 from peg a to peg b
! dan-topas-pro-2:recursion rditldmt$
