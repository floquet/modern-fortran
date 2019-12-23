!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! http://math.scu.edu/~dsmolars/ma60/notesforrec.html
! http://math.scu.edu/~dsmolars/ma60/fibon.f90

program main  ! recursive fibonacci program in fortran 90

    implicit none

    integer                :: n = 0, final = 0
    integer                :: fibon
    character ( len = 16 ) :: c_arg  = ""
    character ( len =  7 ) :: origin = ""

        call get_command_argument ( 1, c_arg )
        if ( len_trim ( c_arg ) == 0 ) then
            n = 3
            origin = "default"
        else
            read ( c_arg, '( I10 )' ) n
            origin = "input"
        end if

        final = fibon ( n )
        write ( *, * ) "Final fibonacci value for ", trim ( origin ), " of ", n, " is ", final

end program main

recursive function fibon ( n ) result ( fib_result )

    implicit none

    integer, intent ( in ) :: n
    integer                :: fib_result

        if ( n <= 2 ) then
            fib_result = 1
        else
            fib_result = fibon ( n - 1 ) + fibon ( n - 2 )
        endif

end function fibon

! dan-topas-pro-2:recursion rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/recursion
! dan-topas-pro-2:recursion rditldmt$ date
! Wed Sep  2 16:39:51 CDT 2015
! dan-topas-pro-2:recursion rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fcoarray=none fibonacci.f95
! dan-topas-pro-2:recursion rditldmt$ ./a.out 20
!  Final fibonacci value for input of           20  is         6765
! dan-topas-pro-2:recursion rditldmt$