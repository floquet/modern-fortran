!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! https://gcc.gnu.org/onlinedocs/gfortran/GET_005fCOMMAND_005fARGUMENT.html#GET_005fCOMMAND_005fARGUMENT
program test_get_command_argument

    integer                          :: counter = 0, value = 0
    character ( len = 32 )           :: arg = ""
    character ( len = * ), parameter :: me_program = 'program test_get_command_argument'  ! self-identification

        counter = 0

!       run command
        call get_command_argument ( counter, arg )
        write ( *, 100 ) trim ( arg )

        do
            counter = counter + 1
            call get_command_argument ( counter, arg )

            if ( len_trim ( arg ) == 0 ) then
                if ( counter == 1 ) write ( *, ' ( "No command line arguments discovered." ) ' )
                exit
            end if

!           convert to integer
            read ( arg, '( I10 )' ) value
            write ( *, '( "argument", I3, ": integer value = ", g0, "; string = ", g0, "." )' ) counter, value, arg

        end do

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression

  100  format ( 'Run command: ', g0, '.' )

end program test_get_command_argument


! dan-topas-pro-2:commands rditldmt$ date
! Wed Sep  2 16:09:19 CDT 2015
! dan-topas-pro-2:commands rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/commands
! dan-topas-pro-2:commands rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fcoarray=none get_command_argument.f95
! dan-topas-pro-2:commands rditldmt$ ./a.out 1 2 3
! Run command: ./a.out.
! argument  1: integer value = 1; string = 1                               .
! argument  2: integer value = 2; string = 2                               .
! argument  3: integer value = 3; string = 3                               .
! STOP successful completion for program test_get_command_argument.
! dan-topas-pro-2:commands rditldmt$
