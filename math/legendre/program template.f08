! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2016 01 18

include 'sharedModules/mod precision definitions.f08'

program call_legendre

    use mPrecisionDefinitions, only : rp, zero

    implicit none

    real ( rp )                      :: cpu_0 = zero, cpu_1 = zero

    character ( len = * ), parameter :: myProgram = 'program call_legendre'  ! self-identification

        call cpu_time ( cpu_0 )   ! global cpu time - start

        call cpu_time ( cpu_1 )   ! global cpu time - stop
        write ( *, 100 ) 'all tasks', cpu_1 - cpu_0

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

  100   format ( /, 'CPU time for ', A, ' = ', g0, ' seconds', / )

end program call_legendre

! dan-topas-pro-2:alpha rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/call_legendres/alpha
! dan-topas-pro-2:alpha rditldmt$ date
! Mon Jan 18 09:55:35 CST 2016
! dan-topas-pro-2:alpha rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 program\ call_legendre.f08
