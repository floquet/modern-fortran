! https://stackoverflow.com/questions/32833685/how-to-pass-subroutine-names-as-arguments-in-fortran-classes
include 'myClass_alpha.f08'

program class_pass

    use myClass
    implicit none

    type ( test ) :: myTest

        call myTest % check ()
        call myTest % action ( double_sub )
        call myTest % action ( square_sub )

end program class_pass

! dan-topas-pro-2:passing subroutines rditldmt$ date
! Mon Sep 28 21:22:34 CDT 2015
! dan-topas-pro-2:passing subroutines rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/passing subroutines
! dan-topas-pro-2:passing subroutines rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 class_pass.f08
! dan-topas-pro-2:passing subroutines rditldmt$ ./a.out
!  x =    5.00000000     , y =    10.0000000
!  x =    5.00000000     , y =    25.0000000
