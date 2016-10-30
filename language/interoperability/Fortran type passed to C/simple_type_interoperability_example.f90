PROGRAM main
  USE ISO_C_BINDING, only: C_FLOAT
  USE ISO_FORTRAN_ENV, only: ERROR_UNIT,OUTPUT_UNIT
  IMPLICIT NONE

  ! define the structure of the derived type POINT
  TYPE point
    REAL(kind=C_FLOAT) :: x, y, z
  END TYPE point

  TYPE (point) base                                ! define a variable of the type point

  EXTERNAL flip                                      ! declare the existence of an externally defined thing called FLIP

  base%x = 3.0
  base%y =-2.5
  base%z = 5.6

  WRITE(OUTPUT_UNIT,*)"Base%x = ",base%x," Base%y = ",base%y," Base%z = ",base%z
  CALL flip( base)
  WRITE(OUTPUT_UNIT,*)"Base%x = ",base%x," Base%y = ",base%y," Base%z = ",base%z
END PROGRAM main

! dan-topas-pro-2:wiki 01 rditldmt$ date
! Thu Mar 17 09:49:50 CDT 2016
! dan-topas-pro-2:wiki 01 rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/tomas/C interop/wiki 01
! dan-topas-pro-2:wiki 01 rditldmt$ gcc -c simple_type_interoperability_example.c -o simple_type_interoperability_example.o
! dan-topas-pro-2:wiki 01 rditldmt$ gfortran simple_type_interoperability_example.f90 simple_type_interoperability_example.o -o simple_type_interoperability_example
! dan-topas-pro-2:wiki 01 rditldmt$ ./simple_type_interoperability_example
!  Base%x =    3.00000000      Base%y =   -2.50000000      Base%z =    5.59999990
!  Base%x =   -2.50000000      Base%y =    3.00000000      Base%z =   -11.1999998

! https://wiki.erdc.dren.mil/Simple_Structs_and_Fortran-C_Interoperability
! Simple Fortran derived type passed to a C function
! Expected answer
! Base%x =    3.00000000      Base%y =   -2.50000000      Base%z =    5.59999990
! Base%x =   -2.50000000      Base%y =    3.00000000      Base%z =   -11.1999998
