PROGRAM main
  USE ISO_C_BINDING, only: C_FLOAT
  USE ISO_FORTRAN_ENV, only: ERROR_UNIT,OUTPUT_UNIT
  IMPLICIT NONE

  ! define the structure of the derived type POINT
  TYPE,BIND(C) :: point
    REAL(kind=C_FLOAT) :: x, y, z
  END TYPE point

  TYPE (point) base                                ! define a variable of the type point

  INTERFACE
    SUBROUTINE cflip(which_point) bind(c,name='flip_') ! declare the interface to an externally defined function called flip_
      IMPORT :: point
      TYPE(point),INTENT(INOUT) :: which_point
    END SUBROUTINE cflip
  END INTERFACE

  base%x = 3.0
  base%y =-2.5
  base%z = 5.6

  WRITE(OUTPUT_UNIT,*)"Base%x = ",base%x," Base%y = ",base%y," Base%z = ",base%z
  CALL cflip( base)
  WRITE(OUTPUT_UNIT,*)"Base%x = ",base%x," Base%y = ",base%y," Base%z = ",base%z
END PROGRAM main

! dan-topas-pro-2:Fortran type passed to C rditldmt$ date
! Thu Mar 17 10:13:15 CDT 2016
! dan-topas-pro-2:Fortran type passed to C rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/tomas/C interop/Fortran type passed to C
! dan-topas-pro-2:Fortran type passed to C rditldmt$
! dan-topas-pro-2:Fortran type passed to C rditldmt$ gcc -c simple_type_interoperability_example.c -o simple_type_interoperability_example.o
! dan-topas-pro-2:Fortran type passed to C rditldmt$ gfortran -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 advanced_type_interoperability_example.f90 simple_type_interoperability_example.o -o advanced_type_interoperability_example
! dan-topas-pro-2:Fortran type passed to C rditldmt$ ./advanced_type_interoperability_example
!  Base%x =    3.00000000      Base%y =   -2.50000000      Base%z =    5.59999990
!  Base%x =   -2.50000000      Base%y =    3.00000000      Base%z =   -11.1999998
!
! https://wiki.erdc.dren.mil/Simple_Structs_and_Fortran-C_Interoperability
! Alternative Fortran main program
!
!  Base%x =    3.00000000      Base%y =   -2.50000000      Base%z =    5.59999990
!  Base%x =   -2.50000000      Base%y =    3.00000000      Base%z =   -11.1999998
