SUBROUTINE FFLIP( P )
  USE ISO_C_BINDING, only: C_FLOAT
  IMPLICIT NONE
  TYPE POINT
    REAL(kind=C_FLOAT) :: X, Y, Z
  END TYPE POINT

  TYPE (POINT) P
  REAL :: T

  T = P%X
  P%X = P%Y
  P%Y = T
  P%Z = -2.*P%Z
END SUBROUTINE FFLIP

! dan-topas-pro-2:C struct passed to Fortran rditldmt$ date
! Thu Mar 17 10:00:51 CDT 2016
! dan-topas-pro-2:C struct passed to Fortran rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/tomas/C interop/C struct passed to Fortran
! dan-topas-pro-2:C struct passed to Fortran rditldmt$ gfortran -c simple_struct_interoperability_example.f90 -o simple_struct_interoperability_example.o
! dan-topas-pro-2:C struct passed to Fortran rditldmt$ gcc simple_struct_interoperability_example.c simple_struct_interoperability_example.o -o simple_struct_interoperability_example
! dan-topas-pro-2:C struct passed to Fortran rditldmt$ ./simple_struct_interoperability_example
! point_d.x = 3.000000, point_d.y = -2.500000,point_d.z = 5.600000
! point_d.x = -2.500000, point_d.y = 3.000000,point_d.z = -11.200000
! d
!
! https://wiki.erdc.dren.mil/Simple_Structs_and_Fortran-C_Interoperability
! https://wiki.erdc.dren.mil/Simple_Structs_and_Fortran-C_Interoperability
! Simple C struct passed to a Fortran subroutine
!  point_d.x = 3.000000, point_d.y = -2.500000,point_d.z = 5.600000
!  point_d.x = -2.500000, point_d.y = 3.000000,point_d.z = -11.200000

! dan-topas-pro-2:C struct passed to Fortran rditldmt$ gfortran -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -c simple_struct_interoperability_example.f90 -o simple_struct_interoperability_example.o
! dan-topas-pro-2:C struct passed to Fortran rditldmt$ gcc simple_struct_interoperability_example.c simple_struct_interoperability_example.o -o simple_struct_interoperability_example
! dan-topas-pro-2:C struct passed to Fortran rditldmt$ ./simple_struct_interoperability_example 
! point_d.x = 3.000000, point_d.y = -2.500000,point_d.z = 5.600000
! point_d.x = -2.500000, point_d.y = 3.000000,point_d.z = -11.200000
