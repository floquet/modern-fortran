#include <stdio.h>
int main() {
 // Define the POINT struct
 struct point {
   float x,y,z;
 };

 extern void fflip_ (struct point *) ; // declare existence of externally defined object called fflip_

 struct point point_d;                 // declare variable of type struct point
 struct point *pointer_d = &point_d;    // declare pointer to that variable

 point_d.x = 3.0;
 point_d.y =-2.5;
 point_d.z = 5.6;
 printf("point_d.x = %f, point_d.y = %f,point_d.z = %f \n",point_d.x,point_d.y,point_d.z);
 fflip_ (pointer_d);      // pass the POINTER to the struct, not the struct itself
 printf("point_d.x = %f, point_d.y = %f,point_d.z = %f \n",point_d.x,point_d.y,point_d.z);
};

// dan-topas-pro-2:C struct passed to Fortran rditldmt$ date
// Thu Mar 17 10:00:51 CDT 2016
// dan-topas-pro-2:C struct passed to Fortran rditldmt$ pwd
// /Users/rditldmt/Box Sync/fortran/projects/tomas/C interop/C struct passed to Fortran
// dan-topas-pro-2:C struct passed to Fortran rditldmt$ gfortran -c simple_struct_interoperability_example.f90 -o simple_struct_interoperability_example.o
// dan-topas-pro-2:C struct passed to Fortran rditldmt$ gcc simple_struct_interoperability_example.c simple_struct_interoperability_example.o -o simple_struct_interoperability_example
// dan-topas-pro-2:C struct passed to Fortran rditldmt$ ./simple_struct_interoperability_example
// point_d.x = 3.000000, point_d.y = -2.500000,point_d.z = 5.600000
// point_d.x = -2.500000, point_d.y = 3.000000,point_d.z = -11.200000
//
// https://wiki.erdc.dren.mil/Simple_Structs_and_Fortran-C_Interoperability
// Simple C struct passed to a Fortran subroutine
//  point_d.x = 3.000000, point_d.y = -2.500000,point_d.z = 5.600000
//  point_d.x = -2.500000, point_d.y = 3.000000,point_d.z = -11.200000
