struct point {
  float x,y,z;
};

void flip_( struct point *v )
{
  float t;
  t = v -> x;
  v -> x = v -> y;
  v -> y = t;
  v -> z = -2.*(v -> z);
}

// dan-topas-pro-2:wiki 01 rditldmt$ date
// Thu Mar 17 09:49:50 CDT 2016
// dan-topas-pro-2:wiki 01 rditldmt$ pwd
// /Users/rditldmt/Box Sync/fortran/projects/tomas/C interop/wiki 01
// dan-topas-pro-2:wiki 01 rditldmt$ gcc -c simple_type_interoperability_example.c -o simple_type_interoperability_example.o
// dan-topas-pro-2:wiki 01 rditldmt$ gfortran simple_type_interoperability_example.f90 simple_type_interoperability_example.o -o simple_type_interoperability_example
// dan-topas-pro-2:wiki 01 rditldmt$ ./simple_type_interoperability_example
//  Base%x =    3.00000000      Base%y =   -2.50000000      Base%z =    5.59999990
//  Base%x =   -2.50000000      Base%y =    3.00000000      Base%z =   -11.1999998

// https://wiki.erdc.dren.mil/Simple_Structs_and_Fortran-C_Interoperability
// Simple Fortran derived type passed to a C function
// Expected answer
// Base%x =    3.00000000      Base%y =   -2.50000000      Base%z =    5.59999990
// Base%x =   -2.50000000      Base%y =    3.00000000      Base%z =   -11.1999998
