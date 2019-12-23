! http://owen.sj.ca.us/~rk/howto/slides/f90model/slides/modusr.html
module vector3
    implicit none
    type vector3d
        real :: x, y, z
    end type vector3d

! Predefine unit vectors
    type ( vector3d ), parameter :: i_ = vector3d ( 1.0, 0.0, 0.0 )
    type ( vector3d ), parameter :: j_ = vector3d ( 0.0, 1.0, 0.0 )
    type ( vector3d ), parameter :: k_ = vector3d ( 0.0, 0.0, 1.0 )

! extend definitions for current standard operators
! define operator interface binding to some common operators
    interface operator ( + )      ! must be a pre-existing operator
        module procedure v3add
    end interface
! Define your own operators
! define custom operator interface
    interface operator ( .add. )  ! must be of .OP. form
        module procedure v3add
    end interface

! Can force the use of these operators
! private :: v3add  
! can make internal routines private to force
! use of the operator interface.
contains
! define operations
    type ( vector3d ) function v3add ( v1, v2 )
        implicit none
        type ( vector3d ), intent ( in ) :: v1, v2
            v3add % x = v1 % x + v2 % x
            v3add % y = v1 % y + v2 % y
            v3add % z = v1 % z + v2 % z
        return
    end function v3add

    subroutine v3out ( head, v )
        implicit none
        character* ( * ),  intent ( in )  :: head
        type ( vector3d ), intent ( in )  :: v
            write ( 6, '( 1x, a10, 3f8.3 )' ) head, v % x, v % y, v % z
    end subroutine v3out

end module vector3

program tvector3

    use vector3
    implicit none
! Note the C++-like constructor initialization
! use structure constructors to initialize variables for derived types
    type ( vector3d ) :: v1 = vector3d (  1.0,  2.0,  3.0 )
    type ( vector3d ) :: v2 = vector3d (  1.0,  4.0,  9.0 )
    type ( vector3d ) :: v3 = vector3d ( -1.0, -1.0, -1.0 )

        call v3out ( 'v1 = ', v1 )
        call v3out ( 'v2 = ', v2 )
        call v3out ( 'v3 = ', v3 )
        v3 = v3add ( v1, v2 )       ! illegal if v3add is private
        call v3out ( 'new v3 = ', v3 )
        v3 = v1 .add. i_
        call v3out ( 'new v3 = ', v3 )
        v3 = v1 + j_
        call v3out ( 'new v3 = ', v3 )

end program tvector3


! dan-topas-pro-2:constructors rditldmt$ date
! Tue Sep  8 11:05:43 CDT 2015
! dan-topas-pro-2:constructors rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/constructors
! dan-topas-pro-2:constructors rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 vector3.f95
! dan-topas-pro-2:constructors rditldmt$ ./a.out
!       v1 =    1.000   2.000   3.000
!       v2 =    1.000   4.000   9.000
!       v3 =   -1.000  -1.000  -1.000
!   new v3 =    2.000   6.000  12.000
!   new v3 =    2.000   2.000   3.000
!   new v3 =    1.000   3.000   3.000
! dan-topas-pro-2:constructors rditldmt$
