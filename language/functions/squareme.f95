! $CLASSHG/codes/fortran/fcn1.f90
! https://faculty.washington.edu/rjl/uwamath583s11/sphinx/notes/html/fortran_sub.html

program fcn1
    implicit none
    real(kind=8) :: y,z
    real(kind=8), external :: f

    y = 2.
    z = f(y)
    print *, "z = ",z
end program fcn1

real(kind=8) function f(x)
    implicit none
    real(kind=8), intent(in) :: x
    f = x**2
end function f