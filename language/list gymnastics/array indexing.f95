! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/6-Fortran/array1.html
! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/6-Fortran/Progs/array02.f90

PROGRAM main

     IMPLICIT NONE

     REAL, DIMENSION ( 5 )      :: A
     REAL, DIMENSION ( -2 : 2 ) :: B
     REAL, DIMENSION ( -2 : 2 ) :: C = [ 111, 222, 333, 444, 555 ]

     INTEGER i

        print *, "A(5) "
        do i = 1, 5
            print *, " ", A ( i )
        end do

        print *
        print *, "B ( -2 : 2 ) "
        do i = -2, 2
            print *, " ", B ( i )
        end do

        print *
        print *, "C ( -2 : 2 ) "
        do i = -2, 2
            write ( * , 100 ) i, C ( i )
        end do

        stop 'successful completion'

  100   format ( "C(", g0, ") = ", g0, "." )

    end

!     dan-topas-pro-2:list gymnastics rditldmt$ date
!     Tue Sep  8 11:31:34 CDT 2015
!     dan-topas-pro-2:list gymnastics rditldmt$ pwd
!     /Users/rditldmt/Box Sync/fortran/demos/list gymnastics
!     dan-topas-pro-2:list gymnastics rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 array\ indexing.f95
!     dan-topas-pro-2:list gymnastics rditldmt$ ./a.out
!      A(5)
!          1.65885763E-34
!          1.40129846E-45
!          0.00000000
!          0.00000000
!          0.00000000
!
!      B ( -2 : 2 )
!          3.11999712E+34
!          4.57734143E-41
!          3.11999712E+34
!          4.57734143E-41
!          3.11537812E+34
!
!      C ( -2 : 2 )
!     C(-2) = 111.000000.
!     C(-1) = 222.000000.
!     C(0) = 333.000000.
!     C(1) = 444.000000.
!     C(2) = 555.000000.
!     Note: The following floating-point exceptions are signalling: IEEE_DENORMAL
!     STOP successful completion


! https://www.cisl.ucar.edu/news/01/tips/0209.fpe.html

! Underflow
! One form of underflow exception is signaled by the creation of a tiny nonzero result between the minimum expressible exponent, which, because it is tiny, may cause some other exception later. The other form of underflow exception is signaled by an extraordinary loss of accuracy during the approximation of such tiny numbers by denormalized numbers.

! Remedy
! A = 0
! B = 0