! -------------------------------------------------------------------------
! Show how to use modules...  very very simple...
!
! Once F90 processed "USE MatrixOps", all types, interfaces, and
! functions and subroutines in the modules are accessible
! -------------------------------------------------------------------------

! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/6-Fortran/modules.html
! http://www.mathcs.emory.edu/~cheung/Courses/561/Syllabus/6-Fortran/Progs/main-matrix.f90
include 'MatrixOps.f95'

  PROGRAM Main
   USE MatrixOps
   implicit none

   TYPE(MyReal), dimension( 3, 3 ) :: A, B, C
   TYPE(MyReal), dimension( 3 ) :: v1, v2
   integer :: i, j

   CALL random_number( A(:,:).x )
   CALL random_number( B(:,:).x )
   CALL random_number( v1(:).x )

   v2 = A * v1

   DO i = 1, SIZE(A(:, 1))
      WRITE (6, '("[")', ADVANCE="NO")
      DO j = 1, SIZE(A(1, :))
         WRITE (6, '(2X F6.4)', ADVANCE="NO")   A(i,j).x
      END DO
      WRITE (6, '("]")', ADVANCE="NO")

      WRITE (6, '(6X "[" F6.4 "]")', ADVANCE="NO")   v1(i).x
      WRITE (6, '(6X "[" F6.4 "]")', ADVANCE="NO")   v2(i).x
      print *
   END DO
   print *

   C = A * B

   DO i = 1, SIZE(A(:, 1))
      WRITE (6, '("[")', ADVANCE="NO")
      DO j = 1, SIZE(A(1, :))
         WRITE (6, '(2X F6.4)', ADVANCE="NO")   A(i,j).x
      END DO
      WRITE (6, '("]")', ADVANCE="NO")

      WRITE (6, '("[")', ADVANCE="NO")
      DO j = 1, SIZE(B(1, :))
         WRITE (6, '(2X F6.4)', ADVANCE="NO")   B(i,j).x
      END DO
      WRITE (6, '("]   ")', ADVANCE="NO")

      WRITE (6, '("[")', ADVANCE="NO")
      DO j = 1, SIZE(C(1, :))
         WRITE (6, '(2X F6.4)', ADVANCE="NO")   C(i,j).x
      END DO
      WRITE (6, '("]")', ADVANCE="NO")


      print *
   END DO


   print *

  end program
