! ====================================================================
! Put Matrix*Matrix and Matrix*Vector operators in a module
!
! Module allows use to include our REAL type inside the module !!!
! ====================================================================

  MODULE MatrixOps

   ! ==========================================================
   ! TYPE(MyReal) to un-confuse F90's Matrix*Matrix operation
   ! ==========================================================
   TYPE MyReal
     REAL x
   END TYPE MyReal

   ! ==========================================================
   ! Interface used to map Matrix*Matrix and Matrix*Vector to
   ! their corresponding functions
   ! NOTE: the interface syntax is SIMPLIFIED !!!
   !       You ONLY need to mention the function names !!!
   ! NOTE: the mapped functions must be inside the module !!!
   ! ==========================================================
   INTERFACE operator(*)
    MODULE PROCEDURE MatVecMult, MatMatMult
   END INTERFACE

  CONTAINS   !! The subroutines and functions follows.....

   ! ------------------------------------------------------------
   ! Implements the matrix-vector multiply function
   !
   ! NOTE: you DO NOT need to define TYPE(MyReal)
   !       Module functions KNOWS all types inside the Spec part
   ! ------------------------------------------------------------
   FUNCTION MatVecMult(A, v) result (w)
    implicit none

    TYPE(MyReal), dimension(:,:), INTENT(IN) :: A
    TYPE(MyReal), dimension(:), INTENT(IN) :: v
    TYPE(MyReal), dimension( SIZE(A,1) ) :: w

    integer :: j
    integer :: N

    N = SIZE(A,2)

    w(:).x = 0.0       !! clear whole vector
    DO j = 1, N
       w(:).x = w(:).x + v(j).x * A( :, j ).x
    END DO
   END FUNCTION

   ! --------------------------------------------------------
   ! Implements the matrix-matrix multiply function
   ! --------------------------------------------------------
   FUNCTION MatMatMult(A, B) result (C)
    implicit none

    TYPE(MyReal), dimension(:,:), INTENT(IN) :: A
    TYPE(MyReal), dimension(:,:), INTENT(IN) :: B
    TYPE(MyReal), dimension( size(A,1), size(B, 2) ) :: C
    integer :: M, N, i, j

    M = size(A,1)
    N = size(B,2)

    C(:,:).x = 0.0       !! clear whole matrix

    DO i = 1, M
      DO j = 1, N
        C(:,i).x = C(:,i).x + B(j,i).x*A(:,j).x
      END DO
    END DO
   END FUNCTION

  END MODULE

!   dan-topas-pro-2:modules rditldmt$ date
!   Thu Sep 10 11:10:00 CDT 2015
!   dan-topas-pro-2:modules rditldmt$ pwd
!   /Users/rditldmt/Box Sync/fortran/demos/modules
!   dan-topas-pro-2:modules rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic  -fmax-errors=5 basics.f95
!   MatrixOps.f95:47:8:
!
!        w(:).x = 0.0       !! clear whole vector
!           1
!   Error: Invalid character in name at (1)
!   MatrixOps.f95:49:11:
!
!           w(:).x = w(:).x + v(j).x * A( :, j ).x
!              1
!   Error: Invalid character in name at (1)
!   MatrixOps.f95:67:8:
!
!        C(:,:).x = 0.0       !! clear whole matrix
!           1
!   Error: Invalid character in name at (1)
!   MatrixOps.f95:71:12:
!
!            C(:,i).x = C(:,i).x + B(j,i).x*A(:,j).x
!               1
!   Error: Invalid character in name at (1)
!   basics.f95:13:7:
!
!       USE MatrixOps
!          1
!   Fatal Error: Can't open module file ‘matrixops.mod’ for reading at (1): No such file or directory
!   compilation terminated.
!   dan-topas-pro-2:modules rditldmt$
