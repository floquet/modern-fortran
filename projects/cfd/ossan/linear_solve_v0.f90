!********************************************************************************
!* --------- OSSAN (Oh, Such a Simple 'Ansutorakucha' Navier-Stokes) -----------
!*
!*      This module belongs to the inviscid version: OSSAN-Euler2D-Implicit
!*
!*
!* This module contains all subroutines needed to relax the linear system.
!*
!* Contained subroutines/functions:
!* -----------------------------------------------------------------------------
!* gs_sequential: Sequential Gauss Seidel relaxation
!*    gewp_solve: Gauss elimination with pivoting (to invert the diagonal block)
!* -----------------------------------------------------------------------------
!*
!* Written by Katate Masatsuka (http://www.cfdbooks.com)
!********************************************************************************
 module linear_solve

 private

 public :: gs_sequential

 contains

!********************************************************************************
!* This subroutine relaxes the linear system by Sequential Gauss-Seidel
!*
!* ------------------------------------------------------------------------------
!*  Input:         jac = Jacobian matirx (left hand side)
!*         node(:)%res = Residual (right hand side)
!*              sweeps = The number of GS sweeps to be performed
!*
!* Output: node(:)%du  = Correction
!* ------------------------------------------------------------------------------
!*
!********************************************************************************
 subroutine gs_sequential

 use constants   , only : p2, zero
 use my_main_data, only : nnodes, node, jac, sweeps

 implicit none

!Local variables
 real(p2), dimension(4,4) :: inverse       ! Inverse of diagonal block
 real(p2), dimension(4)   :: b, x
 integer                  :: i, k, idestat

 integer  :: j, ii, kth_nghbr

!--------------------------------------------------------------------------------
! 1. Initialize the correction

  nodes1 : do i = 1, nnodes
   node(i)%du = zero
  end do nodes1

!--------------------------------------------------------------------------------
! 2. Invert the diagonal block and store it (replace jac(i)%diag by its inverse)

   nodes2 : do i = 1, nnodes

!   b is just a dummy variable here.

     b = zero

!   Invert the diagonal block at node i by Gauss elimination with pivoting.
!
!   Note: gewp_solve() actually solves a linear system, Ax=b, but here
!         we use it only to obtain the inverse of A. So, b is a dummy.
!   Note: Gauss Elimination is not the only way. You can try LU decomposition,
!         iterative methods, etc.
!   Note: "x" is a dummy, not used.

!             Ax=b ->          A  b     x   A^{-1}
    call gewp_solve( jac(i)%diag, b, 4, x, inverse, idestat )

!   Report errors if it fails.

    if (idestat/=0) then
     write(*,*) " Error in inverting the diagonal block... Stop"
     do k = 1, 4
      write(*,'(4(es25.15))') ( jac(i)%diag(k,j), j=1,4)
     end do
     stop
    endif

!   Save the inverse (replace "diag" by its inverse)

    jac(i)%diag = inverse

   end do nodes2

!--------------------------------------------------------------------------------
! 3. Linear Relaxation (Sweep)

  relax : do ii = 1, sweeps

!----------------------------------------------------
!    Sequential Gauss-Seidel Relaxation(sweep)

   nodes3 : do i = 1, nnodes

!    Form the right hand side of GS relaxation: [ sum( off_diagonal_block*du ) - residual ]

     b = node(i)%res ! Remember that residual has already been given the minus sign.

     neighbors_of_i : do k = 1, node(i)%nnghbrs

      kth_nghbr = node(i)%nghbr(k)
      b = b - matmul(jac(i)%off(:,:,k), node(kth_nghbr)%du) ! Matrix-vector multiplication

     end do neighbors_of_i

!    Update du by the GS relaxation
!    Note: Remember that diag is now the inverse of the diagonal block.

     node(i)%du = matmul( jac(i)%diag, b ) ! Matrix-vector multiplication

   end do nodes3

!    End of Sequential Gauss-Seidel Relaxation(sweep)
!----------------------------------------------------

  end do relax

!    End of Linear Relaxation (Sweep)
!--------------------------------------------------------------------------------

 end subroutine gs_sequential
!--------------------------------------------------------------------------------


!****************************************************************************
!* ------------------ GAUSS ELIMINATION WITH PIVOTING ---------------------
!*
!*  This computes the inverse of an (nm)x(nm) matrix "ai" and also
!*  computes the solution to a given lienar system.
!*
!*  Input  :   ai = An (nm)x(nm) matrix whoise inverse is sought.
!*             bi = A vector of (nm): Right hand side of the linear sytem
!*             nm = The size of the matrix "ai"
!*
!*  Output :
!*            sol = Solution to the linear system: ai*sol=bi
!*        inverse = the inverse of "ai".
!*       idetstat = 0 -> inverse successfully computed
!*                  1 -> THE INVERSE DOES NOT EXIST (det=0).
!*                  2 -> No unique solutions exist.
!*****************************************************************************
  subroutine gewp_solve(ai,bi,nm, sol,inverse,idetstat)

  use constants   , only : p2, zero, one

  implicit none

  integer , intent( in) :: nm
  real(p2), intent( in) :: ai(nm,nm),bi(nm)
  real(p2), intent(out) :: sol(:),inverse(nm,nm)
  integer , intent(out) :: idetstat
  real(p2)              :: a(nm,nm+1),x(nm)
  integer               :: I,J,K,pp,nrow(nm),m

  do m = 1, nm
!*****************************************************************************
!* Set up the matrix a
!*****************************************************************************

       do J=1,nm
        do I=1,nm
          a(I,J) = ai(I,J)
        end do
       end do

       do k=1,nm
          a(k,nm+1)=zero; nrow(k)=k
       end do
          a(m,nm+1)=one

!*****************************************************************************
!* HONA IKOKA.....
!*****************************************************************************
    do j=1,nm-1
!*****************************************************************************
!* FIND SMALLEST pp FOR a(pp,j) IS MAXIMUM IN JTH COLUMN.
!*****************************************************************************
      call findmax(nm,j,pp,a,nrow)
!*****************************************************************************
!* IF a(nrow(p),j) IS zero, THERE'S NO UNIQUE SOLUTIONS
!*****************************************************************************
      if (a(nrow(pp),j) == zero) then
       write(6,*) 'THE INVERSE DOES NOT EXIST.'
        idetstat = 1
        return
      endif
!*****************************************************************************
!* IF THE MAX IS NOT A DIAGONAL ELEMENT, SWITCH THOSE ROWS
!*****************************************************************************
      if (nrow(pp) .ne. nrow(j)) then
      call switch(nm,j,pp,nrow)
      else
      endif
!*****************************************************************************
!* ELIMINATE ALL THE ENTRIES BELOW THE DIAGONAL ONE
!*****************************************************************************
      call eliminate_below(nm,j,a,nrow)

    end do
!*****************************************************************************
!* CHECK IF a(nrow(N),N)=0.0 .
!*****************************************************************************
      if (a(nrow(nm),nm) == zero) then
        write(6,*) 'NO UNIQUE SOLUTION EXISTS!'
        idetstat = 2
        return
      else
      endif
!*****************************************************************************
!* BACKSUBSTITUTION!
!*****************************************************************************
      call backsub(nm,x,a,nrow)
!*****************************************************************************
!* STORE THE SOLUTIONS, YOU KNOW THEY ARE INVERSE(i,m) i=1...
!*****************************************************************************
      do i=1,nm
         inverse(i,m)=x(i)
      end do
!*****************************************************************************
  end do

!*****************************************************************************
!* Solve
!*****************************************************************************

    do I=1,nm; sol(I)=zero

     do J=1,nm
       sol(I) = sol(I) + inverse(I,J)*bi(J)
     end do

    end do

      idetstat = 0

    return

!*****************************************************************************
 end subroutine gewp_solve

!*****************************************************************************
!* Four subroutines below are used in gewp_solve() above.
!*****************************************************************************
!* FIND MAXIMUM ELEMENT IN jth COLUMN
!*****************************************************************************
      subroutine findmax(nm,j,pp,a,nrow)

      use constants   , only : p2

      implicit none

      integer , intent( in) :: nm
      real(p2), intent( in) :: a(nm,nm+1)
      integer , intent( in) :: j,nrow(nm)
      integer , intent(out) :: pp
      real(p2)              :: max
      integer               :: i

            max=abs(a(nrow(j),j)); pp=j

           do i=j+1,nm

             if (max < abs(a(nrow(i),j))) then

                  pp=i; max=abs(a(nrow(i),j))

             endif

           end do

      return

      end subroutine findmax
!*****************************************************************************
!* SWITCH THOSE ROWS
!*****************************************************************************
      subroutine switch(nm,j,pp,nrow)

      use constants   , only : p2

      implicit none

      integer, intent(   in) :: nm,j,pp
      integer, intent(inout) :: nrow(nm)
      integer                :: ncopy

      if (nrow(pp).ne.nrow(j)) then

         ncopy=nrow(j)
         nrow(j)=nrow(pp)
         nrow(pp)=ncopy

      endif

      return

      end subroutine switch
!*****************************************************************************
!* ELIMINATE ALL THE ENTRIES BELOW THE DIAGONAL ONE
!*(Give me j, the column you are working on now)
!*****************************************************************************
      subroutine eliminate_below(nm,j,a,nrow)

      use constants   , only : p2, zero

      implicit none

      integer , intent(   in) :: nm
      real(p2), intent(inout) :: a(nm,nm+1)
      integer , intent(   in) :: j,nrow(nm)
      real(p2)                :: m
      integer                 :: k,i

      do i=j+1,nm

        m=a(nrow(i),j)/a(nrow(j),j)
        a(nrow(i),j)=zero

          do k=j+1,nm+1
            a(nrow(i),k)=a(nrow(i),k)-m*a(nrow(j),k)
          end do

      end do

      return

      end subroutine eliminate_below
!*****************************************************************************
!* BACKSUBSTITUTION!
!*****************************************************************************
      subroutine backsub(nm,x,a,nrow)

      use constants   , only : p2, zero

      implicit none

      integer , intent( in) :: nm
      real(p2), intent( in) :: a(nm,nm+1)
      integer , intent( in) :: nrow(nm)
      real(p2), intent(out) :: x(nm)
      real(p2)              :: sum
      integer               :: i,k

      x(nm)=a(nrow(nm),nm+1)/a(nrow(nm),nm)

      do i=nm-1,1,-1

         sum=zero

           do k=i+1,nm

              sum=sum+a(nrow(i),k)*x(k)

           end do

      x(i)=(a(nrow(i),nm+1)-sum)/a(nrow(i),i)

      end do

      return

      end subroutine backsub
!*********************************************************************


 end module linear_solve