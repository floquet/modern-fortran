! Gaussian elimination without pivoting: straightforward formulas,
! Fortran 90/95 features, BLAS routines
!!---------------------------------------------------------------------------
! gfortran -ffree-form -Wall -Wextra -lblas -fopenmp
!!---------------------------------------------------------------------------
!
! [1] J. Demmel "Numerical Linear Algebra"
! [2] N. J. Nigham "Gaussian Elimination"
!
!   ELIMINATION
!   for k = 1: n-1
!       for i = k+1: n
!           a(i, k) /= a(k, k)
!       for i = k+1: n
!           for j = k+1: n
!               a(i, j) -= a(i, k) * a(k, j)
!           end
!       end
!   end
!
!   BACKSUBSTITUTION  -  L U y = f  =>  L x = f  =>  x = L \ f  =>  y = U \ x
!   for i = 1: n
!       x(i) = f(i)
!       for j = 1: i-1
!           x(i) -= a(i, j) * x(j)
!       end
!   end
!
!   for i = n: 1
!       y(i) = x(i)
!       for j = n: i+1
!           y(i) -= a(i, j) * y(j)
!       end
!       y(i) /= a(i, i)
!   end 
!!---------------------------------------------------------------------------
program ge
!!---------------------------------------------------------------------------
    use omp_lib, only: omp_get_wtime

    implicit none

    ! Matrix of coefficients; the one is filled by random_number()
    real, dimension(:, :), allocatable :: A

    ! "Analytical" solution; the one is filled by random_number()
    real, dimension(:), allocatable :: u

    ! Right-hand side (RHS); the one is calculated as f = A * u
    ! Numerical solution (NS) of the equation A y = f
    ! RHS is overwritten by NS
    real, dimension(:), allocatable :: y

    ! Size of matrix
    integer, parameter :: n = 5

    ! Time marks
    real(kind(0.d0)) :: elimination_start, elimination_finish
    real(kind(0.d0)) :: backsubstition_start, backsubstition_finish

    ! Allocate memory
    allocate(A(1: n, 1: n))
    allocate(u(1: n))
    allocate(y(1: n))

    ! Algorithm uses straightforward formulas
    call Generate_Data()
    call Straightforward_Elimination()
    call Straightforward_Backsubstition()
    call Print_Norms()
    call Print_Times()

    ! Algorithm uses Fortran 90/95 features
    call Generate_Data()
    call Fortran9x_Elimination()
    call Fortran9x_Backsubstition()
    call Print_Norms()
    call Print_Times()

    ! Algorithm uses BLAS
    call Generate_Data()
    call BLAS_Elimination()
    call BLAS_Backsubstition()
    call Print_Norms()
    call Print_Times()

    ! Free memory
    deallocate(A)
    deallocate(u)
    deallocate(y)
!!---------------------------------------------------------------------------
contains
!!---------------------------------------------------------------------------
subroutine Print_Norms()
    write (*, *) "Norms:", maxval(abs(u)), maxval(abs(y - u))
end subroutine Print_Norms
!!---------------------------------------------------------------------------
subroutine Print_Times()
    write (*, *) "Times:", &
            elimination_finish - elimination_start, &
            backsubstition_finish - backsubstition_start
end subroutine Print_Times
!!---------------------------------------------------------------------------
! This version is a simplified modification of
! http://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
subroutine Init_Random_Seed()
    integer :: i, n
    integer, dimension(:), allocatable :: seed

    call random_seed(size = n)
    allocate(seed(n))

    seed = 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(put = seed)

    deallocate(seed)
end subroutine Init_Random_Seed
!!---------------------------------------------------------------------------
subroutine Generate_Data()
    call Init_Random_Seed()
    call random_number(A)
    call random_number(u)
    y = matmul(A, u)
end subroutine Generate_Data
!!---------------------------------------------------------------------------
subroutine Straightforward_Elimination()
    integer :: i, j, k

    elimination_start = omp_get_wtime()

    do k = 1, n-1
        do i = k+1, n
            a(i, k) = a(i, k) / a(k, k)
        end do

        do j = k+1, n
            do i = k+1, n
                a(i, j) = a(i, j) - a(i, k) * a(k, j)
            end do
        end do
    end do

    elimination_finish = omp_get_wtime()
end subroutine Straightforward_Elimination
!!---------------------------------------------------------------------------
subroutine Fortran9x_Elimination()
    integer :: k

    elimination_start = omp_get_wtime()

    do k = 1, n-1
        a(k+1: n, k) = a(k+1: n, k) / a(k, k)

        a(k+1: n, k+1: n) = a(k+1: n, k+1: n) - &
                matmul(a(k+1: n, k: k), a(k: k, k+1: n))
    end do

    elimination_finish = omp_get_wtime()
end subroutine Fortran9x_Elimination
!!---------------------------------------------------------------------------
subroutine BLAS_Elimination()
    integer :: k

    elimination_start = omp_get_wtime()

    do k = 1, n-1
        ! x = a*x
        call sscal(n-k, 1.0 / a(k, k), a(k+1, k), 1)

        ! A := alpha*x*y'+ A
        call sger(n-k, n-k, -1.0, &
                a(k+1, k), 1, &
                a(k, k+1), n, &
                a(k+1, k+1), n)
    end do

    elimination_finish = omp_get_wtime()
end subroutine BLAS_Elimination
!!---------------------------------------------------------------------------
subroutine Straightforward_Backsubstition()
    integer :: i, j

    backsubstition_start = omp_get_wtime()

    ! L x = f  =>  x = L \ f
    do i = 1, n
        do j = 1, i-1
            y(i) = y(i) - a(i, j) * y(j)
        end do
    end do

    ! U y = x  =>  y = U \ x
    do i = n, 1, -1
        do j = i+1, n
            y(i) = y(i) - a(i, j) * y(j)
        end do

        y(i) = y(i) / a(i, i)
    end do

    backsubstition_finish = omp_get_wtime()
end subroutine Straightforward_Backsubstition
!!---------------------------------------------------------------------------
subroutine Fortran9x_Backsubstition()
    integer :: i

    backsubstition_start = omp_get_wtime()

    ! L x = f  =>  x = L \ f
    do i = 1, n
        y(i) = y(i) - dot_product(a(i, 1: i-1), y(1: i-1))
    end do

    ! U y = x  =>  y = U \ x
    do i = n, 1, -1
        y(i) = y(i) - dot_product(a(i, i+1: n), y(i+1: n))
        y(i) = y(i) / a(i, i)
    end do

    backsubstition_finish = omp_get_wtime()
end subroutine Fortran9x_Backsubstition
!!---------------------------------------------------------------------------
subroutine BLAS_Backsubstition()
    backsubstition_start = omp_get_wtime()

    ! L x = f  =>  x = L \ f
    
    ! op(A)*X = alpha*B
    !call strsm('L', 'L', 'N', 'U', n, 1, 1.0, a, n, y, n)

    ! A * x = b
    call strsv('L', 'N', 'U', n, a, n, y, 1)

    ! U y = x  =>  y = U \ x

    ! op(A)*X = alpha*B
    !call strsm('L', 'U', 'N', 'N', n, 1, 1.0, a, n, y, n)

    ! A * x = b
    call strsv('U', 'N', 'N', n, a, n, y, 1)

    backsubstition_finish = omp_get_wtime()
end subroutine BLAS_Backsubstition
!!---------------------------------------------------------------------------
end program ge
!!---------------------------------------------------------------------------
