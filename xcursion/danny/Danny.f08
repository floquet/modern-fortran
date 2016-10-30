! Danny's first fortran program
program Danny
    implicit none
    real :: a, b
    real :: area, perimeter
    
    ! read parameters
        print *, "input a:"
        read *, a
        
        print *, 'input b:'
        read *, b
        
        ! rectangle rules
        area = a * b
        perimeter = 2 * ( a + b )
        
        print *, "rectangle area = ", area
        print *, "rectangle perimeter = ", perimeter
        
        ! right triangle rules
        area = a * b / 2
        perimeter = a + b + sqrt( a**2 + b**2 )
        
        print *, "triangle area = ", area
        print *, "triangle perimeter = ", perimeter
        
    
end program Danny
!
! Muntz-Szasz:danny dantopa$ date
! Thu Dec 31 14:52:40 MST 2015
! Muntz-Szasz:danny dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/projects/danny
! Muntz-Szasz:danny dantopa$ gfortran Danny.f08
! Muntz-Szasz:danny dantopa$ ./a.out
!  input a:
! 2
!  input b:
! 3
!  rectangle area =    6.00000000
!  rectangle perimeter =    10.0000000
!  triangle area =    3.00000000
!  triangle perimeter =    8.60555077
