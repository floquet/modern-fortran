MODULE xmod

  ABSTRACT INTERFACE
  FUNCTION function_template(n,x) RESULT(y)
      INTEGER, INTENT(in) :: n
      REAL, INTENT(in) :: x(n)
      REAL :: y
  END FUNCTION function_template
  END INTERFACE

CONTAINS

  SUBROUTINE execute_function(n,x,func,y)
    INTEGER, INTENT(in) :: n
    REAL, INTENT(in) :: x(n)
    PROCEDURE(function_template), POINTER :: func
    REAL, INTENT(out) :: y
    y = func(n,x)
  END SUBROUTINE execute_function

END MODULE xmod


PROGRAM xprog

  USE xmod

  REAL :: x(4), y
  PROCEDURE(function_template), POINTER :: func

  x = [1.0, 2.0, 3.0, 4.0]

  func => summation
  CALL execute_function(4,x,func,y)

    func => ssummation
    CALL execute_function(4,x,func,y)

  PRINT*, y  ! should give 10.0

CONTAINS

  FUNCTION summation(n,x) RESULT(y)
    INTEGER, INTENT(in) :: n
    REAL, INTENT(in) :: x(n)
    REAL :: y
    y = SUM(x)
  END FUNCTION summation

  FUNCTION ssummation(n,x) RESULT(y)
    INTEGER, INTENT(in) :: n
    REAL, INTENT(in) :: x(n)
    REAL :: y
    y = SUM( x * x )
  END FUNCTION ssummation

END PROGRAM xprog
