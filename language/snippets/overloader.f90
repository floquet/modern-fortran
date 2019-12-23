!
!	overloader.f90
!	
!
!	Created by Daniel M. Topa on 6/16/13.
! ftp://ftp.nag.co.uk/sc22wg5/N1551-N1600/N1579.pdf

include '/Users/dantopa/Dropbox/Fortran/library/data_types.f90'

module class_error 

      use data_types
      implicit NONE

      TYPE real_err
        real ( dp ) :: num
        real ( dp ) :: err
        CONTAINS
          GENERIC :: OPERATOR   ( + ) => plus1!, plus2, plus3
          GENERIC :: ASSIGNMENT ( = ) => equal_ss!, assign2
      END TYPE
      
      contains

subroutine equal_ss ( lhs, rhs )

      type ( real_err ), intent ( out ) :: lhs  ! left  side of operator =
      type ( real_err ), intent ( in )  :: rhs  ! right side of operator =

      lhs % num = rhs % num
      lhs % err = rhs % err

end subroutine equal_ss

elemental function plus1 ( a, b ) result ( c )

      use data_types
      
      type ( real_err ), intent ( in ) :: a, b
      type ( real_err )                :: c
      
      c % num =          a % num      +   b % num
      c % err = dsqrt( ( a % err )**2 + ( b % err )**2 )

end function plus1

end module class_error 