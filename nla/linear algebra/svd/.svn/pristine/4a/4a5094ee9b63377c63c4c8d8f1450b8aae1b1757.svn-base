!------------------------------------------------------------------------------
! HPCMPO PETTT Program
!------------------------------------------------------------------------------
!
! MODULE: Main program
!
!> @author
!> Daniel Topa PETTT ACE on-site, Vicksburg
!
! DESCRIPTION:
!> sample matrices
!
! REVISION HISTORY:
! 10 Oct 2014 - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

! Hansen and Tompkins, p. 22
module examples

  use basic_parameters
  implicit NONE

  ! matrices: enter by columns, when rank > 1 use reshape ( Metcalf, Reid, Cohen, p. 136 )

  ! identity matrix
  real ( wp )                 :: matrix_a ( 1 : 2, 1 : 2 ) = reshape ( [ [ one, two ], [ -one, two ] ], [ 2, 2 ] )
  real ( wp )                 :: U_matrix_a ( 1 : 2, 1 : 2 ) = reshape ( [ [ zero, one ], [ -one, zero ] ], [ 2, 2 ] )
  real ( wp )                 :: V_matrix_a ( 1 : 2, 1 : 2 ) = reshape ( [ [ one, one ], [ one, -one ] ], [ 2, 2 ] ) / sqrt( two )
  real ( wp )                 :: S_matrix_a ( 1 : 2, 1 : 2 ) = reshape ( [ [ two, zero ], [ zero, one ] ], [ 2, 2 ] ) / sqrt( two )
  real ( wp )                 :: Sigma_matrix_a ( 1 : 2, 1 : 2 ) = S_matrix_a

end module examples