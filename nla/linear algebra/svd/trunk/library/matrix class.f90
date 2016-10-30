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
!> matrices and their manipulation
!
! REVISION HISTORY:
! 10 Oct 2014 - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

! Hansen and Tompkins, p. 22
module matrix_class

  use basic_parameters
  implicit NONE

  type, public                                   :: matrix                           ! name to instantiate

!    private
    real ( wp ), allocatable, dimension ( :, : ) :: elements      ! nodes where the fields live

    integer ( is )                               :: nRows = 0, nCols = 0, rank = -1

  end type matrix

end module matrix_class