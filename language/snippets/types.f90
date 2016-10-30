program main

! USE
  use iso_fortran_env

! DECLARATIONS
  implicit none

  integer :: rkind

  real                                    :: xdef = 1.0
  real ( selected_real_kind ( REAL32 ) )  :: x32  = 1.0
  real ( selected_real_kind ( REAL64 ) )  :: x64  = 1.0
  real ( selected_real_kind ( REAL128 ) ) :: x128 = 1.0

!  character ( kind = ascii, len = 255 )   :: file_name = " ", io_msg = " "

    rkind = kind ( xdef )
    print *, 'rkind = ', rkind

!    select type ( xdef )
!      type is ( real )
!        print *, 'default precision'
!      type is ( real ( selected_real_kind ( REAL32 ) ) )
!        print *, 'single precision'
!      type is ( real ( selected_real_kind ( REAL64 ) ) )
!        print *, 'double precision'
!      type is ( real ( selected_real_kind ( REAL128 ) ) )
!        print *, 'quadruple precision'
!      type default
!        print *, 'no match'
!    end select

end program main