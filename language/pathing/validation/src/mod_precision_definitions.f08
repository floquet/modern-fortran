! Hansen and Tompkins, p. 22
! make all real variables _rp ( working precision )
! then real precision is controlled by one setting
module mPrecisionDefinitions

    use, intrinsic :: iso_fortran_env, only : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

    implicit none

    ! kind parameters
    ! INTEGERS
    integer, parameter          :: aint    = selected_int_kind  ( INT8 )
    integer, parameter          :: sint    = selected_int_kind  ( INT16 )
    integer, parameter          :: lint    = selected_int_kind  ( INT32 )
    integer, parameter          :: zint    = selected_int_kind  ( INT64 )

    ! REALS
    integer, parameter          :: sp      = selected_real_kind ( REAL32 )
    integer, parameter          :: dp      = selected_real_kind ( REAL64 )
    integer, parameter          :: qp      = selected_real_kind ( REAL128 )

    ! CHARACTERS
    integer, parameter          :: def     = selected_char_kind ( 'DEFAULT' )    ! required by Fortran standard
    integer, parameter          :: kindA   =               kind ( 'A' )          ! Metcalf, Reid, Cohen: p. 309
    integer, parameter          :: ascii   = selected_char_kind ( 'ASCII' )      ! optional

    ! Set working precision
    integer, parameter          :: rp      = dp
    integer, parameter          :: ip      = zint

end module mPrecisionDefinitions
