module mSetPrecision

    use, intrinsic :: iso_fortran_env, only : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

    implicit none

    ! kind parameters

        ! REALS
        integer, parameter :: sp = REAL32
        integer, parameter :: dp = REAL64
        integer, parameter :: qp = REAL128

        ! CHARACTERS
        integer, parameter :: def     = selected_char_kind ( 'DEFAULT' )    ! required by Fortran standard
        integer, parameter :: kindA   =               kind ( 'A' )          ! Metcalf, Reid, Cohen: p. 309
        integer, parameter :: ascii   = selected_char_kind ( 'ASCII' )      ! optional

    ! Define working precision: Hansen and Tompkins, p. 22
    integer, parameter :: rp = REAL32
    integer, parameter :: ip = INT64

end module mSetPrecision
