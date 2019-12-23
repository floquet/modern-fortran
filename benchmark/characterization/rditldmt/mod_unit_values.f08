module mUnitValues

    ! USE DECLARATIONS
    use, intrinsic :: iso_fortran_env,  only : REAL32, REAL64, REAL128, INT8, INT16, INT32, INT64

    ! DATA DECLARATIONS
    implicit none

    ! UNIT VALUES
    real,                                       parameter :: xdef = 1.0
    real ( selected_real_kind ( REAL32 ) ),     parameter :: x032 = 1.0_REAL32
    real ( selected_real_kind ( REAL64 ) ),     parameter :: x064 = 1.0_REAL64
    real ( selected_real_kind ( REAL128 ) ),    parameter :: x128 = 1.0_REAL128

    complex,                                    parameter :: cdef = ( xdef, xdef )
    complex ( selected_real_kind ( REAL32 ) ),  parameter :: c032 = ( x032, x032 )
    complex ( selected_real_kind ( REAL64 ) ),  parameter :: c064 = ( x064, x064 )
    complex ( selected_real_kind ( REAL128 ) ), parameter :: c128 = ( x128, x128 )

    integer,                                    parameter :: idef = 1
    integer ( kind ( INT8 ) ),                  parameter :: i08  = 1_INT8
    integer ( kind ( INT16 ) ),                 parameter :: i16  = 1_INT16
    integer ( kind ( INT32 ) ),                 parameter :: i32  = 1_INT32
    integer ( kind ( INT64 ) ),                 parameter :: i64  = 1_INT64

end module mUnitValues
