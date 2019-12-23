program kinder

    use, intrinsic :: iso_fortran_env, only : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

    implicit none

    ! kind parameters
    ! INTEGERS
    integer, parameter :: aint = INT8
    integer, parameter :: sint = INT16
    integer, parameter :: lint = INT32
    integer, parameter :: zint = INT64

    ! REALS
    integer, parameter :: sp = REAL32
    integer, parameter :: dp = REAL64
    integer, parameter :: qp = REAL128

    ! CHARACTERS
    integer, parameter :: def     = selected_char_kind ( 'DEFAULT' )    ! required by Fortran standard
    integer, parameter :: kindA   =               kind ( 'A' )          ! Metcalf, Reid, Cohen: p. 309
    integer, parameter :: ascii   = selected_char_kind ( 'ASCII' )      ! optional

        print *, 'aint                        = ', aint
        print *, 'INT8                        = ', INT8
        print *

        print *, 'sint                        = ', sint
        print *, 'INT16                       = ', INT16
        print *

        print *, 'lint                        = ', lint
        print *, 'INT32                       = ', INT32
        print *

        print *, 'zint                        = ', zint
        print *, 'INT64                       = ', INT64
        print *

        print *, 'sp                            = ', sp
        print *, 'REAL32                        = ', REAL32
        print *

        print *, 'dp                            = ', dp
        print *, 'REAL64                        = ', REAL64
        print *

        print *, 'qp                            = ', qp
        print *, 'REAL128                       = ', REAL128
        print *

        print *, 'def                              = ', def
        print *

        print *, 'kindA                            = ', kindA
        print *, 'kind ( "A" )                     = ', kind ( "A" )
        print *

        print *, 'ascii                            = ', ascii
        print *, 'selected_char_kind ( "ASCII" )   = ', kind ( "A" )
        print *

end program kinder
