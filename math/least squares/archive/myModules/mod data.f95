!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module data

    use precision_definitions, only : is, wp, zero

    implicit none

    integer ( is ), parameter :: numDims         =        2
    integer ( is ), parameter :: maxDataPts      =  1048576

    real ( wp ),    parameter :: origin ( 1 : numDims ) =  [ zero, zero ]

end module data
