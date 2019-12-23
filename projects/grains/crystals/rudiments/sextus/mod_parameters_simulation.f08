! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mParametersSimulation

    use mPrecisionDefinitions, only : ip

    implicit none

    ! parameters
    integer ( ip ),        parameter       :: numData = 1024
    character ( len = * ), parameter       :: path_data       = '../../data/input/apple/'
    character ( len = * ), parameter       :: file_xy_data    = 'xy_data.txt'
    character ( len = * ), parameter       :: file_potentials = 'potentials.txt'

end module mParametersSimulation
