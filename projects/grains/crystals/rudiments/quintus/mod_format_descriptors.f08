! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mFormatDescriptors

    implicit none

    character ( len = * ), parameter :: fmt_results     = '( g0, " = ", g0, " +/- ", g0, "." )'

    character ( len = * ), parameter :: fmt_read_array  = '( /, "Array of ", g0, " elements read in from ", g0, "." )'
    character ( len = * ), parameter :: fmt_x           = '( "( ", g0, " ( I4, 2X ) )" )'
    !character ( len = * ), parameter :: fmt_x          = '( /, "Partitions: ", g0, " ( I4, 2X )" )'

    ! IO errors
    character ( len = * ), parameter :: fmt_ioerror     = '( /, g0, " error for file ", g0, " on newunit ", g0 )'
    character ( len = * ), parameter :: fmt_iostat      = '( "iostat = ", g0 )'
    character ( len = * ), parameter :: fmt_iomsg       = '( "iomsg  = ", g0, "." )'
    character ( len = * ), parameter :: fmt_stem        = '( "action = ", g0, ", access = ", g0, ", format = ", g0,'
    character ( len = * ), parameter :: fmt_iosettings1 = fmt_stem // ' ".", / )'
    character ( len = * ), parameter :: fmt_iosettings2 = fmt_stem // ' "status = ", g0, ".", / )'
    character ( len = * ), parameter :: fmt_iosettings3 = '( "status = ", g0, ", action = ", g0, ", position = ", g0, "." )'
    character ( len = * ), parameter :: fmt_iosettings4 = '( "status = ", g0, ", action = ", g0, "." )'
    character ( len = * ), parameter :: fmt_iocont      = '( "execution continues ...", / )'

    ! allocation errors
    character ( len = * ), parameter :: fmt_allocerror  = '( "Failure to ", g0, "allocate the array ", g0, " which has ", g0,' // &
                                                          '  " elements of type ", g0, "." )'
    character ( len = * ), parameter :: fmt_allocstat   = '( "stat = ", g0 )'
    character ( len = * ), parameter :: fmt_allocmsg    = '( "errmsg  = ", g0, "." )'

    ! shared output formats
    character ( len = * ), parameter :: fmt_tail        = '3X, E25.15, "+/-", E25.15 )'
    character ( len = * ), parameter :: fmt_fortran     = '( g0, " Fortran    ", ' // fmt_tail
    character ( len = * ), parameter :: fmt_mathematica = '( g0, " Mathematica", ' // fmt_tail
    character ( len = * ), parameter :: fmt_difference  = '( "Difference", 19X, E10.3,  18X, E10.3, / )'

end module mFormatDescriptors
