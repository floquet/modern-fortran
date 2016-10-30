program binaryio

    use iso_fortran_env, only : compiler_options, compiler_version, REAL64, INT64
    implicit none

    integer, parameter        :: rp = REAL64, ip = INT64
    integer ( ip ), parameter :: nPts = 10737418240
    integer                   :: io_unit, io_status, k

    character ( len = * ), parameter :: myFile = 'list.r64'
    character ( len = 128 )          :: io_msg

        write ( * , '( "compiler version: ", g0 )' ) compiler_version ( )
        write ( * , '( "compiler options: ", g0 )' ) compiler_options ( )

        ! write a binary file
        open ( newunit = io_unit, file = myFile, &
                action = 'WRITE', status = 'REPLACE', form = 'UNFORMATTED', iostat = io_status, iomsg = io_msg )
                write ( unit = io_unit, iostat = io_status, iomsg = io_msg ) [ ( sin ( real ( k, rp ) ), k = 1, nPts ) ]
        close ( unit = io_unit, iostat = io_status, iomsg = io_msg )

        stop 'Successful run for "binaryio.f08"'

end program binaryio
