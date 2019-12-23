! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mReadLAMMPS

    use iso_fortran_env,       only : iostat_end
    use mPrecisionDefinitions, only : ip, rp, zero
    use mParametersSimulation
    use mFormatDescriptors
    use mShared

    implicit none

    integer ( ip )                             :: kData = 0
    integer ( ip )                             :: io_unit = 0
    character ( len = * ), parameter, private  :: me_mDataStructures = 'module mReadLAMMPS'  ! self-identification

    type :: LAMMPS_data
        real ( rp ), dimension ( 1 : numData ) :: x = zero, y = zero, phi = zero
    contains
        private
        procedure, public :: read_data         => read_data_sub
        procedure, public :: read_xyphi        => read_xyphi_sub
        procedure, public :: read_potentials   => read_potentials_sub
    end type LAMMPS_data

    private :: read_data_sub
    private :: read_xyphi_sub
    private :: read_potentials_sub

contains

    !       ############################################################################################
    !       #                                                                                          #
    !       #  Read *.txt                                                                              #
    !       #                                                                                          #
    !       ############################################################################################

    !       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  read_data

    subroutine read_data_sub ( me )

        class ( LAMMPS_data ), target          :: me

            call read_xyphi_sub ( me )
            call read_potentials_sub ( me )

    end subroutine read_data_sub

    !       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                  read_xyphi

    subroutine read_xyphi_sub ( me )

        class ( LAMMPS_data ), target          :: me

        character ( len = * ), parameter       :: myAction = 'read', myAccess = 'stream', myForm = 'formatted'
        character ( len = * ), parameter       :: myRoutine = 'subroutine read_xyphi_sub'  ! self-identification
        character ( len = * ), parameter       :: stop_msg = 'Execution ending due to error in ' // me_mDataStructures // &
                                                                                            ', ' // myRoutine
            ! open file for reading
            open ( newunit = io_unit, file =  path_data // file_xy_data, action = myAction, access = myAccess, form = myForm, &
                    iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror     ) 'OPEN', path_data // file_xy_data, io_unit
                write ( * , fmt = fmt_iostat      ) io_stat
                write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                stop stop_msg
            end if

            ! read file
            read_loop : do kData = 1, numData
                read  ( io_unit, *, iostat = io_stat, iomsg = io_msg ) me % x ( kData ), me % y ( kData )!, me % phi ( kData )
                if ( io_stat == iostat_end ) exit !EOF
                kRead = kData
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror     ) 'READ', path_data // file_xy_data, io_unit
                    write ( * , fmt = fmt_iostat      ) io_stat
                    write ( * , fmt = fmt_iomsg       ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                    stop stop_msg
                end if
            end do read_loop

            write ( * , fmt = fmt_read_array ) kRead, path_data // file_xy_data

            ! close file
            close ( unit = io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', path_data // file_xy_data, io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

            return

    end subroutine read_xyphi_sub

    !       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             read_potentials

    subroutine read_potentials_sub ( me )

        class ( LAMMPS_data ), target          :: me

        character ( len = * ), parameter       :: myAction = 'read', myAccess = 'stream', myForm = 'formatted'
        character ( len = * ), parameter       :: myRoutine = 'subroutine read_potentials_sub'  ! self-identification
        character ( len = * ), parameter       :: stop_msg = 'Execution ending due to error in ' // me_mDataStructures // &
                                                                                            ', ' // myRoutine
            ! open file for reading
            open ( newunit = io_unit, file = path_data // file_potentials, action = myAction, access = myAccess, form = myForm, &
                    iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror    ) 'OPEN', path_data // file_potentials, io_unit
                write ( * , fmt = fmt_iostat     ) io_stat
                write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                stop stop_msg
            end if

            ! read file
            read_loop : do kData = 1, numData
                read  ( io_unit, *, iostat = io_stat, iomsg = io_msg ) me % phi ( kData )
                if ( io_stat == iostat_end ) exit !EOF
                kRead = kData
                if ( io_stat /= 0 ) then
                    write ( * , fmt = fmt_ioerror    ) 'READ', path_data // file_potentials, io_unit
                    write ( * , fmt = fmt_iostat     ) io_stat
                    write ( * , fmt = fmt_iomsg      ) trim ( io_msg )
                    write ( * , fmt = fmt_iosettings1 ) myAction, myAccess, myForm
                    stop stop_msg
                end if
            end do read_loop

            write ( * , fmt = fmt_read_array ) kRead, path_data // file_potentials

            ! close file
            close ( unit = io_unit, iostat = io_stat, iomsg = io_msg )
            if ( io_stat /= 0 ) then
                write ( * , fmt = fmt_ioerror ) 'CLOSE', path_data // file_potentials, io_unit
                write ( * , fmt = fmt_iostat  ) io_stat
                write ( * , fmt = fmt_iomsg   ) trim ( io_msg )
                write ( * , fmt = fmt_iocont  )
            end if

            return

  end subroutine read_potentials_sub

end module mReadLAMMPS
