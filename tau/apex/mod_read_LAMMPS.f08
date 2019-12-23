! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mReadLAMMPS

    use iso_fortran_env,       only : iostat_end
    use mPrecisionDefinitions, only : ip, rp
    use mConstants,            only : zero

    use mFileHandling,         only : safeopen_readonly
    use mParametersSimulation, only : numData, path_data, file_xy_data, file_potentials

    implicit none

    integer ( ip ), private                   :: kData, kRead
    integer ( ip ), private                   :: io_unit, io_stat
    character ( len = * ), private, parameter :: fmt_read_array  = '( /, "Array of ", g0, " elements read in from ", g0, "." )'

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

    !       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +           read_data

    subroutine read_data_sub ( me )

        class ( LAMMPS_data ), target :: me

            call read_xyphi_sub ( me )
            call read_potentials_sub ( me )

    end subroutine read_data_sub

    !       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +          read_xyphi

    subroutine read_xyphi_sub ( me )

        class ( LAMMPS_data ), target :: me

            io_unit = safeopen_readonly ( filename = path_data // file_xy_data )

            ! read to EOF
            read_loop : do kData = 1, numData
                read  ( io_unit, *, iostat = io_stat ) me % x ( kData ), me % y ( kData ) !, me % phi ( kData )
                if ( io_stat == iostat_end ) exit ! EOF
                kRead = kData
            end do read_loop

            close ( unit = io_unit )

            return

    end subroutine read_xyphi_sub

    !       +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +     read_potentials

    subroutine read_potentials_sub ( me )

        class ( LAMMPS_data ), target :: me

            io_unit = safeopen_readonly ( filename = path_data // file_potentials )

            ! read to EOF
            read_loop : do kData = 1, numData
                read  ( io_unit, *, iostat = io_stat ) me % phi ( kData )
                if ( io_stat == iostat_end ) exit ! EOF
                kRead = kData
            end do read_loop

            close ( unit = io_unit )

    end subroutine read_potentials_sub

end module mReadLAMMPS
