! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 23456789 023456789 123456789 223456789 32
module mSystemInfo

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options

    use mFileHandling,                  only : safeopen_writereplace, safeopen_readonly
    use mSetPrecision,                  only : ip
    use mAllocationTimes,               only : data_type, measurements

    implicit none ! protects all methods in scope (module and submodules)

    character ( len = * ), parameter :: myPath = '/transporter/memtimes/'

    type :: environment_variables
        character ( len = 512 ) :: wdhpc, pbs_jobnumber, machine_name, pbs_jobid, pbs_jobname
    contains
        private
        procedure, public :: get_env_var => get_env_var_sub
    end type environment_variables

    type :: system_info
        character ( len = 512 ) :: host, run_cmd
        character ( len =  10 ) :: myTime
        character ( len =   8 ) :: myDate
        character ( len =   4 ) :: year
        character ( len =   2 ) :: month, day, hour, min, sec
    contains
        private
        procedure, public :: get_system_var => get_system_var_sub
    end type system_info

    type :: file_names
        character ( len = 512 )        :: myDir, FileNameSummary, FileNameSequence
        type ( environment_variables ) :: thisEnvVar
        type ( system_info )           :: thisSysInfo
    contains
        private
        procedure, public :: build_names => build_names_sub
    end type file_names

    private :: get_system_var_sub, get_env_var_sub, build_names_sub
    !procedure, public :: build_names => build_names_sub

contains

    subroutine get_env_var_sub ( me )

        class ( environment_variables ), target :: me

            call get_environment_variable ( "wdhpc",       me % wdhpc )         ! e.g. root for git repos
            call get_environment_variable ( "JOBID",       me % pbs_jobnumber ) ! e.g. topaz-big-pbs
            call get_environment_variable ( "BC_HOST",     me % machine_name )  ! e.g. topaz
            call get_environment_variable ( "PBS_JOBID",   me % pbs_jobid )     ! e.g. 123456
            call get_environment_variable ( "PBS_JOBNAME", me % pbs_jobname )   ! e.g. topaz-big-pbs

            ! in case program runs on local machine
            if ( len ( trim ( me % pbs_jobid )     ) == 0 ) me % pbs_jobid     = 'jobid'
            if ( len ( trim ( me % pbs_jobname )   ) == 0 ) me % pbs_jobname   = 'jobname'
            if ( len ( trim ( me % pbs_jobnumber ) ) == 0 ) me % pbs_jobnumber = 'jobnumber'

    end subroutine get_env_var_sub

    subroutine get_system_var_sub ( me )

        class ( system_info ), target :: me

        integer :: host_status

            host_status =  hostnm ( me % host ) ! r23i7n7
            call get_command ( me % run_cmd )   ! grabs run command
            call date_and_time ( DATE = me % myDate, TIME = me % myTime )

            me % year  = me % myDate ( 1 : 4 )
            me % month = me % myDate ( 5 : 6 )
            me % day   = me % myDate ( 7 : 8 )

            me % hour = me % myTime ( 1 : 2 )
            me % min  = me % myTime ( 3 : 4 )
            me % sec  = me % myTime ( 5 : 6 )

    end subroutine get_system_var_sub

    subroutine build_names_sub ( me )

        class ( file_names ), target :: me

        character ( len = 512 ) :: stem_name

        !character ( len = 512 ) :: myDir, myFileName
            call get_system_var_sub ( me % thisSysInfo )
            call get_env_var_sub    ( me % thisEnvVar )

            ! /p/home/dantopa/hpc/transporter/memtimes/topaz
            me % myDir = trim ( me % thisEnvVar % wdhpc ) // myPath // trim ( me % thisEnvVar % machine_name ) // '/'
            ! topaz_lmem02_R64_596879
            stem_name = trim ( me % thisEnvVar % machine_name ) // '_' // trim ( me % thisSysInfo % host ) // '_' // &
                        data_type // '_' // trim ( me % thisEnvVar % pbs_jobnumber )

            me % FileNameSummary  = trim ( me % myDir ) // trim ( stem_name ) // '.txt'
            me % FileNameSequence = trim ( me % myDir ) // trim ( stem_name ) // '.dat'

    end subroutine build_names_sub

    subroutine write_header_sub ( io_summary, io_sequence )

        integer,               intent ( out ) :: io_sequence, io_summary ! io handles

        integer                               :: io_version, io_status
        character ( len = * ), parameter      :: file_fortran_version = 'fortran_version.txt'
        character ( len = 512 )               :: str_fortran_version  = '', io_message

        type ( file_names ), target  :: myFileNames
        type ( file_names ), pointer :: p

            p => null ()
            call myFileNames % build_names ( )

            print *, 'myFileNames % myDir            = ', trim ( myFileNames % myDir ), '.'
            print *, 'myFileNames % FileNameSummary  = ', trim ( myFileNames % FileNameSummary ), '.'
            print *, 'myFileNames % FileNameSequence = ', trim ( myFileNames % FileNameSequence ), '.'

            io_summary  = safeopen_writereplace ( myFileNames % FileNameSummary )
            io_sequence = safeopen_writereplace ( myFileNames % FileNameSequence )
            !io_version  = safeopen_readonly     ( file_fortran_version )

            ! grab command line output for gfortran --version
            write ( io_summary, '( ''gfortran --version:'' )' )
            call execute_command_line ( 'gfortran --version >> ' // file_fortran_version )
            io_version = safeopen_readonly ( file_fortran_version )
            do
                read  ( unit = io_version, fmt = 100, iostat = io_status, iomsg = io_message ) str_fortran_version
                if ( io_status /= 0 ) exit ! EOF
                write ( unit = io_summary, fmt = 100 ) trim ( str_fortran_version )
            end do
            call execute_command_line ( 'rm ' // file_fortran_version ) ! clean up and remove file

            write ( io_summary, 110 ) compiler_version ( )
            write ( io_summary, 120 ) compiler_options ( )

            p => myFileNames
                write ( io_summary, 200 ) trim ( p % thisEnvVar % machine_name ), trim ( p % thisSysInfo % host )

                write ( io_summary, 210 ) trim ( p % thisEnvVar % pbs_jobid )
                write ( io_summary, 220 ) trim ( p % thisEnvVar % pbs_jobname )
                write ( io_summary, 230 ) trim ( p % thisEnvVar % pbs_jobnumber )

                write ( io_summary, 240 ) trim ( p % thisSysInfo % run_cmd )
                write ( io_summary, 250 ) p % thisSysInfo % year, p % thisSysInfo % month, p % thisSysInfo % day, &
                                          p % thisSysInfo % hour, p % thisSysInfo % min,   p % thisSysInfo % sec
            p => null( )

            write ( io_summary, 300 ) measurements
            write ( io_summary, 310 ) trim ( myFileNames % FileNameSequence )
            write ( io_summary, 320 )

        100 format ( A )
        110 format ( /, 'Fortran compiler version: ', A )
        120 format (    'Fortran compiler options: ', A, / )

        200 format ( 'Machine: ', A, ', node: ', A )
        210 format ( 'PBS job ID:     ', A )
        220 format ( 'PBS job name:   ', A )
        230 format ( 'PBS job number: ', A )
        240 format ( "Program launched via ", A, "." )
        250 format ( "Date: ", A, " - ", A, " - ", A, ", time: ", A, ":", A, ":", A, / )

        300 format ( 'Number of times each measurement is repeated: ', I5 )
        310 format ( 'Measurements written to file ', A, / )
        320 format ( 'column order: array size (elements), array size (GB), time mean, time s.d., time min, time max', / )

    end subroutine write_header_sub

end module mSystemInfo
