module mComponents
    use mFileHandling, only : safeopen_writereplace

    implicit none

    integer :: kJobs
    integer :: io_out

    character ( len = 3 ) :: who

contains

    subroutine pbs_script ( account = account, queue = queue, cores = cores, walltime = walltime, machine = machine, nJobs = nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: account, queue, cores, walltime, machine

        ! write pbs files
        do kJobs = 1, jobs
            write ( unit = nJobs, fmt = '( I3.3 )' ) kJobs
            io_out = safeopen_writereplace ( machine // '/cpu/cpu_' // who )

            write ( io_out, 100 )
            write ( io_out, 110 )
            write ( io_out, 200 ) 'A', trim ( account )
            write ( io_out, 200 ) 'q', trim ( queue )
            write ( io_out, 200 ) 'l', trim ( cores )
            write ( io_out, 200 ) 'l', trim ( wall_time )
            write ( io_out, 200 ) 'j', 'oe'
            write ( io_out, 200 ) 'V', ''
            write ( io_out, 120 )
            write ( io_out, 130 ) '$dir_mersenne'
            write ( io_out, 130 ) who // '/'
            write ( io_out, 120 )
            write ( io_out, 140 )
            write ( io_out, 120 )

            close ( io_out )
        end do

    100 format ( '#!/bin/bash' )
    110 format ( '## Required PBS Directives --------------------------------------' )
    120 format ( '' )
    130 format ( 'cd ', g0 )
    140 format ( './cpu_characterization -d > timing_results.out' )

    200 format ( '#PBS -', g0, ' ', g0 )

    end subroutine pbs_script

    subroutine launcher_script ( machine = machine, nJob = nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: who

            io_out = safeopen_writereplace ( machine // '/launcher' )
            do kJobs = 1, nJobs
                write ( io_out, 300 )
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                write ( io_out, 310 ) who // '/'
                write ( io_out, 320 ) who
                write ( io_out, '( "" )' )
            end do
            close ( io_out )

        300 format ( 'cd $dir_mersenne' )
        310 format ( 'cd cpu/', g0 )
        320 format ( 'qsub topaz_cpu_', g0 )

    end subroutine launcher_script


end module mComponents
