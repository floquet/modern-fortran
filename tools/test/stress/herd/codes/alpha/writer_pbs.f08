program writer_pbs
    use mFileHandling, only : safeopen_writereplace

    implicit none

    integer, parameter :: jobs = 100
    character ( len = * ), parameter :: account   = 'ERDCS97270PET'
    character ( len = * ), parameter :: queue     = 'background'
    character ( len = * ), parameter :: cores     = 'select=1:ncpus=32:mpiprocs=32'
    character ( len = * ), parameter :: wall_time = '4:00:00'

    integer :: kJobs  = 0
    integer :: io_pbs = 0

    character ( len = 3 ) :: who = ''

        do kJobs = 1, jobs
            write ( unit = who, fmt = '( I3.3 )' ) kJobs
            io_pbs = safeopen_writereplace ( 'pbs/garnet_' // who )

            write ( io_pbs, 100 )
            write ( io_pbs, 110 )
            write ( io_pbs, 200 ) 'A', trim ( account )
            write ( io_pbs, 200 ) 'q', trim ( queue )
            write ( io_pbs, 200 ) 'l', trim ( cores )
            write ( io_pbs, 200 ) 'l', trim ( wall_time )
            write ( io_pbs, 200 ) 'j', 'oe'
            write ( io_pbs, 120 )
            write ( io_pbs, 130 ) '$dir_mersenne'
            write ( io_pbs, 130 ) 'garnet_' // who // '/'
            write ( io_pbs, 120 )
            write ( io_pbs, 140 )
            write ( io_pbs, 120 )

            close ( io_pbs )
        end do

    100 format ( '#!/bin/bash' )
    110 format ( '## Required PBS Directives --------------------------------------' )
    120 format ( '' )
    130 format ( 'cd ', g0 )
    140 format ( './cpu_characterization -d > timing_results.out' )

    200 format ( '#PBS -', g0, ' ', g0 )

end program writer_pbs
