module mComponents
    use mFileHandling, only : safeopen_writereplace

    implicit none

    integer :: kJobs
    integer :: io_out
    integer :: status_chdir

    character ( len = 512 )          :: cwd
    character ( len =   3 )          :: who
    character ( len = * ), parameter :: myDir = '$wdmersenne', exec = 'cpu_calibrate'

contains

    subroutine pbs_script ( account, queue, proc, cores, walltime, machine, nJobs, launch )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: account, queue, cores, walltime, machine, proc, launch

        do kJobs = 1, nJobs
            write ( unit = who, fmt = '( I3.3 )' ) kJobs
            call execute_command_line ( 'mkdir ' // who )
            io_out = safeopen_writereplace ( trim ( cwd ) // '/' // who // '/' // machine // '-' // trim ( proc ) // '-' // who )

            write ( io_out, 100 )
            write ( io_out, 110 )
            write ( io_out, 200 ) 'A', trim ( account )
            write ( io_out, 200 ) 'q', trim ( queue )
            write ( io_out, 200 ) 'l', trim ( cores )
            write ( io_out, 200 ) 'l', trim ( walltime )
            write ( io_out, 200 ) 'j', 'oe'
            write ( io_out, 200 ) 'V', ''
            write ( io_out, 120 )
            write ( io_out, 130 ) myDir // '/' // trim ( proc ) // '/' // who // '/'
            write ( io_out, 120 )
            write ( io_out, 140 ) launch, exec
            write ( io_out, 120 )

            close ( io_out )
        end do

    100 format ( '#!/bin/bash' )
    110 format ( '## Required PBS Directives --------------------------------------' )
    120 format ( '' )
    130 format ( 'cd ', g0 )
    140 format ( g0, ' ./', g0, ' -d > timing_results.out' )

    200 format ( '#PBS -', g0, ' ', g0 )

    end subroutine pbs_script

    subroutine launcher_script ( machine, proc, nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: machine, proc

            io_out = safeopen_writereplace ( trim ( cwd ) // '/launcher' )
            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                write ( io_out, 300 ) myDir, trim ( proc ), who, machine, trim ( proc ), who
                write ( io_out, '( "" )' )
            end do
                write ( io_out, '( "cd ", g0 )' ) myDir
            close ( io_out )

        300 format ( 'qsub ', g0, '/', g0, '/', g0, '/', g0, '-', g0, '-', g0 )

    end subroutine launcher_script

    subroutine spreader_script ( nJobs )
        integer,               intent ( in ) :: nJobs
        !character ( len = * ), intent ( in ) :: machine, proc

            io_out = safeopen_writereplace ( trim ( cwd ) // '/spreader' )

            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                write ( io_out, 300 ) exec, who // '/'
                write ( io_out, '( "" )' )
            end do

            close ( io_out )

        300 format ( 'cp ../', g0, ' ', g0, '.' )

    end subroutine spreader_script

    subroutine prime_txt ( nJobs )
        integer,               intent ( in ) :: nJobs

            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                io_out = safeopen_writereplace ( trim ( cwd ) // '/' // who // '/prime.txt' )
                    write ( io_out, 100 )
                    write ( io_out, 110 )
                    write ( io_out, 120 )
                    write ( io_out, 130 )
                    write ( io_out, 140 )
                    write ( io_out, 150 )
                    write ( io_out, 160 )
                    write ( io_out, 170 )
                    write ( io_out, 180 )
                    write ( io_out, 190 )

                    write ( io_out, 200 )
                    write ( io_out, 210 )
                    write ( io_out, 220 )
                    write ( io_out, 230 )
                    write ( io_out, 240 )
                    write ( io_out, 250 )
                    write ( io_out, 260 )
                    write ( io_out, 270 )
                    write ( io_out, 280 )
                    write ( io_out, 290 )

                    write ( io_out, 300 )
                close ( io_out )
            end do

        100 format ( 'V24OptionsConverted=1' )
        110 format ( 'WGUID_version=2' )
        120 format ( 'StressTester=0' )
        130 format ( 'UsePrimenet=1' )
        140 format ( 'DialUp=0' )
        150 format ( 'V5UserID=dantopa' )
        160 format ( 'WorkPreference=0' )
        170 format ( 'OutputIterations=100000' )
        180 format ( 'ResultsFileIterations=999999999' )
        190 format ( 'DiskWriteTime=15' )

        200 format ( 'NetworkRetryTime=2' )
        210 format ( 'NetworkRetryTime2=15' )
        220 format ( 'DaysOfWork=5' )
        230 format ( 'DaysBetweenCheckins=          1' )
        240 format ( 'NumBackupFiles=3' )
        250 format ( 'SilentVictory=1' )
        260 format ( '' )
        270 format ( '[PrimeNet]' )
        280 format ( 'Debug=0' )
        290 format ( 'ProxyHost=' )

        300 format ( 'UseCURL=0' )  ! sockets work better

    end subroutine prime_txt

    ! +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    subroutine local_txt ( machine, proc, nJobs, ram, cpus, threads )
        integer,               intent ( in ) :: nJobs, ram, cpus, threads
        character ( len = * ), intent ( in ) :: machine, proc

            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                io_out = safeopen_writereplace ( trim ( cwd ) // '/' // who // '/local.txt' )
                    write ( io_out, 100 ) 'OldCpuSpeed=3063'
                    write ( io_out, 100 ) 'NewCpuSpeedCount=0'
                    write ( io_out, 100 ) 'NewCpuSpeed=0'
                    write ( io_out, 100 ) 'RollingAverage=1000'
                    write ( io_out, 100 ) 'RollingAverageIsFromV27=1'
                    write ( io_out, 100 ) 'ComputerGUID='
                    write ( io_out, 110 ) 'ComputerID=', machine // '-' // trim ( proc ) // '-' // who
                    write ( io_out, 120 ) ram, ram
                    write ( io_out, 100 ) 'Affinity=100'
                    write ( io_out, 110 ) 'ThreadsPerTest=', threads
                    write ( io_out, 100 ) 'Pid='
                    write ( io_out, 100 ) 'SrvrUID='
                    write ( io_out, 100 ) 'SrvrComputerName='
                    write ( io_out, 100 ) 'SrvrPO1=0'
                    write ( io_out, 100 ) 'SrvrPO2=10'
                    write ( io_out, 100 ) 'SrvrPO3=3'
                    write ( io_out, 110 ) 'SrvrPO4=', ram
                    write ( io_out, 110 ) 'SrvrPO5=', ram
                    write ( io_out, 100 ) 'SrvrPO6=450'
                    write ( io_out, 100 ) 'SrvrPO7=1410'
                    write ( io_out, 100 ) 'SrvrPO8=1'
                    write ( io_out, 110 ) 'SrvrPO9=', cpus
                    write ( io_out, 100 ) 'SrvrP00=2'
                    write ( io_out, 100 ) 'LastEndDatesSent='
                close ( io_out )
            end do

        100 format ( g0 )
        110 format ( g0, g0 )
        120 format ( 'Memory=', g0, ' during 7:30-23:30 else ', g0 )

    end subroutine local_txt

end module mComponents
