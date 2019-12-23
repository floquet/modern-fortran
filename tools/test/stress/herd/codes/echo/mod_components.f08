module mComponents
    use mFileHandling, only : safeopen_writereplace

    implicit none

    integer :: kJobs
    integer :: io_out

    character ( len = 3 )            :: who
    character ( len = * ), parameter :: proc = 'cpu'
    character ( len = * ), parameter :: myDir = '$wdmersenne'

contains

    subroutine pbs_script ( account, queue, cores, walltime, machine, nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: account, queue, cores, walltime, machine

        ! write pbs files
        call execute_command_line ( 'rm -rf ' // machine // '/' )
        call execute_command_line ( 'mkdir ' // machine // '/' )
        call execute_command_line ( 'cd ' // machine // '/' )

        call execute_command_line ( 'mkdir ' // machine // '/' // proc // '/' )

        do kJobs = 1, nJobs
            write ( unit = who, fmt = '( I3.3 )' ) kJobs
            call execute_command_line ( 'mkdir ' // machine // '/' // proc // '/' // who )
            !print *, 'mkdir leo/gpu/' // who
            io_out = safeopen_writereplace ( machine // '/' // proc // '/' // who // '/' // machine // '_' // proc // '_' // who )

            write ( io_out, 100 )
            write ( io_out, 110 )
            write ( io_out, 200 ) 'A', trim ( account )
            write ( io_out, 200 ) 'q', trim ( queue )
            write ( io_out, 200 ) 'l', trim ( cores )
            write ( io_out, 200 ) 'l', trim ( walltime )
            write ( io_out, 200 ) 'j', 'oe'
            write ( io_out, 200 ) 'N', 'direct'
            write ( io_out, 200 ) 'V', ''
            write ( io_out, 120 )
            write ( io_out, 130 ) myDir // '/' // proc // '/' // who // '/'
            write ( io_out, 120 )
            write ( io_out, 140 )
            write ( io_out, 120 )

            close ( io_out )
        end do

    100 format ( '#!/bin/bash' )
    110 format ( '## Required PBS Directives --------------------------------------' )
    120 format ( '' )
    130 format ( 'cd ', g0 )
    140 format ( './cpu -d > timing_results.out' )

    200 format ( '#PBS -', g0, ' ', g0 )

    end subroutine pbs_script

    subroutine launcher_script ( machine, nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: machine

            io_out = safeopen_writereplace ( machine // '/' // proc // '/launcher' )
            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                write ( io_out, 300 ) myDir, proc, who, machine, proc, who
                write ( io_out, '( "" )' )
            end do
                write ( io_out, '( "cd ", g0 )' ) myDir
            close ( io_out )

        300 format ( 'qsub ', g0, '/', g0, '/', g0, '/', g0, '_', g0, '_', g0 )

    end subroutine launcher_script

    subroutine spreader_script ( machine, nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: machine

            io_out = safeopen_writereplace ( machine // '/' // proc // '/spreader' )

            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                write ( io_out, 300 ) who // '/'
                write ( io_out, '( "" )' )
            end do

            close ( io_out )

        300 format ( 'cp ../../cpu ', g0, '.' )

    end subroutine spreader_script

    subroutine local_txt ( machine, nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: machine

            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                io_out = safeopen_writereplace ( machine // '/' // proc // '/' // who // '/local.txt' )
                    write ( io_out, 100 )
                    write ( io_out, 110 )
                    write ( io_out, 120 )
                    write ( io_out, 130 )
                    write ( io_out, 140 )
                    write ( io_out, 150 )
                    write ( io_out, 160 ) machine // '_' // proc // '_' // who
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

        100 format ( 'OldCpuSpeed=2700' )
        110 format ( 'NewCpuSpeedCount=0' )
        120 format ( 'NewCpuSpeed=0' )
        130 format ( 'RollingAverage=1000' )
        140 format ( 'RollingAverageIsFromV27=1' )
        150 format ( 'ComputerGUID=' )
        160 format ( 'ComputerID=', g0 )
        170 format ( 'Memory=64513 during 7:30-23:30 else 64513' )
        180 format ( 'SrvrUID=1257384106' )
        190 format ( 'SrvrComputerName=187578647' )

        200 format ( 'SrvrPO1=0' )
        210 format ( 'SrvrPO2=1' )
        220 format ( 'SrvrPO3=5' )
        230 format ( 'SrvrPO4=64513' )
        240 format ( 'SrvrPO5=64513' )
        250 format ( 'SrvrPO6=450' )
        260 format ( 'SrvrPO7=1410' )
        270 format ( 'SrvrPO8=1' )
        280 format ( 'SrvrPO9=6' )
        290 format ( 'SrvrP00=1' )

        300 format ( 'LastEndDatesSent=1466544618' )

    end subroutine local_txt

    subroutine prime_txt ( machine, nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: machine

            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                io_out = safeopen_writereplace ( machine // '/' // proc // '/' // who // '/prime.txt' )
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
                close ( io_out )
            end do

        100 format ( 'V24OptionsConverted=1' )
        110 format ( 'WGUID_version=2' )
        120 format ( 'StressTester=0' )
        130 format ( 'UsePrimenet=1' )
        140 format ( 'DialUp=0' )
        150 format ( 'V5UserID=dantopa' )
        160 format ( 'WorkPreference=0' )
        170 format ( 'OutputIterations=1000000' )
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

    end subroutine prime_txt

    ! +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +

    subroutine local_txt_cpu_lightning ( machine, nJobs )
        integer,               intent ( in ) :: nJobs
        character ( len = * ), intent ( in ) :: machine

            do kJobs = 1, nJobs
                write ( unit = who, fmt = '( I3.3 )' ) kJobs
                io_out = safeopen_writereplace ( machine // '/' // proc // '/' // who // '/local.txt' )
                    write ( io_out, 100 )
                    write ( io_out, 110 )
                    write ( io_out, 120 )
                    write ( io_out, 130 )
                    write ( io_out, 140 )
                    write ( io_out, 150 )
                    write ( io_out, 160 ) machine // '_' // proc // '_' // who
                    write ( io_out, 170 )
                    write ( io_out, 171 )
                    write ( io_out, 172 )
                    write ( io_out, 173 )
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

        100 format ( 'OldCpuSpeed=2700' )
        110 format ( 'NewCpuSpeedCount=0' )
        120 format ( 'NewCpuSpeed=0' )
        130 format ( 'RollingAverage=1000' )
        140 format ( 'RollingAverageIsFromV27=1' )
        150 format ( 'ComputerGUID=' )
        160 format ( 'ComputerID=', g0 )
        170 format ( 'Memory=64513 during 7:30-23:30 else 64513' )
        171 format ( 'Affinity=100' )
        172 format ( 'ThreadsPerTest=4' )
        173 format ( 'Pid=20225')
        180 format ( 'SrvrUID=1257384106' )
        190 format ( 'SrvrComputerName=655168948' )

        200 format ( 'SrvrPO1=0' )
        210 format ( 'SrvrPO2=1' )
        220 format ( 'SrvrPO3=5' )
        230 format ( 'SrvrPO4=64513' )
        240 format ( 'SrvrPO5=64513' )
        250 format ( 'SrvrPO6=450' )
        260 format ( 'SrvrPO7=1410' )
        270 format ( 'SrvrPO8=1' )
        280 format ( 'SrvrPO9=6' )
        290 format ( 'SrvrP00=1' )

        300 format ( 'LastEndDatesSent=1466605040' )

    end subroutine local_txt_cpu_lightning

end module mComponents
