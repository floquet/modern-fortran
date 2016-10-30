program machine_pbs
    use mComponents, only : pbs_script, launcher_script, spreader_script, prime_txt, local_txt, cwd, status_chdir

    implicit none

    integer, parameter :: nJobs = 4

    character ( len = * ), parameter :: machine      = 'lightning'
    character ( len = * ), parameter :: account      = 'ARLAP96070PET'
    character ( len = * ), parameter :: launch       = 'aprun'

    character ( len = * ), parameter :: queue_GPU    = 'gpu'
    character ( len = * ), parameter :: cores_GPU    = 'select=1:ncpus=10:accelerator_model=Tesla_K40s'
    character ( len = * ), parameter :: walltime_gpu = 'walltime=168:00:00'

    character ( len = * ), parameter :: queue_cpu    = 'background'
    character ( len = * ), parameter :: cores_cpu    = 'select=1:ncpus=24:mpiprocs=24'
    character ( len = * ), parameter :: walltime_std = 'walltime=120:00:00'

    integer :: ram = 0, cpus = 0, threads = 4!, status_chdir = 0

    character ( len =   8 ) :: proc = ''
    character ( len = 256 ) :: myQueue = '', cores = '', launch_dir = '', walltime = ''

        status_chdir = getcwd ( launch_dir )

        call execute_command_line ( 'rm -rf ' // machine // '/' )
        call execute_command_line ( 'mkdir '  // machine // '/' )
        call chdir ( trim ( launch_dir ) // '/' // machine )
        status_chdir = getcwd ( launch_dir )

        ! GPU
        proc     = 'gpu'
        myQueue  = queue_GPU
        cores    = cores_GPU
        walltime = walltime_gpu
        ram      = 128827
        cpus     = 2
        threads  = 4

            call chdir ( trim ( launch_dir ) )
            call execute_command_line ( 'mkdir '  // trim ( proc ) // '/' )
            call chdir ( trim ( launch_dir ) // '/' // trim ( proc ) )
            status_chdir = getcwd ( cwd )
            call pbs_script ( account = account, machine = trim ( machine ), queue = trim ( myQueue ), proc = trim ( proc ), &
                              cores = trim ( cores ), walltime = trim ( walltime ), nJobs = nJobs, launch = trim ( launch ) )

            call launcher_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write launcher
            call spreader_script ( nJobs = nJobs ) ! write spreader to move exectuable
            call local_txt       ( machine = machine, proc = proc, nJobs = nJobs, ram = ram, cpus = cpus, threads = threads )
            call prime_txt       ( nJobs = nJobs )

        ! compute
        proc     = 'cpu'
        myQueue  = queue_cpu
        cores    = cores_cpu
        walltime = walltime_std
        ram      = 128826
        cpus     = 9
        threads  = 4

            call chdir ( trim ( launch_dir ) )
            call execute_command_line ( 'mkdir '  // trim ( proc ) // '/' )
            call chdir ( trim ( launch_dir ) // '/' // trim ( proc ) )
            status_chdir = getcwd ( cwd )
            call pbs_script ( account = account, machine = trim ( machine ), queue = trim ( myQueue ), proc = trim ( proc ), &
                              cores = trim ( cores ), walltime = trim ( walltime ), nJobs = nJobs, launch = trim ( launch ) )

            call launcher_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write launcher
            call spreader_script ( nJobs = nJobs ) ! write spreader to move exectuable
            call local_txt       ( machine = machine, proc = proc, nJobs = nJobs, ram = ram, cpus = cpus, threads = threads )
            call prime_txt       ( nJobs = nJobs )

end program machine_pbs
