program machine_pbs
    use mComponents, only : pbs_script, launcher_script, spreader_script, prime_txt, local_txt, cwd, status_chdir

    implicit none

    integer, parameter :: nJobs = 4

    character ( len = * ), parameter :: machine   = 'topaz'
    character ( len = * ), parameter :: account   = 'ARLAP96070PET'
    character ( len = * ), parameter :: walltime  = 'walltime=168:00:00'

    character ( len = * ), parameter :: queue_big = 'standard'
    character ( len = * ), parameter :: cores_big = 'select=1:ncpus=32:mpiprocs=32:bigmem=1'

    !character ( len = * ), parameter :: queue_GPU = 'GPU'
    character ( len = * ), parameter :: queue_GPU = 'standard'
    character ( len = * ), parameter :: cores_GPU = 'select=1:ncpus=28:mpiprocs=28:ngpus=1'

    character ( len = * ), parameter :: queue_cpu = 'standard'
    character ( len = * ), parameter :: cores_cpu = 'select=1:ncpus=36:mpiprocs=36'

    ! character ( len = * ), parameter :: queue_std = 'standard'
    ! character ( len = * ), parameter :: cores_cpu = 'select=1:ncpus=36:mpiprocs=36'

    integer :: ram = 0, cpus = 0, threads = 4!, status_chdir = 0

    character ( len =   8 ) :: proc = ''
    character ( len = 256 ) :: myMachine = '', launch = '', myQueue = '', cores = '', launch_dir = ''

        status_chdir = getcwd ( launch_dir )
        ! print *, '1 current working directory = ', cwd, '; status_chdir = ', status_chdir

        call execute_command_line ( 'rm -rf ' // machine // '/' )
        call execute_command_line ( 'mkdir '  // machine // '/' )
        call chdir ( trim ( launch_dir ) // '/' // machine )
        ! print *, 'cd ' // trim ( cwd ) // '/' // machine
        status_chdir = getcwd ( launch_dir )
        print *, '2 launch_dir = ', launch_dir

        ! bigmem
        proc    = 'big'
        launch  = 'mpiexec_mpt'
        myQueue = queue_big
        cores   = cores_big
        ram     = 969154
        cpus    = 8
        threads = 4

        ! print *, 'mkdir '  // trim ( proc ) // '/'
        ! call execute_command_line ( 'cd '     // trim ( proc ) // '/' )
        !print *, 'cd '     // trim ( proc ) // '/'
        ! stop "check"

        !myMachine = machine // '/' // trim ( proc )

            ! write pbs files
            call execute_command_line ( 'mkdir '  // trim ( proc ) // '/' )
            call chdir ( trim ( launch_dir ) // '/' // trim ( proc ) )
            status_chdir = getcwd ( cwd )
            print *, '3 current working directory = ', cwd, '; status_chdir = ', status_chdir
            call pbs_script ( account = account, machine = trim ( machine ), queue = trim ( myQueue ), proc = trim ( proc ), &
                              cores = trim ( cores ), walltime = walltime, nJobs = nJobs, launch = trim ( launch ) )

            call launcher_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write launcher
            !call spreader_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write spreader to move exectuable
            call spreader_script ( nJobs = nJobs ) ! write spreader to move exectuable
            call local_txt       ( machine = machine, proc = proc, nJobs = nJobs, ram = ram, cpus = cpus, threads = threads )
            !call prime_txt       ( machine = machine, proc = proc, nJobs = nJobs )
            call prime_txt       ( nJobs = nJobs )

        ! GPU
        proc    = 'gpu'
        !launch  = 'mpiexec_mpt'
        myQueue = queue_GPU
        cores   = cores_GPU
        ram     = 128827
        cpus    = 7
        threads = 4

        !myMachine = machine // '/' // trim ( proc )

            call chdir ( trim ( launch_dir ) )
            call execute_command_line ( 'mkdir '  // trim ( proc ) // '/' )
            print *, '** launch_dir = ', launch_dir
            call chdir ( trim ( launch_dir ) // '/' // trim ( proc ) )
            print *, trim ( launch_dir ) // '/' // trim ( proc )
            status_chdir = getcwd ( cwd )
            print *, '4 current working directory = ', cwd
            call pbs_script ( account = account, machine = trim ( machine ), queue = trim ( myQueue ), proc = trim ( proc ), &
                              cores = trim ( cores ), walltime = walltime, nJobs = nJobs, launch = trim ( launch ) )

            call launcher_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write launcher
            !call spreader_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write spreader to move exectuable
            call spreader_script ( nJobs = nJobs ) ! write spreader to move exectuable
            call local_txt       ( machine = machine, proc = proc, nJobs = nJobs, ram = ram, cpus = cpus, threads = threads )
            !call prime_txt       ( machine = machine, proc = proc, nJobs = nJobs )
            call prime_txt       ( nJobs = nJobs )

        ! compute
        proc    = 'cpu'
        !launch  = 'mpiexec_mpt'
        myQueue = queue_cpu
        cores   = cores_cpu
        ram     = 128826
        cpus    = 9
        threads = 4

        !myMachine = machine // '/' // trim ( proc )

            call chdir ( trim ( launch_dir ) )
            call execute_command_line ( 'mkdir '  // trim ( proc ) // '/' )
            print *, '** launch_dir = ', launch_dir
            call chdir ( trim ( launch_dir ) // '/' // trim ( proc ) )
            print *, trim ( launch_dir ) // '/' // trim ( proc )
            status_chdir = getcwd ( cwd )
            print *, '4 current working directory = ', cwd
            call pbs_script ( account = account, machine = trim ( machine ), queue = trim ( myQueue ), proc = trim ( proc ), &
                              cores = trim ( cores ), walltime = walltime, nJobs = nJobs, launch = trim ( launch ) )

            call launcher_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write launcher
            !call spreader_script ( machine = machine, proc = proc, nJobs = nJobs ) ! write spreader to move exectuable
            call spreader_script ( nJobs = nJobs ) ! write spreader to move exectuable
            call local_txt       ( machine = machine, proc = proc, nJobs = nJobs, ram = ram, cpus = cpus, threads = threads )
            !call prime_txt       ( machine = machine, proc = proc, nJobs = nJobs )
            call prime_txt       ( nJobs = nJobs )

end program machine_pbs
