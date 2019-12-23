program machine_pbs
    use mComponents, only : pbs_script, launcher_script, spreader_script, prime_txt, local_txt_cpu_lynx
    ! local_txt_cpu_lightning, local_txt_cpu_leo, local_txt_cpu_thunder, local_txt_phi_thunder, local_txt_gpu_thunder, local_txt_big_thunder

    implicit none

    integer, parameter :: nJobs = 4

    ! character ( len = * ), parameter :: machine  = 'leo'
    ! character ( len = * ), parameter :: account  = 'ERDCS97290KAT'
    ! character ( len = * ), parameter :: proc     = 'cpu'
    ! character ( len = * ), parameter :: queue    = 'standard'
    ! character ( len = * ), parameter :: cores    = 'select=1:ncpus=10:mpiprocs=10' ! thunder cpu
    ! character ( len = * ), parameter :: walltime = 'walltime=168:00:00' ! lightning, thunder

    ! character ( len = * ), parameter :: machine  = 'lynx'
    ! character ( len = * ), parameter :: account  = 'ERDCS97290KAT'
    ! character ( len = * ), parameter :: proc     = 'cpu'
    ! character ( len = * ), parameter :: queue    = 'standard'
    ! character ( len = * ), parameter :: cores    = 'select=1:ncpus=10:mpiprocs=10'
    ! character ( len = * ), parameter :: walltime = 'walltime=168:00:00'

    ! character ( len = * ), parameter :: machine  = 'thunder'
    ! character ( len = * ), parameter :: account  = 'ARLAP96070PET'
    ! character ( len = * ), parameter :: proc     = 'cpu'
    ! character ( len = * ), parameter :: queue    = 'background'
    ! character ( len = * ), parameter :: cores    = 'select=1:ncpus=36:mpiprocs=36' ! thunder cpu
    ! character ( len = * ), parameter :: walltime = 'walltime=120:00:00' ! lightning, thunder

    ! character ( len = * ), parameter :: machine  = 'thunder'
    ! character ( len = * ), parameter :: account  = 'ARLAP96070PET'
    ! character ( len = * ), parameter :: proc     = 'phi'
    ! character ( len = * ), parameter :: queue    = 'PHI'
    ! character ( len = * ), parameter :: cores    = 'select=1:ncpus=28:mpiprocs=28:nmics=2' ! thunder bigmem
    ! character ( len = * ), parameter :: walltime = 'walltime=120:00:00' ! lightning, thunder

    !character ( len = * ), parameter :: queue    = 'background'
    !character ( len = * ), parameter :: queue    = 'GPU'
    !character ( len = * ), parameter :: queue    = 'PHI'
    !character ( len = * ), parameter :: queue    = 'standard'
    !character ( len = * ), parameter :: cores    = 'select=1:ncpus=10:accelerator_model=Tesla_K40s'
    !character ( len = * ), parameter :: cores    = 'select=1:ncpus=10:accelerator_model=Tesla_K40s' lighting gpu
    !character ( len = * ), parameter :: cores    = 'select=1:ncpus=24:mpiprocs=24' lighting
    !character ( len = * ), parameter :: cores    = 'select=1:ncpus=28:mpiprocs=28:ngpus=2' ! thunder gpu

    ! character ( len = * ), parameter :: machine  = 'thunder'
    ! character ( len = * ), parameter :: account  = 'ARLAP96070PET'
    ! character ( len = * ), parameter :: proc     = 'gpu'
    ! character ( len = * ), parameter :: queue    = 'GPU'
    ! character ( len = * ), parameter :: cores    = 'select=1:ncpus=28:mpiprocs=28:ngpus=1' ! thunder bigmem
    ! character ( len = * ), parameter :: walltime = 'walltime=120:00:00' ! lightning, thunder

    ! character ( len = * ), parameter :: machine  = 'thunder'
    ! character ( len = * ), parameter :: account  = 'ARLAP96070PET'
    ! character ( len = * ), parameter :: proc     = 'bigmem'
    ! character ( len = * ), parameter :: queue    = 'bigmem'
    ! character ( len = * ), parameter :: cores    = 'select=1:ncpus=36:mpiprocs=36:bigmem=1' ! thunder bigmem
    ! character ( len = * ), parameter :: walltime = 'walltime=120:00:00' ! lightning, thunder

    character ( len = * ), parameter :: machine  = 'topaz'
    character ( len = * ), parameter :: account  = 'ARLAP96070PET'
    character ( len = * ), parameter :: proc     = 'bigmem'
    character ( len = * ), parameter :: queue    = 'standard'
    character ( len = * ), parameter :: cores    = 'select=1:ncpus=32:mpiprocs=32:bigmem=1' ! thunder bigmem
    character ( len = * ), parameter :: walltime = 'walltime=120:00:00' ! lightning, thunder

        ! write pbs files
        call pbs_script ( account = account, machine = machine, queue = queue, proc = proc, cores = cores, &
                          walltime = walltime, nJobs = nJobs )

        call launcher_script         ( machine = machine, proc = proc, nJobs = nJobs ) ! write launcher
        call spreader_script         ( machine = machine, proc = proc, nJobs = nJobs ) ! write spreader to move exectuable
        !call local_txt_cpu_lightning ( machine = machine, nJobs = nJobs )
        call local_txt_cpu_lynx      ( machine = machine, proc = proc, nJobs = nJobs )
        call prime_txt               ( machine = machine, proc = proc, nJobs = nJobs )

end program writer_pbs
