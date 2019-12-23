program writer_pbs
    use mComponents, only : pbs_script, launcher_script

    implicit none

    integer, parameter :: nJobs = 4
    character ( len = * ), parameter :: account   = 'ERDCS97270PET'
    character ( len = * ), parameter :: queue     = 'background'
    character ( len = * ), parameter :: cores     = 'select=1:ncpus=36:mpiprocs=36'
    character ( len = * ), parameter :: wall_time = 'walltime=4:00:00'
    character ( len = * ), parameter :: machine   = 'leo'

    integer :: kJobs  = 0

        ! write pbs files
        call pbs_script ( account = account, queue = queue, cores = cores, walltime = walltime, machine = machine, nJobs = nJobs )

        ! write launcher
        call launcher_script ( machine = machine, nJob = nJobs )

end program writer_pbs
