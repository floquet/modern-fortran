program writer_pbs
    use mComponents, only : pbs_script, launcher_script, spreader_script, local_txt, prime_txt

    implicit none

    integer, parameter :: nJobs = 4
    character ( len = * ), parameter :: account  = 'ARLAP96070PET'
    character ( len = * ), parameter :: queue    = 'background'
    character ( len = * ), parameter :: cores    = 'select=1:ncpus=10:accelerator_model=Tesla_K40s'
    character ( len = * ), parameter :: walltime = 'walltime=120:00:00'
    character ( len = * ), parameter :: machine  = 'lightning'

        ! write pbs files
        call pbs_script ( account = account, queue = queue, cores = cores, walltime = walltime, machine = machine, nJobs = nJobs )

        call launcher_script ( machine = machine, nJobs = nJobs ) ! write launcher
        call spreader_script ( machine = machine, nJobs = nJobs ) ! write spreader to move exectuable
        call local_txt ( machine = machine, nJobs = nJobs )
        call prime_txt ( machine = machine, nJobs = nJobs )

end program writer_pbs
