module mPBS
    implicit none
contains
    subroutine write_pbs_file ( vnode )
        character ( len = * ), intent ( in ) :: vnode
    100 format ( '#!/bin/bash' )
    100 format ( '## Required PBS Directives --------------------------------------' )
    100 format ( '#PBS -V' )
    100 format ( '#PBS -A ERDCS97270PET' )
    100 format ( '#PBS -q background' )
    100 format ( '#PBS -l select=100:ncpus=36:mpiprocs=36:vnode=r24i1n5' )
    100 format ( '#PBS -l walltime=4:00:00' )
    100 format ( '#PBS -j oe' )

    100 format ( "export myProject=${fortran}'tools/test/stress/flopper/delta/echo_thunder'"  )
    100 format ( 'echo $myProject' )
    100 format ( 'cd ${myProject}' )

    100 format ( 'module switch intel-compilers/15.0.3 gcc-compilers/5.3.0' )
    100 format ( 'make' )
    100 format ( 'rm -rf *.o *.mod *.smod *.dSYM' )

    100 format ( '## Execution Block ----------------------------------------------' )
    100 format ( '# Environment Setup' )
    100 format ( '# cd to your scratch directory in /work' )
    100 format ( 'cd ${WORKDIR}' )

    100 format ( '# create a job-specific subdirectory based on JOBID and cd to it' )
    100 format ( 'JOBID=`echo ${PBS_JOBID} | cut -d '.' -f 1`' )
    100 format ( 'if [ ! -d ${JOBID} ]; then' )
    100 format ( '  mkdir -p ${JOBID}' )
    100 format ( 'fi' )
    100 format ( 'cd ${JOBID}' )

    100 format ( '## Launching -----------------------------------------------------' )
    100 format ( '# copy executable from $HOME and submit it' )
    100 format ( 'cp -a ${myProject}/.  .' )

    100 format ( '${fortran_execute_mpi}  ./flops > out.dat' )

    100 format ( '## Clean up -----------------------------------------------------' )
    100 format ( '# Remove temporary files' )
    100 format ( 'rm -rf *.o *.mod *.smod *.dSYM' )

    end subroutine write_pbs_file
end module mPBS
