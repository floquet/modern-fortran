! https://gcc.gnu.org/onlinedocs/gfortran/CO_005fMAX.html#CO_005fMAX
! CO_MAX determines element-wise the maximal value of A on all images of the current team
program program_co_max

    implicit none
    integer :: val

        val = this_image ()
        call co_max ( val, result_image = 1 )
        if ( this_image ( ) == 1 ) then
            write ( * , '( "Maximal value   ", g0 )' ) val  ! prints value of highest image
            write ( * , '( "Expected answer ", g0 )' ) num_images ()  ! prints num_images()
        end if

end program program_co_max

!  16:44 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib co_max.f08 -L/opt/local/lib/ -lcaf_mpi -o co_max
!  16:44 dan-topas-pro-2 rditldmt $ mpirun -np 3 ./co_max
!  Maximal value           3
!  16:44 dan-topas-pro-2 rditldmt $ mpirun -np 33 ./co_max
!  Maximal value          33
!  16:44 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib co_max.f08
!  16:45 dan-topas-pro-2 rditldmt $ cafrun -np 2 ./a.out
!  Maximal value           2
!  16:45 dan-topas-pro-2 rditldmt $ cafrun -np 22 ./a.out
!  Maximal value          22
!  16:45 dan-topas-pro-2 rditldmt $ cafrun -np 222 ./a.out
! [proxy:0:0@dan-topas-pro-2.erdc.dren.mil] HYDU_create_process (utils/launch/launch.c:22): pipe error (Too many open files)
! [proxy:0:0@dan-topas-pro-2.erdc.dren.mil] launch_procs (pm/pmiserv/pmip_cb.c:705): create process returned error
! [proxy:0:0@dan-topas-pro-2.erdc.dren.mil] HYD_pmcd_pmip_control_cmd_cb (pm/pmiserv/pmip_cb.c:892): launch_procs returned error
! [proxy:0:0@dan-topas-pro-2.erdc.dren.mil] HYDT_dmxu_poll_wait_for_event (tools/demux/demux_poll.c:76): callback returned error status
! [proxy:0:0@dan-topas-pro-2.erdc.dren.mil] main (pm/pmiserv/pmip.c:206): demux engine error waiting for event
! [mpiexec@dan-topas-pro-2.erdc.dren.mil] control_cb (pm/pmiserv/pmiserv_cb.c:200): assert (!closed) failed
! [mpiexec@dan-topas-pro-2.erdc.dren.mil] HYDT_dmxu_poll_wait_for_event (tools/demux/demux_poll.c:76): callback returned error status
! [mpiexec@dan-topas-pro-2.erdc.dren.mil] HYD_pmci_wait_for_completion (pm/pmiserv/pmiserv_pmci.c:198): error waiting for event
! [mpiexec@dan-topas-pro-2.erdc.dren.mil] main (ui/mpich/mpiexec.c:344): process manager error waiting for completion

! guest@rouson-VirtualBox:~/github/fortran/coarray/demos/co_max$ echo $cflags
! -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib
! guest@rouson-VirtualBox:~/github/fortran/coarray/demos/co_max$ caf $cflags co_max.f08
! guest@rouson-VirtualBox:~/github/fortran/coarray/demos/co_max$ cafrun -np 4 ./a.out
!  Maximal value           4
! guest@rouson-VirtualBox:~/github/fortran/coarray/demos/co_max$ cafrun -np 8 ./a.out
!  Maximal value           8
! guest@rouson-VirtualBox:~/github/fortran/coarray/demos/co_max$ date
! Sat Apr 16 14:30:05 PDT 2016
! guest@rouson-VirtualBox:~/github/fortran/coarray/demos/co_max$ pwd
! /home/guest/github/fortran/coarray/demos/co_max
