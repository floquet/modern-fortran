! https://gcc.gnu.org/onlinedocs/gfortran/CO_005fSUM.html
! CO_SUM sums up the values of each element of A on all images of the current team.

program test
    implicit none
    integer                 :: val, cs_stat
    character ( len = 256 ) :: cs_msg
        cs_msg = ''
        val = this_image ( )
        call co_sum ( val, result_image = 1, stat = cs_stat, errmsg = cs_msg )
        if ( cs_stat /= 0 ) then
            write ( * , '( "Error in call to CO_SUM with num_images = ", g0 )' ) num_images ( )
            write ( * , '( "image  = ", g0 )' ) val
            write ( * , '( "errmsg = ", g0, "." )' ) trim ( cs_msg )
            write ( * , '( "stat   = ", g0, / )' ) cs_stat
        end if
        if ( this_image ( ) == 1 ) then
            write ( * , '( g0, ": cosum" )' ) val
            write ( * , '( g0, ": Expected answer" )' ) num_images ( ) * ( num_images ( ) + 1 ) / 2
        end if
end program test

!       CAF and MPI

!  16:10 ITL-DTOPA-MP rditldmt $ echo $gflags
! -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5
!  16:11 ITL-DTOPA-MP rditldmt $ caf $gflags deluxe_co_sum.f08
!  16:11 ITL-DTOPA-MP rditldmt $ cafrun -np 4 ./a.out
! 10: cosum
! 10: Expected answer
!  16:11 ITL-DTOPA-MP rditldmt $ cafrun -np 8 ./a.out
! Error in call to CO_SUM with num_images = 8
! image  = 2
! errmsg = .
! stat   = -1
!
! Error in call to CO_SUM with num_images = 8
! image  = 4
! errmsg = .
! stat   = -1
!
! Error in call to CO_SUM with num_images = 8
! image  = 6
! errmsg = .
! stat   = -1
!
! Error in call to CO_SUM with num_images = 8
! image  = 8
! errmsg = .
! stat   = -1
!
! Error in call to CO_SUM with num_images = 8
! image  = 3
! errmsg = .
! stat   = -1
!
! Error in call to CO_SUM with num_images = 8
! image  = 5
! errmsg = .
! Error in call to CO_SUM with num_images = 8
! image  = 7
! errmsg = .
! stat   = -1
!
! Error in call to CO_SUM with num_images = 8
! image  = 36
! errmsg = .
! stat   = -1
!
! 36: cosum
! 36: Expected answer
! stat   = -1
!

!  16:11 ITL-DTOPA-MP rditldmt $ mpifort -fcoarray=lib co_sum.f08 -L/opt/local/lib/ -lcaf_mpi -o deluxe_co_sum
!  16:12 ITL-DTOPA-MP rditldmt $ mpirun -np 4 ./deluxe_co_sum
! 10: cosum
! 10: Expected answer
!  16:13 ITL-DTOPA-MP rditldmt $ mpirun -np 8 ./deluxe_co_sum
! 36: cosum
! 36: Expected answer
!  16:13 ITL-DTOPA-MP rditldmt $ mpirun -np 16 ./deluxe_co_sum
! 136: cosum
! 136: Expected answer

!  16:15 ITL-DTOPA-MP rditldmt $ echo $cflags 
! -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib
!  16:16 ITL-DTOPA-MP rditldmt $ mpifort $cflags co_sum.f08 -L/opt/local/lib/ -lcaf_mpi -o deluxe_co_sum
!  16:16 ITL-DTOPA-MP rditldmt $ mpirun -np 4 ./deluxe_co_sum
! 10: cosum
! 10: Expected answer
!  16:16 ITL-DTOPA-MP rditldmt $ mpirun -np 8 ./deluxe_co_sum
! Error in call to CO_SUM with num_images = 8
! This image is number 2
! errmsg = .
! stat   = -1
! Error in call to CO_SUM with num_images = 8
! This image is number 6
! errmsg = .
! stat   = -1
! Error in call to CO_SUM with num_images = 8
! This image is number 4
! errmsg = .
! stat   = -1
! Error in call to CO_SUM with num_images = 8
! This image is number 8
! errmsg = .
! stat   = -1
! Error in call to CO_SUM with num_images = 8
! This image is number 7
! errmsg = .
! stat   = -1
! Error in call to CO_SUM with num_images = 8
! This image is number 3
! errmsg = .
! stat   = -1
! Error in call to CO_SUM with num_images = 8
! This image is number 36
! errmsg = .
! stat   = -1
! 36: cosum
! 36: Expected answer
! Error in call to CO_SUM with num_images = 8
! This image is number 5
! errmsg = .
! stat   = -1
