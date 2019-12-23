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
            write ( * , '( "This image is number ", g0 )' ) val
            write ( * , '( "errmsg = ", g0, "." )' ) trim ( cs_msg )
            write ( * , '( "stat   = ", g0 )' ) cs_stat
        end if
        if ( this_image ( ) == 1 ) then
            write ( * , '( g0, ": cosum" )' ) val
            write ( * , '( g0, ": Expected answer" )' ) num_images ( ) * ( num_images ( ) + 1 ) / 2
        end if
end program test

! 15:29 dan-topas-pro-2 rditldmt $ caf -v
!
! OpenCoarrays Coarray Fortran Compiler Wrapper (caf version 1.3.6)
! Copyright (C) 2015-2016 Sourcery, Inc.
!
! OpenCoarrays comes with NO WARRANTY, to the extent permitted by law.
! You may redistribute copies of OpenCoarrays under the terms of the
! BSD 3-Clause License.  For more information about these matters, see
! the file named LICENSE.
!
!  15:29 dan-topas-pro-2 rditldmt $ cofortran -v
! -bash: cofortran: command not found
!  15:30 dan-topas-pro-2 rditldmt $
!  15:30 dan-topas-pro-2 rditldmt $ caf -v
!
! OpenCoarrays Coarray Fortran Compiler Wrapper (caf version 1.3.6)
! Copyright (C) 2015-2016 Sourcery, Inc.
!
! OpenCoarrays comes with NO WARRANTY, to the extent permitted by law.
! You may redistribute copies of OpenCoarrays under the terms of the
! BSD 3-Clause License.  For more information about these matters, see
! the file named LICENSE.
!
!  15:30 dan-topas-pro-2 rditldmt $ gfortran -v
! Using built-in specs.
! COLLECT_GCC=gfortran
! COLLECT_LTO_WRAPPER=/usr/local/gfortran/libexec/gcc/x86_64-apple-darwin14/5.1.0/lto-wrapper
! Target: x86_64-apple-darwin14
! Configured with: ../gcc-5.1.0/configure --prefix=/usr/local/gfortran --with-gmp=/Users/fx/devel/gcc/deps-static/x86_64 --enable-languages=c,c++,fortran,objc,obj-c++ --build=x86_64-apple-darwin14
! Thread model: posix
! gcc version 5.1.0 (GCC)
!  15:30 dan-topas-pro-2 rditldmt $ gcc -v
! Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
! Apple LLVM version 7.0.2 (clang-700.1.81)
! Target: x86_64-apple-darwin15.3.0
! Thread model: posix
!  15:30 dan-topas-pro-2 rditldmt $ date
! Tue Mar 15 15:30:23 CDT 2016
!  15:30 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/demos/co_sum
!  15:30 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib co_sum.f08
!  15:30 dan-topas-pro-2 rditldmt $ cafrun -np 4 ./a.out
! 10: cosum
! 10: Expected answer
!  15:30 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib co_sum.f08 -L/opt/local/lib/ -lcaf_mpi -o co_sum
!  15:30 dan-topas-pro-2 rditldmt $ mpirun -np 6 ./co_sum
! 21: cosum
! 21: Expected answer


!  15:32 dan-topas-pro-2 rditldmt $ cafrun -np 3 ./a.out
! Error in call to CO_SUM with num_images = 3
! errmsg = ? ? ??5@.5?w?[???s???!@.5?5???5?5?????????????ί??D?:?ί??
! stat   = -1
! 6: cosum
! 6: Expected answer
!  15:33 dan-topas-pro-2 rditldmt $ cafrun -np 5 ./a.out
! Error in call to CO_SUM with num_images = 5
! errmsg =  ( (??{	@^{	???V?;	ңf     ?;	?6g	@^{	?G{	???G{	???G{	???;	?????????ί??D?	?ί??
! stat   = -1
! 15: cosum
! 15: Expected answer
! Error in call to CO_SUM with num_images = 5
! errmsg =  ( (???z???_^?`??`?V??z??g????g????g????`?????????ί??D6??ί??
! stat   = -1
! Error in call to CO_SUM with num_images = 5
! errmsg =  ( (஥?:??wZZ?e҃?e???:??'????'????'????e?????????ί??Dƪ?ί??
! stat   = -1
! Error in call to CO_SUM with num_images = 5
! ?ί???????ί??DƎN?
! stat   = -1
! Error in call to CO_SUM with num_images = 5
! ?R??Q?g?\??ކ=??Q??Q????Q????Q???????????ί??DvW?ί??
! stat   = -1

!  15:34 dan-topas-pro-2 rditldmt $ mpirun -np 3 ./co_sum
! 6: cosum
! 6: Expected answer
!  15:34 dan-topas-pro-2 rditldmt $ mpirun -np 5 ./co_sum
! 15: cosum
! 15: Expected answer
