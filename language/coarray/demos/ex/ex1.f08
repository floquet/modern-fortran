!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! https://www.sharcnet.ca/help/images/f/f5/Coarray_2015_08_05_final.pdfprogram ex1
program ex1

    implicit none

    real    :: z [ * ]
    integer :: i

    sync all

        if ( this_image () == 1 ) then
            read *, z ! value to send to all images
            print '( "Image ",i4, ": before: z = ", f10.5 )', this_image ( ), z
            do i = 2, num_images ( )
                z [ i ] = z
            enddo
        endif

        sync all

        print ' ( "Image ", i4, ": after:  z = ", f10.5 ) ', this_image ( ), z

end program ex1

!  17:00 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib ex1.f08 -L/opt/local/lib/ -lcaf_mpi -o ex1
!  17:01 dan-topas-pro-2 rditldmt $ mpirun -np 13 ./ex1
! 5
! Image    1: before: z =    5.00000
! Image    1: after:  z =    5.00000
! Image   13: after:  z =    5.00000
! Image    3: after:  z =    5.00000
! Image    5: after:  z =    5.00000
! Image    9: after:  z =    5.00000
! Image   11: after:  z =    5.00000
! Image    2: after:  z =    5.00000
! Image    7: after:  z =    5.00000
! Image    4: after:  z =    5.00000
! Image   12: after:  z =    5.00000
! Image    8: after:  z =    5.00000
! Image   10: after:  z =    5.00000
! Image    6: after:  z =    5.00000

!  17:02 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib ex1.f08
!  17:03 dan-topas-pro-2 rditldmt $ cafrun -np 13 ./a.out
! 5
! Image    1: before: z =    5.00000
! Image    1: after:  z =    5.00000
! Image    2: after:  z =    5.00000
! Image    3: after:  z =    5.00000
! Image    4: after:  z =    5.00000
! Image    9: after:  z =    5.00000
! Image   13: after:  z =    5.00000
! Image    5: after:  z =    5.00000
! Image   10: after:  z =    5.00000
! Image    6: after:  z =    5.00000
! Image    7: after:  z =    5.00000
! Image   11: after:  z =    5.00000
! Image    8: after:  z =    5.00000
! Image   12: after:  z =    5.00000
