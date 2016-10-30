!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! https://www.sharcnet.ca/help/images/f/f5/Coarray_2015_08_05_final.pdfprogram ex1
program ex2

    implicit none

    character ( 80 ) :: host [ * ] ! Note: host – local; host[i] – on image i
    integer          :: i

        call get_environment_variable ( 'HOME', value = host )

        if ( this_image ( ) == 1 ) then
            do i = 1, num_images ( )
                write ( * , 100 ) i, trim ( host [ i ] )
            enddo
        endif

        stop
    100 format ( 'Hello from image ', g0, ' on host ', g0, '.' )

end program ex2

 ! 17:05 dan-topas-pro-2 rditldmt $ caf -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib ex2.f08
 ! 17:06 dan-topas-pro-2 rditldmt $ cafrun -np 12 ./a.out
 ! Hello from image            1  on host /Users/rditldmt
 ! Hello from image            2  on host /Users/rditldmt
 ! Hello from image            3  on host /Users/rditldmt
 ! Hello from image            4  on host /Users/rditldmt
 ! Hello from image            5  on host /Users/rditldmt
 ! Hello from image            6  on host /Users/rditldmt
 ! Hello from image            7  on host /Users/rditldmt
 ! Hello from image            8  on host /Users/rditldmt
 ! Hello from image            9  on host /Users/rditldmt
 ! Hello from image           10  on host /Users/rditldmt
 ! Hello from image           11  on host /Users/rditldmt
 ! Hello from image           12  on host /Users/rditldmt
 ! 17:06 dan-topas-pro-2 rditldmt $ mpifort -fcoarray=lib ex2.f08 -L/opt/local/lib/ -lcaf_mpi -o ex2
 ! 17:07 dan-topas-pro-2 rditldmt $ mpirun -np 12 ./ex2
 ! Hello from image            1  on host /Users/rditldmt
 ! Hello from image            2  on host /Users/rditldmt
 ! Hello from image            3  on host /Users/rditldmt
 ! Hello from image            4  on host /Users/rditldmt
 ! Hello from image            5  on host /Users/rditldmt
 ! Hello from image            6  on host /Users/rditldmt
 ! Hello from image            7  on host /Users/rditldmt
 ! Hello from image            8  on host /Users/rditldmt
 ! Hello from image            9  on host /Users/rditldmt
 ! Hello from image           10  on host /Users/rditldmt
 ! Hello from image           11  on host /Users/rditldmt
 ! Hello from image           12  on host /Users/rditldmt


!  17:22 dan-topas-pro-2 rditldmt $ mp ex2
!  17:22 dan-topas-pro-2 rditldmt $ mpirun -np 4 ./ex2
! Hello from image 1 on host /Users/rditldmt.
! Hello from image 2 on host /Users/rditldmt.
! Hello from image 3 on host /Users/rditldmt.
! Hello from image 4 on host /Users/rditldmt.
!  17:22 dan-topas-pro-2 rditldmt $ cf ex2
!  17:22 dan-topas-pro-2 rditldmt $ cafrun -np 6 ./ex2
! Hello from image 1 on host /Users/rditldmt.
! Hello from image 2 on host /Users/rditldmt.
! Hello from image 3 on host /Users/rditldmt.
! Hello from image 4 on host /Users/rditldmt.
! Hello from image 5 on host /Users/rditldmt.
! Hello from image 6 on host /Users/rditldmt.
