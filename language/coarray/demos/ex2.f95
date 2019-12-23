!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! https://www.sharcnet.ca/help/images/f/f5/Coarray_2015_08_05_final.pdfprogram ex1
program ex2

    implicit none

    character ( 80 ) :: host [ * ] ! Note: host – local; host[i] – on image i
    integer          :: i

        call get_environment_variable ( 'HOME', value = host )

        if ( this_image ( ) == 1 ) then
            do i = 1, num_images ( )
                print *, 'Hello from image ', i, ' on host ', trim ( host [ i ] )
            enddo
        endif

end program ex2

! dan-topas-pro-2:demos rditldmt$ date
! Mon Aug 31 17:07:00 CDT 2015
! dan-topas-pro-2:demos rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/demos
!  ! -fcoarray options: none, single, lib
! dan-topas-pro-2:demos rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=single ex2.f95
! dan-topas-pro-2:demos rditldmt$ ./a.out
!  Hello from image            1  on host /Users/rditldmt
! dan-topas-pro-2:demos rditldmt$

! gfortran: note: valid arguments to ‘-fcoarray=’ are: lib none single
