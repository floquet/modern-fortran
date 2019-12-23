!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! https://www.sharcnet.ca/help/images/f/f5/Coarray_2015_08_05_final.pdfprogram ex1
program ex1

    implicit none

    real :: z[*]
    integer :: i

    sync all

        if (this_image() == 1) then
            read *, z
            print '( "Image ",i4, " : before: z = ", f10.5 )', this_image ( ), z
            do i = 2, num_images( )
                z [ i ] = z
            enddo
        endif

        sync all

        print '("Image ",i4, ": after: z = ", f10.5) ', this_image ( ), z

end program ex1

! dan-topas-pro-2:demos rditldmt$ date
! Mon Aug 31 17:08:59 CDT 2015
! dan-topas-pro-2:demos rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/demos
! dan-topas-pro-2:demos rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=single ex1.f95
! ex1.f95:16:35:
!
!              do i = 2, num_images( )
!                                    1
! Warning: DO loop at (1) will be executed zero times [-Wzerotrip]
! dan-topas-pro-2:demos rditldmt$ ./a.out
!
!
! ^C
! dan-topas-pro-2:demos rditldmt$

! gfortran: note: valid arguments to ‘-fcoarray=’ are: lib none single
