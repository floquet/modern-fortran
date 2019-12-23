! Created by Tobias Burnus 2010.
! https://en.wikipedia.org/wiki/Coarray_Fortran

program Hello_World
  implicit none
  integer :: i  ! Local variable
  character(len=20) :: name[*] ! scalar coarray
  ! Note: "name" is the local variable while "name[<index>]"
  ! accesses the variable on a remote image

  ! Interact with the user on Image 1
  if (this_image() == 1) then
    write(*,'(a)',advance='no') 'Enter your name: '
    read(*,'(a)') name

    ! Distribute information to other images
    do i = 2, num_images()
      name[i] = name
    end do
  end if

  sync all ! Barrier to make sure the data has arrived

  ! I/O from all nodes
  write(*,'(3a,i0)') 'Hello ',trim(name),' from image ', this_image()
end program Hello_world

! dan-topas-pro-2:demos rditldmt$ date
! Mon Mar 14 17:45:24 CDT 2016
! dan-topas-pro-2:demos rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/demos
! dan-topas-pro-2:demos rditldmt$ mpifort -fcoarray=lib gcc_hello.f08 -L/opt/local/lib/ -lcaf_mpi -o gcc_hello
! dan-topas-pro-2:demos rditldmt$ mpirun -np 4 ./gcc_hello
! Enter your name: Gort
! Hello Gort from image 1
! Hello Gort from image 3
! Hello Gort from image 2
! Hello Gort from image 4

! dan-topas-pro-2:demos rditldmt$ date
! Mon Mar 14 17:47:07 CDT 2016
! dan-topas-pro-2:demos rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/demos
! dan-topas-pro-2:demos rditldmt$ caf gcc_hello.f08
! dan-topas-pro-2:demos rditldmt$ ./a.out
! Enter your name: Daniel
! Hello Daniel from image 1

! dan-topas-pro-2:demos rditldmt$ date
! Mon Mar 14 17:53:30 CDT 2016
! dan-topas-pro-2:demos rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/demos
! dan-topas-pro-2:demos rditldmt$ caf gcc_hello.f08
! dan-topas-pro-2:demos rditldmt$ cafrun -np 6 ./a.out
! Enter your name: Dan
! Hello Dan from image 1
! Hello Dan from image 3
! Hello Dan from image 4
! Hello Dan from image 5
! Hello Dan from image 6
! Hello Dan from image 2
