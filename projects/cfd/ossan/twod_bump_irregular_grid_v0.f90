!  g95 -O2 -o mg  twod_flat_plate_irregular.f90
!  g95 -O2 -o mg -Wall -Wextra -fbounds-check twod_flat_plate_irregular.f90

!********************************************************************************
!* --- Grid Generation for a Bump in a Channel ---
!*
!*        written by Dr. Katate Masatsuka (info[at]cfdbooks.com),
!*
!* the author of useful CFD books, "I do like CFD" (http://www.cfdbooks.com).
!*
!* This program generates a triangular grid for a small bump in a channel.
!*
!* Input ---------------------------------------------------
!*              nx = Number of nodes in x-direction.
!*              ny = Number of nodes in y-direction.
!*            xmin = Left end of the rectangular domain
!*            xmax = Right end of the domain
!*            ymin = Top end of the domain
!*            ymax = Bottom end of the domain (except the bump)
!*
!* Output --------------------------------------------------
!*  "bump.grid"         = A grid data file containing boundary-node information.
!*                        This file is read by the OSSAN solver.
!*  "bump.bcmap"        = Boundary condition file for the OSSAN solver.
!*  "bump_grid_tec.dat" = Tecplot file for viewing the grid.
!*
!* This is Version 0 (January 2013).
!* This F90 code is written and made available for an educational purpose.
!* This file may be updated in future.
!*
!* Katate Masatsuka, January 2013. http://www.cfdbooks.com
!********************************************************************************
 program twod_bump_grid

 implicit none

!Local parameters
  integer, parameter :: sp = kind(1.0)
  integer, parameter :: p2 = selected_real_kind(2*precision(1.0_sp))
  real(p2) :: half=0.5_p2, one=1.0_p2, pi=3.141592653589793238_p2

!Local variables

!Input Parameters
 integer  ::   nx,   ny ! Number of nodes in x and y
 real(p2) :: xmin, xmax ! Define left end and right end of the domain
 real(p2) :: ymin, ymax ! Define top and bottom of the domain

!Grid file names
 character(80) :: datafile_tria     = "bump.grid"         ! Grid file for OSSAN
 character(80) :: datafile_bcmap    = "bump.bcmap"        !   BC file for OSSAN
 character(80) :: datafile_tria_tec = "bump_grid_tec.dat" ! Tecplot file

!Local variables
 real(p2), dimension(:,:), allocatable :: xs, ys ! Structured grid data
 integer                               :: nnodes ! Number of total nodes
 integer                               :: ntri   ! Number of triangles
 integer , dimension(:,:), allocatable :: tri    ! Triangle data
 real(p2), dimension(:)  , allocatable :: x, y   ! 1D arrays for x and y
 integer  :: i, j  !
 integer  :: inode !
 real(p2) :: dx    ! Grid spacing in x-direction (constant)
 real(p2) :: dy    ! Grid spacing in y-direction (variable)
 real(p2) :: rn    ! Random number for nodal perturbation
 real(p2) :: sf, s, smin, smax ! Parameters for grid stretching in y

 write(*,*) "***************************************************************"
 write(*,*) " Start generating a grid for a bump in a channel."
 write(*,*) "***************************************************************"
 write(*,*)

!--------------------------------------------------------------------------------
! 1. Input parameters
!
! ymax --------------------------------------
!      .                                    .
!      .                                    .
!      .                                    .
!      .                                    .
!      .                                    .
!      .               Bump                 .
! ymin -----------------/\-------------------
!    xmin                                  xmax
!
     nx   = 320     ! Number of nodes in x-direction
     ny   =  40     ! Number of nodes in y-direction
     xmin = -2.0_p2 ! x-coordinate of the left end
     xmax =  3.0_p2 ! x-coordinate of the right end
     ymin =  0.0_p2 ! y-coordinates of the bottom
     ymax =  2.0_p2 ! y-coordinates of the top

!--------------------------------------------------------------------------------
! 2. Allocate arrays (temporary structured (i,j) data arrays)

     allocate(xs(nx,ny),ys(nx,ny))

!--------------------------------------------------------------------------------
! 3. Generate a Uniform 2D grid, using (i,j) data: go up in y-direction!

     write(*,*) "Generating structured data..."

!    Initial spacing (dy will be modified later)

        dx = (xmax-xmin)/real(nx-1) ! Uniform spacing in x
        dy = (ymax-ymin)/real(ny-1) ! This will be changed below.

!    Generate nodes in (i,j) format (uniform spacing here also)

     x_direction : do i = 1, nx
      y_direction : do j = 1, ny

      xs(i,j) = xmin + dx*real(i-1)

      if ( j == 1 ) then ! Bottom

       if ( xs(i,j) >= 0.3_p2 .and. xs(i,j) <=1.2_p2 ) then

 !      Here is a bump (same as the one in http://turbmodels.larc.nasa.gov).
        ys(i,j) = ymin + 0.05_p2*(sin(pi*xs(i,j)/0.9_p2-(pi/3.0_p2)))**4
             dy = (ymax - ys(i,j)) / real(ny-1) ! Unoform spacing from the bump

       else

             dy = (ymax - ymin) / real(ny-1)    ! Uniform spacing from y=ymin
        ys(i,j) = ymin + dy*real(j-1)

       endif

      else

        ys(i,j) = ys(i,1) + dy*real(j-1)

      endif

      end do y_direction
     end do x_direction

!--------------------------------------------------------------------------------
! 4. Stretching in y-direction: clustering towards the bottom.

     write(*,*) "Stretching in y-direction..."

     sf = 3.5_p2 ! 1.0 for no stretching

     do i = 1, nx
       smin = ys(i, 1)
       smax = ys(i,ny)
      do j = 1, ny
             s = (ys(i,j)-smin)/(smax-smin)    ! Transform ys into s = [0,1].
             s = (one-exp(sf*s))/(one-exp(sf)) ! Stretching in [0,1]
       ys(i,j) = s*(smax-smin) + smin          ! Transform back to ys.
      end do
     end do

!--------------------------------------------------------------------------------
! 5. Perturb the nodal coordinates to generate an irregular grid

     write(*,*) "Perturbing nodal coordinates..."

     do j = 2, ny-1
      do i = 2, nx-1
       call random_number(rn) ! rn = [0,1]
            dx = half*( xs(i+1,j  ) - xs(i-1,j  ) )
            dy = half*( ys(i  ,j+1) - ys(i  ,j-1) )
       xs(i,j) = xs(i,j) + (rn-half)* dx*0.2d0
       ys(i,j) = ys(i,j) - (rn-half)* dy*0.2d0
      end do
     end do

!--------------------------------------------------------------------------------
! 6. Generate unstructured data (1D arrays for x and y, and triangles):

     write(*,*) "Generating unstructured data..."
     nnodes = nx*ny
     allocate(x(nnodes),y(nnodes))

!    Node data (1D array) in lexcographic ordering

     do j = 1, ny
      do i = 1, nx
       inode = i + (j-1)*nx
       x(inode) =   xs(i,j)
       y(inode) =   ys(i,j)
      end do
     end do

!    Deallocate the structured data: xs and ys, which are not needed any more.
!    - You guys helped me create the 1D array. Thanks!

     deallocate(xs, ys)

!--------------------------------------------------------------------------------
! 7. Generate unstructured data: up to this point, we have a quadrilateral grid.

     allocate( tri(2*(nx-1)*(ny-1),3) )

!    Triangular grid (create triangles by randomly inserting diagonals into quads)

     write(*,*) "Generating triangular grid..."
     call generate_tri_grid(tri, ntri, nx, ny)


!    Write a Tecplot file

     write(*,*) "Writing a Tecplot file...", datafile_tria_tec
     call write_tecplot_file(datafile_tria_tec, nnodes, tri, ntri, x, y)


!    Write a grid file for the OSSAN solver

     write(*,*) "Writing a grid file for the OSSAN solver...", datafile_tria
     call write_grid_file(datafile_tria, nnodes, tri, ntri, x, y)


!    Write bcmap file for the OSSAN solver

     write(*,*) "Writing a BC file for the OSSAN solver...", datafile_bcmap
     open(unit=1, file=datafile_bcmap, status="unknown")
     write(1,*) '  Boundary Segment              Boundary Condition'
     write(1,*) '              1                 "slip_wall_weak"  '
     write(1,*) '              2                 "freestream"      '
     write(1,*) '              3                 "freestream"      '
     write(1,*) '              4                 "freestream"      '
     close(1)

!--------------------------------------------------------------------------------

     write(*,*) " nx=",nx, " ny=", ny
     write(*,*) " nx*ny=",nx*ny
     write(*,*) " nnodes=",nnodes
     write(*,*) " Successfully completed grid generation. Stop."
     write(*,*)
     write(*,*)
     write(*,*) "***************************************************************"
     write(*,*) " End of grid generation."
     write(*,*) "***************************************************************"

     stop

 contains

!********************************************************************************
 subroutine generate_tri_grid(tri, ntri, nx, ny)
 implicit none
 integer,                 intent( in) ::   nx, ny
 integer,                 intent(out) :: ntri
 integer, dimension(:,:), intent(out) ::  tri
!Local variables
 integer  :: i, j, inode, i1, i2, i3, i4
 real(p2) :: rn, rn2

! Trianguler grid with random diagonals, i.e., / or \.

 ntri = 0

 do j = 1, ny-1
  do i = 1, nx-1

   inode = i + (j-1)*nx
      i1 = inode
      i2 = inode + 1
      i3 = inode + nx + 1
      i4 = inode + nx

   call random_number(rn)
   rn2 = 2.0d0*rn-1.0d0

   if (rn2 > 0.0d0 ) then
!! /
           ntri = ntri + 1
    tri(ntri,1) = i1
    tri(ntri,2) = i2
    tri(ntri,3) = i3

           ntri = ntri + 1
    tri(ntri,1) = i1
    tri(ntri,2) = i3
    tri(ntri,3) = i4

   else

!! \
           ntri = ntri + 1
    tri(ntri,1) = i1
    tri(ntri,2) = i2
    tri(ntri,3) = i4

           ntri = ntri + 1
    tri(ntri,1) = i2
    tri(ntri,2) = i3
    tri(ntri,3) = i4

   endif

  end do
 end do

 end subroutine generate_tri_grid
!********************************************************************************

!********************************************************************************
 subroutine write_tecplot_file(datafile_tec,nnodes,tri,ntri,x,y)
 implicit none
 character(80),            intent(in) :: datafile_tec
 integer ,                 intent(in) :: ntri, nnodes
 integer , dimension(:,:), intent(in) ::  tri
 real(p2), dimension(:)  , intent(in) :: x , y
 integer :: os
!--------------------------------------------------------------------------------
 open(unit=1, file=datafile_tec, status="unknown", iostat=os)
 write(1,*) 'title = "grid"'
 write(1,*) 'variables = "x","y",'
 write(1,*) 'zone N=',nnodes,',E=',ntri, ',ET=quadrilateral,F=FEPOINT'
!--------------------------------------------------------------------------------
 do i = 1, nnodes
  write(1,*) x(i),y(i)
 end do
!--------------------------------------------------------------------------------
! Both elements in quad format:
 do i = 1, ntri
  write(1,*)  tri(i,1),  tri(i,2), tri (i,3),  tri(i,3) !The last one is a dummy.
 end do
!--------------------------------------------------------------------------------
 close(1)
 end subroutine write_tecplot_file
!********************************************************************************

!********************************************************************************
!* This generates a grid file to be read by OSSAN solver.
!********************************************************************************
 subroutine write_grid_file(datafile_tec,nnodes,tri,ntri,x,y)
 implicit none
 character(80),            intent(in) :: datafile_tec
 integer ,                 intent(in) :: ntri, nnodes
 integer , dimension(:,:), intent(in) :: tri
 real(p2), dimension(:)  , intent(in) :: x, y
 integer :: os
!--------------------------------------------------------------------------------
 open(unit=1, file=datafile_tec, status="unknown", iostat=os)
!--------------------------------------------------------------------------------
! Grid size: # of nodes, # of triangles, # of quadrilaterals
  write(1,*) nnodes, ntri, 0 !<- 0 indicates there are no quadrilaterals.
!--------------------------------------------------------------------------------
! Node data
  do i = 1, nnodes
   write(1,*) x(i), y(i)
  end do
!--------------------------------------------------------------------------------
! Triangles: 3 node numbers that forms a triangle
   do i = 1, ntri
    write(1,*) tri(i,1), tri(i,2), tri(i,3)
   end do

! Boundary nodes are ordered counterclockwise (seeing the interior domain on the left)
  write(1,*) 4  !4 boundary segments
  write(1,*) nx !Number of nodes on the Bottom
  write(1,*) ny !Number of nodes on the Right
  write(1,*) nx !Number of nodes on the Top
  write(1,*) ny !Number of nodes on the Left

! Bottom
  j = 1
   do i = 1, nx
    inode = i + (j-1)*nx
    write(1,*) inode, 0
   end do
! Right
  i = nx
  do j = 1, ny
    inode = i + (j-1)*nx
    write(1,*) inode, 0
  end do
! Top
  j = ny
   do i = nx, 1, -1
    inode = i + (j-1)*nx
    write(1,*) inode, 0
   end do
! Left
  i = 1
  do j = ny, 1, -1
    inode = i + (j-1)*nx
    write(1,*) inode, 0
  end do

 close(1)

 end subroutine write_grid_file
!********************************************************************************

 end program twod_bump_grid