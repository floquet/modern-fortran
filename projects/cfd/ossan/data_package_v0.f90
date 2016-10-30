!********************************************************************************
!* --------- OSSAN (Oh, Such a Simple 'Ansutorakucha' Navier-Stokes) -----------
!*
!*      This module belongs to the inviscid version: OSSAN-Euler2D-Implicit
!*
!*
!* This file contains 3 data modules:
!*
!*  1. module constants
!*  2. module grid_data_type
!*  3. module my_main_data
!*
!* All data in the modules can be accessed by the use statement, 'use constants'.
!*
!* Written by Katate Masatsuka (http://www.cfdbooks.com)
!********************************************************************************



!********************************************************************************
!* --------- OSSAN (Oh, Such a Simple 'Ansutorakucha' Navier-Stokes) -----------
!*
!*         This module belongs to the inviscid version: OSSAN-Euler2D
!*
!*
!* Some useful constants are defined here.
!* They can be accessed by the use statement, 'use constants'.
!*
!* Written by Katate Masatsuka (http://www.cfdbooks.com)
!********************************************************************************
 module constants

  implicit none

  private

  public :: p2
  public :: zero, one, two, three, four, five, six, seven, eight, nine
  public :: half, third, fourth, fifth, sixth, two_third, four_third
  public :: three_fourth, twelfth, pi

  integer , parameter :: sp = kind(1.0)
  integer , parameter :: p2 = selected_real_kind(2*precision(1.0_sp))

  real(p2), parameter :: zero = 0.0_p2, &
                          one = 1.0_p2, &
                          two = 2.0_p2, &
                        three = 3.0_p2, &
                         four = 4.0_p2, &
                         five = 5.0_p2, &
                          six = 6.0_p2, &
                        seven = 7.0_p2, &
                        eight = 8.0_p2, &
                         nine = 9.0_p2, &
                         half = 0.5_p2, &
                        third = 1.0_p2/ 3.0_p2, &
                       fourth = 1.0_p2/ 4.0_p2, &
                        fifth = 1.0_p2/ 5.0_p2, &
                        sixth = 1.0_p2/ 6.0_p2, &
                    two_third = 2.0_p2/ 3.0_p2, &
                   four_third = 4.0_p2/ 3.0_p2, &
                 three_fourth = 3.0_p2/ 4.0_p2, &
                      twelfth = 1.0_p2/12.0_p2

  real(p2), parameter :: pi = 3.141592653589793238_p2

 end module constants
!********************************************************************************




!********************************************************************************
!* --------- OSSAN (Oh, Such a Simple 'Ansutorakucha' Navier-Stokes) -----------
!*
!*         This module belongs to the inviscid version: OSSAN-Euler2D
!*
!*
!* This module defines custom grid data types for unstructured grids
!*
!* NOTE: These data types are designed to make it easier to understand the code.
!*       They may not be the best in terms of efficiency.
!*
!* NOTE: Custom grid data types (derived types) are very useful.
!*       For example, if I declare a variable, "a", by the statemant:
!*           type(node_type), dimension(100) :: a
!*       The variable, a, is a 1D array each component of which contains all data
!*       defined as below. These data can be accessed by %, e.g.,
!*           a(1)%x, a(1)%y, a(1)%nghbr(1:nnghbrs), etc.
!*       In C-programming, this type of data is called "structure", I think.
!*
!* Written by Katate Masatsuka (http://www.cfdbooks.com)
!********************************************************************************
 module grid_data_type

  use constants, only : p2

  implicit none

  private

  public ::  node_type
  public ::   elm_type
  public ::  edge_type
  public :: bgrid_type
  public :: jac_type

!Data type for nodal quantities
  type node_type
!  to be read from a grid file
   real(p2)                         :: x, y     !nodal coordinates
!  to be constructed in the code
   integer                          :: nnghbrs  !number of neighbors
   integer,   dimension(:), pointer :: nghbr    !list of neighbors
   integer                          :: nelms    !number of elements
   integer,   dimension(:), pointer :: elm      !list of elements
   real(p2)                         :: vol      !dual-cell volume
!  to be computed in the code
   real(p2),  dimension(4)          :: u        !conservative variables
   real(p2),  dimension(4)          :: du       !change in conservative variables
   real(p2),  dimension(4)          :: res      !residual (rhs)
   real(p2),  dimension(4)          :: w        !primitive variables(optional)
   real(p2),  dimension(4,2)        :: gradw    !gradient of w
   real(p2),  dimension(4)          :: w_exact  !exact solutions (primitive)
   real(p2)                         :: phi      !limiter function (0 <= phi <= 1)
   real(p2)                         :: dt       !local time step
   real(p2)                         :: wsn      !Half the max wave speed at face
   real(p2),  dimension(2,2)        :: lsq01_2x2_minv !Inverse of linear LSQ matrix
   integer                          :: bmark
  end type node_type


!Data type for element quantities
  type elm_type
!  to be read from a grid file
   integer                          :: nvtx     !number of vertices
   integer,   dimension(:), pointer :: vtx      !list of vertices
!  to be constructed in the code
   integer                          :: nnghbrs  !number of neighbors
   integer,   dimension(:), pointer :: nghbr    !list of neighbors
   real(p2)                         :: x, y     !cell center coordinates
   real(p2)                         :: vol      !cell volume
  end type elm_type


!Data type for edge quantities
  type edge_type
!  to be constructed in the code
   integer                          :: n1, n2 !associated nodes
   integer                          :: e1, e2 !associated elements
   real(p2),           dimension(2) :: dav    !unit directed-area vector
   real(p2)                         :: da     !magnitude of the directed-area vector
   real(p2),           dimension(2) :: ev     !unit edge vector
   real(p2)                         :: e      !magnitude of the edge vector
   integer                          :: kth_nghbr_of_1 !neighbor index of node 2 w.r.t. node 1
   integer                          :: kth_nghbr_of_2 !neighbor index of node 1 w.r.t. node 2
  end type edge_type


!Data type for boundary quantities
  type bgrid_type
!  to be read from a boundary grid file
   character(80)                    :: bc_type !type of boundary condition
   integer                          :: nbnodes !# of boundary nodes
   integer,   dimension(:), pointer :: bnode   !list of boundary nodes
!  to be constructed in the code
   integer                          :: nbfaces !# of boundary faces
   real(p2),  dimension(:), pointer :: bfnx    !x-component of the face outward normal
   real(p2),  dimension(:), pointer :: bfny    !y-component of the face outward normal
   real(p2),  dimension(:), pointer :: bfn     !magnitude of the face normal vector
   real(p2),  dimension(:), pointer :: bnx     !x-component of the outward normal
   real(p2),  dimension(:), pointer :: bny     !y-component of the outward normal
   real(p2),  dimension(:), pointer :: bn      !magnitude of the normal vector
   integer ,  dimension(:), pointer :: belm    !list of elm adjacent to boundary face
   integer ,  dimension(:), pointer :: kth_nghbr_of_1
   integer ,  dimension(:), pointer :: kth_nghbr_of_2
  end type bgrid_type

  type jac_type
!  to be constructed in the code
   real(p2),  dimension(:,:)  , pointer :: diag ! Jacobian matrix
   real(p2),  dimension(:,:,:), pointer :: off
  end type jac_type


 end module grid_data_type
!********************************************************************************




!********************************************************************************
!* --------- OSSAN (Oh, Such a Simple 'Ansutorakucha' Navier-Stokes) -----------
!*
!*         This module belongs to the inviscid version: OSSAN-Euler2D
!*
!*
!* This module defines the main data that will be used in the code.
!*
!* The main data include parameters and data arrays. They can be accessed by
!* other routines via the use statement: 'use my_main_data'.
!*
!* Written by Katate Masatsuka (http://www.cfdbooks.com)
!********************************************************************************
 module my_main_data

  use constants     , only : p2
  use grid_data_type, only : node_type, elm_type, edge_type, bgrid_type, jac_type

  implicit none

  private

! Make these variables accesible in other modules

  public :: max_iterations, inviscid_flux, inviscid_jac
  public ::     CFL, CFLexp
  public ::   M_inf, rho_inf, u_inf, v_inf, p_inf, gamma

  public :: nnodes, node
  public ::  ntria, nquad, nelms, elm
  public :: nedges, edge
  public :: nbound, bound

  public :: tolerance

  public :: jac
  public :: iteration_method
  public :: CFL1, CFL2, CFL_ramp_steps
  public :: sweeps

!  Global Parameters

       integer :: max_iterations      ! Maximum number of iterations
 character(80) :: inviscid_flux       ! Inviscid flux for the residual
 character(80) :: inviscid_jac        ! Inviscid flux for Jacobian
      real(p2) :: M_inf, rho_inf      ! Free stream Mach and density
      real(p2) :: u_inf, v_inf, p_inf ! Free stream velocities and pressure
      real(p2) :: gamma               ! Ration of specfic heats
      real(p2) :: tolerance           ! Tolerance for steady convergence
      real(p2) :: CFL                 ! Actual CFL number
 character(80) :: iteration_method    ! explicit or implicit
!  Parameter for explicit scheme
      real(p2) :: CFLexp              ! Input CFL number for explicit RK2 scheme
!  Parameters for implicit scheme
      real(p2) :: CFL1, CFL2          ! Initial and terminal CFL number for ramping
       integer :: CFL_ramp_steps      ! Number of iterations to reach CFL2 from CFL1
       integer :: sweeps              ! Number of GS relaxation

!  Node data
   integer                                 :: nnodes !total number of nodes
   type(node_type), dimension(:), pointer  :: node   !array of nodes

!  Element data (element=cell)
   integer                                 :: ntria  !total number of triangles
   integer                                 :: nquad  !total number of quadrilaterals
   integer                                 :: nelms  !total number of elements
   type(elm_type),  dimension(:), pointer  :: elm    !array of elements

!  Edge data
   integer                                 :: nedges !total number of edges
   type(edge_type), dimension(:), pointer  :: edge   !array of edges

!  Boundary data
   integer                                 :: nbound !total number of boundary types
   type(bgrid_type), dimension(:), pointer :: bound  !array of boundary segments

!  Matrix data
   type(jac_type), dimension(:)  , pointer  :: jac   !array of edges

 end module my_main_data
!********************************************************************************

