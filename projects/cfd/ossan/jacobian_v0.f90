!********************************************************************************
!* --------- OSSAN (Oh, Such a Simple 'Ansutorakucha' Navier-Stokes) -----------
!*
!*     This module belongs to the inviscid version: OSSAN-Euler2D-Implicit
!*
!*
!* This module contains all subroutines needed to advance the solution in time.
!*
!* Contained subroutines/functions:
!* -----------------------------------------------------------------------------
!* construct_jacobian_ncfv         : Computation of Jacobian
!* inviscid_pressure_flux_jacobian : Exact jacobian for the 2D pressure flux
!* inviscid_roe_n_flux_jacobian    : Exact Jacobian of the 2D Roe flux
!* w2u                             : Primitive to conservative variables
!* -----------------------------------------------------------------------------
!*
!* Written by Katate Masatsuka (http://www.cfdbooks.com)
!********************************************************************************
 module jacobian

 private

 public :: construct_jacobian_ncfv

 contains

!********************************************************************************
!* This subroutine computes the Jacobian matrix (LHS), which is the left hand side
!* of      [V/dt+dR/dU]*dU = -Residual, where dt is a local time step.
!*
!*
!* Note: The Jacobian matirx is stored in two parts: diagonal and off-diagonal blocks.
!*
!*       For example, consider the residual at node j, Rj, which is a vector,
!*       and suppose j has 5 neighbor nodes (1, 2, 3, 4, 5).
!*       (Note: Actual node number of the neighbors are stored in node(j)%nghbr(1:k).)
!*       For defect correction, the residual is taken as 1st-order scheme, so that
!*       Res_j is a function of Uj, U1, U2, U3, U4, and U5 (compact stencil).
!*       Then, the j-th component of [V/dt+dR/dU]*dU = -Residual is
!*
!*       [V/dt1+dRj/dU1]*dU1 + [V/dt2+dRj/dU2]*dU2 + [V/dt3+dRj/dU3]*dU3 +
!*       [V/dtj+dRj/dUj]*dUj + [V/dt4+dRj/dU4]*dU4 + [V/dt5+dRj/dU5]*dU5 = - Rj
!*
!*       where dtj is the local time step at j, dRj/dUj is the derivative of Rj
!*       with respect to the solution at j, i.e., Uj, and similarly for others.
!*       The Gauss-Seidel relaxation is given by
!*
!*       dUj = [V/dtj+dR/dUj]^{-1}*(- [V/dt1+dRj/dU1]*dU1 - [V/dt2+dRj/dU2]*dU2
!*                                  - [V/dt3+dRj/dU3]*dU3 - [V/dt4+dRj/dU4]*dU4
!*                                  - [V/dt5+dRj/dU5]*dU5 - Res_j )
!*
!*       To perform this operation, we store two types of data:
!*
!*        1.  4x4     Diagonal block:  jac(j)%diag(:,:)  = [V/dtj+dRj/dUj]
!*        2.  4x4 Off-Diagonal Block:  jac(j)%off(:,:,k) = [V/dtk+dRj/dUk], k = 1,2,3,4,5
!*
!*       so, we can perform the relaxation at every node by looping over the neighbors:
!*
!*         b = -Res_j
!*        do k = 1, nnghbrs
!*         b = b - (jac(j)%off(:,:,k))*dUk
!*        end do
!*         dUj = (jac(j)%diag)^{-1}*b
!*
!*       Actual relaxation will be performed in linear_solve_v0.f90.
!*
!*       To compute dR/dUj and dR/dUk, just like the residual is computed as a sum of
!*       numerical fluxes over edges, we compute the Jacobian blocks as a sum of
!*       derivatives of the numerical flux overe edges. So, we loop ever edges,
!*       compute the derivatives of the numerical flux with respect to each end node
!*       of the edge, and accumulate the contribution at the two nodes.
!*       At the end of the edge loop, at every node j, we have
!*
!*               dRj/dUj = sum_over_edges ( dflux/dUj )
!*               dRj/dUk = dflux/dUk
!*
!*       which forms the j-th row of the global Jacobian. Here, 'flux' is the
!*       numerical flux between nodes j and k.
!*
!*       So, the following subroutine is very similar to the subroutine that computes
!*       the residual as you can see below.
!*
!* Note: The Roe flux Jacobian subroutine is very inefficient as it is not optimized
!*       for the sake of readability. Think about how you can optimize it.
!*
!* ------------------------------------------------------------------------------
!*  Input:  node(:)%u   = the current solution
!*
!* Output:  jac(:)%diag = 4x4 diagonal block at each node
!*          jac(:)%off  = 4x4 off-diagonal block for each neighbor of each node
   !* ------------------------------------------------------------------------------
!*
!********************************************************************************
 subroutine construct_jacobian_ncfv

 use constants   , only : p2, zero, one, two, four, half, third

 use my_main_data, only : nnodes, node, nedges, edge, nbound, bound, &
                          rho_inf, u_inf, v_inf, p_inf, &
                          inviscid_flux, jac, inviscid_jac
 implicit none

!Local variables
 real(p2), dimension(2) :: n12            !Unit face normal vector
 real(p2)               :: mag_n12        !Magnitude of the face-normal vector
 integer                :: node1, node2   !Left and right nodes of each edge
 integer                :: n1, n2         !Left and right nodes of boundary face
 integer                :: i, j, k

 real(p2), dimension(4,4) :: bjac1, bjac2 !Boundary flux diagonal Jacobian at nodes 1 and 2

 real(p2), dimension(4)   :: w_out, u_out     !State outside the domain.
 real(p2), dimension(4,4) :: dFnducL, dFnducR !Flux Jacobian matrices
!-------------------------------------------------------------------------
! Jacobian computation

! Initialization
  nodes : do i = 1, nnodes
   jac(i)%diag      = zero
   jac(i)%off       = zero
  end do nodes

! Jacobian computation across internal edges (to be accumulated in diag and off)
!
!   node2
!       o
!        \   face      1. Compute the flux Jacobian
!         \ -------c2  2. Add it to the jacobian for n1, and subtract it from
!        / \              the jacobian for n2.
!   face/   \ edge
!      /     o         Directed area is the sum of the left and the right faces.
!    c1    node1       Left/right face is defined by the edge-midpoint and
!                      the centroid of the left/right element.
!                      Directed area is positive in n1 -> n2
!
! (c1, c2: element centroids)
!
!--------------------------------------------------------------------------------
  edges : do i = 1, nedges
!--------------------------------------------------------------------------------

! Left and right nodes of the i-th edge

    node1 = edge(i)%n1  ! Left node of the edge
    node2 = edge(i)%n2  ! Right node of the edge
      n12 = edge(i)%dav ! This is the directed area vector (unit vector)
  mag_n12 = edge(i)%da  ! Magnitude of the directed area vector

!  Compute the flux Jacobian

!  (1) Roe flux jacobian (carbuncle is expected for strong shocks)
   if     (trim(inviscid_jac)=="roe") then

    call inviscid_roe_n_flux_jacobian(node(node1)%u, node(node2)%u, n12, dFnducL, dFnducR)

   else

    write(*,*) " Invalid input for inviscid_flux = ", trim(inviscid_flux)
    write(*,*) " Choose roe (no others available), and try again."
    write(*,*) " ... Stop."
    stop

   endif

!  Add the Jacobian multiplied by the magnitude of the directed area vector to node1,
!  and accumulate the max wave speed quantity for use in the time step calculation.
!

   jac(node1)%diag       = jac(node1)%diag       + dFnducL * mag_n12

   k = edge(i)%kth_nghbr_of_1
   jac(node1)%off(:,:,k) = jac(node1)%off(:,:,k) + dFnducR * mag_n12


!  Subtract the Jacobian multiplied by the magnitude of the directed area vector from node2,
!  and accumulate the max wave speed quantity for use in the time step calculation.
!
!  NOTE: Subtract because the outward face normal is -n12 for the node2.

   jac(node2)%diag       = jac(node2)%diag       - dFnducR * mag_n12

   k = edge(i)%kth_nghbr_of_2
   jac(node2)%off(:,:,k) = jac(node2)%off(:,:,k) - dFnducL * mag_n12

!--------------------------------------------------------------------------------
  end do edges
!--------------------------------------------------------------------------------

!-------------------------------------------------------------------------
! Close with the boundary flux Jacobian using the element-based formula that is
! exact for linear fluxes (See Nishikawa AIAA2010-5093 for boundary weights
! that ensure the linear exactness for 2D/3D elements).
!
!      |  Interior Domain          |
!      |        .........          |
!      |        .       .          |
!      |        .       .          |
!      o--o--o-----o---------o--o--o  <- Boundary segment
!                  n1   |   n2
!                       v
!                     n12 (unit face normal vector)
!
! NOTE: We visit each boundary face, defined by the nodes n1 and n2,
!       and compute the flux Jacobian across the boundary face: left half for node1,
!       and the right half for node2. In the above figure, the dots indicate
!       the control volume around the node n1. Clearly, the flux across the
!       left half of the face contributes to the node n1. Similarly for n2.
!
!
!--------------------------------------------------------------------------------
  bc_loop : do i = 1, nbound
!--------------------------------------------------------------------------------

!------------------------------------------------
!  BC: Upwind flux Jacobian via free stream values
!

   bc : if (trim(bound(i)%bc_type) == "freestream") then

    bnodes_numerical_flux_via_freestream : do j = 1, bound(i)%nbfaces

           n1 = bound(i)%bnode(j  )  !Left node
           n2 = bound(i)%bnode(j+1)  !Right node
       n12(1) = bound(i)%bfnx(j)     !x-component of the unit face normal vector
       n12(2) = bound(i)%bfny(j)     !y-component of the unit face normal vector
      mag_n12 = bound(i)%bfn(j)*half !Half length of the boundary face, j.

!   1. Left node

      w_out = (/ rho_inf, u_inf, v_inf, p_inf /)
      u_out = w2u(w_out)

      call inviscid_roe_n_flux_jacobian(node(n1)%u, u_out, n12, dFnducL, dFnducR)
      bjac1 = dFnducL ! dFnducR is not needed because the right state is fixed.

!   2. Right node

      w_out = (/ rho_inf, u_inf, v_inf, p_inf /)
      u_out = w2u(w_out)

      call inviscid_roe_n_flux_jacobian(node(n2)%u, u_out, n12, dFnducL, dFnducR)
      bjac2 = dFnducL ! dFnducR is not needed because the right state is fixed.

!   3. Add contributions to the two nodes (See Nishikawa AIAA2010-5093)

      jac(n1)%diag = jac(n1)%diag + (5.0_p2*bjac1)/6.0_p2*mag_n12
      jac(n2)%diag = jac(n2)%diag + (5.0_p2*bjac2)/6.0_p2*mag_n12

      k = bound(i)%kth_nghbr_of_1(j)
      jac(n1)%off(:,:,k) = jac(n1)%off(:,:,k) + (bjac2)/6.0_p2*mag_n12

      k = bound(i)%kth_nghbr_of_2(j)
      jac(n2)%off(:,:,k) = jac(n2)%off(:,:,k) + (bjac1)/6.0_p2*mag_n12

    end do bnodes_numerical_flux_via_freestream

!------------------------------------------------
!  BC: Weak condition on a slip solid wall
!
!      NOTE: Physical flux has only pressure flux since qn=0.

   elseif (trim(bound(i)%bc_type) == "slip_wall_weak") then

    bnodes_slip_wall_weak : do j = 1, bound(i)%nbfaces

         n1 = bound(i)%bnode(j  ) ! Left node
         n2 = bound(i)%bnode(j+1) ! Right node
     n12(1) = bound(i)%bfnx(j)
     n12(2) = bound(i)%bfny(j)
    mag_n12 = bound(i)%bfn(j)*half

!   1. Left node
       bjac1 = inviscid_pressure_flux_jacobian(node(n1)%u, n12)

!   2. Right node
       bjac2 = inviscid_pressure_flux_jacobian(node(n2)%u, n12)

!   3. Add contributions to the two nodes (See Nishikawa AIAA2010-5093)

      jac(n1)%diag = jac(n1)%diag + (5.0_p2*bjac1)/6.0_p2*mag_n12
      jac(n2)%diag = jac(n2)%diag + (5.0_p2*bjac2)/6.0_p2*mag_n12

      k = bound(i)%kth_nghbr_of_1(j)
      jac(n1)%off(:,:,k) = jac(n1)%off(:,:,k) + (bjac2)/6.0_p2*mag_n12

      k = bound(i)%kth_nghbr_of_2(j)
      jac(n2)%off(:,:,k) = jac(n2)%off(:,:,k) + (bjac1)/6.0_p2*mag_n12

    end do bnodes_slip_wall_weak

!------------------------------------------------

   endif bc

!--------------------------------------------------------------------------------
  end do bc_loop
!--------------------------------------------------------------------------------

!------------------------------------------------
! Add a pseudo time term to the diagonals of the diagonal block
! Note: It has no impact if CFL=infinity, i.e., dt=infinity.

  nodes2 : do i = 1, nnodes

   jac(i)%diag(1,1) = jac(i)%diag(1,1) + node(i)%vol / node(i)%dt
   jac(i)%diag(2,2) = jac(i)%diag(2,2) + node(i)%vol / node(i)%dt
   jac(i)%diag(3,3) = jac(i)%diag(3,3) + node(i)%vol / node(i)%dt
   jac(i)%diag(4,4) = jac(i)%diag(4,4) + node(i)%vol / node(i)%dt

  end do nodes2

 end subroutine construct_jacobian_ncfv
!--------------------------------------------------------------------------------


!********************************************************************************
!* -- 2D Analytical Flux Jacobian for the pressure flux --
!*
!* This subroutine computes the analytical flux Jacobian for the physical flux
!* with zero normal velocity (pressure flux).
!*
!* Conservative form of the Euler equations:
!*
!*     dU/dt + dF/dx + dG/dy = 0
!*
!* The normal flux is defined by
!*
!*     Fn = F*nx + G*ny = | rho*qn          |
!*                        | rho*qn*u + p*nx |
!*                        | rho*qn*v + p*ny |
!*                        | rho*qn*H        |    (qn = u*nx + v*ny)
!*
!* At slip boundary, qn = 0, and therefore, we are left with
!*
!*     Fn = F*nx + G*ny = |    0 |
!*                        | p*nx |
!*                        | p*ny |
!*                        |    0 |
!*
!* The analytical flux Jacobian is defined by
!*
!*      dFn/dU
!*
!* This subroutine computes the analytical Jacobian exactly.
!*
!* Katate Masatsuka, January 2013. http://www.cfdbooks.com
!********************************************************************************
 function inviscid_pressure_flux_jacobian(uc, njk)

 use constants   , only : p2                ! Double precision

 implicit none

!Input
 real(p2), dimension(4), intent(in) :: uc  ! uc = [rho, rho*u, rho*v, rho*E]
 real(p2), dimension(2), intent(in) :: njk !Normal vector

!Output
 real(p2), dimension(4,4)           :: inviscid_pressure_flux_jacobian

!Some constants
 real(p2) ::  zero = 0.0_p2
 real(p2) ::   one = 1.0_p2
 real(p2) ::  half = 0.5_p2
 real(p2) :: gamma = 1.4_p2 ! Ratio of specific heats

!Local variables
 real(p2) :: u, v           ! Velocity components.
 real(p2) :: q2             ! Velocity squared
 real(p2) :: nx,ny          ! Normal vector components

 nx = njk(1)
 ny = njk(2)

!Velocity components and speed squared

      u = uc(2)/uc(1)
      v = uc(3)/uc(1)
     q2 = u*u  + v*v

! Initialize the jacobian matrix

   inviscid_pressure_flux_jacobian = zero

! Note: 1st row and the 4th row are zero.
! Note: p = (gamma-1)*( u4 - (u2^2 + u3^2)/(2*u1) ),
!       where u1=rho, u2=rho*u, u3=rho*v, u4=rho*E.

! 2nd row
  inviscid_pressure_flux_jacobian(2,1) =  half*(gamma-one)*q2 * nx ! d(p*nx)/du1
  inviscid_pressure_flux_jacobian(2,2) = -(gamma-one)*u       * nx ! d(p*nx)/du2
  inviscid_pressure_flux_jacobian(2,3) = -(gamma-one)*v       * nx ! d(p*nx)/du3
  inviscid_pressure_flux_jacobian(2,4) =  (gamma-one)         * nx ! d(p*nx)/du4

! 3rd row
  inviscid_pressure_flux_jacobian(3,1) =  half*(gamma-one)*q2 * ny ! d(p*ny)/du1
  inviscid_pressure_flux_jacobian(3,2) = -(gamma-one)*u       * ny ! d(p*ny)/du2
  inviscid_pressure_flux_jacobian(3,3) = -(gamma-one)*v       * ny ! d(p*ny)/du3
  inviscid_pressure_flux_jacobian(3,4) =  (gamma-one)         * ny ! d(p*ny)/du4

 end function inviscid_pressure_flux_jacobian
!--------------------------------------------------------------------------------


!********************************************************************************
!* -- 2D Roe Flux Jacobian (EXACT JACOBIAN) --
!*
!* This subroutine computes the left and right Jacobians for the Roe flux
!* with an entropy fix for the Euler equations in the direction, njk=[nx,ny].
!*
!* Conservative form of the Euler equations:
!*
!*     dU/dt + dF/dx + dG/dy = 0
!*
!* The normal flux is defined by
!*
!*     Fn = F*nx + G*ny = | rho*qn          |
!*                        | rho*qn*u + p*nx |
!*                        | rho*qn*v + p*ny |
!*                        | rho*qn*H        |    (qn = u*nx + v*ny)
!*
!* The Roe flux is given by
!*
!*   Fn_Roe = 1/2 [ Fn(UR) + Fn(UL) - |An|dU ],
!*
!*  where
!*
!*    An = dFn/dU,  |An| = R|Lambda|L, dU = UR - UL.
!*
!* The dissipation term, |An|dU, is actually computed as
!*
!*     sum_{k=1,4} |lambda_k| * (LdU)_k * r_k,
!*
!* where lambda_k is the k-th eigenvalue, (LdU)_k is the k-th wave strength,
!* and r_k is the k-th right-eigenvector evaluated at the Roe-average state.
!*
!* Note: The 4th component is expressed in terms of the normal vector only;
!*       a tangent vector is not used. In the way it is implemented below,
!*       (LdU)_4 is not really a wave strength, and r_4 is not really an eigenvector.
!*       See "I do like CFD, VOL.1" about how a tangent vector are eliminated.
!*
!* Note: In the code, the vector of conserative variables are denoted by uc.
!*
!* Note: This subroutine is very inefficient. It is written this way for
!*       an educational purpose. Think about how you can optimize it.
!*
!* ------------------------------------------------------------------------------
!*  Input: ucL(1:4) =  Left state (rhoL, rhoL*uL, rhoL*vL, rhoL*EL)
!*         ucR(1:4) = Right state (rhoR, rhoR*uR, rhoR*vR, rhoR*ER)
!*         njk(1:2) = unit face normal vector (nx, ny), pointing from Left to Right.
!*
!*           njk
!*  Face normal ^   o Right data point
!*              |  .
!*              | .
!*              |.
!*       -------x-------- Face
!*             .
!*            .
!*           .
!*          o Left data point
!*
!*
!* Output: dFnducL: Derivative of the Roe flux w.r.t. the left state ucL
!*         dFnducR: Derivative of the Roe flux w.r.t. the left state ucR
!* ------------------------------------------------------------------------------
!*
!* Katate Masatsuka, January 2013. http://www.cfdbooks.com
!********************************************************************************
 subroutine inviscid_roe_n_flux_jacobian(ucL, ucR, njk, dFnducL, dFnducR)

 use constants   , only : p2                ! Double precision

 implicit none

!Input
 real(p2), dimension(4)  , intent( in) :: ucL       !  Left state (conservative variables)
 real(p2), dimension(4)  , intent( in) :: ucR       ! Right state (conservative variables)
 real(p2), dimension(2)  , intent( in) :: njk       ! Normal vector

!Output
 real(p2), dimension(4,4), intent(out) :: dFnducL   !  Left Jacobian, d(Fn_Roe)/d(ucL)
 real(p2), dimension(4,4), intent(out) :: dFnducR   ! Right Jacobian, d(Fn_Roe)/d(ucR)

!Some constants
 real(p2) ::  zero = 0.0_p2
 real(p2) ::   one = 1.0_p2
 real(p2) ::   two = 2.0_p2
 real(p2) :: fifth = 0.2_p2
 real(p2) ::  half = 0.5_p2
 real(p2) :: gamma = 1.4_p2         ! Ratio of specific heats

!Local variables:
!
!            L = Left
!            R = Right
! No subscript = Roe average

 real(p2) :: nx, ny                 ! Normal vector components

 real(p2) :: uL, uR, vL, vR         ! Velocity components.
 real(p2) :: rhoL, rhoR, pL, pR     ! Primitive variables.
 real(p2) :: qnL, qnR               ! Normal velocities
 real(p2) :: q2L, q2R               ! Magnitude of the velocities
 real(p2) :: aL, aR, HL, HR         ! Speed of sound, Total enthalpy

 real(p2) :: RT                     ! RT = sqrt(rhoR/rhoL)
 real(p2) :: rho,u,v,H,a,qn         ! Roe-averages
 real(p2) :: q2                     ! Squared Roe-averaged speed

 real(p2) :: drho,dqn,dp            ! Differences in rho, qn, p, e.g., dp=pR-pL
 real(p2) :: du, dv                 ! Velocity differences
 real(p2), dimension(4)   :: LdU    ! Wave strengths = L*(UR-UL)
 real(p2), dimension(4)   :: ws     ! Wave speeds
 real(p2), dimension(4)   :: dws    ! Width of a parabolic fit for entropy fix
 real(p2), dimension(4,4) :: R      ! Right-eigenvector matrix

! Derivatives w.r.t. ucL
 real(p2), dimension(4)   :: drho_ducL    ! drho/ducL
 real(p2), dimension(4)   :: dqn_ducL     ! dqn/ducL
 real(p2), dimension(4)   :: du_ducL      ! du/ducL
 real(p2), dimension(4)   :: dv_ducL      ! dv/ducL
 real(p2), dimension(4)   :: dH_ducL      ! dH/ducL
 real(p2), dimension(4)   :: da_ducL      ! da/ducL
 real(p2), dimension(4)   :: dabs_qn_ducL ! d|qn|/ducL

 real(p2), dimension(4)   :: ddrho_ducL   ! d(drho)/ducL
 real(p2), dimension(4)   :: ddp_ducL     ! d(dp)  /ducL
 real(p2), dimension(4)   :: ddu_ducL     ! d(du)  /ducL
 real(p2), dimension(4)   :: ddv_ducL     ! d(dv)  /ducL
 real(p2), dimension(4)   :: ddqn_ducL    ! d(dqn) /ducL

 real(p2), dimension(4)   :: dws1_ducL    ! dws1/ducL
 real(p2), dimension(4)   :: dws2_ducL    ! dws2/ducL
 real(p2), dimension(4)   :: dws3_ducL    ! dws3/ducL
 real(p2), dimension(4)   :: dws4_ducL    ! dws4/ducL

 real(p2), dimension(4)   :: dLdU1_ducL   ! dLdU1/ducL
 real(p2), dimension(4)   :: dLdU2_ducL   ! dLdU2/ducL
 real(p2), dimension(4)   :: dLdU3_ducL   ! dLdU3/ducL
 real(p2), dimension(4)   :: dLdU4_ducL   ! dLdU4/ducL

 real(p2), dimension(4,4) :: dR1_ducL     ! dr1/ducL
 real(p2), dimension(4,4) :: dR2_ducL     ! dr2/ducL
 real(p2), dimension(4,4) :: dR3_ducL     ! dr3/ducL
 real(p2), dimension(4,4) :: dR4_ducL     ! dr4/ducL

! Derivatives w.r.t. ucR
 real(p2), dimension(4)   :: drho_ducR    ! drho/ducR
 real(p2), dimension(4)   :: dqn_ducR     ! dqn/ducR
 real(p2), dimension(4)   :: du_ducR      ! du/ducR
 real(p2), dimension(4)   :: dv_ducR      ! dv/ducR
 real(p2), dimension(4)   :: dH_ducR      ! dH/ducR
 real(p2), dimension(4)   :: da_ducR      ! da/ducR
 real(p2), dimension(4)   :: dabs_qn_ducR ! d|qn|/ducR

 real(p2), dimension(4)   :: ddrho_ducR   ! d(drho)/ducR
 real(p2), dimension(4)   :: ddp_ducR     ! d(dp)  /ducR
 real(p2), dimension(4)   :: ddu_ducR     ! d(du)  /ducR
 real(p2), dimension(4)   :: ddv_ducR     ! d(dv)  /ducR
 real(p2), dimension(4)   :: ddqn_ducR    ! d(dqn) /ducR

 real(p2), dimension(4)   :: dws1_ducR    ! dws1/ducR
 real(p2), dimension(4)   :: dws2_ducR    ! dws2/ducR
 real(p2), dimension(4)   :: dws3_ducR    ! dws3/ducR
 real(p2), dimension(4)   :: dws4_ducR    ! dws4/ducR

 real(p2), dimension(4)   :: dLdU1_ducR   ! dLdU1/ducR
 real(p2), dimension(4)   :: dLdU2_ducR   ! dLdU2/ducR
 real(p2), dimension(4)   :: dLdU3_ducR   ! dLdU3/ducR
 real(p2), dimension(4)   :: dLdU4_ducR   ! dLdU4/ducR

 real(p2), dimension(4,4) :: dR1_ducR     ! dr1/ducR
 real(p2), dimension(4,4) :: dR2_ducR     ! dr2/ducR
 real(p2), dimension(4,4) :: dR3_ducR     ! dr3/ducR
 real(p2), dimension(4,4) :: dR4_ducR     ! dr4/ducR

! Face normal vector (unit vector)

  nx = njk(1)
  ny = njk(2)

!Primitive and other variables.

!  Left state

    rhoL = ucL(1)
      uL = ucL(2)/ucL(1)
      vL = ucL(3)/ucL(1)
     qnL = uL*nx + vL*ny
     q2L = uL*uL+vL*vL
      pL = (gamma-one)*( ucL(4) - half*rhoL*(uL*uL+vL*vL) )
      aL = sqrt(gamma*pL/rhoL)
      HL = aL*aL/(gamma-one) + half*(uL*uL+vL*vL)

!  Right state

    rhoR = ucR(1)
      uR = ucR(2)/ucR(1)
      vR = ucR(3)/ucR(1)
     qnR = uR*nx + vR*ny
     q2R = uR*uR+vR*vR
      pR = (gamma-one)*( ucR(4) - half*rhoR*(uR*uR+vR*vR) )
      aR = sqrt(gamma*pR/rhoR)
      HR = aR*aR/(gamma-one) + half*(uR*uR+vR*vR)

! Compute the Roe-averaged quantities
!  NOTE: See http://www.cfdnotes.com/cfdnotes_roe_averaged_density.html for
!        the Roe-averaged density.

      RT = sqrt(rhoR/rhoL)
     rho = RT*rhoL                         !Roe-averaged density
       u = (uL + RT*uR)/(one + RT)         !Roe-averaged x-velocity
       v = (vL + RT*vR)/(one + RT)         !Roe-averaged y-velocity
       H = (HL + RT*HR)/(one + RT)         !Roe-averaged total enthalpy
      q2 = u*u + v*v                       !Square of Roe-averaged speed
       a = sqrt( (gamma-one)*(H-half*q2) ) !Roe-averaged speed of sound
      qn = u*nx + v*ny                     !Roe-averaged face-normal velocity

!Wave Strengths

    drho = rhoR - rhoL !Density difference
      dp =   pR - pL   !Pressure difference
     dqn =  qnR - qnL  !Normal velocity difference

  LdU(1) = (dp - rho*a*dqn )/(two*a*a) !Left-moving acoustic wave strength
  LdU(2) =  drho - dp/(a*a)            !Entropy wave strength
  LdU(3) = (dp + rho*a*dqn )/(two*a*a) !Right-moving acoustic wave strength
  LdU(4) = rho                         !Shear wave strength (not really, just a factor)

!Absolute values of the wave Speeds

   ws(1) = abs(qn-a) !Left-moving acoustic wave
   ws(2) = abs(qn)   !Entropy wave
   ws(3) = abs(qn+a) !Right-moving acoustic wave
   ws(4) = abs(qn)   !Shear wave

!Harten's Entropy Fix JCP(1983), 49, pp357-393: only for the nonlinear fields.
!NOTE: It avoids vanishing wave speeds by making a parabolic fit near ws = 0.

  dws(1) = fifth
   if ( ws(1) < dws(1) ) ws(1) = half * ( ws(1)*ws(1)/dws(1)+dws(1) )
  dws(3) = fifth
   if ( ws(3) < dws(3) ) ws(3) = half * ( ws(3)*ws(3)/dws(3)+dws(3) )

!Right Eigenvectors

! Left-moving acoustic wave
  R(1,1) = one
  R(2,1) = u - a*nx
  R(3,1) = v - a*ny
  R(4,1) = H - a*qn

! Entropy wave
  R(1,2) = one
  R(2,2) = u
  R(3,2) = v
  R(4,2) = half*(u*u + v*v)

! Right-moving acoustic wave
  R(1,3) = one
  R(2,3) = u + a*nx
  R(3,3) = v + a*ny
  R(4,3) = H + a*qn

! Shear wave (wave strength incorporated), written interms of the normal vector.
! Note: See "I do like CFD, VOL.1" about how a tangent vector is eliminated.
      du = uR - uL
      dv = vR - vL
  R(1,4) = zero
  R(2,4) = du - dqn*nx
  R(3,4) = dv - dqn*ny
  R(4,4) = u*du + v*dv - qn*dqn

! We are now ready to compute the Jacobians for the Roe flux:
!
! Roe flux function -> Fn_Roe = 0.5*[Fn(ucL)+Fn(ucR)] - 0.5*sum_{k=1,4}|lambda_k|*(LdU)_k*r_k

!--------------------------------------------------------------------------------
! Part 1. Compute dFn_Roe/ducL
!
!  dFn_Roe/ducL =   d(0.5*Fn(ucL))/duL
!                 - 0.5*sum_{k=1,4} [d(|lambda_k|)/ducL]*(LdU)_k*r_k
!                 - 0.5*sum_{k=1,4} |lambda_k|*[d(LdU)_k/ducL]*r_k
!                 - 0.5*sum_{k=1,4} |lambda_k|*(LdU)_k*[dr_k/ducL]
!
!  So, we proceed as follows:
!
!  1.1 Compute                d(0.5*Fn(ucL))/duL
!  1.2 Compute various deriavives that will be used in the following steps.
!  1.3 Add the second term, - 0.5*sum_{k=1,4} [d(|lambda_k|)/ducL]*(LdU)_k*r_k
!  1.4 Add the  third term, - 0.5*sum_{k=1,4} |lambda_k|*[d(LdU)_k/ducL]*r_k
!  1.5 Add the fourth term, - 0.5*sum_{k=1,4} |lambda_k|*(LdU)_k*[dr_k/ducL]
!

!--------------------------------------
! 1.1 Compute "d(0.5*Fn(ucL))/ducL"
!
!     (See "I Do Like CFD, VOL.1", page 55, for the analytical Jacobian, dFn(u)/du)

!  1st column
   dFnducL(1,1) = zero
   dFnducL(2,1) = half*(gamma-one)*q2L*nx  - uL*qnL
   dFnducL(3,1) = half*(gamma-one)*q2L*ny  - vL*qnL
   dFnducL(4,1) = half*(gamma-one)*q2L*qnL - HL*qnL

!  2nd column
   dFnducL(1,2) =     nx
   dFnducL(2,2) =  uL*nx - (gamma-one)*uL*nx + qnL
   dFnducL(3,2) =  vL*nx - (gamma-one)*uL*ny
   dFnducL(4,2) =  HL*nx - (gamma-one)*uL*qnL

!  3rd column
   dFnducL(1,3) =     ny
   dFnducL(2,3) =  uL*ny - (gamma-one)*vL*nx
   dFnducL(3,3) =  vL*ny - (gamma-one)*vL*ny + qnL
   dFnducL(4,3) =  HL*ny - (gamma-one)*vL*qnL

!  4th column
   dFnducL(1,4) =  zero
   dFnducL(2,4) = (gamma-one)*nx
   dFnducL(3,4) = (gamma-one)*ny
   dFnducL(4,4) =  gamma*qnL

!  Factor 1/2
   dFnducL = half * dFnducL

!--------------------------------------
! 1.2 Compute various deriavives that will be used in the following steps.

! dqn/ducL

   dqn_ducL(1) = -half*(qnL+qn) / (rhoL+rho)
   dqn_ducL(2) =            nx  / (rhoL+rho)
   dqn_ducL(3) =            ny  / (rhoL+rho)
   dqn_ducL(4) =          zero

! d(|qn|)/ducL

   dabs_qn_ducL(1) = -half*sign(one,qn)*(qnL+qn) / (rhoL+rho)
   dabs_qn_ducL(2) =       sign(one,qn)*     nx  / (rhoL+rho)
   dabs_qn_ducL(3) =       sign(one,qn)*     ny  / (rhoL+rho)
   dabs_qn_ducL(4) =  zero

! da/ducL

   da_ducL(1) =  half*(gamma-one)/a*( half*( uL*u+vL*v + q2 )                        &
              +  half*(HL-H) - aL**2/(gamma-one) + half*(gamma-two)*q2L )/(rhoL+rho)
   da_ducL(2) = -half*(gamma-one)*(u+(gamma-one)*uL)/a  / (rhoL+rho)
   da_ducL(3) = -half*(gamma-one)*(v+(gamma-one)*vL)/a  / (rhoL+rho)
   da_ducL(4) =  half*gamma*(gamma-one)/a               / (rhoL+rho)

! drho/ducL

   drho_ducL(1) = half*rho/rhoL
   drho_ducL(2) = zero
   drho_ducL(3) = zero
   drho_ducL(4) = zero

! du/ducL

   du_ducL(1) = -half*(uL+u) / (rhoL+rho)
   du_ducL(2) =          one / (rhoL+rho)
   du_ducL(3) =  zero
   du_ducL(4) =  zero

! dv/ducL

   dv_ducL(1) = -half*(vL+v) / (rhoL+rho)
   dv_ducL(2) =  zero
   dv_ducL(3) =          one / (rhoL+rho)
   dv_ducL(4) =  zero

! dH/ducL

   dH_ducL(1) = ( half*(HL-H) - aL**2/(gamma-one) + half*(gamma-two)*q2L ) / (rhoL+rho)
   dH_ducL(2) = ( one - gamma )*uL / (rhoL+rho)
   dH_ducL(3) = ( one - gamma )*vL / (rhoL+rho)
   dH_ducL(4) =              gamma / (rhoL+rho)

! d(rhoR-rhoL)/ducL = - drhoL/ducL = - (drhoL/dWL)*(dWL/ducL) = -(1,0,0,0)*dW/dU

   ddrho_ducL(1) = - (  one )
   ddrho_ducL(2) = - ( zero )
   ddrho_ducL(3) = - ( zero )
   ddrho_ducL(4) = - ( zero )

! d(pR-pL)/ducL = - dpL/ducL = - (dpL/dWL)*(dWL/ducL) = -(0,0,0,1)*dW/dU

   ddp_ducL(1)   = - ( half*(gamma-one)*q2L )
   ddp_ducL(2)   = - (    - (gamma-one)*uL  )
   ddp_ducL(3)   = - (    - (gamma-one)*vL  )
   ddp_ducL(4)   = - (       gamma-one      )

! d(qnR-qnL)/ducL = - dqnL/ducL = - (dqnL/dWL)*(dWL/ducL) = -(0,nx,ny,0)*dW/dU

   ddqn_ducL(1)  = - (-qnL/rhoL)
   ddqn_ducL(2)  = - (  nx/rhoL)
   ddqn_ducL(3)  = - (  ny/rhoL)
   ddqn_ducL(4)  = - ( zero    )

! d(uR-uL)/ducL = - duL/ducL = - (duL/dWL)*(dWL/ducL) = -(0,1,0,0)*dW/dU

   ddu_ducL(1)   = - ( -uL/rhoL)
   ddu_ducL(2)   = - ( one/rhoL)
   ddu_ducL(3)   = - ( zero    )
   ddu_ducL(4)   = - ( zero    )

! d(vR-vL)/ducL = - dvL/ducL = - (dvL/dWL)*(dWL/ducL) = -(0,0,1,0)*dW/dU

   ddv_ducL(1)   = - ( -vL/rhoL)
   ddv_ducL(2)   = - ( zero    )
   ddv_ducL(3)   = - ( one/rhoL)
   ddv_ducL(4)   = - ( zero    )

!--------------------------------------
! 1.3 Differentiate the absolute values of the wave speeds, and
!     add the second term, - 0.5*sum_{k=1,4} [d(|lambda_k|)/ducL]*(LdU)_k*r_k

!  dws1_ducL = d(|qn-a|)/dUcL
!
!  Note on entropy fix:
!  Let   dws1' = half * ( ws(1)*ws(1)/dws(1)+dws(1) ) <- Entropy fix
!  Then, dws1'/dUcL = (dws1'/dws1) * dws1_ducL
!                    = ws(1)/dws(1) * dws1_ducL

!  Absolute value

   if (qn-a > zero) then
    dws1_ducL =     dqn_ducL - da_ducL
   else
    dws1_ducL = - ( dqn_ducL - da_ducL )
   endif

!  Entropy fix

   if ( ws(1) < dws(1) ) dws1_ducL = ws(1)/dws(1) * dws1_ducL

   dFnducL(:,1) = dFnducL(:,1) - half * dws1_ducL(1)*LdU(1)*R(:,1)
   dFnducL(:,2) = dFnducL(:,2) - half * dws1_ducL(2)*LdU(1)*R(:,1)
   dFnducL(:,3) = dFnducL(:,3) - half * dws1_ducL(3)*LdU(1)*R(:,1)
   dFnducL(:,4) = dFnducL(:,4) - half * dws1_ducL(4)*LdU(1)*R(:,1)

!  dws2_ducL = d(|qn|)/dUcL

      dws2_ducL = dabs_qn_ducL

   dFnducL(:,1) = dFnducL(:,1) - half * dws2_ducL(1)*LdU(2)*R(:,2)
   dFnducL(:,2) = dFnducL(:,2) - half * dws2_ducL(2)*LdU(2)*R(:,2)
   dFnducL(:,3) = dFnducL(:,3) - half * dws2_ducL(3)*LdU(2)*R(:,2)
   dFnducL(:,4) = dFnducL(:,4) - half * dws2_ducL(4)*LdU(2)*R(:,2)

!  dws3_ducL = d(|qn+a|)/dUcL
!
!  Note on entropy fix:
!  Let   dws3' = half * ( ws(3)*ws(3)/dws(3)+dws(3) ) <- Entropy fix
!  Then, dws3'/dUcL = (dws3'/dws3) * dws3_ducL
!                   = ws(3)/dws(3) * dws3_ducL

!  Absolute value

   if (qn+a > zero) then
    dws3_ducL =     dqn_ducL + da_ducL
   else
    dws3_ducL = - ( dqn_ducL + da_ducL )
   endif

!  Entropy fix

   if ( ws(3) < dws(3) ) dws3_ducL = ws(3)/dws(3) * dws3_ducL

   dFnducL(:,1) = dFnducL(:,1) - half * dws3_ducL(1)*LdU(3)*R(:,3)
   dFnducL(:,2) = dFnducL(:,2) - half * dws3_ducL(2)*LdU(3)*R(:,3)
   dFnducL(:,3) = dFnducL(:,3) - half * dws3_ducL(3)*LdU(3)*R(:,3)
   dFnducL(:,4) = dFnducL(:,4) - half * dws3_ducL(4)*LdU(3)*R(:,3)

!  dws4_ducL = d(|qn|)/dUcL = dws1_ducL

      dws4_ducL = dabs_qn_ducL

   dFnducL(:,1) = dFnducL(:,1) - half * dws4_ducL(1)*LdU(4)*R(:,4)
   dFnducL(:,2) = dFnducL(:,2) - half * dws4_ducL(2)*LdU(4)*R(:,4)
   dFnducL(:,3) = dFnducL(:,3) - half * dws4_ducL(3)*LdU(4)*R(:,4)
   dFnducL(:,4) = dFnducL(:,4) - half * dws4_ducL(4)*LdU(4)*R(:,4)

!--------------------------------------
! 1.4 Differentiate the wave strength, and
!     add the third term, - 0.5*sum_{k=1,4} |lambda_k|*[d(LdU)_k/ducL]*r_k.

!  dLdU1_ducL = d( dp/(2a^2) - rho/(2a)*dqn )/dUcL
!
!             = dp*[d(1/(2a^2))/dUcL] - dqn*[d(rho/(2a))/dUcL]
!             + [d(dp)/dUcL]/(2a^2)   - [d(dqn)/dUcL]*rho/(2a)
!
!             = -a^(-3)*dp*[da/dUcL]  - dqn*( [drho/dUcL]/(2a) - rho/(2*a^2)*[da/dUcL] )
!             + [d(dp)/dUcL]/(2a^2)   - [d(dqn)/dUcL]*rho/(2a)
!
!             = ( -2*dp + rho*a*dqn )/(2a^3)*[da/dUcL] - dqn*[drho/dUcL]/(2a)
!             + [d(dp)/dUcL]/(2a^2)   - [d(dqn)/dUcL]*rho/(2a)

   dLdU1_ducL = half*(-two*dp+rho*a*dqn )/a**3 * (da_ducL) &
              - half*dqn/a * (drho_ducL)                   &
              + half*(ddp_ducL)/a**2                       &
              - half*rho*(ddqn_ducL)/a

   dFnducL(:,1) = dFnducL(:,1) - half * ws(1)*dLdU1_ducL(1)*R(:,1)
   dFnducL(:,2) = dFnducL(:,2) - half * ws(1)*dLdU1_ducL(2)*R(:,1)
   dFnducL(:,3) = dFnducL(:,3) - half * ws(1)*dLdU1_ducL(3)*R(:,1)
   dFnducL(:,4) = dFnducL(:,4) - half * ws(1)*dLdU1_ducL(4)*R(:,1)

!  dLdU2_ducL = d( drho - dp/a^2 )/dUcL
!              = [d(drho)/dUcL] - [d(dp)/dUcL]/a^2 + 2*dp/a^3*[da/dUcL]

   dLdU2_ducL = (ddrho_ducL) - (ddp_ducL)/a**2 + two*dp/a**3*(da_ducL)

   dFnducL(:,1) = dFnducL(:,1) - half * ws(2)*dLdU2_ducL(1)*R(:,2)
   dFnducL(:,2) = dFnducL(:,2) - half * ws(2)*dLdU2_ducL(2)*R(:,2)
   dFnducL(:,3) = dFnducL(:,3) - half * ws(2)*dLdU2_ducL(3)*R(:,2)
   dFnducL(:,4) = dFnducL(:,4) - half * ws(2)*dLdU2_ducL(4)*R(:,2)

!  dLdU3_ducL = d( dp/(2a^2) + rho/(2a)*dqn )/dUcL
!
!             = dp*[d(1/(2a^2))/dUcL] + dqn*[d(rho/(2a))/dUcL]
!             + [d(dp)/dUcL]/(2a^2)   + [d(dqn)/dUcL]*rho/(2a)
!
!             = -a^(-3)*dp*[da/dUcL]  + dqn*( [drho/dUcL]/(2a) - rho/(2*a^2)*[da/dUcL] )
!             + [d(dp)/dUcL]/(2a^2)   + [d(dqn)/dUcL]*rho/(2a)
!
!             = ( -2*dp - rho*a*dqn )/(2a^3)*[da/dUcL]  + dqn*[drho/dUcL]/(2a)
!             + [d(dp)/dUcL]/(2a^2)   + [d(dqn)/dUcL]*rho/(2a)

   dLdU3_ducL = half*(-two*dp-rho*a*dqn )/a**3 * (da_ducL) &
              + half*dqn/a * (drho_ducL)                   &
              + half*(ddp_ducL)/a**2                       &
              + half*rho*(ddqn_ducL)/a

   dFnducL(:,1) = dFnducL(:,1) - half * ws(3)*dLdU3_ducL(1)*R(:,3)
   dFnducL(:,2) = dFnducL(:,2) - half * ws(3)*dLdU3_ducL(2)*R(:,3)
   dFnducL(:,3) = dFnducL(:,3) - half * ws(3)*dLdU3_ducL(3)*R(:,3)
   dFnducL(:,4) = dFnducL(:,4) - half * ws(3)*dLdU3_ducL(4)*R(:,3)

!  dLdU4_ducL = d(rho)/dUcL

   dLdU4_ducL = drho_ducL

   dFnducL(:,1) = dFnducL(:,1) - half * ws(4)*dLdU4_ducL(1)*R(:,4)
   dFnducL(:,2) = dFnducL(:,2) - half * ws(4)*dLdU4_ducL(2)*R(:,4)
   dFnducL(:,3) = dFnducL(:,3) - half * ws(4)*dLdU4_ducL(3)*R(:,4)
   dFnducL(:,4) = dFnducL(:,4) - half * ws(4)*dLdU4_ducL(4)*R(:,4)

!--------------------------------------
! 1.5 Differentiate the right-eigenvectors, and
!     add the fourth term, - 0.5*sum_{k=1,4} |lambda_k|*(LdU)_k*[dr_k/ducL]

! dR1_ducL = dR(:,1)/dUcL
!
! Left-moving acoustic wave
!
!        Eigenvector -> Differentiated
!
!  R(1,1) = one      -> 0
!  R(2,1) = u - a*nx -> du/dUcL - da/dUcL*nx
!  R(3,1) = v - a*ny -> dv/dUcL - da/dUcL*ny
!  R(4,1) = H - a*qn -> dH/dUcL - da/dUcL*qn - dqn/dUcL*a

   dR1_ducL(1,:) = zero
   dR1_ducL(2,:) = (du_ducL) - (da_ducL) * nx
   dR1_ducL(3,:) = (dv_ducL) - (da_ducL) * ny
   dR1_ducL(4,:) = (dH_ducL) - (da_ducL) * qn - (dqn_ducL) * a

   dFnducL = dFnducL - half * ws(1)*LdU(1)*dR1_ducL

! dR2_ducL = dR(:,2)/dUcL
!
! Entropy wave
!
!                  Eigenvector -> Differentiated
!
!  R(1,2) = one                -> 0
!  R(2,2) = u                  -> du/dUcL
!  R(3,2) = v                  -> dv/dUcL
!  R(4,2) = half*(u*u+v*v)     -> u*du/dUcL + v*dv/dUcL

   dR2_ducL(1,:) = zero
   dR2_ducL(2,:) =   (du_ducL)
   dR2_ducL(3,:) =   (dv_ducL)
   dR2_ducL(4,:) = u*(du_ducL) + v*(dv_ducL)

   dFnducL = dFnducL - half * ws(2)*LdU(2)*dR2_ducL

! dR3_ducL = dR(:,3)/dUcL
!
! Right-moving acoustic wave
!
!        Eigenvector -> Differentiated
!
!  R(1,3) = one      -> 0
!  R(2,3) = u + a*nx -> du/dUcL + da/dUcL*nx
!  R(3,3) = v + a*ny -> dv/dUcL + da/dUcL*ny
!  R(4,3) = H + a*qn -> dH/dUcL + da/dUcL*qn + dqn/dUcL*a

   dR3_ducL(1,:) = zero
   dR3_ducL(2,:) = (du_ducL) + (da_ducL) * nx
   dR3_ducL(3,:) = (dv_ducL) + (da_ducL) * ny
   dR3_ducL(4,:) = (dH_ducL) + (da_ducL) * qn + (dqn_ducL) * a

   dFnducL = dFnducL - half * ws(3)*LdU(3)*dR3_ducL

! dR4_ducL = dR(:,4)/dUcL
!
!                           Vector -> Differentiated
!
!  R(1,4) = zero                   -> 0
!  R(2,4) = du - dqn*nx            -> d(du)/dUcL - d(dqn)/dUcL*nx
!  R(3,4) = dv - dqn*ny            -> d(dv)/dUcL - d(dqn)/dUcL*ny
!  R(4,4) = u*du+v*dv-qn*dqn       -> du/dUcL*du     + d(du)/dUcL*u
!                                   + dv/dUcL*dv     + d(dv)/dUcL*v
!                                   - d(qn)/dUcL*dqn - d(dqn)/dUcL*qn

   dR4_ducL(1,:) = zero
   dR4_ducL(2,:) = (ddu_ducL) - (ddqn_ducL)*nx
   dR4_ducL(3,:) = (ddv_ducL) - (ddqn_ducL)*ny
   dR4_ducL(4,:) = du*( du_ducL) + dv*( dv_ducL) - dqn*( dqn_ducL) &
                 +  u*(ddu_ducL) +  v*(ddv_ducL) -  qn*(ddqn_ducL)

   dFnducL = dFnducL - half * ws(4)*LdU(4)*dR4_ducL


!--------------------------------------------------------------------------------
! Part 2. Compute dFn_Roe/ducR
!
!  dFn_Roe/ducR =   d(0.5*Fn(ucR))/duR
!                 - 0.5*sum_{k=1,4} [d(|lambda_k|)/ducR]*(LdU)_k*r_k
!                 - 0.5*sum_{k=1,4} |lambda_k|*[d(LdU)_k/ducR]*r_k
!                 - 0.5*sum_{k=1,4} |lambda_k|*(LdU)_k*[dr_k/ducR]
!
!  So, we proceed as follows:
!
!  1.1 Compute                d(0.5*Fn(ucR))/duR
!  1.2 Compute various deriavives that will be used in the following steps.
!  1.3 Add the second term, - 0.5*sum_{k=1,4} [d(|lambda_k|)/ducR]*(LdU)_k*r_k
!  1.4 Add the  third term, - 0.5*sum_{k=1,4} |lambda_k|*[d(LdU)_k/ducR]*r_k
!  1.5 Add the fourth term, - 0.5*sum_{k=1,4} |lambda_k|*(LdU)_k*[dr_k/ducR]
!

!--------------------------------------
! 2.1 Compute "d(0.5*Fn(ucR))/ducR"
!
!     (See "I Do Like CFD, VOL.1", page 55, for the analytical Jacobian, dFn(u)/du)

!  1st column
   dFnducR(1,1) = zero
   dFnducR(2,1) = half*(gamma-one)*q2R*nx  - uR*qnR
   dFnducR(3,1) = half*(gamma-one)*q2R*ny  - vR*qnR
   dFnducR(4,1) = half*(gamma-one)*q2R*qnR - HR*qnR

!  2nd column
   dFnducR(1,2) =     nx
   dFnducR(2,2) =  uR*nx - (gamma-one)*uR*nx + qnR
   dFnducR(3,2) =  vR*nx - (gamma-one)*uR*ny
   dFnducR(4,2) =  HR*nx - (gamma-one)*uR*qnR

!  3rd column
   dFnducR(1,3) =     ny
   dFnducR(2,3) =  uR*ny - (gamma-one)*vR*nx
   dFnducR(3,3) =  vR*ny - (gamma-one)*vR*ny + qnR
   dFnducR(4,3) =  HR*ny - (gamma-one)*vR*qnR

!  4th column
   dFnducR(1,4) =  zero
   dFnducR(2,4) = (gamma-one)*nx
   dFnducR(3,4) = (gamma-one)*ny
   dFnducR(4,4) =  gamma*qnR

!  Factor 1/2
   dFnducR = half * dFnducR

!--------------------------------------
! 2.2 Compute various deriavives that will be used in the following steps.

! dqn/ducR

   dqn_ducR(1) = -half*(qnR+qn) / (rhoR+rho)
   dqn_ducR(2) =            nx  / (rhoR+rho)
   dqn_ducR(3) =            ny  / (rhoR+rho)
   dqn_ducR(4) =          zero

! d(|qn|)/ducR

   dabs_qn_ducR(1) = -half*sign(one,qn)*(qnR+qn) / (rhoR+rho)
   dabs_qn_ducR(2) =       sign(one,qn)*     nx  / (rhoR+rho)
   dabs_qn_ducR(3) =       sign(one,qn)*     ny  / (rhoR+rho)
   dabs_qn_ducR(4) =  zero

! da/ducR

   da_ducR(1) =  half*(gamma-one)/a*( half*( uR*u+vR*v+ q2 )                         &
              +  half*(HR-H) - aR**2/(gamma-one) + half*(gamma-two)*q2R )/(rhoR+rho)
   da_ducR(2) = -half*(gamma-one)*(u+(gamma-one)*uR)/a  / (rhoR+rho)
   da_ducR(3) = -half*(gamma-one)*(v+(gamma-one)*vR)/a  / (rhoR+rho)
   da_ducR(4) =  half*gamma*(gamma-one)/a               / (rhoR+rho)

! drho/ducR

   drho_ducR(1) = half*rho/rhoR
   drho_ducR(2) = zero
   drho_ducR(3) = zero
   drho_ducR(4) = zero

! du/ducR

   du_ducR(1) = -half*(uR+u) / (rhoR+rho)
   du_ducR(2) =          one / (rhoR+rho)
   du_ducR(3) =  zero
   du_ducR(4) =  zero

! dv/ducR

   dv_ducR(1) = -half*(vR+v) / (rhoR+rho)
   dv_ducR(2) =  zero
   dv_ducR(3) =          one / (rhoR+rho)
   dv_ducR(4) =  zero

! dH/ducR

   dH_ducR(1) = ( half*(HR-H) - aR**2/(gamma-one) + half*(gamma-two)*q2R ) / (rhoR+rho)
   dH_ducR(2) = ( one - gamma )*uR / (rhoR+rho)
   dH_ducR(3) = ( one - gamma )*vR / (rhoR+rho)
   dH_ducR(4) =              gamma / (rhoR+rho)

! d(rhoR-rhoR)/ducR = drhoR/ducR = (drhoR/dWR)*(dWR/ducR) = (1,0,0,0)*dW/dU

   ddrho_ducR(1) =  (  one )
   ddrho_ducR(2) =  ( zero )
   ddrho_ducR(3) =  ( zero )
   ddrho_ducR(4) =  ( zero )

! d(pR-pR)/ducR = dpR/ducR = (dpR/dWR)*(dWR/ducR) = (0,0,0,1)*dW/dU

   ddp_ducR(1)   =  ( half*(gamma-one)*q2R )
   ddp_ducR(2)   =  (    - (gamma-one)*uR  )
   ddp_ducR(3)   =  (    - (gamma-one)*vR  )
   ddp_ducR(4)   =  (       gamma-one      )

! d(qnR-qnR)/ducR = dqnR/ducR = (dqnR/dWR)*(dWR/ducR) = (0,nx,ny,0)*dW/dU

   ddqn_ducR(1)  =  (-qnR/rhoR)
   ddqn_ducR(2)  =  (  nx/rhoR)
   ddqn_ducR(3)  =  (  ny/rhoR)
   ddqn_ducR(4)  =  ( zero    )

! d(uR-uR)/ducR = duR/ducR = (duR/dWR)*(dWR/ducR) = -(0,1,0,0)*dW/dU

   ddu_ducR(1)   =  ( -uR/rhoR)
   ddu_ducR(2)   =  ( one/rhoR)
   ddu_ducR(3)   =  ( zero    )
   ddu_ducR(4)   =  ( zero    )

! d(vR-vR)/ducR = dvR/ducR = (dvR/dWR)*(dWR/ducR) = (0,0,1,0)*dW/dU

   ddv_ducR(1)   =  ( -vR/rhoR)
   ddv_ducR(2)   =  ( zero    )
   ddv_ducR(3)   =  ( one/rhoR)
   ddv_ducR(4)   =  ( zero    )

!--------------------------------------
! 2.3 Differentiate the absolute values of the wave speeds, and
!     add the second term, - 0.5*sum_{k=1,4} [d(|lambda_k|)/ducR]*(LdU)_k*r_k

!  dws1_ducR = d(|qn-a|)/dUcR
!
!  Note on entropy fix:
!  Let   dws1' = half * ( ws(1)*ws(1)/dws(1)+dws(1) ) <- Entropy fix
!  Then, dws1'/dUcR = (dws1'/dws1) * dws1_ducR
!                   = ws(1)/dws(1) * dws1_ducR

!  Absolute value

   if (qn-a > zero) then
    dws1_ducR =     dqn_ducR - da_ducR
   else
    dws1_ducR = - ( dqn_ducR - da_ducR )
   endif

!  Entropy fix

   if ( ws(1) < dws(1) ) dws1_ducR = ws(1)/dws(1) * dws1_ducR

   dFnducR(:,1) = dFnducR(:,1) - half * dws1_ducR(1)*LdU(1)*R(:,1)
   dFnducR(:,2) = dFnducR(:,2) - half * dws1_ducR(2)*LdU(1)*R(:,1)
   dFnducR(:,3) = dFnducR(:,3) - half * dws1_ducR(3)*LdU(1)*R(:,1)
   dFnducR(:,4) = dFnducR(:,4) - half * dws1_ducR(4)*LdU(1)*R(:,1)

!  dws2_ducR = d(|qn|)/dUcR

      dws2_ducR = dabs_qn_ducR

   dFnducR(:,1) = dFnducR(:,1) - half * dws2_ducR(1)*LdU(2)*R(:,2)
   dFnducR(:,2) = dFnducR(:,2) - half * dws2_ducR(2)*LdU(2)*R(:,2)
   dFnducR(:,3) = dFnducR(:,3) - half * dws2_ducR(3)*LdU(2)*R(:,2)
   dFnducR(:,4) = dFnducR(:,4) - half * dws2_ducR(4)*LdU(2)*R(:,2)

!  dws3_ducR = d(|qn+a|)/dUcR
!
!  Note on entropy fix:
!ã€€ Let   dws3' = half * ( ws(3)*ws(3)/dws(3)+dws(3) ) <- Entropy fix
! ã€€Then, dws3'/dUcR = (dws3'/dws3) * dws3_ducR
!                    = ws(3)/dws(3) * dws3_ducR

!  Absolute value

   if (qn+a > zero) then
    dws3_ducR =     dqn_ducR + da_ducR
   else
    dws3_ducR = - ( dqn_ducR + da_ducR )
   endif

!  Entropy fix

   if ( ws(3) < dws(3) ) dws3_ducR = ws(3)/dws(3) * dws3_ducR

   dFnducR(:,1) = dFnducR(:,1) - half * dws3_ducR(1)*LdU(3)*R(:,3)
   dFnducR(:,2) = dFnducR(:,2) - half * dws3_ducR(2)*LdU(3)*R(:,3)
   dFnducR(:,3) = dFnducR(:,3) - half * dws3_ducR(3)*LdU(3)*R(:,3)
   dFnducR(:,4) = dFnducR(:,4) - half * dws3_ducR(4)*LdU(3)*R(:,3)

!  dws4_ducR = d(|qn|)/dUcR = dws1_ducR

      dws4_ducR = dabs_qn_ducR

   dFnducR(:,1) = dFnducR(:,1) - half * dws4_ducR(1)*LdU(4)*R(:,4)
   dFnducR(:,2) = dFnducR(:,2) - half * dws4_ducR(2)*LdU(4)*R(:,4)
   dFnducR(:,3) = dFnducR(:,3) - half * dws4_ducR(3)*LdU(4)*R(:,4)
   dFnducR(:,4) = dFnducR(:,4) - half * dws4_ducR(4)*LdU(4)*R(:,4)

!--------------------------------------
! 2.4 Differentiate the wave strength, and
!     add the third term, - 0.5*sum_{k=1,4} |lambda_k|*[d(LdU)_k/ducR]*r_k.

!  dLdU1_ducR = d( dp/(2a^2) - rho/(2a)*dqn )/dUcR
!
!             = dp*[d(1/(2a^2))/dUcR] - dqn*[d(rho/(2a))/dUcR]
!             + [d(dp)/dUcR]/(2a^2)   - [d(dqn)/dUcR]*rho/(2a)
!
!             = -a^(-3)*dp*[da/dUcR]  - dqn*( [drho/dUcR]/(2a) - rho/(2*a^2)*[da/dUcR] )
!             + [d(dp)/dUcR]/(2a^2)   - [d(dqn)/dUcR]*rho/(2a)
!
!             = ( -2*dp + rho*a*dqn )/(2a^3)*[da/dUcR] - dqn*[drho/dUcR]/(2a)
!             + [d(dp)/dUcR]/(2a^2)   - [d(dqn)/dUcR]*rho/(2a)

   dLdU1_ducR = half*(-two*dp+rho*a*dqn )/a**3 * (da_ducR) &
              - half*dqn/a * (drho_ducR)                   &
              + half*(ddp_ducR)/a**2                       &
              - half*rho*(ddqn_ducR)/a

   dFnducR(:,1) = dFnducR(:,1) - half * ws(1)*dLdU1_ducR(1)*R(:,1)
   dFnducR(:,2) = dFnducR(:,2) - half * ws(1)*dLdU1_ducR(2)*R(:,1)
   dFnducR(:,3) = dFnducR(:,3) - half * ws(1)*dLdU1_ducR(3)*R(:,1)
   dFnducR(:,4) = dFnducR(:,4) - half * ws(1)*dLdU1_ducR(4)*R(:,1)

!  dLdU2_ducR = d( drho - dp/a^2 )/dUcR
!              = [d(drho)/dUcR] - [d(dp)/dUcR]/a^2 + 2*dp/a^3*[da/dUcR]

   dLdU2_ducR = (ddrho_ducR) - (ddp_ducR)/a**2 + two*dp/a**3*(da_ducR)

   dFnducR(:,1) = dFnducR(:,1) - half * ws(2)*dLdU2_ducR(1)*R(:,2)
   dFnducR(:,2) = dFnducR(:,2) - half * ws(2)*dLdU2_ducR(2)*R(:,2)
   dFnducR(:,3) = dFnducR(:,3) - half * ws(2)*dLdU2_ducR(3)*R(:,2)
   dFnducR(:,4) = dFnducR(:,4) - half * ws(2)*dLdU2_ducR(4)*R(:,2)

!  dLdU3_ducR = d( dp/(2a^2) + rho/(2a)*dqn )/dUcR
!
!             = dp*[d(1/(2a^2))/dUcR] + dqn*[d(rho/(2a))/dUcR]
!             + [d(dp)/dUcR]/(2a^2)   + [d(dqn)/dUcR]*rho/(2a)
!
!             = -a^(-3)*dp*[da/dUcR]  + dqn*( [drho/dUcR]/(2a) - rho/(2*a^2)*[da/dUcR] )
!             + [d(dp)/dUcR]/(2a^2)   + [d(dqn)/dUcR]*rho/(2a)
!
!             = ( -2*dp - rho*a*dqn )/(2a^3)*[da/dUcR]  + dqn*[drho/dUcR]/(2a)
!             + [d(dp)/dUcR]/(2a^2)   + [d(dqn)/dUcR]*rho/(2a)

   dLdU3_ducR = half*(-two*dp-rho*a*dqn )/a**3 * (da_ducR) &
              + half*dqn/a * (drho_ducR)                   &
              + half*(ddp_ducR)/a**2                       &
              + half*rho*(ddqn_ducR)/a

   dFnducR(:,1) = dFnducR(:,1) - half * ws(3)*dLdU3_ducR(1)*R(:,3)
   dFnducR(:,2) = dFnducR(:,2) - half * ws(3)*dLdU3_ducR(2)*R(:,3)
   dFnducR(:,3) = dFnducR(:,3) - half * ws(3)*dLdU3_ducR(3)*R(:,3)
   dFnducR(:,4) = dFnducR(:,4) - half * ws(3)*dLdU3_ducR(4)*R(:,3)

!  dLdU4_ducR = d(rho)/dUcR

   dLdU4_ducR = drho_ducR

   dFnducR(:,1) = dFnducR(:,1) - half * ws(4)*dLdU4_ducR(1)*R(:,4)
   dFnducR(:,2) = dFnducR(:,2) - half * ws(4)*dLdU4_ducR(2)*R(:,4)
   dFnducR(:,3) = dFnducR(:,3) - half * ws(4)*dLdU4_ducR(3)*R(:,4)
   dFnducR(:,4) = dFnducR(:,4) - half * ws(4)*dLdU4_ducR(4)*R(:,4)

!--------------------------------------
! 2.5 Differentiate the right-eigenvectors, and
!     add the fourth term, - 0.5*sum_{k=1,4} |lambda_k|*(LdU)_k*[dr_k/ducL]

! dR1_ducR = dR(:,1)/dUcR
!
! Left-moving acoustic wave
!
!        Eigenvector -> Differentiated
!
!  R(1,1) = one      -> 0
!  R(2,1) = u - a*nx -> du/dUcR - da/dUcR*nx
!  R(3,1) = v - a*ny -> dv/dUcR - da/dUcR*ny
!  R(4,1) = H - a*qn -> dH/dUcR - da/dUcR*qn - dqn/dUcR*a

   dR1_ducR(1,:) = zero
   dR1_ducR(2,:) = (du_ducR) - (da_ducR) * nx
   dR1_ducR(3,:) = (dv_ducR) - (da_ducR) * ny
   dR1_ducR(4,:) = (dH_ducR) - (da_ducR) * qn - (dqn_ducR) * a

   dFnducR = dFnducR - half * ws(1)*LdU(1)*dR1_ducR

! dR2_ducR = dR(:,2)/dUcR
!
! Entropy wave
!
!                  Eigenvector -> Differentiated
!
!  R(1,2) = one                -> 0
!  R(2,2) = u                  -> du/dUcR
!  R(3,2) = v                  -> dv/dUcR
!  R(4,2) = half*(u*u+v*v)     -> u*du/dUcR + v*dv/dUcR

   dR2_ducR(1,:) = zero
   dR2_ducR(2,:) =   (du_ducR)
   dR2_ducR(3,:) =   (dv_ducR)
   dR2_ducR(4,:) = u*(du_ducR) + v*(dv_ducR)

   dFnducR = dFnducR - half * ws(2)*LdU(2)*dR2_ducR

! dR3_ducR = dR(:,3)/dUcR
!
! Right-moving acoustic wave
!
!        Eigenvector -> Differentiated
!
!  R(1,3) = one      -> 0
!  R(2,3) = u + a*nx -> du/dUcR + da/dUcR*nx
!  R(3,3) = v + a*ny -> dv/dUcR + da/dUcR*ny
!  R(4,3) = H + a*qn -> dH/dUcR + da/dUcR*qn + dqn/dUcR*a

   dR3_ducR(1,:) = zero
   dR3_ducR(2,:) = (du_ducR) + (da_ducR) * nx
   dR3_ducR(3,:) = (dv_ducR) + (da_ducR) * ny
   dR3_ducR(4,:) = (dH_ducR) + (da_ducR) * qn + (dqn_ducR) * a

   dFnducR = dFnducR - half * ws(3)*LdU(3)*dR3_ducR

! dR4_ducR = dR(:,4)/dUcR
!
!                           Vector -> Differentiated
!
!  R(1,4) = zero                   -> 0
!  R(2,4) = du - dqn*nx            -> d(du)/dUcR - d(dqn)/dUcR*nx
!  R(3,4) = dv - dqn*ny            -> d(dv)/dUcR - d(dqn)/dUcR*ny
!  R(4,4) = u*du+v*dv-qn*dqn       -> du/dUcR*du     + d(du)/dUcR*u
!                                   + dv/dUcR*dv     + d(dv)/dUcR*v
!                                   - d(qn)/dUcR*dqn - d(dqn)/dUcR*qn

   dR4_ducR(1,:) = zero
   dR4_ducR(2,:) = (ddu_ducR) - (ddqn_ducR)*nx
   dR4_ducR(3,:) = (ddv_ducR) - (ddqn_ducR)*ny
   dR4_ducR(4,:) = du*( du_ducR) + dv*( dv_ducR) - dqn*( dqn_ducR) &
                 +  u*(ddu_ducR) +  v*(ddv_ducR) -  qn*(ddqn_ducR)

   dFnducR = dFnducR - half * ws(4)*LdU(4)*dR4_ducR

 end subroutine inviscid_roe_n_flux_jacobian
!--------------------------------------------------------------------------------


!********************************************************************************
!* Compute U from W
!*
!* ------------------------------------------------------------------------------
!*  Input:  w =    primitive variables (rho,     u,     v,     p)
!* Output:  u = conservative variables (rho, rho*u, rho*v, rho*E)
!* ------------------------------------------------------------------------------
!*
!********************************************************************************
 function w2u(w) result(u)

 use constants   , only : p2, one, half
 use my_main_data, only : gamma

 implicit none

 real(p2), dimension(4), intent(in) :: w

!Local variables
 real(p2), dimension(4)             :: u !output

  u(1) = w(1)
  u(2) = w(1)*w(2)
  u(3) = w(1)*w(3)
  u(4) = w(4)/(gamma-one)+half*w(1)*(w(2)*w(2)+w(3)*w(3))

 end function w2u
!--------------------------------------------------------------------------------

 end module jacobian