!  ************************************+*************************************  !
!                                                                              !
!                          lattice field theory.f90                            !
!                                                                              !
!  A lattice field theory code for nonperturbative field theories              !
!                                                                              !
!  Based on the work of Kevin Cahill                                           !
!                                                                              !
!  Created by Daniel M. Topa on 05/03/2013                                     !
!  Updated                      01/12/2015                                     !
!                                                                              !
!  ************************************+*************************************  !

include 'library/basic data types.f90'
include 'library/timer classes.f90'
include 'library/node class.f90'
include 'library/mod ave.f90'
include 'library/other types.f90'
include 'library/lattice class.f90'

program lft

  ! bring structures and procedures in via modules
  use lattice_type
  use other_types
  use cpu_timer_class
  use clock_timer_class
  implicit none

  ! scalar and vector field values
  real ( dp )             :: phi_list    ( 1 : num_nodes )               ! injection lists
  real ( dp )             :: varphi_list ( 1 : num_nodes )               ! injection lists
  real ( dp )             :: eker_list   ( 1 : num_nodes )               ! injection lists
  real ( dp )             :: psi_list    ( 1 : num_nodes, 1 : num_dim )  ! injection lists

  integer ( lint )        :: uid, pid, gid                               ! various Unix process IDs
  integer ( lint )        :: j, k                                        ! dummy counters

  character ( len = 30 )  :: timestamp
  character ( len = 64 )  :: dir_home, dir_working, user_name

  ! instantiate lattice
  type ( lattice )        :: ord
  ! instantiate a set of simulation parameters
  type ( run_parameters ) :: params
  ! instantiate timer
  type ( cpu_timer )      :: tau

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 BEGIN

  call admin ( dir_home, dir_working )                ! directory structure
  call tags  ( user_name, uid, pid, gid )             ! user and process identifications

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        RUN PARAMETERS

  call nml_reader ( ord, params, dir_working )        ! read the NAMELIST files

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       CREATE LATTICES

 call ord % load_indices ( )                          ! sweep through the nodes and assign multi-index addresses

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   LOAD INITIAL FIELDS

  ! random number initialization
  call initialize_rng ( params )

  phi_list    = zero
  varphi_list = zero
  psi_list    = pi
  call ord % load_lattice ( phi_list = phi_list, varphi_list = varphi_list )
  call ord % load_lattice ( psi_list = psi_list )

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     INITIALIZE FIELDS

  call ord % init_lattice ( )

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                SWEEPS

  write ( *, * )
  write ( *, * ) 'attempting', params % nsweeps, 'sweeps...'
  write ( *, * )

  call ord % sweep_lattice_sim ( params )

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        OUTPUT SUMMARY

!  call ord % measure_lattice_ord ( params )

  call ord % close_lattice ( )
  call ord % goodbye ( params )
  call ord % print_lattice ( )

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           END OF MAIN

  write ( *, * )

  stop 'successful program termination for program lft'

end program lft

include 'library/admin routines.f90'
include 'library/lft routines.f90'