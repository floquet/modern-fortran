  ! modules and global parameters
  include 'mod kind types.f90'
  include 'mod constants and parameters.f90'
  include 'mod linear regression.f90'
  include 'mod quality measures.f90'
  include 'mod surface fit.f90'
  include 'mod timer.f90'
  include 'mod plasma.f90'
  include 'mod Helios output.f90'
  include 'mod compiler.f90'
  include 'mod Helios.f90'
  include 'mod system_info.f90'
  include 'mod input parameters.f90'

  program main

  use system_info_mod
  use plasma
  use compiler
  use cpu_timer_class
  use clock_timer_class
  use HELIOS_data
  use input_parameters
  implicit none

  type ( HELIOS )                :: HeB_emmissivity  ! instantiate class
  type ( HELIOS )                :: LyB_emmissivity  ! instantiate class
  type ( HELIOS )                :: HeB_opacity      ! instantiate class
  type ( HELIOS )                :: LyB_opacity      ! instantiate class

  type ( thermo )                :: DT               ! line of sight

  type ( system_info )           :: job              ! system level inquiries

  type ( input_deck )            :: parameters       ! domain and toy model specification

  type ( cpu_timer )             :: cpu_self         ! instantiate CPU timer
  type ( clock_timer )           :: clock_self       ! instantiate clock timer
  type ( cpu_timer )             :: cpu_poly         ! instantiate CPU timer for polynomial evaluation
  type ( clock_timer )           :: clock_poly       ! instantiate clock timer for polynomial evaluation
  type ( cpu_timer )             :: cpu_numr         ! instantiate CPU timer for polynomial evaluation
  type ( clock_timer )           :: clock_numr       ! instantiate clock timer for polynomial evaluation
  real ( dp )                    :: cpu_time   = zero
  real ( dp )                    :: clock_time = zero
  real ( dp )                    :: amps ( 1 : t )
  real ( dp )                    :: fcn, x
  real ( dp )                    :: xi = 0.001_dp    ! wall thickness
  integer ( lint )               :: k, temps, dens, many

  character ( len = 30 )         :: timestamp
  character ( len = 64 )         :: dir_working


!  |        |       |        |       |        |       |        |       |        |       |        |       |        |       |        |
  interface
  !  *
    function Taylor_polynomial_otro_fcn ( x, y, a )  result ( f )
      use constants_and_parameters
      implicit none
      real ( wp ), intent ( in ) :: x, y
      real ( wp ), intent ( in ) :: a ( 1 : t )
      real ( wp )                :: f
    end function Taylor_polynomial_otro_fcn
  !  *
  end interface
!  |        |       |        |       |        |       |        |       |        |       |        |       |        |       |        |

  !  + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +     LOAD DATA

  !  start timers
  call cpu_self   % cpu_timer_grab    ( )
  call clock_self % clock_start_timer ( )

  call load_data ( HeB_emmissivity, LyB_emmissivity, HeB_opacity, LyB_opacity )  !  load the HELIOS data

  !  stop the CPU timer
  cpu_time   = cpu_self   % cpu_timer_stop     ( )
  clock_time = clock_self % clock_elapsed_time ( )

  print *
  call how_long_sub ( io_unit_default, cpu_time,   'elapsed CPU time for load (sec) :   ' )
  call how_long_sub ( io_unit_default, clock_time, 'elapsed clock time for load (sec) : ' )

  !  + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +   CREATE MESH

  call nml_reader ( parameters, dir_working )
!  print *, parameters % num_intervals, parameters % domain_a, parameters % domain_b

  call DT % create_mesh ( parameters % num_intervals, parameters % domain_a, parameters % domain_b )

  !  + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +    THERMODYNAMICS

  call DT % populate_pressure    ( parameters % pressure_xi, parameters % pressure_eta )  ! toy model - parabolic
  print *, 'DT % pressure_list = ', DT % pressure_list

  call DT % populate_density     ( parameters % density_xi, parameters % density_eta )  ! toy model - parabolic
  print *, 'DT % density_list = ', DT % density_list

  call DT % populate_temperature ( )            ! EOS
  print *, 'DT % temperature_list = ', DT % temperature_list

  !  + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +        FINISH

  !  closing statements

  print *
  print *, 'Fortran compiler version : ', compiled_by
  print *, 'Fortran compiler options : ', compiled_with

  write ( *, * ) timestamp ( )

  end program main

!  assortment of utility routines

include 'sub routines.f90'