  ! modules and global parameters
  include 'mod kind types.f90'
  include 'mod constants and parameters.f90'
  include 'mod linear regression.f90'
  include 'mod surface fit.f90'
  include 'mod timer.f90'
  include 'mod quality measures.f90'
  include 'mod Helios output.f90'
  include 'mod Helios.f90'
  include 'mod thermodynamics.f90'

  program main

  use cpu_timer_class
  use clock_timer_class
  use HELIOS_data
  implicit none

  type ( HELIOS )                :: HeB_emmissivity  ! instantiate class
  type ( HELIOS )                :: LyB_emmissivity  ! instantiate class
  type ( HELIOS )                :: HeB_opacity      ! instantiate class
  type ( HELIOS )                :: LyB_opacity      ! instantiate class

  type ( cpu_timer )             :: cpu_self         ! instantiate CPU timer instances
  type ( clock_timer )           :: clock_self       ! instantiate clock timer instances
  real ( dp )                    :: cpu_time   = zero
  real ( dp )                    :: clock_time = zero

  character ( len = 30 )         :: timestamp

  !  start timers
  call cpu_self   % cpu_timer_grab    ( )
  call clock_self % clock_start_timer ( )

  call load_data ( HeB_emmissivity, LyB_emmissivity, HeB_opacity, LyB_opacity )

  !  stop the CPU timer
  cpu_time   = cpu_self   % cpu_timer_stop ( )
  clock_time = clock_self % clock_elapsed_time ( )

  call how_long_sub ( io_unit_default, cpu_time,   'elapsed CPU time (sec) :       ' )
  call how_long_sub ( io_unit_default, clock_time, 'elapsed clock time (sec) :     ' )

  write ( *, * ) timestamp ( )

  end program main

!  assortment of utility routines
include 'sub routines.f90'