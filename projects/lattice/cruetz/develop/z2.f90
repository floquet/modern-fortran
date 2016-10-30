!                                                                              !
!  *********************************** + ************************************  !

!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'library/data types.f90'
include 'library/other types.f90'
include 'library/timer classes.f90'
!include 'library/node class.f90'
!include 'library/mod ave.f90'
!include 'library/other types.f90'
!include 'library/lattice class.f90'

program z2

  ! bring structures and procedures in via modules
  use data_types
  use lattice_types
  use cpu_timer_class
  use clock_timer_class

  implicit none

  integer ( is ), parameter             :: num_dim = 4_is                      !  INPUT
  integer ( is ), parameter             :: lattice_size = 5_is                 !  INPUT
  integer ( is ),                       :: extent ( 1 : num_dim ) = 10_is

  real ( wp )                             :: beta, beta_inc, action

  type ( lattice ) :: myLattice
  type ( cpu_timer )                      :: cpu_global                        ! instantiate timer instances for global execution
  type ( clock_timer )                    :: clock_global

!   ASSIGNMENTS
!    file_name = trim ( "characterization.txt" )

    call cpu_global   % cpu_start_timer   ( )                                  ! CPU time in measurement
    call clock_global % clock_start_timer ( )                                  ! clock time in measurement

    write ( *, * ) "extent = ", extent

    write ( *, 100 )  cpu_global   % cpu_elapsed_time   ( )                    !  record clock and CPU times
    write ( *, 110 )  clock_global % clock_elapsed_time ( )

100 format ( 'total cpu time   = ', g0, ' s' )
110 format ( 'total clock time = ', g0, ' s' )

  stop ( "end program z2" )

end program z2