!------------------------------------------------------------------------------
! HPCMPO PETTT Program
!------------------------------------------------------------------------------
!
! MODULE: Main program
!
!> @author
!> Daniel Topa PETTT ACE on-site, Vicksburg
!
! DESCRIPTION:
!> Call the LAPACK SVD routines
!
! REVISION HISTORY:
! 10 Oct 2014 - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'library/basic parameters.f90'                                      ! load first for subsequent modules
include 'library/timer classes.f90'
include 'library/matrices.f90'
include 'library/measured type.f90'
include 'library/whoiam.f90'

program svd

  use basic_parameters                                                      ! data types defined
  use cpu_timer_class                                                       ! track CPU time
  use clock_timer_class                                                     ! track computation time
!  use matrix_class
  use examples

! DECLARATIONS
  implicit none

  integer ( is )                          :: io_status = 0, io_data = 0, status = 0

  real ( wp )                             :: M = one                        ! ambiguous majorization constant (gray box)

  character ( kind = ascii, len = 255 )   :: file_name = " ", io_msg = " "

  type ( cpu_timer )                      :: cpu_read                       ! instantiate timer instances for global execution
  type ( cpu_timer )                      :: cpu_ops                        ! instantiate timer instances for global execution
  type ( cpu_timer )                      :: cpu_global                     ! instantiate timer instances for global execution
  type ( clock_timer )                    :: clock_global

!   start timers
    call cpu_global   % cpu_start_timer   ( )                               ! CPU time in measurement
    call clock_global % clock_start_timer ( )                               ! clock time in measurement

!   important identifications
    call whoami ( )

!   read data files
    call cpu_read % cpu_start_timer ( )                                     ! start  CPU time for reading
    call read_data ( raw )



!   write data files
    call write_data ( raw )

!   SUCCESSFUL EXECUTION
    write ( *, 100 ) "binary file read ", cpu_read % cpu_elapsed_time ( )   ! output CPU time for reading
    write ( *, 100 ) "vector operations", cpu_ops % cpu_elapsed_time ( )    ! output CPU time for manipulation
    write ( *, 100 ) "total computation", cpu_global % cpu_elapsed_time ( ) ! output CPU time for total effort
    write ( *, 110 ) "total computation", clock_global % clock_elapsed_time ( ) ! output clock time for total effort

100 format ( "CPU time for ",   A, " = ", g0, " seconds" )
110 format ( "Clock time for ", A, " = ", g0, " seconds" )


end program corrode