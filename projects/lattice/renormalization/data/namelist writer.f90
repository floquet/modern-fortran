!
!	write namelist.f90
!
!   create NML files for lattice program
!
!	Created by Daniel M. Topa on 6/15/13.

include '/Users/dantopa/Dropbox/Fortran/library/data_types.f90'

program nml_writer ! dimensionless variables, careful about energies

      use data_types
      implicit NONE

      integer   ( sint ), parameter :: ndim        = 4
      integer   ( sint ), parameter :: io_unit_nml = 1

      integer   ( sint )            :: dim, nml_unit, io_status
      integer   ( lint )            :: npts, nsweeps, nsamples

      integer   ( sint )            :: extent         ( 1 : ndim )
      integer   ( sint )            :: canonical_seed ( 1 : 14 )
      integer   ( sint )            :: new_seed       ( 1 : 14 )

      real      ( dp )              :: isotropy ( 1 : ndim )
      real      ( dp )              :: alpha, beta, df

      character ( len = 16 )        :: descriptor
      character ( len = 64 )        :: file_name_command, file_name_param_base, file_name_mesh, file_name_param_update, dir_working

      logical                       :: use_nml_seed_flag = .false.   ! is this seed used?

      namelist / mesh_params /      extent, isotropy, descriptor
      namelist / run_params /       alpha, beta, df, nsweeps, nsamples, canonical_seed, use_nml_seed_flag, new_seed
      namelist / command_params /   file_name_mesh, file_name_param_base, file_name_param_update

      call getcwd ( dir_working )
      print *, 'working directory = ', dir_working

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  COMMAND FILE

!     create command file
      file_name_mesh         = 'namelist mesh primus.nml'
      file_name_param_base   = 'namelist run parameters base.nml'
      file_name_param_update = 'namelist run parameters update.nml'

include 'nml command.f90'

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     MESH BASICS

!     create mesh parameters
      dim        = ndim
      descriptor = 'primus'
      extent     = 20 * [ 1, 1, 1, 1 ]
      npts       = product ( extent )
      isotropy   = [ 1._dp, 1._dp, 1._dp, 1._dp ]

include 'nml mesh.f90'

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  RUN PARAMETERS

!     create master table of simulation parameters
      alpha              = 1.0_dp
      beta               = 1.0_dp
      df                 = 1.3E-03
      nsweeps            = 1000
      nsamples           =    2
      canonical_seed     = [ 155719726, 156199294, 156319186, 156439078, 156678862, 156918646, 157198394, 157318286, 157398214, &
                             157438178, 157518106, 157877782, 157997674, 158237458 ]
      use_nml_seed_flag = .true.
      new_seed           = 0

include 'nml param base.f90'

!     create local table of simulation parameters
      alpha = 9.765625E-004

include 'nml param update.f90'

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      END MAIN

      stop 'program write namelist'

end program nml_writer