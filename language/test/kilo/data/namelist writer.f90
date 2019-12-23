!
!	write namelist.f90
!
!   create NML files for lattice program
!
!	Created by Daniel M. Topa on 6/15/13.

include '/Users/dantopa/Dropbox/Fortran/data reduction/juliet/mod kind types.f90'
include '/Users/dantopa/Dropbox/Fortran/data reduction/juliet/mod constants and parameters.f90'

program nml_writer ! dimensionless variables, careful about energies

      use constants_and_parameters
      implicit NONE

      integer   ( lint )            :: num_intervals
      integer   ( lint )            :: io_status

      real      ( dp )              :: domain_a, domain_b                      ! x \in [ a, b ]
      real      ( dp )              :: xi_pressure,  xi_density  ! wall thickness
      real      ( dp )              :: eta_pressure, eta_density ! depth ( quadratic )
      real      ( dp )              :: xi = 0.001_dp

      character ( len = 64 )        :: file_name_locator
      character ( len = 64 )        :: file_name_domain, file_name_toy_pressure, file_name_toy_density
      character ( len = 64 )        :: dir_working

      namelist / locator /             file_name_domain, file_name_toy_pressure, file_name_toy_density
      namelist / domain /              num_intervals, domain_a, domain_b
      namelist / toy_pressure /        xi_pressure, eta_pressure
      namelist / toy_density /         xi_density,  eta_density

      call getcwd ( dir_working )
      print *, 'working directory = ', dir_working

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  IDENTIFY NML FILES

!     create locator file
      file_name_domain       = 'namelist domain.nml'
      file_name_toy_pressure = 'namelist toy pressure.nml'
      file_name_toy_density  = 'namelist toy density.nml'

include 'nml directory.f90'

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     MESH BASICS

!     create mesh parameters
      num_intervals = 5_lint
      domain_a      = zero
      domain_b      = one

include 'nml domain.f90'

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  PRESSURE

!     parabolic model
      xi_pressure        = xi
      eta_pressure       = -0.1_dp

include 'nml pressure.f90'

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  PRESSURE

!     parabolic model
      xi_density        = xi
      eta_density       = 0.05_dp

include 'nml density.f90'

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      END MAIN

      stop 'program namelist writer'

end program nml_writer