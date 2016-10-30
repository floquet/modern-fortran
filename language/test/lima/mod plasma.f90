module plasma

  use constants_and_parameters
  implicit none

  ! derived data
  type, public                                   :: thermo    !  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
  !  define the inputs
  real ( dp ),      allocatable, dimension ( : ) :: alpha_list, j_list
  real ( dp ),      allocatable, dimension ( : ) :: LOS_list, pressure_list, density_list, temperature_list
  integer ( lint ), allocatable, dimension ( : ) :: LOS_indices

  integer ( lint )                               :: num_intervals

  real ( dp )                                    :: temperature_max = zero, temperature_min = one
  real ( dp )                                    :: pressure_max    = zero, pressure_min    = one
  real ( dp )                                    :: density_max     = zero, density_min     = one

  ! these variables are used to match computed temperature and density to HELIOS indices
  real ( dp )                                    :: density_intcpt     = zero, density_slope     = one
  real ( dp )                                    :: temperature_intcpt = zero, temperature_slope = one

  real ( dp )                                    :: temperature_index_max = 81.0_dp, temperature_index_min = one
  real ( dp )                                    :: density_index_max     = 51.0_dp, density_index_min     = one

  real ( dp )                                    :: boundary_left   = zero
  real ( dp )                                    :: boundary_right  = one
  real ( dp )                                    :: boundary_length = zero                          ! physical units


  real ( dp )                                    :: map_slope, map_intpt                            ! line of sight -> capsule

  contains                                                                                          ! bound procedures

    ! functions
    procedure, public                            :: toy                     =>    toy_fcn
    procedure, public                            :: g                       =>    g_fcn
    procedure, public                            :: X                       =>    X_fcn
    procedure, public                            :: Y                       =>    Y_fcn

    ! subroutines
    procedure, public                            :: create_mesh             =>    create_mesh_sub
    procedure, public                            :: populate_temperature    =>    populate_temperature_sub
    procedure, public                            :: populate_pressure       =>    populate_pressure_sub
    procedure, public                            :: populate_density        =>    populate_density_sub

  end type                                          thermo    !  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *

! subroutines
  private                                        :: create_mesh_sub
  private                                        :: populate_temperature_sub
  private                                        :: populate_pressure_sub
  private                                        :: populate_density_sub

! functions
  private                                        :: toy_fcn
  private                                        :: g_fcn
  private                                        :: X_fcn
  private                                        :: Y_fcn

  contains                                                                                      ! methods: subroutines and functions

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                lattice_action_09

    subroutine create_mesh_sub ( self, num_intervals, boundary_right, boundary_left )

      class   ( thermo ), target                 :: self

      real    ( dp ),   intent ( in )            :: boundary_right, boundary_left
      integer ( lint ), intent ( in )            :: num_intervals
      real    ( dp ),   dimension ( : ), pointer :: LOS, density, pressure, temperature
      integer ( lint ), dimension ( : ), pointer :: indices
      integer ( lint )                           :: k                 !  dummy counter
      integer ( lint )                           :: alloc_status      !  allocation error status flag

      character ( 64 )                           :: err_msg_allocate  !  allocation error message

!     ------------------------------------------------------------------------------------------------------ domain variables

      ! check for valid number of intervals
      if ( num_intervals > 0_lint ) then
        self % num_intervals = num_intervals
      else
        write ( * , * ) 'Error in specifying the number of intervals'
        write ( * , * ) 'Value must exceed 0: requested value is ', num_intervals
        stop 'FAIL: input data error'
      end if

      ! check boundary values
      if ( boundary_right == boundary_left ) then
        write ( * , * ) 'Error in specifying the size of the domain in capsule space'
        write ( * , * ) 'Right and left values are the same: ', boundary_right
        stop 'FAIL: input data error'
      end if

      ! enforce Archimedean ordering
      if ( boundary_right > boundary_left ) then
        self % boundary_right = boundary_right
        self % boundary_left  = boundary_left
      else
        self % boundary_right = boundary_left
        self % boundary_left  = boundary_right
      end if

      ! prepare the mapping from capsule space to interval number
      self % boundary_length = self % boundary_right - self % boundary_left
      self % map_slope       = self % boundary_length / self % num_intervals
      self % map_intpt       = self % boundary_left

!     ------------------------------------------------------------------------------------------------------ allocate memory

!      indices     => self % LOS_indices
!      LOS         => self % LOS_list
!      density     => self % density_list
!      pressure    => self % pressure_list
!      temperature => self % temperature_list

      allocate ( self % LOS_indices ( 0 : num_intervals ), stat = alloc_status, errmsg = err_msg_allocate )
      if ( alloc_status /= 0 ) then
        write ( *, * ) 'failure to allocate integer lint array LOS_indices ( 0 : ', num_intervals, ' )'
        write ( *, * ) 'allocation status variable = ', alloc_status
        write ( *, * ) 'error message              = ', err_msg_allocate
        stop 'memory allocation failure for Line Of Sight indices'
      end if

      allocate ( self % LOS_list ( 0 : num_intervals ), stat = alloc_status, errmsg = err_msg_allocate )
      if ( alloc_status /= 0 ) then
        write ( *, * ) 'failure to allocate real dp array LOS_list ( 0 : ', num_intervals, ' )'
        write ( *, * ) 'allocation status variable = ', alloc_status
        write ( *, * ) 'error message              = ', err_msg_allocate
        stop 'memory allocation failure for Line Of Sight mesh'
      end if

      allocate ( self % pressure_list ( 0 : num_intervals ), stat = alloc_status, errmsg = err_msg_allocate )
      if ( alloc_status /= 0 ) then
        write ( *, * ) 'failure to allocate real dp array pressure_list ( 0 : ', num_intervals, ' )'
        write ( *, * ) 'oddly, the preceding allocation for LOS_list was successful'
        write ( *, * ) 'allocation status variable = ', alloc_status
        write ( *, * ) 'error message              = ', err_msg_allocate
        stop 'memory allocation failure for list of pressure values on mesh'
      end if

      allocate ( self % density_list ( 0 : num_intervals ), stat = alloc_status, errmsg = err_msg_allocate )
      if ( alloc_status /= 0 ) then
        write ( *, * ) 'failure to allocate real dp array density_list ( 0 : ', num_intervals, ' )'
        write ( *, * ) 'curiously, the preceding allocations for LOS_list and density_list were successful'
        write ( *, * ) 'allocation status variable = ', alloc_status
        write ( *, * ) 'error message              = ', err_msg_allocate
        stop 'memory allocation failure for list of density values on mesh'
      end if

      allocate ( self % temperature_list ( 0 : num_intervals ), stat = alloc_status, errmsg = err_msg_allocate )
      if ( alloc_status /= 0 ) then
        write ( *, * ) 'failure to allocate real dp array temperature_list ( 0 : ', num_intervals, ' )'
        write ( *, * ) 'strangely, the preceding allocations for LOS_list, pressure_list, and density_list were successful'
        write ( *, * ) 'allocation status variable = ', alloc_status
        write ( *, * ) 'error message              = ', err_msg_allocate
        stop 'memory allocation failure for list of temperature values mesh'
      end if

      allocate ( self % alpha_list ( 0 : num_intervals ), stat = alloc_status, errmsg = err_msg_allocate )
      if ( alloc_status /= 0 ) then
        write ( *, * ) 'failure to allocate real dp array alpha_list ( 0 : ', num_intervals, ' )'
        write ( *, * ) 'preceding allocations for LOS_list, pressure_list, density_list, and temperature_list were successful'
        write ( *, * ) 'allocation status variable = ', alloc_status
        write ( *, * ) 'error message              = ', err_msg_allocate
        stop 'memory allocation failure for list of alpha values (opacity) mesh'
      end if

      allocate ( self % j_list ( 0 : num_intervals ), stat = alloc_status, errmsg = err_msg_allocate )
      if ( alloc_status /= 0 ) then
        write ( *, * ) 'failure to allocate real dp array j_list ( 0 : ', num_intervals, ' )'
        write ( *, * ) 'all preceding allocations for real dp arrays successful: &
                        LOS_list, pressure_list, density_list, temperature_list, and alpha'
        write ( *, * ) 'allocation status variable = ', alloc_status
        write ( *, * ) 'error message              = ', err_msg_allocate
        stop 'memory allocation failure for list of j values (emissivity) mesh'
      end if


    end subroutine create_mesh_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  toy EOS
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine populate_temperature_sub ( self )           ! ideal gas law

      class ( thermo )                       :: self

      ! ideal gas law EOS
      self % temperature_list = self % pressure_list / self % density_list

      self % temperature_max = maxval ( self % temperature_list )
      self % temperature_min = minval ( self % temperature_list )

!     map computed slope to HELIOS indices
      self % temperature_slope  = ( self % temperature_index_max - self % temperature_index_min ) * one / &
                                  ( self % temperature_max - self % temperature_min )
      self % temperature_intcpt = self % temperature_slope

    end subroutine populate_temperature_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  emmissivity
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine populate_j_sub ( self ) 

      class ( thermo )                         :: self

      real ( dp ), intent ( in )               :: xi   ! wall thickness
      integer ( lint )                         :: k

      do k = 0, self % num_intervals
        self % j_list ( k ) = dot ( amplitudes ( self ),
                                    basis ( self, self % temperature_list, self % density_list )
      end do

    end subroutine populate_j_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  create toy models
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine populate_pressure_sub ( self, xi, eta )           ! parabolic profiles for inside the capsule

      class ( thermo )                         :: self

      real ( dp ), intent ( in )               :: xi   ! wall thickness
      real ( dp ), intent ( in )               :: eta  ! well depth
      integer ( lint )                         :: k

      do k = 0, self % num_intervals
        self % pressure_list ( k ) = toy_fcn ( self, k, xi, eta )
      end do
!      self % pressure_list = toy_fcn ( self, self % LOS_indices, xi, eta )

      self % pressure_max = maxval ( self % pressure_list )
      self % pressure_min = minval ( self % pressure_list )

    end subroutine populate_pressure_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  create toy models
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine populate_density_sub ( self, xi, eta )           ! parabolic profiles for inside the capsule

      class ( thermo )                       :: self

      real ( dp ), intent ( in )             :: xi   ! wall thickness
      real ( dp ), intent ( in )             :: eta  ! well depth
      integer ( lint )                                         :: k

!      self % density_list = toy_fcn ( self, indices, xi, eta )
      do k = 0, self % num_intervals
        self % density_list ( k ) = toy_fcn ( self, k, xi, eta )
      end do

      self % density_max = maxval ( self % density_list )
      self % density_min = minval ( self % density_list )

!     could a pointer to a pressure structure clean up the typing
!     map computed slope to HELIOS indices
      self % density_slope  = ( self % density_index_max - self % density_index_min ) * one / &
                              ( self % density_max - self % density_min )
      self % density_intcpt = self % density_slope

    end subroutine populate_density_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  create toy models
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    elemental function toy_fcn ( self, k, xi, eta ) result ( y )          ! parabolic profiles for inside the capsule

      class ( thermo ), intent ( in )        :: self

      integer ( lint ), intent ( in )        :: k      !  in capsule coordinates
      real ( dp ),      intent ( in )        :: xi     !  wall thickness
      real ( dp ),      intent ( in )        :: eta    !  well depth
      real ( dp )                            :: x      !  in capsule coordinates
      real ( dp )                            :: delta  !  avoid division by 0
      real ( dp )                            :: y

      delta = 0.00000095367431640625_dp                ! 2^(-20)  avoid division by 0

      x = g_fcn ( self, k )                            ! index -> capsule coordinates

      if ( x < ( one - xi ) ) then
        y = ( eta / ( one - xi ) ** 2 ) * x ** 2 + ( one - eta )  !  quadratic model
      else
        y = ( ( delta - one ) / xi ) * ( x - one ) + delta        !  linear model
      end if

    end function toy_fcn

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  map mesh to capsule space
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    elemental function g_fcn ( self, k ) result ( x )

      class ( thermo ), intent ( in )        :: self

      real ( dp )                            :: x
      integer ( lint ), intent ( in )        :: k

      x = self % map_slope * k + self % map_intpt

    end function g_fcn


!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  map index to temperature
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    elemental function X_fcn ( self, unscaled ) result ( scaled )

      class ( thermo ), intent ( in )        :: self

      real ( dp ), intent ( in )             :: unscaled
      real ( dp )                            :: scaled

      scaled = self % temperature_slope * unscaled + self % temperature_slope

    end function X_fcn

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  map index to temperature
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    elemental function Y_fcn ( self, unscaled ) result ( scaled )

      class ( thermo ), intent ( in )        :: self

      real ( dp ), intent ( in )             :: unscaled
      real ( dp )                            :: scaled

      scaled = self % density_slope * unscaled + self % density_slope

    end function Y_fcn

end module plasma