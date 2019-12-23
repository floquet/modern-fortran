module lattice_type

!  use parameters
  use averager
  use node_type
  use   cpu_timer_class
  use clock_timer_class
  implicit NONE

  ! derived data types
  type, public                             :: lattice                          ! name to instantiate
    private
    ! specify lattice
    type      ( node )                     :: nodes    ( 1 : num_nodes )       ! nodes where the fields live
    integer   ( lint )                     :: extent   ( 1 : num_dim )         ! spatial extent
    real      ( dp )                       :: isotropy ( 1 : num_dim )         ! spatial isotropy
    character ( len = 16 )                 :: descriptor                       ! tag data files
    ! scalar fields
    real      ( dp )                       :: phi_mean                         ! phi    scalar field mean
    real      ( dp )                       :: varphi_mean                      ! varphi scalar field mean
    real      ( dp )                       :: eker_mean                        ! eker   scalar field mean
    real      ( dp )                       :: phi_sd                           ! phi    scalar field standard deviation
    real      ( dp )                       :: varphi_sd                        ! varphi scalar field standard deviation
    real      ( dp )                       :: eker_sd                          ! eker   scalar field standard deviation

    real      ( dp )                       :: absphi_mean                      ! | phi    | scalar field mean
    real      ( dp )                       :: absvarphi_mean                   ! | varphi | scalar field mean
    real      ( dp )                       :: abseker_mean                     ! | eker   | scalar field mean
    real      ( dp )                       :: absphi_sd                        ! | phi    | scalar field standard deviation
    real      ( dp )                       :: absvarphi_sd                     ! | varphi | scalar field standard deviation
    real      ( dp )                       :: abseker_sd                       ! | eker   | scalar field standard deviation

    ! vector field
    real      ( dp )                       :: psi_mean ( 1 : num_dim )         ! vector field mean
    real      ( dp )                       :: psi_sd   ( 1 : num_dim )         ! vector field standard deviation

    real      ( dp )                       :: psi_mean_norm_L1                 ! vector field mean L1 norm
    real      ( dp )                       :: psi_mean_norm_L2                 ! vector field mean L2 norm
    real      ( dp )                       :: psi_mean_norm_Linf               ! vector field mean L-infinity norm
    real      ( dp )                       :: psi_norm_L1_max                  ! vector field L1 norm max
    real      ( dp )                       :: psi_norm_L2_max                  ! vector field L2 norm max
    real      ( dp )                       :: psi_norm_Linf_max                ! vector field L-infinity norm max
    integer   ( lint )                     :: psi_norm_L1_max_loc              ! linear address of L1 norm max
    integer   ( lint )                     :: psi_norm_L2_max_loc              ! linear address of L2 norm max
    integer   ( lint )                     :: psi_norm_Linf_max_loc            ! linear address of L-infinity norm max
    ! probes and diagnostics
    real      ( dp )                       :: rate_accept
    real      ( dp )                       :: sweeps_per_sec
    real      ( dp )                       :: processor_efficiency
    real      ( dp )                       :: cpu_elapsed_time
    real      ( dp )                       :: clock_elapsed_time

    ! census numbers
    integer   ( lint )                     :: count_sweeps                      ! counts number of sweeps
    integer   ( lint )                     :: count_measures                    ! counts number of sweeps
    integer   ( lint )                     :: count_accepts                     ! counts number of accepts
    integer   ( lint )                     :: count_rejects                     ! counts number of rejects

    character ( len = 30 )                 :: time_begin                        ! clock time at run start
    character ( len = 30 )                 :: time_end                          ! clock time at run stop
    character ( len = 32 )                 :: uuid                              ! unique identifier
    ! status flags
    logical                                :: ready_to_load                     ! is lattice ready for loading
    logical                                :: ready_to_run                      ! is lattice ready for sweeps and measures
    logical                                :: run_completed                     ! are sweeps and measures complete
    logical                                :: phi_loaded                        ! are initial values for scalar field phi loaded
    logical                                :: varphi_loaded                     ! are initial values for scalar field varphi loaded
    logical                                :: eker_loaded                       ! are initial values for scalar field eker loaded
    logical                                :: psi_loaded                        ! are initial values for vector field psi loaded

    ! instantiate timer instances for entire program
    type ( cpu_timer )                     :: cpu_self
    type ( clock_timer )                   :: clock_self
    type ( cpu_timer )                     :: tau

    contains                                                                ! bound procedures

      ! functions
      procedure, public                    :: multi_to_linear          =>    multi_addr_to_linear_addr_fcn
      procedure, public                    :: kernel                   =>    kernel_fcn
      procedure, public                    :: lattice_action_09        =>    lattice_action_09_fcn
      procedure, public                    :: lattice_action_10        =>    lattice_action_10_fcn
      procedure, public                    :: lattice_action_13        =>    lattice_action_13_fcn

      ! subs
      procedure, public                    :: goodbye                  =>    goodbye_sub
      procedure, public                    :: load_indices             =>    load_multi_indices_address_sub
      procedure, public                    :: init_lattice             =>    initialize_lattice_sub
      procedure, public                    :: load_lattice             =>    load_fields_from_list_sub
      procedure, public                    :: print_lattice            =>    print_lattice_sub
      procedure, public                    :: close_lattice            =>    close_lattice_sub
      procedure, public                    :: sweep_lattice_lite       =>    sweep_lattice_lite_sub
      procedure, public                    :: sweep_lattice_sim        =>    sweep_lattice_sim_sub
      procedure, public                    :: measure_lattice_lite     =>    measure_lattice_lite_sub
      procedure, public                    :: sweep_lattice_ord        =>    sweep_lattice_ord_sub
      procedure, public                    :: measure_lattice          =>    measure_lattice_sub
      procedure, public                    :: populate_neighbor_addr   =>    populate_neighbor_addr_sub
      procedure, public                    :: load_parameters          =>    load_lattice_parameters_sub
      procedure, public                    :: ping                     =>    ping_sub

  end type lattice
  ! end derived data type

  ! functions
  private                                  :: multi_addr_to_linear_addr_fcn
  private                                  :: kernel_fcn, lattice_action_09_fcn, lattice_action_10_fcn, lattice_action_13_fcn
  ! subs
  private                                  :: goodbye_sub, load_multi_indices_address_sub, initialize_lattice_sub
  private                                  :: print_lattice_sub, close_lattice_sub, sweep_lattice_ord_sub, measure_lattice_sub
  private                                  :: populate_neighbor_addr_sub, load_lattice_parameters_sub, sweep_lattice_lite_sub
  private                                  :: measure_lattice_lite_sub, load_fields_from_list_sub, sweep_lattice_sim_sub

  private                                  :: diagnostic_print_fields_sub, diagnostic_print_stats_sub, how_long_sub

  contains                                                                                    ! methods: subroutines and functions

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            sweep_lattice_sim_sub

    subroutine sweep_lattice_sim_sub ( self, params )                                         ! corresponding line numbers sim20.f95

      use other_types
      implicit none

      class ( lattice ), target              :: self
      type ( run_parameters ), intent ( in ) :: params

      ! arrays holding the values of the fields phi ( f ) and varphi ( E ) at the nearest neighbors to the nu-th point
      real ( dp ), pointer                   :: f ( : )
      real ( dp ), pointer                   :: E ( : )
      ! field values at the neighbors
      real ( dp )                            :: f_neighborhood         ( 1 : 2 * num_dim ) = zero
      real ( dp )                            :: new_eker_neighborhood  ( 1 : 2 * num_dim ) = zero
      real ( dp )                            :: old_eker_neighborhood  ( 1 : 2 * num_dim ) = zero
      real ( dp )                            :: diff_eker_neighborhood ( 1 : 2 * num_dim ) = zero
      real ( dp )                            :: offset                 ( 1 : 2 * num_dim ) = zero
      real ( dp )                            :: identity_vector        ( 1 : 2 * num_dim ) = one
      ! simulation parameters
      real ( dp )                            :: alpha = zero, beta = zero, b2 = zero, a4 = zero, hia = zero, df = zero
      ! helpful intermediates
      real ( dp )                            :: rnd = zero, deltaf = zero, oldf = zero, newf = zero, diff = zero
      real ( dp )                            :: newE = zero, oldE = zero, neweker = zero, new_eker_Linf = zero

      ! nu, mu point to field values on the lattice              1 <= nu, mu <= num_nodes
      ! j, k   point to field values in the subset of neighbors  1 <=  j,  k <= 2 * num_dim
      integer ( lint )                       :: nu                                            ! linear address of target node
      integer ( lint )                       :: mu, i, j, k, jump                             ! dummy indices
      integer ( lint )                       :: xi, eta                                       ! dummy indices
      integer ( lint )                       :: io_status                                     ! I/O status
      integer ( lint ), pointer              :: neighborhood ( : , : )                        ! linear address of the nearest nodes
      integer ( lint )                       :: neighbors    ( 1 : 2 * num_dim )              ! flatten ( neighborhood )

      character ( len =  5 )                 :: str                                           ! mark the sweep on file name
      character ( len = 10 )                 :: id                                            ! stamp the output files
      character ( len = 64 )                 :: io_msg, file_name_stats, file_name_trace      ! compiler error message

!     | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
      interface
        elemental function fcn ( x ) result ( y )                                             ! heavy usage
          use data_types
          real ( dp ), intent ( in )         :: x                                             ! x < 1
          real ( dp )                        :: y                                             ! y > 0
        end function fcn
      end interface
!     | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |

!     begin by grabbing a stopwatch
      call self % tau % cpu_timer_grab ( )                                                    ! time in measurement
      call self % tau % cpu_timer_pause ( )

!     simulation specific parameters                                                           annotations from sim20.f95
      df    = params % df
      alpha = params % alpha
      beta  = params % beta
      a4    = alpha ** 4                                                                      !                                 # 55
      b2    = half * beta  ** 2                                                               !                                 # 55
      hia   = one / ( 2 * alpha ) ** 2                                                        !                                 # 55

!     pointer associations
      f     => self % nodes %    phi                                                          ! Kevin: f
      E     => self % nodes % varphi                                                          ! Kevin: eker

      jump  = params % nsweeps / params % nsamples                                            ! thermalizations between measurements

!     looping
      sweep_measure:   do xi = 1, params % nsamples                                           ! loop over measurements
        sweep_thermal: do eta = 1, jump                                                       ! loop over a batch of thermalizations

          sweep_nodes: do nu = 1, num_nodes                                                   ! master loop nu = 1, num_nodes
            call random_number ( rnd )                                                        ! 0 <= rnd <= 1                  # 105
            deltaf = ( rnd - half ) * df                                                      ! translation, dilation          # 106
            oldf   = f ( nu )                                                                 !                                # 106
            newf   = oldf + deltaf                                                            !                                # 107
            offset = ( newf ** 2 - oldf ** 2 ) * identity_vector                              ! new energy kernel        # 121 - 128

!           create list of field values at the neighbors
            neighborhood => self % nodes ( nu ) % neighbors                                   ! linear address for neighbors
            neighbors    = [ ( ( neighborhood ( j, k ), j = 1, num_dim ), k = 1, 2 ) ]        ! flatten address array into a list
            f_neighborhood         = f ( neighbors )
            old_eker_neighborhood  = E ( neighbors )                                          ! new energy kernel        # 117 - 120
            diff_eker_neighborhood = f_neighborhood - newf

            diff    = hia * sum ( diff_eker_neighborhood * diff_eker_neighborhood )           ! faster than dot product it seems
            neweker = b2  * newf ** 2 + diff                                                  ! new energy kernel        # 108 - 116

            new_eker_neighborhood = old_eker_neighborhood &
                                  + hia * ( 2 * ( oldf - newf ) * f_neighborhood - offset )   ! new energy kernel        # 121 - 128
            new_eker_Linf         = maxval ( abs ( new_eker_neighborhood ) )

              ! level 1 if
              if ( neweker >= one  .or. new_eker_Linf >= one ) then                           ! new energy kernel        # 129 - 131

                self % count_rejects = self % count_rejects + 1                               !                                # 132

              else                                                                            ! new elements of energy         # 133

                newE = a4 * ( fcn ( neweker )  - 9.0_dp &
                     + sum  ( fcn ( new_eker_neighborhood ) ) )                               !                          # 134 - 138
                oldE = a4 * ( fcn ( E ( nu ) ) - 9.0_dp &
                     + sum  ( fcn ( old_eker_neighborhood ) ) )                               !                          # 139 - 143

                ! level 2 if
                if ( newE <= oldE ) then                                                      !                                # 144

                  self % count_accepts = self % count_accepts + 1                             !                                # 150
                  f ( nu ) = newf                                                             !                                # 145
                  E ( nu ) = newE                                                             !                                # 145

                  E ( neighbors ) = new_eker_neighborhood                                     !                          # 146 - 149

                else                                                                          !                                # 151

                  call random_number ( rnd )                                                  !                                # 152

                  ! level 3 if
                  if ( exp ( - ( newE - oldE ) ) >= rnd ) then                                !                                # 153
                    self % count_accepts = self % count_accepts + 1                           !                                # 159
                    f ( nu ) = newf                                                           !                                # 154
                    E ( nu ) = newE                                                           !                                # 154
                  else
                    self % count_rejects = self % count_rejects + 1                           !                                # 161
                  endif ! level 3:  ( exp ( - ( newE - oldE ) ) >= rnd )

                end if  ! level 2:  ( newE <= oldE )

              end if    ! level 1:  ( neweker >= one  .or. new_eker_Linf > = one )            !                                # 164

            end do sweep_nodes                                                                ! master loop nu = 1, num_nodes

        self % count_sweeps = self % count_sweeps + 1
        end do sweep_thermal                                                                  ! loop over a batch of thermalizations

        write  ( *, * )                                                                       ! status bar
        write  ( *, 100, advance = 'no' ) '['
        write  ( *, 100, advance = 'no' ) repeat ( '❚', xi )
        write  ( *, 100, advance = 'no' ) repeat ( ' ', params % nsamples - xi )
        write  ( *, 100, advance = 'no' ) ']'
100     format ( A )
        write  ( *, 110, advance = 'no' ) self % count_sweeps / 1000, dble( self % count_sweeps ) * 100 / params % nsweeps
110     format ( I10, ' kilosweeps completed, (', F5.1,'%)' )
        write  ( *, * )

        call self % cpu_self % cpu_timer_pause ( )
        call self % tau      % cpu_timer_resume ( )

        write ( id, '( I10 )' ) xi * jump  ! tag for output files
        call self % measure_lattice_lite ( params, id )
        call diagnostic_print_fields_sub ( xi * jump, f, E, self % descriptor )

        call self % tau      % cpu_timer_pause ( )
        call self % cpu_self % cpu_timer_resume ( )

      self % count_measures = self % count_measures + 1
      end do sweep_measure                                                                   ! loop over measurements  # 168

      print *, 'cpu time in measures = ', self % tau % cpu_timer_cum_read ( )

!     pointer deallocation (vestigial)
      f            => null ( )
      E            => null ( )
      neighborhood => null ( )

    end subroutine sweep_lattice_sim_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                sweep_lattice_sub

    subroutine sweep_lattice_lite_sub ( self, params )                                    ! corresponding line numbers in lite20.f95

      use other_types
      implicit none

      class ( lattice ), target              :: self
      type ( run_parameters ), intent ( in ) :: params

      ! arrays holding the values of the fields phi ( f ) and varphi ( E ) at the nearest neighbors to the nu-th point
      real ( dp ), pointer                   :: f ( : ), E ( : )
      ! field values at the neighbors
      real ( dp )                            :: f_neighborhood ( 1 : 2 * num_dim )
      ! simulation parameters
      real ( dp )                            :: alpha, beta, b2, a4, qia, df
      real ( dp )                            :: f_mean, f_sd, E_mean, E_sd
      ! helpful intermediates
      real ( dp )                            :: rnd, deltaf, oldf, newf, newE, oldE

      ! nu, mu point to field values on the lattice              1 <= nu, mu <= num_nodes
      ! j, k   point to field values in the subset of neighbors  1 <=  j,  k <= 2 * num_dim
      integer ( lint )                       :: nu                                           ! linear address of target node
      integer ( lint )                       :: mu, i, j, k, jump                            ! dummy indices
      integer ( lint )                       :: xi, eta                                      ! dummy indices
      integer ( lint )                       :: io_status                                    ! I/O status
      integer ( lint ), pointer              :: neighborhood ( : , : )                       ! linear address of the nearest nodes
      integer ( lint )                       :: vlist        ( 1 : 2 * num_dim )             ! flatten ( neighborhood )

      character ( len =  5 )                 :: str                                          ! mark the sweep on file name
      character ( len = 10 )                 :: id                                           ! stamp the output files
      character ( len = 64 )                 :: io_msg, file_name_stats, file_name_trace     ! compiler error message

      call self % tau % cpu_timer_grab ( )  ! time in measure

!     simulation specific parameters                                                           annotations from lite20.f95
      df    = params % df
      alpha = params % alpha
      beta  = params % beta
      a4    = alpha ** 4
      b2    = half * beta  ** 2
      qia   = one / ( 2 * alpha ) ** 2

!     pointer associations
      f     => self % nodes %    phi
      E     => self % nodes % varphi

      jump  = params % nsweeps / params % nsamples                                           ! thermalizations between measurements

!     looping
      sweep_measure:   do xi = 1, params % nsamples                                          ! loop over measurements
        sweep_thermal: do eta = 1, jump                                                      ! loop over a batch of thermalizations

          sweep_nodes: do nu = 1, num_nodes                                                  ! master loop nu = 1, num_nodes
            neighborhood => self % nodes ( nu ) % neighbors                                  ! linear address for neighbors
            call random_number ( rnd )                                                       ! 0 <= rnd <= 1                   # 104
            deltaf = ( rnd - half ) * df                                                     ! translation, dilation           # 105
            oldf   = f ( nu )                                                                !                                 # 106
            newf   = oldf + deltaf                                                           !                                 # 106

            if ( abs ( newf ) > one ) then                                                   ! n.b. abs in lieu of square      # 107

              self % count_rejects = self % count_rejects + 1                                !                                 # 108

            else ! compute new elements of energy

!             create f_neighborhood, list of f field values at the neighbors
!              vlist = [ ( ( neighborhood ( j, k ), j = 1, num_dim ), k = 1, 2 ) ]            ! flatten address array into a list
!              vlist = reshape ( neighborhood, [ 1 : 2 * num_dim ] ]                           ! flatten address array into a list
              f_neighborhood = [ ( f ( vlist ( k ) ), k = 1, 2 * num_dim ) ]
              f_neighborhood = f ( vlist )
              f_neighborhood = f_neighborhood - newf

              newE = b2 * ( one / ( one - newf ** 2 ) - one )                                ! new energy kernel         # 110 - 119
              newE = newE + qia * dot_product ( f_neighborhood, f_neighborhood )
              newE = a4 * newE

              if ( newE <= E ( nu ) ) then

                f ( nu ) = newf                                                              !                                 # 121
                E ( nu ) = newE                                                              !                                 # 121
                self % count_accepts = self % count_accepts + 1                              !                                 # 122

              else                                                                           !                                 # 123

                call random_number ( rnd )                                                   ! 0 <= rnd <= 1                   # 124
                if ( exp ( - ( newE - E ( nu ) ) ) >= rnd ) then                             !                                 # 125

                  f ( nu ) = newf                                                            !                                 # 126
                  E ( nu ) = newE                                                            !                                 # 126
                  self % count_accepts = self % count_accepts + 1                            !                                 # 127

                else                                                                         !                                 # 128

                  self % count_rejects = self % count_rejects + 1                            !                                 # 129

                end if  ! ( exp ( - ( newE - E ( nu ) ) ) >= rnd )

              end if    ! ( newE <= E ( nu ) )

            end if      ! ( abs ( newf ) > one )

          end do sweep_nodes                                                                 ! master loop nu = 1, num_nodes

          self % count_sweeps = self % count_sweeps + 1

        end do sweep_thermal                                                                 ! loop over a batch of thermalizations

        write  ( *, * )
        write  ( *, 100, advance = 'no' ) '['
        write  ( *, 100, advance = 'no' ) repeat ( '❚', xi )
        write  ( *, 100, advance = 'no' ) repeat ( ' ', params % nsamples - xi )
        write  ( *, 100, advance = 'no' ) ']'
100     format ( A )
        write  ( *, 110, advance = 'no' ) self % count_sweeps / 1000, dble( self % count_sweeps ) * 100 / params % nsweeps
110     format ( I10, ' kilosweeps completed, (', F5.1,'%)' )
        write  ( *, * )

      call self % cpu_self % cpu_timer_pause ( )

      call self % tau      % cpu_timer_resume ( )

      write ( id, '( I10 )' ) xi * jump
      call self % measure_lattice_lite ( params, id )
      call diagnostic_print_fields_sub ( xi * jump, f, E, self % descriptor )

      call self % tau      % cpu_timer_pause ( )

      call self % cpu_self % cpu_timer_resume ( )

      end do sweep_measure                                                                   ! loop over measurements

      print *, 'time in measure = ', self % tau % cpu_timer_cum_read ( )

!     pointer deallocation (vestigial)
      f            => null ( )
      E            => null ( )
      neighborhood => null ( )

    end subroutine sweep_lattice_lite_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                sweep_lattice_sub

    subroutine diagnostic_print_stats_sub ( k, f, E, file_name )

      use averager
      implicit none

      integer ( lint )                      :: io_status
      integer ( lint ), intent ( in )       :: k                                              ! self % count_sweeps

      real ( dp ),      intent ( in )       :: f ( 1 : num_nodes )
      real ( dp ),      intent ( in )       :: E ( 1 : num_nodes )
      real ( dp )                           :: out ( 1 : 2 )                                  ! mean and standard dev
      real ( dp )                           :: f_mean, f_sd
      real ( dp )                           :: E_mean, E_sd

      character ( len = 64 ), intent ( in ) :: file_name
      character ( len = 64 )                :: io_msg
      character ( len =  4 )                :: str

      ! global field properties
      out    = average ( f )
      f_mean = out ( 1 )
      f_sd   = out ( 2 )

      out    = average ( E )
      E_mean = out ( 1 )
      E_sd   = out ( 2 )

      write ( io_unit_stats, *, iostat = io_status, iomsg = io_msg ) k, sum ( abs ( f ) ), f_mean, f_sd, E_mean, E_sd
      if ( io_status /= 0 ) then
        write ( *, * ) 'write error on unit ', io_unit_stats, ' file name = ', file_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

    end subroutine diagnostic_print_stats_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                sweep_lattice_sub

    subroutine diagnostic_print_fields_sub ( k, f, E, lattice_descriptor )                          ! binary output

      use data_types
      implicit none

      integer ( lint )                      :: io_status
      integer ( lint ), intent ( in )       :: k                                                    ! self % count_sweeps
      real ( dp ),      intent ( in )       :: f ( 1 : num_nodes )
      real ( dp ),      intent ( in )       :: E ( 1 : num_nodes )

      character ( len = 10 )                :: str, suffix
      character ( len = 64 )                :: io_msg, f_name
      character ( len = 16 ), intent ( in ) :: lattice_descriptor

!     binary files are real, double precision
      suffix = '.r64'

      write ( str, '( I10 )', iostat = io_status, iomsg = io_msg ) k
      if ( io_status /= 0 ) then
        write ( *, * ) 'error on writing the integer self % count_sweeps = ', k, ' to "str"'
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      ! write f
      f_name = 'results/' // trim ( lattice_descriptor ) // 'f ' // str // suffix
      open  ( unit = io_unit_arrays, file = f_name, action = 'write', status = 'unknown', form = 'unformatted', access = 'stream', &
                                                                                        iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'file open error on unit ', io_unit_arrays, ' file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      write ( io_unit_arrays, iostat = io_status, iomsg = io_msg ) f
      if ( io_status /= 0 ) then
        write ( *, * ) 'write error on unit ', io_unit_arrays, ' file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      close ( unit = io_unit_arrays, iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'error closing unit ', io_unit_arrays, ' for file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      ! write E
      f_name = 'results/' // trim ( lattice_descriptor ) // 'E ' // str // suffix
      open  ( unit = io_unit_arrays, file = f_name, action = 'write', status = 'unknown', form = 'unformatted', access = 'stream', &
                                                                                        iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'file open error on unit ', io_unit_arrays, ' file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      write ( io_unit_arrays, iostat = io_status, iomsg = io_msg ) f
      if ( io_status /= 0 ) then
        write ( *, * ) 'write error on unit ', io_unit_arrays, ' file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      close ( unit = io_unit_arrays, iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'error closing unit ', io_unit_arrays, ' for file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

    end subroutine diagnostic_print_fields_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                sweep_lattice_sub

    subroutine measure_lattice_lite_sub ( self, params, id )                              ! corresponding line numbers in lite20.f95

      use other_types
      use averager
      implicit none

      class ( lattice ), target              :: self
      type ( run_parameters ), intent ( in ) :: params
      character ( len = * ),   intent ( in ) :: id

      ! arrays holding the values of the fields phi ( f ) and varphi ( E ) at the nearest neighbors to the nu-th point
      real ( dp ), pointer                   :: f ( : )
      real ( dp ), pointer                   :: E ( : )
      real ( dp )                            :: g              ( 1 : num_nodes )             ! used to build doubly even powers
      real ( dp )                            :: h              ( 1 : num_nodes )             ! used to build f ** 6

      real ( dp )                            :: f_neighborhood ( 1 : num_dim )               ! field values at down neighbors
      real ( dp )                            :: out            ( 1 : 2 )                     ! mean and standard deviation
      real ( dp )                            :: meanff         ( 1 : num_nodes, 1 : num_dim )
      real ( dp )                            :: two_pnt_fcn                                  ! dimensionless 2 point function

      integer ( lint )                       :: mu, nu, io                                   ! ... looping over spatial dimensions
      integer ( lint )                       :: neighborhood ( 1 : num_dim, 1 : 2 )          ! every dimension has up, down neighbor
      integer ( lint )                       :: vlist        ( 1 : num_dim )                 ! linear addresses, nearest down nodes

      integer ( lint )                       :: io_status

      character ( len = 64 )                 :: io_msg, file_name

!     Kevin's code: if ( temp == 'hot' .and. mod(isweep,5) == 0 )

      ! pointer associations
      f     => self % nodes %    phi
      E     => self % nodes % varphi

      ! averages
      out = average ( f )
      self % phi_mean    = out ( 1 )
      self % phi_sd      = out ( 2 )

      out = average ( abs( f ) )
      self % absphi_mean = out ( 1 )
      self % absphi_sd   = out ( 2 )

      out = average ( E )
      self % varphi_mean = out ( 1 )
      self % varphi_sd   = out ( 2 )

      sweep_nodes: do nu = 1, num_nodes

        ! create f_neighborhood, list of f field values at the neighbors
        neighborhood = self % nodes ( nu ) % neighbors                                        ! linear address for all neighbors
        vlist = [ ( neighborhood ( mu, 2 ), mu = 1, num_dim ) ]                               ! flatten address for down neighbors
        f_neighborhood = self % nodes ( vlist ) % phi                                         ! scalar field values, down neighbors

        sweep_down_neighbors: do io = 1, num_dim                                              ! loop encodes lines ...    # 153 - 158

          meanff ( nu, io ) = f ( nu ) * f_neighborhood ( io )

        end do sweep_down_neighbors

      end do sweep_nodes

      call self % measure_lattice ( )

      ! write means
      file_name = 'results/' // trim ( self % descriptor ) // 'means ' // id // '.out'
      open  ( unit = io_unit_measure, file = file_name, action = 'write', status = 'unknown', iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'file open error on unit ', io_unit_measure, ' file name = ', file_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      ! the write statements are self-documenting; little need for comments
      write  ( io_unit_measure, * )   'lattice and simulation:'
      write  ( io_unit_measure, 100 ) ' number of sweeps       =', self % count_sweeps
      write  ( io_unit_measure, 100 ) ' number of measurements =', self % count_measures
      write  ( io_unit_measure, 100 ) ' number of accepts      =', self % count_accepts
      write  ( io_unit_measure, 100 ) ' number of rejects      =', self % count_rejects
 100  format ( A , I12 )
      write  ( io_unit_measure,  * )  'size of lattice        =', self % extent

      write  ( io_unit_measure, * )
      write  ( io_unit_measure, * )   'inputs and derived quantities:'
      write  ( io_unit_measure, 110 ) ' df                  = ', params % df
      write  ( io_unit_measure, 110 ) ' alpha               = ', params % alpha
      write  ( io_unit_measure, 110 ) ' beta ( m / M )      = ', params % beta
      write  ( io_unit_measure, 120 ) one / ( 2 * params % alpha ) ** 2
 110  format ( A , G12.6 )
 120  format ( ' qia                 = ', G12.6, ' = ( 2 * alpha ) ** ( -2 )' )

      write  ( io_unit_measure, * )
      write  ( io_unit_measure, * )   'results:'
      write  ( io_unit_measure, 130 ) ' mean  E             = ', self % varphi_mean, self % varphi_sd
      write  ( io_unit_measure, 110 ) ' mean energy density = ', self % varphi_mean / ( self % count_sweeps * params % alpha ** 4 )
      write  ( io_unit_measure, 130 ) ' mean  f             = ', self %    phi_mean, self %    phi_sd
      write  ( io_unit_measure, 130 ) ' mean |f|            = ', self % absphi_mean, self % absphi_sd
 130  format ( A, G12.6, ' ± ', G12.3 )

      g = f * f   ! f ** 2
      out = average ( g )
      write  ( io_unit_measure, 130 ) ' mean  f ** 2        = ', out ( 1 ), out ( 2 )

      h = g * f   ! f ** 3
      h = h ** 2  ! f ** 6

      g = g * g   ! f ** 4
      out = average ( g )
      write  ( io_unit_measure, 130 ) ' mean  f ** 4        = ', out ( 1 ), out ( 2 )
      out = average ( h )
      write  ( io_unit_measure, 130 ) ' mean  f ** 6        = ', out ( 1 ), out ( 2 )

      g = g * g   ! f ** 8
      out = average ( g )
      write  ( io_unit_measure, 130 ) ' mean  f ** 8        = ', out ( 1 ), out ( 2 )

      do mu = 1, num_dim
        out = average ( meanff ( : , mu ) )
        write  ( io_unit_measure, 140 ) mu, out ( 1 ), out ( 2 )
 140    format ( ' mean ff (', I2, ' )       = ', G12.6, ' ± ', G12.3 )
      end do

      two_pnt_fcn = sum ( meanff ) / ( 4 * self % count_sweeps )
      write  ( io_unit_measure, * )   'dimensionless 2-point function                  =', two_pnt_fcn
      write  ( io_unit_measure, * )   'dimensionless 2-point function * ( a * M ) ** 2 =', two_pnt_fcn * params % alpha ** 2

      write  ( io_unit_measure, * )
      write  ( io_unit_measure, * )   'performance:'
      write  ( io_unit_measure, 110 ) ' sweeps per cpu second       = ', self % sweeps_per_sec
      write  ( io_unit_measure, 110 ) ' measurements per cpu second = ', one
      write  ( io_unit_measure, 110 ) ' processor sweep efficiency  = ', self % processor_efficiency

      call how_long_sub ( io_unit_measure, self %   cpu_elapsed_time, ' cpu time elapsed (s)        = ' )
      call how_long_sub ( io_unit_measure, self % clock_elapsed_time, ' clock time elapsed (s)      = ' )

      write  ( io_unit_measure, * )
      write  ( io_unit_measure, * ) self % time_end

      ! fin
      close ( unit = io_unit_measure, iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'error closing unit ', io_unit_measure, ' for file name = ', file_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      ! release pointer associations
      f     => null ( )
      E     => null ( )

    end subroutine measure_lattice_lite_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                sweep_lattice_sub

    subroutine ping_sub ( self, msg )

      implicit none

      class ( lattice )                    :: self
      character ( len = 8 ), intent ( in ) :: msg

      print *, trim( msg ), ': nodes ( 2 ) % addr ( 3 )  = ', self % nodes ( 2 ) % addr ( 3 )

    end subroutine ping_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                sweep_lattice_sub

    subroutine probe_print_sub ( self, j )

      implicit none

      class ( lattice )               :: self
      integer ( lint ), intent ( in ) :: j
      integer ( lint )                :: k
      character ( len = 64 )          :: file_name
      character ( len = 3 )           :: str

      write ( str, '( I3 )' ) j
      file_name = 'results/' // str // '.probe'
      open   ( unit = 6, file = file_name )
      do k = 1, num_nodes
        write  ( 6, 100 ) k, self % nodes ( k ) % addr
 100    format ( I6, 5X, 4I12 )
      end do
      close  ( 6 )

    end subroutine probe_print_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            sweep_lattice_ord_sub

    subroutine sweep_lattice_ord_sub ( self, params )

      use other_types
      implicit none

      class ( lattice ),       target        :: self
      type ( run_parameters ), intent ( in ) :: params

      real ( dp )                            :: new_eker       ( 1 : 2 * num_dim )           ! new energy kernel at neighbors
      real ( dp )                            :: old_eker       ( 1 : 2 * num_dim )           ! old energy kernel at neighbors
      real ( dp )                            :: f_neighborhood ( 1 : 2 * num_dim )           ! f at neighbors
      real ( dp )                            :: alpha, beta, df, a4, b2, hia                 ! simulation parameters
      real ( dp )                            :: newf, oldf, deltaf, rnd                      ! update f
      real ( dp )                            :: newE, oldE, neweker                          ! update E
      real ( dp ), pointer                   :: varphi ( : )                                 ! shorthand
      real ( dp ), pointer                   :: E ( : )                                      ! shorthand

      integer ( lint )                       :: neighborhood ( 1 : num_dim, 1 : 2 )          ! list of up, down neighbors at node nu
      integer ( lint )                       :: vlist        ( 1 : 2 * num_dim )             ! flattened list of up, down neighbors
      integer ( lint )                       :: mulist       ( 1 : 2 * num_dim )             ! 1, 2, 3, ... 2 * num_dim
      integer ( lint )                       :: jlist        ( 1 : 2 * num_dim )             !
      integer ( lint )                       :: jump                                         ! number of sweeps between measurements
      integer ( lint )                       :: i, j, k                                      ! dummy indices
      integer ( lint )                       :: xi, eta, nu                                  ! outer loop indices
      integer ( lint )                       :: mu                                           ! neighbor loop indices

      character ( len = 20 )                 :: id                                           ! stamp for data files

!     simulation specific parameters
      df    = params % df
      alpha = params % alpha
      beta  = params % beta
      a4    = alpha ** 4
      b2    = beta  ** 2
      hia   = one / ( 2 * alpha ) ** 2

      jump = params % nsweeps / params % nsamples                                            ! thermalizations between measurements

      ! connect fields to data structure
      varphi => self % nodes % varphi
      E => self % nodes % eker

      sweep_measure:   do xi  = 1, params % nsamples                                         ! loop over measurements
        sweep_thermal: do eta = 1, jump                                                      ! loop over a batch of thermalizations
          sweep_nodes: do nu  = 1, num_nodes                                                 ! master loop nu = 1, num_nodes

            call random_number ( rnd )                                                         ! 0 <= rnd <= 1                   # 105
            deltaf = ( rnd - half ) * df                                                       ! translation, dilation           # 106
            oldf   = varphi ( nu )                                                             !                                 # 107
            newf   = oldf + deltaf                                                             !                                 # 107

!           create f_neighborhood, list of f field values at the neighbors
            neighborhood = self % nodes ( nu ) % neighbors                                     ! linear address, 2 * num_dim neighbors
            vlist = [ ( ( neighborhood ( j, k ), j = 1, num_dim ), k = 1, 2 ) ]                ! flatten address array into a list
!            vlist = reshape ( neighborhood, [ 1 : 2 * num_dim ] ]                              ! flatten address array into a list
!            f_neighborhood = [ ( varphi ( vlist ( k ) ), k = 1, 2 * num_dim ) ]
            f_neighborhood = varphi ( vlist )
            f_neighborhood = f_neighborhood - newf                                             ! vector of differences
            neweker = b2 * newf ** 2 + hia * sum ( f_neighborhood * f_neighborhood )           ! compute the new energy kernel   # 108

!           create old_eker, list of eker field values at the neighbors
            mulist = [ ( k, k = 1, 2 * num_dim ) ]
            jlist  = vlist ( mulist )
!           old_eker = [ ( E ( vlist ( k ) ), k = 1, 2 * num_dim ) ]                           !                           # 117 - 120
            old_eker = E ( jlist )
            new_eker ( mulist )  = ( f_neighborhood ( mulist ) - newf ) ** 2 + ( f_neighborhood ( mulist ) - oldf ) ** 2
            new_eker ( mulist )  = hia * new_eker ( mulist ) + E ( jlist )

            ! energies
            newE = a4 * ( neweker  + sum ( new_eker ) ) / 2                                    !                           # 129 - 133
            oldE = a4 * ( E ( nu ) + sum ( new_eker ) ) / 2                                    !                           # 134 - 137

            ! acceptable configuration?
            call random_number ( rnd )                                                         !                                 # 147

            if ( newE <= oldE .or. exp ( -( newE - oldE ) ) >= rnd ) then                      !                            # 139, 148

              self % count_accepts = self % count_accepts + 1                                  !                            # 145, 155
              ! update the lattice
              varphi ( nu ) = newf                                                             !                                 # 140
              E      ( nu ) = neweker                                                          !                                 # 140

              E ( jlist )   = new_eker ( mulist )

            else

              self % count_rejects = self % count_rejects + 1                                  !                                 # 156

            end if                                                                             ! acceptable configuration?

          end do sweep_nodes                                                                 ! master loop nu = 1, num_nodes

          self % count_sweeps = self % count_sweeps + 1

        end do sweep_thermal                                                                 ! loop over a batch of thermalizations

        call self % cpu_self % cpu_timer_pause ( )                                           ! pause sweep timer
        call self % tau      % cpu_timer_resume ( )                                          ! activate measurement timer

        write ( id, '( A, I10 )' ) trim ( self % descriptor ), xi * jump                            ! e.g. 'ord     10000'
!        write ( id, '( A, I10 )' ) trim ( self % descriptor ), xi * jump                            ! e.g. 'ord     10000'
        call self % measure_lattice_lite ( params, trim ( id ) )                                      ! measurement
!        call diagnostic_print_fields_sub ( xi * jump, f, E, self % descriptor )

        call self % tau      % cpu_timer_pause ( )                                           ! pause sweep timer
        call self % cpu_self % cpu_timer_resume ( )                                          ! activate measurement timer

        write  ( *, * )
        write  ( *, 100, advance = 'no' ) '['
        write  ( *, 100, advance = 'no' ) repeat ( '❚', xi )
        write  ( *, 100, advance = 'no' ) repeat ( ' ', params % nsamples - xi )
        write  ( *, 100, advance = 'no' ) ']'
100     format ( A )
        write  ( *, 110, advance = 'no' ) self % count_sweeps / 1000, dble( self % count_sweeps ) * 100 / params % nsweeps
110     format ( I10, ' kilosweeps completed, (', F5.1,'%)' )
        write  ( *, * )

      end do sweep_measure                                                                   ! loop over measurements


    end subroutine sweep_lattice_ord_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                       kernel_fcn

!   Strinking Properties of Finite Nonrenormalizable Theories
!     Kevin Cahill

    real ( dp ) function kernel_fcn ( self, nu )                                 ! common component for vertex actions

      class ( lattice ), target       :: self

      real ( dp ), pointer            :: phi_0, phi_up, phi_dn                   ! shorthand for more legible code
      real ( dp )                     :: phi_sum_sq                              ! sum of squared differences

      integer ( lint ), intent ( in ) :: nu                                      ! self % nodes ( nu )
      integer ( lint )                :: mu                                      ! dummy variable for dimensions
      integer ( lint ), pointer       :: up, down                                ! linear address for nearest neighbors

      ! target node
      phi_0      => self % nodes ( nu ) % phi
      phi_sum_sq = 0.0_dp

      ! average the forward and reverse derivatives in the mu direction
      do mu = 1, num_dim

        up     => self % nodes ( nu )   % neighbors ( nu, 1 )                    ! linear address for nearest up neighbor
        down   => self % nodes ( nu )   % neighbors ( nu, 2 )                    ! linear address for nearest down neighbor

        phi_up => self % nodes ( up )   % phi                                    ! field value, up neighbor
        phi_dn => self % nodes ( down ) % phi                                    ! field value, down neighbor

        phi_up = phi_up * self % isotropy ( mu )                                 ! allow for anisotropy
        phi_dn = phi_dn * self % isotropy ( mu )                                 ! allow for anisotropy

        phi_sum_sq = phi_sum_sq + ( phi_0 - phi_up - phi_dn ) ** 2               ! recurrent theme

      end do                                                                     ! loop over dimensions

      kernel_fcn = phi_sum_sq

    end function kernel_fcn

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                lattice_action_09

!   Strinking Properties of Finite Nonrenormalizable Theories
!     Kevin Cahill

    real ( dp ) function lattice_action_09_fcn ( self, nu, a, m_lc, m_up )       ! find linear address of nearest neighbor nodes

      class ( lattice )                    :: self

      real ( dp ), optional, intent ( in ) :: a, m_lc, m_up                      ! m = m_lc, M = m_uc
      real ( dp )                          :: aM2
      real ( dp )                          :: S                                  ! vertex action
      integer ( lint ),      intent ( in ) :: nu                                 ! self % nodes ( nu )
      integer ( lint )                     :: mu                                 ! dummy variable for dimensions

!     intermediate variable
      aM2 = ( a * m_up ) ** 2

!     compute the vertex action
      S = kernel_fcn ( self, nu ) / aM2
      S = ( S + ( m_lc / m_up ) ** 2 * ( 1 / ( 1 - self % nodes ( nu ) % phi ** 2 ) - 1 ) ) / 2
      S = S * aM2 ** 2

      lattice_action_09_fcn = S

    end function lattice_action_09_fcn

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                lattice_action_10

!   Strinking Properties of Finite Nonrenormalizable Theories
!     Kevin Cahill

    real ( dp ) function lattice_action_10_fcn ( self, nu, a, m_lc, m_uc )       ! find linear address of nearest neighbor nodes

      class ( lattice )                    :: self

      real ( dp ), optional, intent ( in ) :: a, m_lc, m_uc                      ! m = m_lc, M = m_uc
      real ( dp )                          :: aM2
      real ( dp )                          :: S                                  ! vertex action
      integer ( lint ), intent ( in )      :: nu                                 ! self % nodes ( nu )
      integer ( lint )                     :: mu                                 ! dummy variable for dimensions

!     intermediate variable
      aM2 = ( a * m_uc ) ** 2

!     compute the vertex action
      S = kernel_fcn ( self, nu ) / aM2
      S = S / 2 +  ( ( m_lc / m_uc ) * self % nodes ( nu ) % phi ) ** 2
      S = sqrt ( 1 - S )
      S = aM2 ** 2 / S - aM2 ** 2

      lattice_action_10_fcn = S

    end function lattice_action_10_fcn

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                lattice_action_13

!   Strinking Properties of Finite Nonrenormalizable Theories
!     Kevin Cahill

    real ( dp ) function lattice_action_13_fcn ( self, nu, a, m )                ! find linear address of nearest neighbor nodes

      class ( lattice )                    :: self

      real ( dp ), optional, intent ( in ) :: a, m                               ! m = m_lc, M = m_uc ( M is not used in eqn 13 )
      real ( dp )                          :: am2
      real ( dp )                          :: S                                  ! vertex action
      integer ( lint ),      intent ( in ) :: nu                                 ! self % nodes ( nu )
      integer ( lint )                     :: mu                                 ! dummy variable for dimensions

!     intermediate variable
      am2 = ( a * m ) ** 2

!     compute the vertex action
      S = kernel_fcn ( self, nu ) / am2
      S = S / 2 + self % nodes ( nu ) % phi ** 2
      S = S * am2 ** 2 / 2

      lattice_action_13_fcn = S

    end function lattice_action_13_fcn

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       populate_neighbor_addr_sub

    subroutine populate_neighbor_addr_sub ( self )                        ! find linear address of nearest neighbor nodes

      class ( lattice ), target            :: self                        ! instantiate lattice

      integer ( lint )                     :: i, j, k, mu
      integer ( lint ), pointer            :: dir  ( : )                  ! extent
      integer ( lint ), pointer            :: p    ( : )                  ! multi-index for manipulation
      integer ( lint ), pointer            :: q    ( : , : )              ! neighbors
      integer ( lint )                     :: node ( 1 : num_dim )        ! destructible copy

      ! Certainly an argument can be made that if node b is the up neighbor to node a
      ! then we could assign node a as the down neighbor to node b.
      dir => self % extent                                                ! extents in each spatial direction

      ! sweep through the nodes
      sweep_nodes: do k = 1, num_nodes                                    ! k  loop over nodes

        p => self % nodes ( k ) % addr                                    ! multi-index address
        q => self % nodes ( k ) % neighbors                               ! neighbor list

        sweep_dimensions: do mu = 1, num_dim                              ! mu  loop over dimensions

          node = p                                                        ! maleable copy
          j = node ( mu ) + 1                                             ! increment for up neighbor       > > >
          if ( j > dir ( mu ) ) j = 1                                     ! cyclic increment
          node ( mu ) = j

          i = multi_addr_to_linear_addr_fcn ( self, node )                ! retrieve linear address
          q ( mu, 1 ) = i                                                 ! assign the index for the up neighbor

          node = p                                                        ! maleable copy
          j = node ( mu ) - 1                                             ! decrement for down neighbor     < < <
          if ( j < 1 ) j = dir ( mu )                                     ! cyclic decrement
          node ( mu ) = j

          i = multi_addr_to_linear_addr_fcn ( self, node )                ! retrieve linear address
          q ( mu, 2 )  = i                                                ! assign the index for the down neighbor

        end do sweep_dimensions                                           ! mu  loop over dimensions

      end do   sweep_nodes                                                ! k   loop over nodes

    end subroutine populate_neighbor_addr_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++              measure_lattice_sub

    subroutine measure_lattice_sub ( self )

      class ( lattice ), target            :: self                                         ! instantiate lattice

      real ( dp )                          :: out ( 1 : 2 )                                ! out = [ mean, sd ]

      real ( dp ), pointer                 :: psi      ( : )                               ! vector field

      real ( dp ), pointer                 :: phi      ( : )                               ! scalar field
      real ( dp ), pointer                 :: varphi   ( : )                               ! scalar field
      real ( dp ), pointer                 :: eker     ( : )                               ! scalar field
      real ( dp )                          :: old, new

      integer ( lint )                     :: k, mu                                        ! counters

      ! -----------------------------------  scalar fields
      phi    => self % nodes % phi
      varphi => self % nodes % varphi
      eker   => self % nodes % eker

      ! average scalar fields
      out = average ( phi )
      self % phi_mean    = out ( 1 )
      self % phi_sd      = out ( 2 )

      out = average ( varphi )
      self % varphi_mean = out ( 1 )
      self % varphi_sd   = out ( 2 )

      out = average ( eker )
      self % eker_mean   = out ( 1 )
      self % eker_sd     = out ( 2 )

      ! average magnitude of scalar fields
      out = average ( abs ( phi ) )
      self % absphi_mean    = out ( 1 )
      self % absphi_sd      = out ( 2 )

      out = average ( abs ( varphi ) )
      self % absvarphi_mean = out ( 1 )
      self % absvarphi_sd   = out ( 2 )

      out = average ( abs ( eker ) )
      self % abseker_mean   = out ( 1 )
      self % abseker_sd     = out ( 2 )

      ! -----------------------------------  vector field

      ! sweep through nodes and populate norms
      do k = 1, num_nodes
        call self % nodes ( k ) % psi % vector_norms ( )
      end do

      ! sweep through dims and average vector components
      do mu = 1, num_dim
        out = average   ( self % nodes % psi % field ( mu ) )
        self % psi_mean ( mu ) = out ( 1 )
        self % psi_sd   ( mu ) = out ( 2 )
      end do

      ! populate the data structure
      self % psi_mean_norm_L1   = sum    ( abs ( self % psi_mean ) )
      self % psi_mean_norm_L2   = norm2        ( self % psi_mean )
      self % psi_mean_norm_Linf = maxval ( abs ( self % psi_mean ) )

      self % count_measures     = self % count_measures + 1

    end subroutine measure_lattice_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                print_lattice_sub

    subroutine print_lattice_sub ( self )

      class ( lattice ), intent ( in )     :: self       ! instantiate lattice

      integer   ( lint ), parameter        :: n = 8      ! number of points to sample
      integer   ( lint )                   :: mu = 0, i= 0, j = 0, k = 0
      integer   ( lint )                   :: klist ( 1 : n ) = 0
      character ( len = 256 )              :: numbers = '', x = ''
      character ( len = 30 )               :: now = ''

      ! ***
      write  ( *, * )
      write  ( *, 100 ), trim ( self % descriptor )
 100  format ( 'lattice descriptor = ', A )

      ! lattice extent
      numbers = 'lattice extent     ='
      do k = 1, num_dim
        write ( x, '( I11 ) ' ) self % extent ( k )
        numbers = trim ( numbers ) // trim ( x )
      end do
      write  ( *, '( A )' ), trim ( numbers )

      ! lattice isotropy
      numbers = 'lattice isotropy   ='
      do k = 1, num_dim
        write ( x, '( G15.5 ) ' ) self % isotropy ( k )
        numbers = trim ( numbers ) // trim ( x )
      end do
      write  ( *, '( A )' ), trim ( numbers )

      ! create a list of the lowest and highest indices
      do k = 1, n / 2
        klist ( k ) = k
      end do
      j = n / 2
      do k = num_nodes - n / 2 + 1, num_nodes
        j = j + 1
        klist ( j ) = k
      end do

      ! output the node values
      write ( *, * )
      do j = 1, n
        k = klist ( j )
        i = multi_addr_to_linear_addr_fcn ( self, self % nodes ( k ) % addr )
        write ( *, * ) 'indices for node', k, '=', self % nodes ( k ) % addr, &
                       ': echo = ', i
      end do

      ! output the neighborhood
      sweep_dimension: do mu = 1, num_dim  ! loop over dimensions
      write ( *, * )
        sweep_neighbors: do j = 1, n       ! loop over node subset
          k = klist ( j )
          write  ( *, 110 ) mu, k, self % nodes ( k ) % neighbors ( mu, 1 ), &
                                   self % nodes ( k ) % neighbors ( mu, 2 )
 110      format ( 'dim = ', I2, ', node = ', I7, ': up = ', I7, ', dn = ', I7 )
        end do sweep_neighbors             ! loop over node subset
      end do sweep_dimension               ! loop over dimensions

      ! output the norm values
      write ( *, * )
      sweep_node_subset: do j = 1, n
        k = klist ( j )
        write  ( *, 120 ) k, self % nodes ( k ) % psi % norm_L1, &
                             self % nodes ( k ) % psi % norm_L2, &
                             self % nodes ( k ) % psi % norm_Linf
 120    format ( 'k =', I7, '; norm_L1 = ', G10.5, '; norm_L2 = ', G10.5, '; norm_Linf = ', G10.5 )
      end do sweep_node_subset

      write ( *, * )
      write ( *, * ) 'mean scalar field phi    value =', self % phi_mean,    ' ± ', self % phi_sd
      write ( *, * ) 'mean scalar field varphi value =', self % varphi_mean, ' ± ', self % varphi_sd
      write ( *, * ) 'mean scalar field eker   value =', self % eker_mean,   ' ± ', self % eker_sd
      write ( *, * ) 'mean vector field value        =', self % psi_mean
      write ( *, * ) '                               ±', self % psi_sd
      write ( *, * ) 'mean vector field norms: L1 =',    self % psi_mean_norm_L1, &
                                            '; L2 =',    self % psi_mean_norm_L2, &
                                            '; Linf =',  self % psi_mean_norm_Linf

      write ( *, * )
      write ( *, * ) 'sweep count   =', self % count_sweeps
      write ( *, * ) 'measure count =', self % count_measures

      write ( *, * )

    end subroutine print_lattice_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    load_lattice_from_list_sub

    subroutine load_fields_from_list_sub ( self, phi_list, varphi_list, eker_list, psi_list )

      implicit NONE

      class ( lattice )                    :: self

      real ( dp ), optional, intent ( in ) ::    phi_list ( 1 : num_nodes )               ! list of scalar field values
      real ( dp ), optional, intent ( in ) :: varphi_list ( 1 : num_nodes )               ! list of scalar field values
      real ( dp ), optional, intent ( in ) ::   eker_list ( 1 : num_nodes )               ! list of scalar field values
      real ( dp ), optional, intent ( in ) ::    psi_list ( 1 : num_nodes, 1 : num_dim )  ! list of vector field values
      real ( dp )                          ::    psi      ( 1 : num_dim )

      integer ( lint )                     :: j, k                                        ! dummy counters

      ! load input vectors
      if ( present ( phi_list ) ) then                                                    ! load scalar field values for phi
        do k = 1, num_nodes
          self % nodes ( k ) % phi = phi_list ( k )
        end do
        self % phi_loaded  = .true.
      end if

      if ( present ( varphi_list ) ) then                                                 ! load scalar field values for varphi
        do k = 1, num_nodes
          self % nodes ( k ) % varphi = varphi_list ( k )
        end do
        self % varphi_loaded  = .true.
      end if

      if ( present ( eker_list ) ) then                                                   ! load scalar field values for eker
        do k = 1, num_nodes
          self % nodes ( k ) % eker = eker_list ( k )
        end do
        self % eker_loaded  = .true.
      end if

      if ( present ( psi_list ) ) then                                                    ! load vector field values for psi
        do k = 1, num_nodes

          psi = [ ( psi_list ( k, j ), j = 1, num_dim ) ]

          self % nodes ( k ) % psi % field     = psi
          self % nodes ( k ) % psi % norm_L1   = sum    ( abs ( psi ) )
          self % nodes ( k ) % psi % norm_L2   = norm2        ( psi )
          self % nodes ( k ) % psi % norm_Linf = maxval ( abs ( psi ) )

        end do
        self % psi_loaded  = .true.
      end if

    end subroutine load_fields_from_list_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++              initialize_lattice_sub

    subroutine initialize_lattice_sub ( self )

      implicit NONE

      class ( lattice )                    :: self

      integer   ( lint )                   :: io_status
      character ( len = 6 )                :: file_name_uuid = 'uuid.out'
      character ( len = 30 )               :: timestamp
      character ( len = 32 )               :: uuid, io_msg

!     locate the neighbors
      call populate_neighbor_addr_sub ( self )

      ! zero out counters
      self % count_sweeps   = 0
      self % count_measures = 0
      self % count_accepts  = 0
      self % count_rejects  = 0

      ! time and date stamps
      self % time_begin     = timestamp ( )

      ! instantiate timer instances for entire program
      call self %   cpu_self % cpu_timer_grab ( )     ! track cpu time
      call self % clock_self % clock_start_timer ( )  ! track clock time

      ! clear for summation
      self % phi_mean    = zero;   self % phi_sd    = zero
      self % varphi_mean = zero;   self % varphi_sd = zero
      self % eker_mean   = zero;   self % eker_sd   = zero

!     The uuidgen program creates a new universally unique identifier (UUID) using the libuuid(3) library.
!     The new UUID can reasonably be considered unique among all UUIDs created on the local system, and among UUIDs
!     created on other systems in the past and in the future.

      ! This is a quick way to get a UUID.
      call execute_command_line ( 'rm ' // file_name_uuid )
      call execute_command_line ( 'uuidgen >> ' // file_name_uuid )

      ! open file containing uuid
      open ( unit = io_unit_system, file = file_name_uuid, iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then                               ! can't open file
        write ( *, * )
        write ( *, * ) 'unable to open file ', file_name_uuid
        write ( *, * ) 'trying to open the namelist mesh_params'
        write ( *, * ) 'io unit = ', io_unit_system
        write ( *, * ) 'iostat  = ', io_status
        write ( *, * ) 'iomsg   = ', io_msg
        write ( *, * ) 'Non-fatal error; continue with program execution'
      end if

      ! read file
      read ( unit = io_unit_system, fmt = '( A )', iostat = io_status, iomsg = io_msg ) uuid
      if ( io_status /= 0 ) then                               ! can't read file
        write ( *,  * )
        write ( *,  * ) 'unable to read file ', file_name_uuid
        write ( *,  * ) 'trying to read the namelist mesh_params'
        write ( *,  * ) 'io unit = ', io_unit_system
        write ( *,  * ) 'iostat  = ', io_status
        write ( *,  * ) 'iomsg   = ', io_msg
      end if

      ! close file
      close ( unit = io_unit_system, iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then                              ! can't close file
        write ( *,  * )
        write ( *,  * ) 'unable to close file ', file_name_uuid
        write ( *,  * ) 'trying to close the namelist mesh_params'
        write ( *,  * ) 'io unit = ', io_unit_system
        write ( *,  * ) 'iostat  = ', io_status
        write ( *,  * ) 'iomsg   = ', io_msg
      end if

      self % uuid = uuid
      call execute_command_line ( 'rm ' // file_name_uuid )

!     status flags
      self % ready_to_load = .true.
      self % ready_to_run  = .false.
      self %    phi_loaded = .false.
      self % varphi_loaded = .false.
      self %   eker_loaded = .false.
      self %    psi_loaded = .false.

    end subroutine initialize_lattice_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                         goodbye_sub

    subroutine goodbye_sub ( self, params )

      use other_types
      implicit NONE

      class     ( lattice )                       :: self
      type      ( run_parameters ), intent ( in ) :: params
      integer   ( lint )                          :: io_status
      character ( len = 30 )                      :: timestamp, io_msg

!      call diagnostic_print_fields_sub ( self % count_sweeps, self % nodes % phi, self % nodes % varphi )

      write ( *, * )
      write ( *, * ) '<---------------------  Summary report  --------------------->'
      write ( *, * )
      write ( *, '( "descriptor:                    ", A )' )       self % descriptor
      write ( *, * )
      write ( *, '( "kilosweeps completed:        ", I12 )' )       self % count_sweeps / 1000
      write ( *, '( "sweeps per CPU second:         ", G10.5 )' )   self % sweeps_per_sec
      write ( *, * )
      write ( *, '( "configurations accepted:       ", I12 )' )     self % count_accepts
      write ( *, '( "configurations rejected:       ", I12 )' )     self % count_rejects
      write ( *, '( "configuration acceptance rate: ", G10.5 )' )   self % rate_accept
      write ( *, * )

      call how_long_sub ( io_unit_default, self %   cpu_elapsed_time, "elapsed CPU time (sec) :       " )
      call how_long_sub ( io_unit_default, self % clock_elapsed_time, "elapsed clock time (sec) :     " )

      write ( *, '( "processor equivalence :        ", G10.5 )' )   self % processor_efficiency
      write ( *, * )
      write ( *, '( "universally unique identifier: ", A )' )       self % uuid
      write ( *, * )
      if ( params % use_nml_seed_flag ) then
        write ( *, '( "random number seed:            ", 14I12 )' ) params % canonical_seed  ! hardcode 14 - fix
      else
        write ( *, '( "random number seed:            ", 14I12 )' ) params % new_seed
      end if
      write ( *, * )
      write ( *, '( "time begin:                    ", A )' )       self % time_begin
      write ( *, '( "time end:                      ", A )' )       self % time_end
      write ( *, * )

    end subroutine goodbye_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                        how_long_sub

    subroutine how_long_sub ( io_unit_out, time_sec_fixed, descriptor )

      use data_types
      implicit NONE

      real ( dp )                           :: time_sec_volatile  ! local copy of time
      real ( dp )                           :: compare            ! convert to next larger unit
      real ( dp ),            intent ( in ) :: time_sec_fixed     ! time in seconds
      integer ( lint ),       intent ( in ) :: io_unit_out        ! where to write the results
      character ( len = 31 ), intent ( in ) :: descriptor         ! e.g. "elapsed CPU time (sec) :       "

!     when do we convert to next higher unit (e.g. seconds to minutes)?
      compare = 0.5_dp

      time_sec_volatile = time_sec_fixed      ! destructible copy

      write ( io_unit_out, '( A, G10.5 )', advance = 'no' ) descriptor, time_sec_volatile
      time_sec_volatile = time_sec_volatile / 60

      if ( time_sec_volatile >= compare ) then         ! minutes
        write ( io_unit_out, '( 5X, G10.5, "(min)" )', advance = 'no' ) time_sec_volatile
        time_sec_volatile = time_sec_volatile / 60

        if ( time_sec_volatile >= compare ) then       ! hours
          write ( io_unit_out, '( 5X, G10.5, "(hr)" )', advance = 'no' ) time_sec_volatile
          time_sec_volatile = time_sec_volatile / 24

          if ( time_sec_volatile >= compare ) then     ! days
            write ( io_unit_out, '( 5X, G10.5, "(day)" )', advance = 'no' ) time_sec_volatile

          end if                              ! days

        end if                                ! hours

      end if                                  ! minutes

      write ( io_unit_out, * )

    end subroutine how_long_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   close_lattice_sub

    subroutine close_lattice_sub ( self )

      implicit NONE

      class ( lattice )                    :: self
      character ( len = 30 )               :: timestamp

      self % run_completed = .true.

!     acceptance rate
      self % rate_accept = dble ( self % count_accepts ) / ( self % count_accepts + self % count_rejects )
!     timers
      self %   cpu_elapsed_time   = self %   cpu_self % cpu_timer_stop ( )      ! cpu time for sweeps
      self % clock_elapsed_time   = self % clock_self % clock_elapsed_time ( )  ! clock time for sweeps

      self % processor_efficiency = self % cpu_elapsed_time / self % clock_elapsed_time
      self % sweeps_per_sec       = dble ( self % count_sweeps ) / self % cpu_elapsed_time
      self % time_end             = timestamp ( )

    end subroutine close_lattice_sub

!   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    load_multi_indices_sub

    subroutine load_multi_indices_address_sub ( self )

      implicit NONE

      class ( lattice )                    :: self

      integer ( lint )                     :: mu                               ! loop over spatial dimensions
      integer ( lint )                     :: i, j, k, l                       ! dummy counters
      integer ( lint )                     :: repeat_index ( 1 : num_dim )
      integer ( lint )                     :: repeat_block ( 1 : num_dim )

!     repeat_index and repeat_block are two arrays used in index gymnastics
      repeat_index = 1
      repeat_block = repeat_index

!     how many times to repeat the mu-th index
      do j = 2, num_dim
        do k = j, num_dim
          repeat_index ( k ) = repeat_index ( k ) * self % extent ( j - 1 )
        end do
      end do

!     how many times to duplicate the block of repeated indices
      do j = num_dim, 2, -1
        do k = j - 1, 1, -1
          repeat_block ( k )  = repeat_block ( k ) * self % extent ( j )
        end do
      end do

!     sweep through lattice and assign addresses
      sweep_dimensions:     do mu = 1,      num_dim                            ! mu  sweep over the dimensions
        i = 0                                                                  ! counter goes from 1 to num_nodes
        sweep_repeat_block:   do l = 1,     repeat_block   ( mu )              ! l  # times to repeat blocks of indices
          sweep_index:          do k = 1,   self % extent  ( mu )              ! k  index to repeat
            sweep_repeat_index:   do j = 1, repeat_index   ( mu )              ! j  # times to repeat index

              i = i + 1                                                        ! increment counter
              self % nodes ( i ) % addr ( num_dim - mu + 1 ) = k               ! load the mu-th digit of the address

            end do sweep_repeat_index                                          ! j  # times to repeat index
          end do sweep_index                                                   ! k  index to repeat
        end do sweep_repeat_block                                              ! l  # times to repeat blocks of indices
      end do sweep_dimensions                                                  ! mu  sweep over the dimensions

    end subroutine load_multi_indices_address_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    multi_addr_to_linear_addr_fcn

    function multi_addr_to_linear_addr_fcn ( self, multi_address ) result ( linear_address )  ! e.g.  [ 1, 7, 19, 2 ] => 138542

      class ( lattice )                    :: self
!     extent describes lattice dimensions
!     click represents the indices for a specific node
      integer ( lint ), intent ( in )      :: multi_address     ( 1 : num_dim )               ! e.g.  [ 1, 7, 19, 2 ]
      integer ( lint )                     :: multi_address_rev ( 1 : num_dim )               ! e.g.  [ 1, 7, 19, 2 ]
      integer ( lint )                     :: linear_address                                  ! e.g.  138542

!     indices
      integer ( lint )                     :: j   ! accumulate the linear address
      integer ( lint )                     :: mu  ! dummy index to loop over spatial dimensions

!     builds products like extent ( 1 ) * extent ( 2 ) ... extent ( k )
      logical                              :: stencil ( 1 : num_dim )

!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           BEGIN

      do j = 1, num_dim
        multi_address_rev ( j ) = multi_address ( num_dim - j + 1 )
      end do

!     initial stencil
      stencil = .FALSE.

!     solution is accumulated
      j = multi_address_rev ( 1 )
!     include contributions from all other dimensions
      do mu = 2, num_dim
        stencil ( mu - 1 ) = .TRUE.
        j = j + ( multi_address_rev ( mu ) - 1 ) * product ( self % extent, mask = stencil )
      end do

      linear_address = j

    end function multi_addr_to_linear_addr_fcn

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      load_lattice_parameters_sub

    subroutine load_lattice_parameters_sub ( self, extent, isotropy, descriptor )

      use other_types
      implicit none

      class ( lattice )                     :: self

      real      ( dp ),       intent ( in ) :: isotropy ( 1 : num_dim )
      integer   ( lint ),     intent ( in ) :: extent   ( 1 : num_dim )
      character ( len = 16 ), intent ( in ) :: descriptor

      self % extent     = extent
      self % isotropy   = isotropy
      self % descriptor = descriptor

    end subroutine load_lattice_parameters_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                sweep_lattice_sub

    subroutine print_scalar_field_sub ( k, field, field_descriptor, lattice_descriptor )                             ! binary output

      use data_types
      implicit none

      integer ( lint )                      :: io_status
      integer ( lint ), intent ( in )       :: k                                              ! typically self % count_sweeps
      real ( dp ),      intent ( in )       :: field ( 1 : num_nodes )

      character ( len = 10 )                :: str, suffix
      character ( len = 64 )                :: io_msg, f_name
      character ( len = 16 ), intent ( in ) :: lattice_descriptor
      character ( len = 4 ),  intent ( in ) :: field_descriptor

!     binary files are real, double precision
      suffix = '.r64'

      write ( str, '( I10 )', iostat = io_status, iomsg = io_msg ) k
      if ( io_status /= 0 ) then
        write ( *, * ) 'error on writing the integer self % count_sweeps = ', k, ' to "str"'
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      ! write f
      f_name = 'results/' // trim ( lattice_descriptor ) // trim ( field_descriptor ) // str // suffix
      open  ( unit = io_unit_arrays, file = f_name, action = 'write', status = 'unknown', form = 'unformatted', access = 'stream', &
                                                                                        iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'file open error on unit ', io_unit_arrays, ' file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      write ( io_unit_arrays, iostat = io_status, iomsg = io_msg ) field
      if ( io_status /= 0 ) then
        write ( *, * ) 'write error on unit ', io_unit_arrays, ' file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

      close ( unit = io_unit_arrays, iostat = io_status, iomsg = io_msg )
      if ( io_status /= 0 ) then
        write ( *, * ) 'error closing unit ', io_unit_arrays, ' for file name = ', f_name
        write ( *, * ) 'iostat = ', io_status
        write ( *, * ) 'io_msg = ', io_msg
      end if

    end subroutine print_scalar_field_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

end module lattice_type