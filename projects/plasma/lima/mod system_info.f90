module system_info_mod

  use iso_fortran_env, only : compiler_options, compiler_version, REAL32, REAL64, REAL128
  use kind_types

  private  compiler_options, compiler_version

  ! dimensional parameters

  type                                      :: system_info
    integer   ( lint )                      :: uid, pid, gid
    character ( len = 64 )                  :: dir_home, dir_working
    character ( len = 64 )                  :: user_name
    character ( len = 64 )                  :: compiled_by   = compiler_version ( )
    character ( len = 64 )                  :: compiled_with = compiler_options  ( )


    contains                                                                ! bound procedures

      ! functions

      ! printing subroutines
      procedure, public                    :: print_compiler_info     =>    print_compiler_info_sub
      procedure, public                    :: print_system_info       =>    print_system_info_sub

      ! loading subroutines
      procedure, public                    :: load_system_ids         =>    load_system_ids_sub
      procedure, public                    :: load_network_info       =>    load_network_info_sub

  end type system_info

  ! end derived data type

  ! functions
  !  private                                  :: multi_addr_to_linear_addr_fcn

  ! subs
  private                                  :: print_compiler_info_sub, print_system_info_sub
  private                                  :: load_system_ids_sub, load_network_info_sub

  contains                                                                                    ! methods: subroutines and functions

!   P R I N T
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   information about the compiler

    subroutine print_system_info_sub ( self )

      class ( system_info ) :: self

      print *
      print *, 'User name  .  .  .  .  .  .  .  .  .  .  .  .  .  .  ', self % user_name
      print *, 'Numerical user ID of the current process .  .  .  .  ', self % uid
      print *, 'Numerical process identifier of the current process  ', self % pid

    end subroutine print_system_info_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   information about the compiler

    subroutine print_compiler_info_sub ( self )

      class ( system_info ) :: self

      print *
      print *, 'Fortran compiler version : ', self % compiled_by
      print *, 'Fortran compiler options : ', self % compiled_with
      print *
      print *, 'KIND types:'
      print *, 'REAL32  = ', REAL32
      print *, 'REAL64  = ', REAL64
      print *, 'REAL128 = ', REAL128

    end subroutine print_compiler_info_sub

!   L O A D
!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++             user and process IDs

    subroutine load_system_ids_sub ( self )

      use kind_types
      implicit none

      integer   ( lint )                      :: getuid, getpid, getgid

      class ( system_info ) :: self

      call getlog ( self % user_name )   ! moniker
      self % uid = getuid ( )            ! numerical user ID of the current process
      self % gid = getgid ( )            ! numerical group ID of the current process
      self % pid = getpid ( )            ! numerical process identifier of the current process

    end subroutine load_system_ids_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            sweep_lattice_sim_sub

    subroutine load_network_info_sub ( self )

      class ( system_info ) :: self

      call get_environment_variable ( 'HOME', self % dir_home )  !  /Users/dantopa
      call getcwd ( self % dir_working )                         ! ~Dropbox/Fortran/lattice/alpha

    end subroutine load_network_info_sub

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


end module system_info_mod