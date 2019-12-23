module vector_field

  use data_types
  implicit NONE

  type, public             :: vector                              ! name to instantiate

    real ( dp )            :: field ( 1 : num_dim )
    real ( dp )            :: norm_L1, norm_L2, norm_Linf

    contains

      ! subs
      procedure, public    :: vector_norms    =>    vector_norms_sub

  end type vector

  contains

    subroutine vector_norms_sub ( self )

      use data_types
      implicit NONE

      class ( vector )     :: self

      self % norm_L1   = sum    ( abs ( self % field ) )
      self % norm_L2   = norm2        ( self % field )
      self % norm_Linf = maxval ( abs ( self % field ) )

    end subroutine vector_norms_sub

end module vector_field

!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

module spinor_field

  use data_types
  implicit NONE

  type, private                                :: spinor                              ! name to instantiate

    complex ( dp ), dimension ( 1 : 2, 1 : 2 ) :: matrix_operator
    complex ( dp ), dimension ( 1 : 3 )        :: theta, theta_unit
    real    ( dp )                             :: theta_norm_L2
    real    ( dp )                             :: im_part, re_part

    contains                                                                       ! TOC for methods

      ! subs
      procedure, public                        :: build_spinor    =>    build_spinor_sub
!      procedure, public                        :: verify_unitary  =>    verify_unitary_sub

  end type spinor

  contains                                                                                    ! methods: subroutines and functions

!   compute the spinor using a 3-vector field and the Pauli matrices
    subroutine build_spinor_sub ( self )

      use data_types
      implicit NONE

      class ( spinor ), target                            :: self

      real    ( dp ), pointer                             :: norm_t
      complex ( dp ), pointer, dimension ( : )            :: unit_t, t
      complex ( dp ), pointer, dimension ( : , : )        :: m

      real                                                :: im_part, re_part
      integer ( lint )                                    :: k

      interface
        function Pauli ( index ) result ( pauli_matrix )
          use data_types
          integer ( lint ), intent ( in )                 :: index
          complex ( dp )                                  :: pauli_matrix ( 1 : 2, 1 : 2 )
        end function Pauli
      end interface

      ! pointer assignments
      t      => self % theta
      norm_t => self % theta_norm_L2
      unit_t => self % theta_unit
      m      => self % matrix_operator

      ! normalize the field theta
      norm_t = norm2 ( abs ( t ) )
      unit_t = t / norm_t

      ! real and imaginary components
      re_part = cos ( norm_t / 2 )
      im_part = sin ( norm_t / 2 )

      ! assemble the matrix U
      m = re_part * id_matrix_2

      do k = 1, 3
        m = m - im_part * unit_modulus * unit_t ( k ) * Pauli ( k )
      end do

      ! release pointers
      t      => null ( )
      norm_t => null ( )
      unit_t => null ( )
      m      => null ( )

    end subroutine build_spinor_sub

end module spinor_field

!     ########################################################################################################################     !
!     #                                                                                                                      #     !
!     ########################################################################################################################     !

module node_type

  use spinor_field
  use vector_field
!  use parameters
  implicit NONE

  type, public             :: node                              ! name to instantiate

    integer ( lint )       :: neighbors ( 1 : num_dim, 1 : 2 )  ! up and down neighbors
    integer ( lint )       :: addr      ( 1 : num_dim )         ! multi-index address [ x, y, z, t, ... ]

    real    ( dp )         :: phi, varphi, eker                 ! scalar fields
    type    ( vector )     :: psi                               ! vector fields

  end type node

end module node_type