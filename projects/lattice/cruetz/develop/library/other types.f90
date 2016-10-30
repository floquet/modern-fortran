module lattice_types

!  use parameters
  use data_types

  implicit NONE

  ! derived data types
  type, public             :: node                                             ! name to instantiate

    integer ( is )       :: neighbors ( 1 : num_dim, 1 : 2 )                 ! up and down neighbors
    integer ( is )       :: addr      ( 1 : num_dim )                        ! multi-index address [ x, y, z, t, ... ]

    real    ( dp )         :: phi                                              ! scalar fields

  end type node

  type, public                             :: lattice                          ! name to instantiate

!    private
    type      ( node )                     :: nodes    ( 1 : num_nodes )       ! nodes where the fields live
    integer   ( is )                       :: extent   ( 1 : num_dim )         ! spatial extent
    real      ( wp )                       :: isotropy ( 1 : num_dim )         ! spatial isotropy
    character ( len = 16 )                 :: descriptor                       ! tag data files

  end type lattice

end module lattice_types