module averager

    use data_types
    implicit none

  ! routines handle the four basic data types
  interface average
    module procedure average_sp
    module procedure average_dp
    module procedure average_sint
    module procedure average_lint
  end interface average

  contains

! -----------------------------------------------

    function average_dp ( list ) result ( out )

      use data_types
      implicit none

      real ( dp ), intent ( in )      :: list ( : )
      real ( dp )                     :: out  ( 1 : 2 )
      real ( dp )                     :: mean, sd                             ! mean and standard deviation of list

      integer ( lint )                :: num_terms                            ! number of elements in list

!     begin by measuring list
      num_terms = size ( list )

      mean      = sum ( list ) / num_terms                                    ! mean

      sd        = sqrt ( sum ( ( list - mean ) ** 2 ) / num_terms )           ! standard deviation

      out       = [ mean, sd ]                                                ! return structure

    end function average_dp

! -----------------------------------------------

    function average_sp ( list ) result ( out )

      use data_types
      implicit none

      real ( sp ), intent ( in )      :: list ( : )
      real ( dp )                     :: out  ( 1 : 2 )
      real ( dp )                     :: mean, sd                             ! mean and standard deviation of list

      integer ( lint )                :: num_terms                            ! number of elements in list

      out = average_dp ( dble ( list ) )                                      ! return structure

    end function average_sp

! -----------------------------------------------

    function average_sint ( list ) result ( out )

      use data_types
      implicit none

      integer ( sint ), intent ( in ) :: list ( : )
      real ( dp )                     :: out  ( 1 : 2 )
      real ( dp )                     :: mean, sd                             ! mean and standard deviation of list

      integer ( lint )                :: num_terms                            ! number of elements in list

      out = average_dp ( dble ( list ) )                                      ! return structure

    end function average_sint

! -----------------------------------------------

    function average_lint ( list ) result ( out )

      use data_types
      implicit none

      integer ( lint ), intent ( in ) :: list ( : )
      real ( dp )                     :: out  ( 1 : 2 )
      real ( dp )                     :: mean, sd                             ! mean and standard deviation of list

      integer ( lint )                :: num_terms                            ! number of elements in list

      out = average_dp ( dble ( list ) )                                      ! return structure

    end function average_lint

end module averager