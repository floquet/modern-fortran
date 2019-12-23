module other_types

  use data_types
  implicit none

  type                                              :: run_parameters
    sequence
    integer ( lint )                                :: nsamples
    integer ( lint )                                :: nsweeps
    integer ( sint )                                :: canonical_seed ( 1 : 14 )  ! unhappy with the hardcode 14 for dimensions
    integer ( sint )                                :: new_seed       ( 1 : 14 )  ! unhappy with the hardcode 14 for dimensions
    real    ( dp )                                  :: alpha, beta, df
    logical                                         :: use_nml_seed_flag
  end type run_parameters

end module other_types