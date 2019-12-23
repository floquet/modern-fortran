!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
include 'library/basic parameters.f90'                                      ! load first for subsequent modules
program svd

  use basic_parameters                                                      ! data types defined

! DECLARATIONS
  implicit none

  integer ( is ), parameter           :: rows = 4, cols = 5
  integer ( is ), parameter           :: nTerms = rows * cols

  real ( wp ), dimension ( 1:rows, 1:cols ) :: matrix
  real ( wp ), dimension ( 1:rows, 1:cols ) :: matrix

  list = [ ( real( k, wp ), k = 1, nTerms ) ]
  print *, 'list = ', g0

  DO r = 1, rows
     DO c = 1, cols
        dset_data( r, c ) = ( r - 1 ) * cols + c
     END DO
  END DO
